import { bytesToBigInt } from "@ethereumjs/util";
import { bytesToHex, hexToBytes } from "ethereum-cryptography/utils";
import {
    ASTNode,
    ASTReader,
    ContractDefinition,
    EventDefinition,
    FunctionDefinition,
    FunctionVisibility,
    SourceUnit,
    StateVariableVisibility,
    TypeIdentifier,
    VariableDeclaration,
    assert,
    getABIEncoderVersion,
    repeat,
    signatureHash,
    toABIType,
    typeOf
} from "solc-typed-ast";
import { ABIEncoderVersion } from "solc-typed-ast/dist/types/abi";
import {
    detectArtifactCompilerVersion,
    getCodeHash,
    getCreationCodeHash
} from "../../artifacts/helpers";
import { PartialBytecodeDescription, PartialSolcOutput } from "../../artifacts/solc";
import { getFunctionSelector } from "../../utils/misc";
import { findContractDef, findFallbackFun, findReceiveFun } from "../../utils/solidity";
import { DecodedBytecodeSourceMapEntry, fastParseBytecodeSourceMapping } from "../../utils/srcmap";
import { OpcodeInfo } from "../opcodes";
import { EventDefInfo, EventDesc, HexString, UnprefixedHexString } from "../types";
import { BytecodeTemplate, makeTemplate, matchesTemplate } from "./bytecode_templates";
import {
    ArtifactInfo,
    BytecodeInfo,
    ContractInfo,
    ImmutableRefMap,
    LinkMap,
    LinkRefMap,
    SourceFileInfo,
    SourceFileType
} from "./types";

export interface IArtifactManager {
    getContractFromDeployedBytecode(code: Uint8Array): ContractInfo | undefined;
    getContractFromCreationBytecode(code: Uint8Array): ContractInfo | undefined;
    getContractFromMDHash(hash: HexString): ContractInfo | undefined;
    artifacts(): ArtifactInfo[];
    contracts(): ContractInfo[];
    // TODO: Need a better way of identifying runtime contracts than (bytecode, isCreation)
    getFileById(id: number, code: Uint8Array, isCreation: boolean): SourceFileInfo | undefined;
    findMethod(
        selector: HexString | Uint8Array
    ): [ContractInfo, FunctionDefinition | VariableDeclaration] | undefined;
    findEntryPoint(
        data: Uint8Array,
        contract: ContractInfo
    ): FunctionDefinition | VariableDeclaration | undefined;
    getEventDefInfo(topic: bigint | Uint8Array | EventDesc): EventDefInfo | undefined;
    getContractInfo(contract: ContractDefinition): ContractInfo | undefined;
    link(bytecode: BytecodeInfo, linkMap: LinkMap): Uint8Array;
}

/**
 * Build an offset-to-instruction index map for the given bytecode. Note
 * that since its not easy to tell exactly where the instruction section ends, we
 * over-approximate by also mapping any potential data sections at the end of bytecode.
 *
 * The main assumption we make is that all non-instruction bytecode comes at the end of the
 * bytecode.
 */
function buildOffsetToIndexMap(bytecode: Uint8Array | UnprefixedHexString): Map<number, number> {
    if (typeof bytecode === "string") {
        bytecode = hexToBytes(bytecode);
    }

    const res = new Map<number, number>();

    for (let i = 0, off = 0; off < bytecode.length; i++) {
        const op = OpcodeInfo[bytecode[off]];

        res.set(off, i);

        off += op.length;
    }

    return res;
}

export function getOffsetSrc(off: number, bytecode: BytecodeInfo): DecodedBytecodeSourceMapEntry {
    const idx = bytecode.offsetToIndexMap.get(off);

    assert(idx !== undefined, `No index for code offset ${off}`);
    assert(
        idx >= 0 && idx < bytecode.srcMap.length,
        `Instruction index ${idx} outside of source map (0-${bytecode.srcMap.length})`
    );

    return bytecode.srcMap[idx];
}

function buildBytecodeInfo(bytecodeInfo: PartialBytecodeDescription): BytecodeInfo {
    const generatedFileMap = new Map<number, SourceFileInfo>();

    if (bytecodeInfo.generatedSources) {
        for (const src of bytecodeInfo.generatedSources) {
            generatedFileMap.set(src.id, {
                rawAst: src.ast,
                ast: undefined,
                name: src.name ? src.name : "",
                contents: src.contents,
                type: SourceFileType.InternalYul,
                fileIndex: src.id
            });
        }
    }

    const linkReferences: LinkRefMap = new Map();

    if (bytecodeInfo.linkReferences) {
        for (const sourceUnitKey in bytecodeInfo.linkReferences) {
            for (const contractName in bytecodeInfo.linkReferences[sourceUnitKey]) {
                linkReferences.set(
                    `${sourceUnitKey}:${contractName}`,
                    bytecodeInfo.linkReferences[sourceUnitKey][contractName]
                );
            }
        }
    }

    const immutableReferences: ImmutableRefMap = new Map();

    if (bytecodeInfo.immutableReferences) {
        for (const id in bytecodeInfo.immutableReferences) {
            immutableReferences.set(Number(id), bytecodeInfo.immutableReferences[id]);
        }
    }

    let bytecodeSansLinkRefs = bytecodeInfo.object;

    if (bytecodeInfo.linkReferences !== undefined) {
        for (const fileName in bytecodeInfo.linkReferences) {
            for (const libName in bytecodeInfo.linkReferences[fileName]) {
                for (const range of bytecodeInfo.linkReferences[fileName][libName]) {
                    bytecodeSansLinkRefs =
                        bytecodeSansLinkRefs.slice(0, range.start * 2) +
                        repeat("00", range.length).join("") +
                        bytecodeSansLinkRefs.slice((range.start + range.length) * 2);
                }
            }
        }
    }

    // @todo This assumes all 32 byte link references. We could make this more generic by using the link references json
    const bytecodeObj = hexToBytes(bytecodeSansLinkRefs);

    return {
        generatedFileMap,
        srcMap: fastParseBytecodeSourceMapping(bytecodeInfo.sourceMap),
        offsetToIndexMap: buildOffsetToIndexMap(bytecodeObj),
        bytecode: bytecodeObj,
        linkReferences,
        immutableReferences
    };
}

/**
 * ArtifactManager contains a set of solc standard JSON compiler artifacts, and allows for quick
 * lookup from creation or deployed bytecode to the actual compiler artifact.
 */
export class ArtifactManager implements IArtifactManager {
    private _artifacts: ArtifactInfo[];
    private _contracts: ContractInfo[];
    private _mdHashToContractInfo: Map<string, ContractInfo>;
    private _creationBytecodeTemplates: BytecodeTemplate[];
    private _deployedBytecodeTemplates: BytecodeTemplate[];
    private _topicToEventInfo: Map<bigint, EventDefInfo>;

    private _unitToArtifact: Map<SourceUnit, ArtifactInfo>;
    private _contractToInfo: Map<ContractDefinition, ContractInfo>;

    /**
     * Helper to pick a canonical ABI encode version for a set of units.
     * For now just pick the highest version among the files
     * @todo (dimo) I am not sure this function is correct. Seems to work for now
     */
    private pickABIEncoderVersion(units: SourceUnit[], compilerVersion: string): ABIEncoderVersion {
        const versions = new Set<ABIEncoderVersion>(
            units.map((unit) => getABIEncoderVersion(unit, compilerVersion))
        );

        if (versions.has(ABIEncoderVersion.V2)) {
            return ABIEncoderVersion.V2;
        }

        return ABIEncoderVersion.V1;
    }

    constructor(artifacts: Array<PartialSolcOutput | [PartialSolcOutput, string]>) {
        this._artifacts = [];
        this._contracts = [];
        this._mdHashToContractInfo = new Map<string, ContractInfo>();
        this._creationBytecodeTemplates = [];
        this._deployedBytecodeTemplates = [];
        this._topicToEventInfo = new Map();

        for (const arg of artifacts) {
            const reader = new ASTReader();
            let artifact: PartialSolcOutput;
            let compilerVersion: string;

            if (arg instanceof Array) {
                [artifact, compilerVersion] = arg;
            } else {
                artifact = arg;
                const maybeCompilerVersion = detectArtifactCompilerVersion(artifact);
                assert(
                    maybeCompilerVersion !== undefined,
                    `Couldn't find compiler version for artifact`
                );

                compilerVersion = maybeCompilerVersion;
            }

            const units = reader.read(artifact);
            const abiEncoderVersion = this.pickABIEncoderVersion(units, compilerVersion);
            const fileMap = new Map<number, SourceFileInfo>();
            const unitMap = new Map<number, SourceUnit>(units.map((unit) => [unit.id, unit]));

            for (const fileName in artifact.sources) {
                const sourceInfo = artifact.sources[fileName];
                // TODO: This is hacky. Figure out a cleaner aay to get the fileIndex
                const fileIdx =
                    sourceInfo.fileIndex !== undefined ? sourceInfo.fileIndex : sourceInfo.id;

                fileMap.set(fileIdx, {
                    contents: sourceInfo.contents,
                    rawAst: sourceInfo.ast,
                    ast: unitMap.get(sourceInfo.ast.id),
                    name: fileName,
                    fileIndex: fileIdx,
                    type: SourceFileType.Solidity
                });
            }

            const srcMap = new Map<string, ASTNode>();

            for (const unit of units) {
                unit.walkChildren((child) => srcMap.set(child.src, child));
            }

            this._artifacts.push({
                artifact,
                units,
                ctx: reader.context,
                compilerVersion,
                abiEncoderVersion,
                fileMap,
                srcMap,
                codegen: "old"
            });
        }

        for (const artifactInfo of this._artifacts) {
            const artifact = artifactInfo.artifact;

            // Find all events and add them to the map
            for (const unit of artifactInfo.units) {
                const ctx = unit.requiredContext;
                unit.walkChildren((definition) => {
                    // @todo support anonymous events as well
                    if (definition instanceof EventDefinition && !definition.anonymous) {
                        const args: Array<[string, TypeIdentifier, boolean]> =
                            definition.vParameters.vParameters.map((d) => [
                                d.name,
                                toABIType(typeOf(d), ctx),
                                d.indexed
                            ]);

                        const topic = bytesToBigInt(signatureHash(definition));

                        const info: EventDefInfo = {
                            definition,
                            artifact: artifactInfo,
                            args
                        };

                        this._topicToEventInfo.set(topic, info);
                    }
                });
            }

            for (const fileName in artifact.contracts) {
                for (const contractName in artifact.contracts[fileName]) {
                    const contractDef = findContractDef(artifactInfo.units, fileName, contractName);
                    const contractArtifact = artifact.contracts[fileName][contractName];

                    if (contractArtifact.evm.deployedBytecode.object.length === 0) {
                        continue;
                    }

                    const hash = getCodeHash(contractArtifact.evm.deployedBytecode.object);
                    const bytecodeInfo = buildBytecodeInfo(contractArtifact.evm.bytecode);
                    const deployedBytecodeInfo = buildBytecodeInfo(
                        contractArtifact.evm.deployedBytecode
                    );

                    const contractInfo: ContractInfo = {
                        artifact: artifactInfo,
                        contractArtifact: contractArtifact,
                        fileName,
                        contractName,
                        ast: contractDef,
                        bytecode: bytecodeInfo,
                        deployedBytecode: deployedBytecodeInfo,
                        mdHash: hash
                    };

                    this._contracts.push(contractInfo);

                    if (hash !== undefined) {
                        this._mdHashToContractInfo.set(hash, contractInfo);
                    }

                    this._creationBytecodeTemplates.push(makeTemplate(bytecodeInfo));

                    this._deployedBytecodeTemplates.push(makeTemplate(deployedBytecodeInfo));
                }
            }
        }

        this._unitToArtifact = new Map();
        this._contractToInfo = new Map();

        for (const artifact of this._artifacts) {
            for (const unit of artifact.units) {
                this._unitToArtifact.set(unit, artifact);
            }
        }

        for (const contractInfo of this._contracts) {
            if (contractInfo.ast !== undefined) {
                this._contractToInfo.set(contractInfo.ast, contractInfo);
            }
        }
    }

    link(bytecode: BytecodeInfo, linkMap: LinkMap): Uint8Array {
        const linkedBytecode = new Uint8Array(bytecode.bytecode);

        for (const [libraryId, ranges] of bytecode.linkReferences) {
            const addr = linkMap.get(libraryId);
            assert(addr !== undefined, `Missing link information for ${libraryId}`);

            for (const range of ranges) {
                assert(range.length === 20, ``);
                linkedBytecode.set(addr.bytes, range.start);
            }
        }

        return linkedBytecode;
    }

    artifacts(): ArtifactInfo[] {
        return this._artifacts;
    }

    getContractInfo(nd: ASTNode): ContractInfo | undefined {
        const contract =
            nd instanceof ContractDefinition ? nd : nd.getClosestParentByType(ContractDefinition);

        if (contract === undefined) {
            return undefined;
        }

        return this._contractToInfo.get(contract);
    }

    getArtifact(nd: ASTNode): ArtifactInfo {
        const unit = nd instanceof SourceUnit ? nd : nd.getClosestParentByType(SourceUnit);
        assert(unit !== undefined, `No source unit for {0}`, nd);

        const artifact = this._unitToArtifact.get(unit);
        assert(artifact !== undefined, `No artifact info for unit {0}`, unit);

        return artifact;
    }

    getContractFromMDHash(hash: HexString): ContractInfo | undefined {
        return this._mdHashToContractInfo.get(hash);
    }

    getContractFromDeployedBytecode(bytecode: Uint8Array): ContractInfo | undefined {
        const hash = getCodeHash(bytecode);

        if (hash) {
            return this._mdHashToContractInfo.get(hash);
        }

        for (let i = 0; i < this._deployedBytecodeTemplates.length; i++) {
            const templ = this._deployedBytecodeTemplates[i];

            if (matchesTemplate(bytecode, templ, false)) {
                return this._contracts[i];
            }
        }

        return undefined;
    }

    getContractFromCreationBytecode(creationBytecode: Uint8Array): ContractInfo | undefined {
        const hash = getCreationCodeHash(creationBytecode);

        if (hash) {
            return this._mdHashToContractInfo.get(hash);
        }

        for (let i = 0; i < this._creationBytecodeTemplates.length; i++) {
            const templ = this._creationBytecodeTemplates[i];

            if (matchesTemplate(creationBytecode, templ, true)) {
                return this._contracts[i];
            }
        }

        return undefined;
    }

    getFileById(
        id: number,
        arg: Uint8Array | ContractInfo,
        isCreation: boolean
    ): SourceFileInfo | undefined {
        let contractInfo: ContractInfo | undefined;

        if (typeof arg === "string" || arg instanceof Uint8Array) {
            contractInfo = isCreation
                ? this.getContractFromCreationBytecode(arg)
                : this.getContractFromDeployedBytecode(arg);
        } else {
            contractInfo = arg;
        }

        if (contractInfo === undefined) {
            return undefined;
        }

        const genFilesMap = isCreation
            ? contractInfo.bytecode.generatedFileMap
            : contractInfo.deployedBytecode.generatedFileMap;

        const res = genFilesMap.get(id);

        if (res) {
            return res;
        }

        return contractInfo.artifact.fileMap.get(id);
    }

    contracts(): ContractInfo[] {
        return this._contracts;
    }

    findMethod(
        selector: HexString | Uint8Array,
        info?: ContractInfo
    ): [ContractInfo, FunctionDefinition | VariableDeclaration] | undefined {
        if (selector instanceof Uint8Array) {
            selector = `0x${bytesToHex(selector)}`;
        }

        for (const contract of info ? [info] : this._contracts) {
            if (!contract.ast) {
                continue;
            }

            const ast = contract.ast;

            const candidates = [
                ...ast.vFunctions.filter(
                    (method) =>
                        method.visibility === FunctionVisibility.External ||
                        method.visibility === FunctionVisibility.Public
                ),
                ...ast.vStateVariables.filter(
                    (getter) => getter.visibility === StateVariableVisibility.Public
                )
            ];

            for (const node of candidates) {
                if (bytesToHex(signatureHash(node)) === selector) {
                    return [contract, node];
                }
            }
        }

        return undefined;
    }

    /**
     * Given a msg.data and a target contract, compute the intended AST entry point. If any.
     * This handles the following cases:
     *      - receive functions
     *      - fallback functions
     *      - normal function match
     */
    findEntryPoint(
        data: Uint8Array,
        info: ContractInfo
    ): FunctionDefinition | VariableDeclaration | undefined {
        const contract = info.ast;

        if (!contract) {
            return undefined;
        }

        // Not enough data for a signature
        if (data.length < 4) {
            // First check if receive function is specified
            const recvF = findReceiveFun(contract);

            if (recvF) {
                return recvF;
            }

            // Otherwise we fall back to the fallback fun
            return findFallbackFun(contract);
        }

        const strSelector: UnprefixedHexString = bytesToHex(data.slice(0, 4));

        for (const base of contract.vLinearizedBaseContracts) {
            if (!base) {
                continue;
            }

            for (const fun of base.vFunctions) {
                const funSel = getFunctionSelector(fun);
                if (funSel == strSelector) {
                    return fun;
                }
            }

            for (const v of base.vStateVariables) {
                if (v.visibility !== StateVariableVisibility.Public) {
                    continue;
                }

                const hash = bytesToHex(signatureHash(v));

                if (hash == strSelector) {
                    return v;
                }
            }
        }

        // the fallback fun if there is one
        return findFallbackFun(contract);
    }

    getEventDefInfo(arg: bigint | Uint8Array | EventDesc): EventDefInfo | undefined {
        let topic: bigint;

        if (typeof arg === "bigint") {
            topic = arg;
        } else if (arg instanceof Uint8Array) {
            topic = bytesToBigInt(arg);
        } else {
            if (arg.topics.length < 1) {
                return undefined;
            }

            topic = bytesToBigInt(arg.topics[0]);
        }

        return this._topicToEventInfo.get(topic);
    }
}
