import { Address } from "@ethereumjs/util";
import { bytesToHex } from "ethereum-cryptography/utils";
import {
    AddressType,
    ArrayType,
    assert,
    BoolType,
    BytesType,
    ContractDefinition,
    EnumDefinition,
    FixedBytesType,
    FunctionDefinition,
    FunctionKind,
    InferType,
    IntType,
    PointerType,
    StringType,
    StructDefinition,
    TypeNode,
    UserDefinedType,
    UserDefinedValueTypeDefinition
} from "solc-typed-ast";
import {
    ArtifactManager,
    ContractInfo,
    decodeValue,
    ExternalFrame,
    Frame,
    FrameKind,
    SolTxDebugger,
    SourceFileInfo,
    StepState
} from "../debug";
import {
    decodeSourceLoc,
    ExternalFrameInfo,
    getContractInfo,
    InternalFrameInfo,
    MapKeys,
    topExtFrame
} from "../debug/tracers/transformers";

const srcLocation = require("src-location");
const fse = require("fs-extra");

function ppValue(typ: TypeNode, v: any, infer: InferType): string {
    if (v === undefined) {
        return `<failed decoding>`;
    }

    if (typ instanceof IntType) {
        return (v as bigint).toString();
    }

    if (typ instanceof AddressType) {
        return (v as Address).toString();
    }

    if (typ instanceof FixedBytesType) {
        return bytesToHex(v as Uint8Array);
    }

    if (typ instanceof BoolType) {
        return v ? "true" : "false";
    }

    if (typ instanceof UserDefinedType) {
        const def = typ.definition;

        if (def instanceof EnumDefinition) {
            const optInd = Number(v as bigint);

            assert(
                optInd >= 0 && optInd < def.vMembers.length,
                `Enum value ${optInd} outside of enum range 0-${def.vMembers.length} of ${typ.pp()}`
            );

            return `${def.name}.${def.vMembers[optInd].name}`;
        }

        if (def instanceof ContractDefinition) {
            return (v as Address).toString();
        }

        if (def instanceof UserDefinedValueTypeDefinition) {
            const underlyingType = infer.typeNameToTypeNode(def.underlyingType);

            return ppValue(underlyingType, v, infer);
        }

        throw new Error(`NYI ppValue of user-defined type ${typ.pp()}`);
    }

    if (typ instanceof PointerType) {
        if (typ.to instanceof ArrayType) {
            const elT = typ.to.elementT;

            return `[${(v as any[]).map((el) => ppValue(elT, el, infer)).join(", ")}]`;
        }

        if (typ.to instanceof BytesType) {
            return `0x${bytesToHex(v as Uint8Array)}`;
        }

        if (typ.to instanceof StringType) {
            return `"${v}"`;
        }

        if (typ.to instanceof UserDefinedType && typ.to.definition instanceof StructDefinition) {
            const fields = typ.to.definition.vMembers;
            const strFields: string[] = [];

            for (const field of fields) {
                try {
                    const fieldT = infer.variableDeclarationToTypeNode(field);

                    strFields.push(field.name + ": " + ppValue(fieldT, v[field.name], infer));
                } catch (e) {
                    strFields.push(field.name + ": <failed decoding>");
                }
            }

            return `{${strFields.join(", ")}}`;
        }

        throw new Error(`NYI ppValue of referenced type ${typ.to.pp()}`);
    }

    throw new Error(`NYI ppValue of type ${typ.pp()}`);
}

export function flattenStack(
    step: InternalFrameInfo & ExternalFrameInfo,
    trace: Array<InternalFrameInfo & ExternalFrameInfo>
): Frame[] {
    const res: Frame[] = [];

    while (true) {
        const curExtFrame = topExtFrame(step);

        if (step.intStack) {
            res.unshift(...step.intStack);
        }

        res.unshift(curExtFrame);

        assert(curExtFrame.startStep > 0 || step.stack.length === 1, ``);

        if (curExtFrame.startStep === 0) {
            break;
        }

        step = trace[curExtFrame.startStep - 1];
    }

    return res;
}

export function ppStackTrace(
    solDbg: SolTxDebugger,
    trace: StepState[],
    step: StepState,
    curOffset: number,
    mapKeys?: MapKeys
): string {
    const res: string[] = [];
    const stack = flattenStack(step, trace);

    for (let i = 0; i < stack.length; i++) {
        const frame = stack[i];

        let frameStr: string;

        const lastOffset = i < stack.length - 1 ? trace[stack[i + 1].startStep - 1].pc : curOffset;
        const extFrame = frame.kind === FrameKind.InternalCall ? frame.nearestExtFrame : frame;
        const [lastPosInFrame] = decodeSourceLoc(lastOffset, extFrame);

        const info = extFrame.info;

        let funArgs: string;
        let funName: string | undefined;
        let fileName: string | undefined;

        const offset = trace[frame.startStep].pc;

        if (info) {
            fileName = info.fileName;
        }

        if (frame.callee) {
            if (frame.callee instanceof FunctionDefinition) {
                let calleeName: string;

                if (frame.callee.isConstructor) {
                    calleeName = "constructor";
                } else if (frame.callee.kind === FunctionKind.Fallback) {
                    calleeName = "fallback";
                } else if (frame.callee.kind === FunctionKind.Receive) {
                    calleeName = "receiver";
                } else {
                    calleeName = frame.callee.name;
                }

                fileName = (
                    frame.callee.vScope instanceof ContractDefinition
                        ? frame.callee.vScope.vScope
                        : frame.callee.vScope
                ).sourceEntryKey;

                funName =
                    (frame.callee.vScope instanceof ContractDefinition
                        ? frame.callee.vScope.name + "."
                        : "") + calleeName;
            } else {
                funName = `<compiler-generated function>@${offset}`;
            }
        } else {
            funName = `<unknown function>@${offset}`;
        }

        if (
            extFrame.info &&
            lastPosInFrame &&
            extFrame.info.artifact.fileMap.has(lastPosInFrame.sourceIndex)
        ) {
            const sourceInfo = extFrame.info.artifact.fileMap.get(
                lastPosInFrame.sourceIndex
            ) as SourceFileInfo;

            if (sourceInfo.contents !== undefined) {
                const t = srcLocation.indexToLocation(
                    sourceInfo.contents,
                    lastPosInFrame.start,
                    true
                );

                fileName += `:${t.line}:${t.column}`;
            }
        }

        if (frame.arguments) {
            const funArgEls: string[] = [];

            for (const [, view] of frame.arguments) {
                if (view === undefined) {
                    funArgEls.push("<unknown>");

                    continue;
                }

                const state = trace[frame.startStep];
                assert(info !== undefined, ``);
                const infer = solDbg.artifactManager.infer(info.artifact.compilerVersion);

                const val = decodeValue(view, state, infer, mapKeys);

                funArgEls.push(ppValue(view.type, val, infer));
            }

            funArgs = funArgEls.join(", ");
        } else {
            funArgs = "<unknown>";
        }

        if (frame.kind === FrameKind.InternalCall) {
            assert(fileName !== undefined, ``);

            frameStr = fileName + " ";
            frameStr += `${funName}(${funArgs})`;
        } else if (frame.kind === FrameKind.Call) {
            if (frame.info === undefined) {
                frameStr = `<unknown function(s) in contract ${frame.address.toString()}>`;
            } else {
                // If we have debug info for this contract ignore the external frame - it will duplicate the internal frame
                continue;
            }
        } else {
            if (frame.info === undefined || frame.info.ast === undefined) {
                frameStr = `<deploying unknown contract>`;
            } else {
                frameStr = `${fileName} `;
                frameStr += `<deploying ${funName}(${funArgs})>`;
            }
        }

        res.push(frameStr);
    }

    return res.reverse().join("\n");
}

export function printStepSourceString(
    step: StepState,
    lastExtStep: ExternalFrame,
    sources: Map<string, string>,
    artifactManager: ArtifactManager,
    prefix: string | undefined
): string | undefined {
    const info = getContractInfo(step);

    if (info === undefined) {
        return undefined;
    }

    const errorLoc = step.src;

    if (errorLoc === undefined) {
        return undefined;
    }

    const fileInd = errorLoc.sourceIndex;
    const fileInfo = artifactManager.getFileById(
        fileInd,
        info as ContractInfo,
        lastExtStep.kind === "creation"
    );

    if (fileInfo === undefined) {
        return undefined;
    }

    const fileName = fileInfo.name;

    let fileContents = sources.get(fileName as string);

    if (fileContents === undefined) {
        const actualFileName = prefix ? prefix + fileName : fileName;

        fileContents = fse.readFileSync(actualFileName, {
            encoding: "utf-8"
        }) as string;

        sources.set(fileName, fileContents);
    }

    /**
     * @todo It is worth fixing as we refer to a file contents as string,
     * while compiler refer to it as bytes.
     *
     * This may cause issues when file contains multibyte charactes.
     * It worth to use `Uint8Array` instead.
     */
    return (fileContents as string).substring(errorLoc.start, errorLoc.start + errorLoc.length);
}

export function debugDumpTrace(
    trace: StepState[],
    artifactManager: ArtifactManager,
    prefix?: string
): void {
    const sources = new Map<string, string>();

    for (let i = 0; i < trace.length; i++) {
        const step = trace[i];

        const jumpType =
            (step.op.mnemonic === "JUMP" || step.op.mnemonic === "JUMPI") && step.src
                ? step.src.jump
                : "";

        const srcString = printStepSourceString(
            step,
            topExtFrame(step),
            sources,
            artifactManager,
            prefix
        );

        console.error(
            `${i} ${step.pc}: ${step.op.mnemonic} ${jumpType} ${
                srcString !== undefined ? srcString : ""
            }`
        );
    }
}

export function ppStep(step: StepState): string {
    const addrStr = `${step.address.toString().slice(36)}`;

    let contractId: string;

    const extFrame = topExtFrame(step);
    const code: Uint8Array = extFrame.code;
    const codeMdHash: string | undefined = extFrame.codeMdHash;
    const contractInfo = getContractInfo(step);

    if (contractInfo) {
        contractId = `${contractInfo.contractName}@${addrStr}`;
    } else if (codeMdHash) {
        contractId = `0x${codeMdHash.slice(0, 6)}...${codeMdHash.slice(60)}@${addrStr}`;
    } else {
        contractId = `unknown@${addrStr}`;
    }

    const immStr = step.op.immediates
        .map((imm) => bytesToHex(code.slice(step.pc + 1, step.pc + 1 + imm.length)))
        .join(" ");

    const stackStr = step.evmStack
        .slice(step.evmStack.length - step.op.nPop, step.evmStack.length)
        .map((v) => bytesToHex(v))
        .join(", ");

    return `${contractId}# ${step.pc}: ${step.op.mnemonic}(${step.op.opcode.toString(
        16
    )}) ${immStr} [${stackStr}]`;
}
