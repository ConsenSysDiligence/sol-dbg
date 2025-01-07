import { PrefixedHexString } from "@ethereumjs/util";
import {
    ABIEncoderVersion,
    ASTContext,
    ASTNode,
    ContractDefinition,
    SourceUnit
} from "solc-typed-ast";
import { PartialCompiledContract, PartialSolcOutput, RawAST } from "../../artifacts/solc";
import { DecodedBytecodeSourceMapEntry } from "../../utils/srcmap";

export interface BytecodeInfo {
    // Map from the file-id (used in source maps in this artifact) to the generated Yul sources for this contract's creation bytecode.
    // Note that multiple contracts have overlapping generated units ids, so we need a mapping per-contract
    generatedFileMap: Map<number, SourceFileInfo>;
    srcMap: DecodedBytecodeSourceMapEntry[];
    offsetToIndexMap: Map<number, number>;
}

export interface ContractInfo {
    artifact: ArtifactInfo;
    contractArtifact: PartialCompiledContract;
    contractName: string;
    fileName: string;
    ast: ContractDefinition | undefined;
    bytecode: BytecodeInfo;
    deployedBytecode: BytecodeInfo;
    mdHash: PrefixedHexString | undefined;
}

export interface ArtifactInfo {
    artifact: PartialSolcOutput;
    units: SourceUnit[];
    ctx: ASTContext;
    compilerVersion: string;
    abiEncoderVersion: ABIEncoderVersion;
    // Map from the file-id (used in source maps in this artifact) to the actual sources entry (and some additional info)
    fileMap: Map<number, SourceFileInfo>;
    // Map from src triples to AST nodes with that source range
    srcMap: Map<string, ASTNode>;
}

export enum SourceFileType {
    Solidity = "solidity",
    InternalYul = "internal_yul"
}

export interface SourceFileInfo {
    contents: string | undefined;
    rawAst: RawAST;
    ast: SourceUnit | undefined;
    name: string;
    fileIndex: number;
    type: SourceFileType;
}
