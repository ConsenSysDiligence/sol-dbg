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

/**
 * Bytecode info kept by the artifact manager, for each bytecode array.
 */
export interface BytecodeInfo {
    /**
     * Map from the file-id (used in source maps in this artifact) to the compiler-generated Yul sources for this contract's creation bytecode.
     * Note that multiple contracts have overlapping generated units ids, so we need a mapping per-contract
     */
    generatedFileMap: Map<number, SourceFileInfo>;
    /**
     * Source map for the bytecode. An array of {@link DecodedBytecodeSourceMapEntry}
     */
    srcMap: DecodedBytecodeSourceMapEntry[];
    /**
     * Map from bytecode offsets to instruction indices. (since some instructions are multi-byte, offset != instruction index).
     */
    offsetToIndexMap: Map<number, number>;
    /**
     * Actual bytecode
     */
    bytecode: Uint8Array;
}

/**
 * Contract info kept by the artifact manager for each compiled contract.
 */
export interface ContractInfo {
    /**
     * Reference to the compiler artifact in which we found the contract
     */
    artifact: ArtifactInfo;
    /**
     * Reference to the compiled bytecode info for this contract. (inside the {@link ContractInfo.artifact})
     */
    contractArtifact: PartialCompiledContract;
    /**
     * Contract name
     */
    contractName: string;
    /**
     * File name where the contract is defined
     */
    fileName: string;
    /**
     * A solc-typed-ast built {@link https://consensysdiligence.github.io/solc-typed-ast/classes/ContractDefinition.html ContractDefinition} for this contract (if any)
     */
    ast: ContractDefinition | undefined;
    /**
     * Creation bytecode info
     */
    bytecode: BytecodeInfo;
    /**
     * Deployed bytecode info
     */
    deployedBytecode: BytecodeInfo;
    /**
     * Metadata hash found in either the deployed or creation bytecode (if any).
     */
    mdHash: PrefixedHexString | undefined;
}

/**
 * Information about a complete solc compilation artifact (that may include several contracts).
 */
export interface ArtifactInfo {
    /**
     * Raw solc JSON
     */
    artifact: PartialSolcOutput;
    /**
     * List of solc-typed-ast decoded {@link https://consensysdiligence.github.io/solc-typed-ast/classes/SourceUnit.html SourceUnit}s
     */
    units: SourceUnit[];
    /**
     * solc-typed-ast {@link https://consensysdiligence.github.io/solc-typed-ast/classes/ASTContext.html ASTContext} that owns all the decoded {@link https://consensysdiligence.github.io/solc-typed-ast/classes/SourceUnit.html SourceUnit}s
     */
    ctx: ASTContext;
    /**
     * solc compiler version that produced this artifact
     */
    compilerVersion: string;
    /**
     * abi encoder version for files in this artifact.
     */
    abiEncoderVersion: ABIEncoderVersion;
    /**
     * Map from the file-id (used in source maps in this artifact) to the actual sources entry (and some additional info)
     */
    fileMap: Map<number, SourceFileInfo>;
    /**
     * Map from src triples to AST nodes with that source range
     */
    srcMap: Map<string, ASTNode>;
    /**
     * Codegen type
     */
    codegen: "ir" | "old";
}

/**
 * Solidity file type. Either source file, or compiler-generated Yul.
 */
export enum SourceFileType {
    Solidity = "solidity",
    InternalYul = "internal_yul"
}

/**
 * Source-file info
 */
export interface SourceFileInfo {
    /**
     * File contents (if any). If we got a compiler JSON artifact with no sources this may be undefined.
     */
    contents: string | undefined;
    /**
     * Raw JSON ast.
     */
    rawAst: RawAST;
    /**
     * solc-typed-ast decoded AST (may be undefined)
     */
    ast: SourceUnit | undefined;
    /**
     * File name
     */
    name: string;
    /**
     * File index (used in the import statements in ASTs to refer to files I think)
     */
    fileIndex: number;
    /**
     * Type of the source file (normal or internal Yul)
     */
    type: SourceFileType;
}
