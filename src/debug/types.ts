import { Address, PrefixedHexString } from "@ethereumjs/util";
import * as sol from "solc-typed-ast";
import { FunctionDefinition } from "solc-typed-ast";
import { ImmMap } from "../utils/immutable_map";
import { ArtifactInfo, ContractInfo } from "./artifact_manager/types";
import { EVMOpInfo } from "./opcodes";
import { View } from "./decoding/view";

/**
 * A type alias for 0x-prefixed hex strings. Used for documentation purposes.
 */
export type HexString = PrefixedHexString;

/**
 * A type alias for hex strings without 0x prefix. Used for documentation purposes.
 */
export type UnprefixedHexString = string;

export type DecodedSolValue = { value: any; type: string };

export interface StackFrame {
    fun: FunctionDefinition;
    argumentsAtStart: { [argName: string]: DecodedSolValue };
    argsAndLocals: { [varName: string]: DecodedSolValue };
}

export type StackTrace = StackFrame[];

export enum FrameKind {
    Call = "call",
    Creation = "creation",
    InternalCall = "internal_call"
}

/// require("@ethereumjs/evm/dist/cjs/types").EVMOpts
export type EVMOpts = any;

/**
 * Base interface for Stack frames maintained by the debugger
 */
export interface BaseFrame {
    readonly kind: FrameKind;
    /**
     * AST node causing the call. Note that this is not always a FunctionCall. For example this could be:
     * 1. A contract public state var VariableDeclaration
     * 2. Any checked arithmetic operation in sol > 0.8.0 (these are implemented as internal functions)
     * 3. Some other random non-call AST node, that is implemented as a compiler generated function
     */
    readonly callee: sol.ASTNode | undefined;
    /**
     * If we have a `callee` try and infer where the arguments are placed in the VM state. Some arguments may not
     * exist in the case of msg.data generated from a fuzzer for example.
     */
    readonly arguments: Array<[string, View]> | undefined;
    readonly startStep: number;
}
/**
 * Base class for a stack frame corresponding to an external call.
 */
export interface BaseExternalFrame extends BaseFrame {
    readonly sender: Address;
    readonly msgData: Uint8Array;
    readonly address: Address;
    readonly info?: ContractInfo;
    readonly code: Uint8Array;
    readonly codeMdHash: HexString | undefined;
    // Set if the internal call/returns in a contract dont match up.
    internalFramesSus: boolean;
}

/**
 * Stack frame corresponding to an external call
 */
export interface CallFrame extends BaseExternalFrame {
    readonly kind: FrameKind.Call;
    readonly receiver: Address;
    readonly codeAddress: Address;
}

/**
 * Stack frame corresponding to a contract creation call
 */
export interface CreationFrame extends BaseExternalFrame {
    readonly kind: FrameKind.Creation;
}

/**
 * Stack frame corresponding to an internal function call
 */
export interface InternalCallFrame extends BaseFrame {
    readonly kind: FrameKind.InternalCall;
    readonly nearestExtFrame: CallFrame | CreationFrame;
    readonly offset: number;
}

export type ExternalFrame = CallFrame | CreationFrame;
export type Frame = ExternalFrame | InternalCallFrame;
export type DbgStack = Frame[];

export function isFrame(a: any): a is Frame {
    return (
        a instanceof Object &&
        a.hasOwnProperty("kind") &&
        (a.kind === FrameKind.Call ||
            a.kind === FrameKind.Creation ||
            a.kind === FrameKind.InternalCall)
    );
}

export type Memory = Uint8Array;
export type Stack = Uint8Array[];
export type Storage = ImmMap<bigint, Uint8Array>;

export interface EventDesc {
    payload: Uint8Array;
    topics: Uint8Array[];
}

export interface EventDefInfo {
    definition: sol.EventDefinition;
    artifact: ArtifactInfo;
    args: Array<[string, sol.TypeNode, boolean]>;
}

export interface DecodedEventDesc {
    def: EventDefInfo;
    args: Array<[string, any]>;
}

/**
 * Low-level machine state at a given trace step. It directly mirrors the state reported from Web3
 * and doesn't include any higher-level information that requires debug info.
 */
export interface StepVMState {
    evmStack: Stack;
    memory: Memory;
    storage: Storage;
    op: EVMOpInfo;
    pc: number;
    gasCost: bigint;
    dynamicGasCost: bigint;
    gas: bigint;
    depth: number;
    address: Address;
}

/**
 * State that the debugger maintains for each trace step.
 * It includes the basic VM state (`StepVmState`) and all the info computed
 * by additional transformers
 */
export interface StepState extends StepVMState {
    /**
     * List of external call frames
     */
    stack: ExternalFrame[];
    /**
     * If the current instruction is a return, include return information
     */
    retInfo?: {
        /**
         * Step at which the call that just returned started
         */
        callStartStep: number;
        /**
         * Raw returned data
         */
        rawReturnData: Uint8Array;
        /**
         * Decoded returned data (if ast info is available)
         */
        decodedReturnData?: any[];
    };
    /**
     * If the current instruction throws an exception, includes exception info
     */
    excInfo?: {
        /**
         * Raw exception bytes
         */
        data: Uint8Array;
    };
    /**
     * Internal call stack at the current instruction
     */
    intStack: InternalCallFrame[];
    /**
     * Source location for the current instruction (if a src map is available)
     */
    src: sol.DecodedBytecodeSourceMapEntry | undefined;
    /**
     * AST node that corresponds to the source location of the current instruction (if any)
     */
    astNode: sol.ASTNode | undefined;
    /**
     * If the current instruction emits an event, includes the raw event info
     */
    emittedEvent: EventDesc | undefined;
    /**
     * If we were able to decode the event, include the decoded event info
     */
    decodedEvent: DecodedEventDesc | undefined;
    /**
     * If this is the instruction after we return from a create call, add the newly created address here.
     */
    contractCreated?: Address;
    /**
     * If this is a SELFDESTRUCT instruction, recored the destroyed contract
     */
    contractKilled?: Address;
    /**
     * If this is a KECCAK256 instruction record the preimage and the hash
     */
    keccak?: {
        from: Uint8Array;
        to: bigint;
    };
}

/**
 * Trace step struct contained in the array returned by web3.debug.traceTransaction().
 * We translate this into `StepVmState`.
 */
export interface Web3DbgState {
    stack: HexString[];
    memory: HexString[];
    storage?: any;
    op: string;
    pc: number;
    gasCost: string;
    gas: string;
    depth: number;
    error?: any;
}
