import { InterpreterStep } from "@ethereumjs/evm";
import { TypedTransaction } from "@ethereumjs/tx";
import { Address } from "@ethereumjs/util";
import { VM } from "@ethereumjs/vm";
import { assert, FunctionDefinition, VariableDeclaration } from "solc-typed-ast";
import { getCodeHash, getCreationCodeHash } from "../../../artifacts";
import {
    bigEndianBufToBigint,
    bigEndianBufToNumber,
    mustReadMem,
    stackTop,
    wordToAddress,
    ZERO_ADDRESS
} from "../../../utils/misc";
import { ContractInfo, IArtifactManager } from "../../artifact_manager";
import { createsContract, increasesDepth, OPCODES } from "../../opcodes";
import { CallFrame, CreationFrame, ExternalFrame, FrameKind, HexString } from "../../types";
import { BasicStepInfo } from "./basic_info";
import { View } from "../../decoding/view";
import { buildMsgViews } from "../../abi";

export interface ExternalFrameInfo {
    stack: ExternalFrame[];
}

export function topExtFrame(arg: ExternalFrameInfo): ExternalFrame {
    return stackTop(arg.stack);
}

export function getContractInfo(step: ExternalFrameInfo): ContractInfo | undefined {
    return topExtFrame(step).info;
}

export function getCode(step: ExternalFrameInfo): Uint8Array {
    return topExtFrame(step).code;
}

/**
 * Build a `CallFrame` from the given `sender` address, `receiver` address, `data` `Uint8Array`, (msg.data) and the current trace step number.
 */
function makeCallFrame(
    sender: Address,
    receiver: Address,
    codeAddress: Address,
    data: Uint8Array,
    receiverCode: Uint8Array,
    codeHash: HexString | undefined,
    step: number,
    artifactManager: IArtifactManager
): CallFrame {
    const contractInfo: ContractInfo | undefined =
        codeHash === undefined ? codeHash : artifactManager.getContractFromMDHash(codeHash);

    let callee: FunctionDefinition | VariableDeclaration | undefined;
    let args: Array<[string, View]> | undefined;

    if (contractInfo && contractInfo.ast) {
        callee = artifactManager.findEntryPoint(data, contractInfo);

        if (callee !== undefined) {
            try {
                args = buildMsgViews(callee);
            } catch (e) {
                args = undefined;
            }
        }
    }

    return {
        kind: FrameKind.Call,
        sender,
        msgData: data,
        receiver: receiver,
        code: receiverCode,
        info: contractInfo,
        callee,
        address: receiver,
        startStep: step,
        arguments: args,
        codeMdHash: codeHash,
        codeAddress,
        internalFramesSus: false
    };
}

/**
 * Build a `CreationFrame` from the given `sender` address, `data` `Uint8Array`(msg.data) and the current trace step number.
 */
function makeCreationFrame(
    sender: Address,
    data: Uint8Array,
    step: number,
    artifactManager: IArtifactManager
): CreationFrame {
    const contractInfo = artifactManager.getContractFromCreationBytecode(data);
    let args: Array<[string, View]> | undefined;
    const callee = contractInfo && contractInfo.ast ? contractInfo.ast.vConstructor : undefined;

    if (contractInfo && callee instanceof FunctionDefinition) {
        // TODO: Try and find the arguments inside the creation code and decode them
    }

    return {
        kind: FrameKind.Creation,
        sender,
        msgData: data,
        code: data,
        info: contractInfo,
        callee,
        address: ZERO_ADDRESS,
        startStep: step,
        arguments: args,
        codeMdHash: getCreationCodeHash(data),
        internalFramesSus: false
    };
}

/**
 * Decode a *CALL* instruction. Computes:
 * 1. The receiver address
 * 2. The code address
 * 3. The msg.data
 * @param step
 */
export function decodeCall(step: BasicStepInfo): [Address, Address, Uint8Array, bigint, bigint] {
    const op = step.op;
    assert(
        op.opcode === OPCODES.CALL ||
            op.opcode === OPCODES.CALLCODE ||
            op.opcode === OPCODES.DELEGATECALL ||
            op.opcode === OPCODES.STATICCALL,
        `Unexpected call instruction {0}`,
        op.mnemonic
    );

    const stackTop = step.evmStack.length - 1;
    const argStackOff = op.opcode === OPCODES.CALL || op.opcode === OPCODES.CALLCODE ? 3 : 2;
    const argSizeStackOff = argStackOff + 1;

    const receiverArg = wordToAddress(step.evmStack[stackTop - 1]);

    const receiver = op.opcode === OPCODES.DELEGATECALL ? step.address : receiverArg;
    const codeAddr = receiverArg;
    const gas = bigEndianBufToBigint(step.evmStack[stackTop]);
    let value = 0n;

    if (op.opcode === OPCODES.CALL || op.opcode === OPCODES.CALLCODE) {
        value = bigEndianBufToBigint(step.evmStack[stackTop - 2]);
    }

    const start = bigEndianBufToNumber(step.evmStack[stackTop - argStackOff]);
    const size = bigEndianBufToNumber(step.evmStack[stackTop - argSizeStackOff]);
    const msgData = size === 0 ? new Uint8Array() : mustReadMem(start, size, step.memory);

    return [receiver, codeAddr, msgData, gas, value];
}

/**
 * Adds external frame info for each step
 */
export async function addExternalFrame<T extends object & BasicStepInfo>(
    vm: VM,
    step: InterpreterStep,
    state: T,
    trace: Array<T & ExternalFrameInfo>,
    artifactManager: IArtifactManager,
    tx: TypedTransaction
): Promise<T & ExternalFrameInfo> {
    let extFrame: ExternalFrame;

    if (trace.length === 0) {
        const sender = tx.getSenderAddress();

        if (tx.to === undefined) {
            extFrame = makeCreationFrame(sender, tx.data, 0, artifactManager);
        } else {
            const code = await vm.stateManager.getCode(tx.to);

            /// @todo remove - arbitrary restriction, only good for debugging
            assert(code.length > 0, "Missing code for address {0}", tx.to.toString());

            const codeHash = getCodeHash(code);

            extFrame = makeCallFrame(
                sender,
                tx.to,
                tx.to,
                tx.data,
                code,
                codeHash,
                0,
                artifactManager
            );
        }

        return {
            stack: [extFrame],
            ...state
        };
    }

    const lastStep = trace[trace.length - 1];

    if (lastStep.depth === state.depth) {
        return {
            stack: lastStep.stack,
            ...state
        };
    }

    const lastStackTop = lastStep.evmStack.length - 1;
    const lastOp = lastStep.op;

    if (state.depth > lastStep.depth) {
        assert(
            state.depth === lastStep.depth + 1,
            `Unexpected depth increase by more than 1 on step {0}`,
            trace.length
        );
        assert(increasesDepth(lastOp), `Unexpected depth increase on op ${lastOp.mnemonic}`);

        if (createsContract(lastOp)) {
            // Contract creation call
            const creationBytecode = mustReadMem(
                lastStep.evmStack[lastStackTop - 1],
                lastStep.evmStack[lastStackTop - 2],
                lastStep.memory
            );

            extFrame = makeCreationFrame(
                lastStep.address,
                creationBytecode,
                trace.length,
                artifactManager
            );
        } else {
            const [receiver, codeAddr, msgData] = decodeCall(lastStep);

            const code = await vm.stateManager.getCode(codeAddr);
            const codeHash = getCodeHash(code);

            extFrame = await makeCallFrame(
                lastStep.address,
                receiver,
                codeAddr,
                msgData,
                code,
                codeHash,
                trace.length,
                artifactManager
            );
        }

        return {
            stack: [...lastStep.stack, extFrame],
            ...state
        };
    } else {
        const newStack = [...lastStep.stack];
        // External return or exception
        let nFramesPopped = lastStep.depth - state.depth;

        // Pop as many external frames as neccessary to match the decrease in
        // depth reported by web3. We need the loop since we don't count the internal frames as decreasing depth
        while (nFramesPopped > 0 && newStack.length > 0) {
            const topFrame = newStack[newStack.length - 1];

            if (topFrame.kind === FrameKind.Creation || topFrame.kind === FrameKind.Call) {
                nFramesPopped--;
            }

            newStack.pop();
        }

        return {
            stack: newStack,
            ...state
        };
    }
}
