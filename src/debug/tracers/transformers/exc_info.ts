import { InterpreterStep } from "@ethereumjs/evm";
import { bytesToBigInt } from "@ethereumjs/util";
import { VM } from "@ethereumjs/vm";
import { nyi } from "../../../solvm/exceptions";
import { mustReadMem, stackInd, stackTop } from "../../../utils";
import { IArtifactManager } from "../../artifact_manager";
import { OPCODES } from "../../opcodes";
import { BasicStepInfo } from "./basic_info";
import { ExternalFrameInfo } from "./ext_stack";

export interface ExceptionInfo {
    excInfo?: {
        data: Uint8Array;
        // @todo Add decoded data
    };
}

/**
 * Adds exception info for steps that revert for some reason.
 */
export async function addExceptionInfo<T extends object & BasicStepInfo & ExternalFrameInfo>(
    vm: VM,
    step: InterpreterStep,
    state: T,
    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    trace: Array<T & ExceptionInfo>,
    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    artifactManager: IArtifactManager
): Promise<T & ExceptionInfo> {
    // INVALID
    if (!state.op.valid) {
        return {
            ...state,
            excInfo: {
                data: new Uint8Array()
            }
        };
    }

    // REVERT
    if (state.op.opcode === OPCODES.REVERT) {
        const off = bytesToBigInt(stackTop(state.evmStack));
        const len = bytesToBigInt(stackInd(state.evmStack, 1));

        return {
            ...state,
            excInfo: {
                data: mustReadMem(off, len, state.memory)
            }
        };

        nyi("REVERT exception state");

        // @todo try and decode revert
    }

    // @todo GAS?

    return state;
}
