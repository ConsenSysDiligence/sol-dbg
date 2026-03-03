import { InterpreterStep } from "@ethereumjs/evm";
import { VM } from "@ethereumjs/vm";
import {
    DataLocation,
    FunctionDefinition,
    FunctionKind,
    StateVariableVisibility,
    VariableDeclaration
} from "solc-typed-ast";
import { mustReadMem, stackInd, stackTop } from "../../../utils/misc";
import { OPCODES } from "../../opcodes";
import { FrameKind } from "../../types";
import { BasicStepInfo } from "./basic_info";
import { ExternalFrameInfo, topExtFrame } from "./ext_stack";
import { BaseCalldataView, makeCalldataViews, RawBytesView } from "../../decoding/calldata/view";
import { BaseRuntimeType, typeIdToRuntimeType } from "../../runtime_types";
import { getReturns } from "../../../utils";
import { Value } from "../../decoding";

export interface ReturnInfo {
    retInfo?: {
        // Step at which the call that just returned started
        callStartStep: number;
        // Raw returned data
        rawReturnData: Uint8Array;
        // Decoded returned data (if ast info is available)
        decodedReturnData?: any[];
    };
}

/**
 * Make Views to the return values of the given method
 */
function getReturnViews(
    callee: FunctionDefinition | VariableDeclaration
): Array<BaseCalldataView<Value, BaseRuntimeType>> {
    // fallback() methods either have no returns, or return raw unencoded bytes.
    if (callee instanceof FunctionDefinition && callee.kind === FunctionKind.Fallback) {
        if (callee.vReturnParameters.vParameters.length === 0) {
            return [];
        }

        return [new RawBytesView()];
    }

    const returns = getReturns(callee);
    const ctx = callee.requiredContext;
    return makeCalldataViews(
        returns.map(([, t]) => typeIdToRuntimeType(t, ctx, DataLocation.CallData)),
        0n
    );
}

/**
 * Adds return info for steps in the callee context, right after a return.
 */
export async function addReturnInfo<T extends object & BasicStepInfo & ExternalFrameInfo>(
    vm: VM,
    step: InterpreterStep,
    state: T
): Promise<T & ReturnInfo> {
    if (state.op.opcode !== OPCODES.RETURN && state.op.opcode !== OPCODES.STOP) {
        return state;
    }

    const extFrame = topExtFrame(state);
    const callStartStep = extFrame.startStep;

    const rawReturnData =
        state.op.opcode === OPCODES.RETURN
            ? mustReadMem(stackTop(state.evmStack), stackInd(state.evmStack, 1), state.memory)
            : new Uint8Array(0);

    // Special case: For creation frames we know that the consturctor doesn't "return anything" at the Solidity level
    if (extFrame.kind === FrameKind.Creation) {
        return {
            ...state,
            retInfo: {
                callStartStep,
                rawReturnData,
                decodedReturnData: []
            }
        };
    }

    if (
        !(
            extFrame.info &&
            (extFrame.callee instanceof FunctionDefinition ||
                (extFrame.callee instanceof VariableDeclaration &&
                    extFrame.callee.stateVariable &&
                    extFrame.callee.visibility === StateVariableVisibility.Public))
        )
    ) {
        return {
            ...state,
            retInfo: {
                callStartStep,
                rawReturnData
            }
        };
    }

    const views = getReturnViews(extFrame.callee);

    if (views.length === 0) {
        return {
            ...state,
            retInfo: {
                callStartStep,
                rawReturnData,
                decodedReturnData: []
            }
        };
    }

    // We treat these as in calldata, since they should already be abi-encoded in memory for the Return instruction
    const decodedReturnData = views.map((v) => v.decode(rawReturnData));

    return {
        ...state,
        retInfo: {
            callStartStep,
            rawReturnData,
            decodedReturnData
        }
    };
}
