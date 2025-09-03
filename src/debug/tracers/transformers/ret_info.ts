import { InterpreterStep } from "@ethereumjs/evm";
import { VM } from "@ethereumjs/vm";
import {
    DataLocation,
    FunctionDefinition,
    FunctionType,
    StateVariableVisibility,
    VariableDeclaration
} from "solc-typed-ast";
import { mustReadMem, stackInd, stackTop } from "../../../utils/misc";
import { IArtifactManager } from "../../artifact_manager";
import { OPCODES } from "../../opcodes";
import { FrameKind } from "../../types";
import { BasicStepInfo } from "./basic_info";
import { ExternalFrameInfo, topExtFrame } from "./ext_stack";
import { makeCalldataViews } from "../../decoding/calldata/view";
import { astToRuntimeType } from "../../runtime_types";

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
 * Adds return info for steps in the callee context, right after a return.
 */
export async function addReturnInfo<T extends object & BasicStepInfo & ExternalFrameInfo>(
    vm: VM,
    step: InterpreterStep,
    state: T,
    trace: Array<T & ReturnInfo>,
    artifactManager: IArtifactManager
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

    const infer = artifactManager.infer(extFrame.info.artifact.compilerVersion);
    let type: FunctionType;

    try {
        type =
            extFrame.callee instanceof FunctionDefinition
                ? infer.funDefToType(extFrame.callee)
                : infer.getterFunType(extFrame.callee);
    } catch {
        return {
            ...state,
            retInfo: {
                callStartStep,
                rawReturnData,
                decodedReturnData: []
            }
        };
    }

    if (type.returns.length === 0) {
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
    const views = makeCalldataViews(
        type.returns.map((t) => astToRuntimeType(t, infer, DataLocation.CallData)),
        0n
    );
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
