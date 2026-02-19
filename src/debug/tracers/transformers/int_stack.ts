import { InterpreterStep } from "@ethereumjs/evm";
import { VM } from "@ethereumjs/vm";
import { assert, FunctionDefinition, VariableDeclaration } from "solc-typed-ast";
import { IArtifactManager } from "../../artifact_manager";
import { is2SlotStackType } from "../../decoding/utils";
import { OPCODES } from "../../opcodes";
import { Frame, FrameKind, InternalCallFrame, Stack } from "../../types";
import { BasicStepInfo } from "./basic_info";
import { ExternalFrameInfo, topExtFrame } from "./ext_stack";
import { SourceInfo } from "./source";
import { View } from "../../decoding/view";
import { makeStackView } from "../../decoding";
import { typeIdToRuntimeType } from "../../runtime_types";
import { getArgs } from "../../../utils";

export interface InternalFrameInfo {
    intStack: InternalCallFrame[];
}

function topFrame(step: InternalFrameInfo & ExternalFrameInfo & BasicStepInfo): Frame {
    if (step.intStack.length > 0) {
        return step.intStack[step.intStack.length - 1];
    }

    assert(step.stack.length > 0, `Unexpected empty stack in step at pc {0}`, step.pc);
    return step.stack[step.stack.length - 1];
}

/**
 * Given a callable (function definition or public state variable) try to build
 * `View`s for all the callable arguments. On failure return undefined.
 */
function buildFunArgViews(
    callee: FunctionDefinition | VariableDeclaration,
    stack: Stack
): Array<[string, View]> | undefined {
    const res: Array<[string, View]> = [];
    const ctx = callee.requiredContext;

    const formals = getArgs(callee);
    let offsetFromTop = -1;

    for (let i = formals.length - 1; i >= 0; i--) {
        const [name, typ] = formals[i];
        const rttTyp = typeIdToRuntimeType(typ, ctx);
        const stackSize = is2SlotStackType(rttTyp) ? 2 : 1;

        offsetFromTop += stackSize;

        if (offsetFromTop > stack.length) {
            // Stack underflow. Could be due to optimized code?
            return undefined;
        }

        res.unshift([name, makeStackView(rttTyp, offsetFromTop)]);
    }

    return res;
}

/**
 * Adds external frame info for each step
 */
export async function addInternalFrameInfo<
    T extends object & BasicStepInfo & ExternalFrameInfo & SourceInfo
>(
    vm: VM,
    step: InterpreterStep,
    state: T,
    trace: Array<T & InternalFrameInfo>,
    artifactManager: IArtifactManager,
    strict: boolean
): Promise<T & InternalFrameInfo> {
    // No internal stack frame on first step of trace. We are in the contract preamble
    if (trace.length === 0) {
        return {
            ...state,
            intStack: []
        };
    }

    const lastStep = trace[trace.length - 1];

    // Return/exception
    if (state.depth < lastStep.depth) {
        // Check if upon normal return there were leftover internal stack frames in previous context
        if (lastStep.op.opcode === OPCODES.RETURN) {
            const lastExtFrame = topExtFrame(lastStep);

            // If we had a normal return with multiple stack frames left in the last frame, then it was probably broken
            lastExtFrame.internalFramesSus =
                lastExtFrame.internalFramesSus || lastStep.intStack.length > 1;
        }

        // Assume we return in the same internal stack as right before we made the call
        const lastStepBeforeCall = trace[lastStep.stack[state.stack.length].startStep - 1];

        return {
            ...state,
            intStack: lastStepBeforeCall.intStack
        };
    }

    // Call/creation - initially empty internal stack as we start in the contract preamble
    if (state.depth === lastStep.depth + 1) {
        return {
            ...state,
            intStack: []
        };
    }

    assert(state.depth === lastStep.depth, ``);

    // There are 2 ways to enter an internal function:
    let enteringInternalFun = false;

    //  1. Jumping into an internal function (the previous instruction is a JUMP with source map jump index i)
    if (
        state.op.mnemonic === "JUMPDEST" &&
        lastStep.op.mnemonic === "JUMP" &&
        lastStep.src &&
        lastStep.src.jump === "i"
    ) {
        enteringInternalFun = true;
    }

    const ast = state.astNode;
    const curExtFrame = topExtFrame(state);

    //  2. Fall-through (the previous instruction is literally the pervious instruction in the contract body,
    //      AND the current JUMPDEST corresponds to a whole function, AND the pervious instructions' callee is different
    //      from the current instruction's function.
    if (
        !enteringInternalFun &&
        state.op.mnemonic === "JUMPDEST" &&
        (ast instanceof FunctionDefinition ||
            (ast instanceof VariableDeclaration && ast.stateVariable)) &&
        topFrame(lastStep).callee !== ast
    ) {
        enteringInternalFun = true;
    }

    if (enteringInternalFun) {
        let args: Array<[string, View]> | undefined;

        if (
            ast instanceof FunctionDefinition ||
            (ast instanceof VariableDeclaration && ast.stateVariable)
        ) {
            assert(curExtFrame.info !== undefined, ``);
            args = buildFunArgViews(ast, state.evmStack);
        }

        const newFrame: InternalCallFrame = {
            kind: FrameKind.InternalCall,
            nearestExtFrame: curExtFrame,
            callee: ast,
            offset: state.pc,
            startStep: trace.length,
            arguments: args
        };

        return {
            ...state,
            intStack: [...lastStep.intStack, newFrame]
        };
    }

    // Returning from an internal function call
    if (state.op.mnemonic === "JUMP" && state.src && state.src.jump === "o") {
        const curFrame = topFrame(lastStep);

        if (strict) {
            assert(
                curFrame.kind === FrameKind.InternalCall,
                `Mismatched internal return from frame `,
                curFrame.kind
            );
        } else {
            if (curFrame.kind !== FrameKind.InternalCall) {
                curExtFrame.internalFramesSus = true;
            }
        }

        return {
            ...state,
            intStack: lastStep.intStack.slice(0, -1)
        };
    }

    return {
        ...state,
        intStack: lastStep.intStack
    };
}
