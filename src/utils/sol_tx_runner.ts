import { equalsBytes } from "ethereum-cryptography/utils";
import * as sol from "solc-typed-ast";
import { FrameKind, IArtifactManager, OPCODES, StepState, Storage } from "../debug";
import { SolCallResult, SolVM, WorldInterface } from "../solvm";
import { SolMessage } from "../solvm/state";
import { SolTrace } from "../solvm/trace";
import { stackTop, ZERO_ADDRESS } from "./misc";

export interface SolTraceSegment {
    start: number;
    end: number;
    alignedPoints: Array<[number, number]>;
    trace: SolTrace;
    finalState: {
        storage: Storage;
    };
}

export function findIndexAfter<T>(
    list: T[],
    predicate: (el: T, idx: number) => boolean,
    after: number
): number {
    for (let i = after; i < list.length; i++) {
        if (predicate(list[i], i)) {
            return i;
        }
    }

    return -1;
}

function noCtxChangeAt(trace: StepState[], idx: number): boolean {
    return trace[idx].stack.length === trace[idx + 1].stack.length;
}

function callsAt(trace: StepState[], idx: number): boolean {
    return idx === 0 || trace[idx].stack.length < trace[idx + 1].stack.length;
}

function returnOrExceptionAt(trace: StepState[], idx: number): boolean {
    return idx === trace.length || trace[idx].stack.length > trace[idx + 1].stack.length;
}

function getSolCalResult(step: StepState): SolCallResult {
    if (step.retInfo) {
        return {
            reverted: false,
            data: step.retInfo.rawReturnData
        };
    }

    if (step.excInfo) {
        return {
            reverted: true,
            data: step.excInfo.data
        };
    }

    fail(`Unexpected getSolCalResult(${step.op.mnemonic})`);
}

/**
 * Try building a solidity trace starting at index `idx` of `evmTrace`.
 * Returns a tuple containing:
 *  1) the list of solidity trace segments computed,
 *  2) the step index after we return/revert from the current context
 *  3) the result of this context's execution
 */
export async function buildSolTrace(
    evmTrace: StepState[],
    artifactManager: IArtifactManager,
    idx: number
): Promise<[SolTraceSegment[], number, SolCallResult]> {
    const res: SolTraceSegment[] = [];

    if (returnOrExceptionAt(evmTrace, idx)) {
        return [res, idx + 1, getSolCalResult(evmTrace[idx])];
    }

    // Step must be the start of a new execution context
    sol.assert(callsAt(evmTrace, idx), ``);

    const startHeight = evmTrace[idx].stack.length;
    const frame = stackTop(evmTrace[idx].stack);

    if (!(frame.info && frame.info.ast)) {
        // We don't have contract info in this context. Walk the trace at this level. If we:
        // 1) Encounter an inner call, recursively call buildSolTrace to see what we can do in that context.
        //    upon return, continue walking the trace at that step
        // 2) If we ever decrease the stack height (return or exception) return the currently accumulated segments and position
        while (true) {
            if (returnOrExceptionAt(evmTrace, idx)) {
                return [res, idx, getSolCalResult(evmTrace[idx])];
            }

            if (noCtxChangeAt(evmTrace, idx)) {
                idx++;
                continue;
            }

            // Must be a call
            const [fragments, newIdx, ctxRes] = await buildSolTrace(evmTrace, artifactManager, idx);

            res.push(...fragments);
            idx = newIdx;

            if (idx >= evmTrace.length) {
                return [fragments, idx, ctxRes];
            }
        }
    }

    function getStorage(): Storage {
        return evmTrace[idx].storage;
    }

    // Instantiate the SolVm and try executing
    const world: WorldInterface = {
        call: (msg) => call(msg, OPCODES.CALL),
        delegatecall: (msg) => call(msg, OPCODES.DELEGATECALL),
        staticcall: (msg) => call(msg, OPCODES.STATICCALL),
        getStorage
    };

    const msg: SolMessage = {
        to: frame.kind === FrameKind.Call ? frame.receiver : ZERO_ADDRESS,
        data: frame.msgData,
        gas: evmTrace[idx].gas, // @todo Not sure this is correct
        value: 0n // @todo implement value!!
    };

    let solTraceLoc = 0;

    function getSegment(start: number, end: number): SolTraceSegment {
        const trace = vm.getTrace();
        const traceSeg = trace.slice(solTraceLoc, trace.length);
        // Alignment points consist of the start, end of each segment as well as any event emissions
        // @todo align event emissions
        const alignedPoints: Array<[number, number]> = [
            [start, 0],
            [end, traceSeg.length]
        ];

        const res = {
            start,
            end,
            alignedPoints,
            trace: traceSeg,
            finalState: {
                storage: vm.getStorage()
            }
        };
        solTraceLoc = trace.length;
        return res;
    }

    async function call(msg: SolMessage, callOp: OPCODES): Promise<SolCallResult> {
        const callStepIdx = findIndexAfter(
            evmTrace,
            (step, i) =>
                step.op.opcode === callOp &&
                i < evmTrace.length &&
                // @todo also check value matches up here!
                equalsBytes(stackTop(evmTrace[i + 1].stack).msgData, msg.data),
            idx
        );

        sol.assert(callStepIdx > 0, `No call index found`);
        res.push(getSegment(idx, callStepIdx));

        const [segments, newIdx, ctxRes] = await buildSolTrace(
            evmTrace,
            artifactManager,
            callStepIdx + 1
        );

        idx = newIdx;
        res.push(...segments);

        return ctxRes;
    }

    const vm = new SolVM(world, artifactManager, frame.info, msg);

    await vm.run();

    // find final step
    let finalStep = findIndexAfter(evmTrace, (step) => step.stack.length < startHeight, idx);
    finalStep = finalStep < 0 ? evmTrace.length : finalStep;

    // get fragment fpr [idx, final step]
    res.push(getSegment(idx, finalStep - 1));

    return [res, finalStep, getSolCalResult(evmTrace[finalStep - 1])];
}
