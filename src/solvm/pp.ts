import * as sol from "solc-typed-ast";
import { nyi } from "./exceptions";
import {
    EvalStep,
    ExecStep,
    PopScope,
    PushScope,
    ReturnStep,
    SolTrace,
    SolTraceStep
} from "./trace";

export function pp(t: SolTraceStep | SolTrace): string {
    if (t instanceof Array) {
        return t.map(pp).join("\n");
    }

    if (t instanceof PushScope) {
        return `Push Scope ${sol.pp(t.node)}`;
    }

    if (t instanceof PopScope) {
        return `Pop Scope`;
    }

    if (t instanceof EvalStep) {
        return `Eval ${sol.pp(t.expr)} to ${t.value}`;
    }

    if (t instanceof ReturnStep) {
        return `Return ${t.values}`;
    }

    if (t instanceof ExecStep) {
        return `Exec ${sol.pp(t.stmt)}`;
    }

    nyi(`pp(${t.constructor.name})`);
}
