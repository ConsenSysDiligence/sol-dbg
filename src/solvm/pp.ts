import * as sol from "solc-typed-ast";
import { nyi } from "./exceptions";
import { SolStepKind, SolTrace, SolTraceStep } from "./trace";

export function pp(t: SolTraceStep | SolTrace): string {
    if (t instanceof Array) {
        return t.map(pp).join("\n");
    }

    if (t.kind === SolStepKind.PushScope) {
        return `Push Scope ${sol.pp(t.node)}`;
    }

    if (t.kind === SolStepKind.PopScope) {
        return `Pop Scope`;
    }

    if (t.kind === SolStepKind.Eval) {
        return `Eval ${sol.pp(t.expr)} to ${t.value}`;
    }

    if (t.kind === SolStepKind.Exec) {
        return `Exec ${sol.pp(t.stmt)}`;
    }

    if (t.kind === SolStepKind.Assign) {
        return `${sol.pp(t.to)} := ?`;
    }

    nyi(`pp(${t.kind})`);
}
