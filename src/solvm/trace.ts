import * as sol from "solc-typed-ast";
import { SolValue } from "./state";

export class BaseSolStep {}
export class PushScope extends BaseSolStep {
    constructor(public readonly node: ScopeNode) {
        super();
    }
}
export class PopScope extends BaseSolStep {}
export class EvalStep<T extends sol.Expression> extends BaseSolStep {
    constructor(
        public readonly expr: T,
        public readonly value: SolValue
    ) {
        super();
    }
}
export class ExecStep<T extends sol.Statement> extends BaseSolStep {
    constructor(public readonly stmt: T) {
        super();
    }
}
export class ReturnStep extends ExecStep<sol.Return> {
    constructor(
        public readonly stmt: sol.Return,
        public readonly values: SolValue[]
    ) {
        super(stmt);
    }
}

export type ScopeNode =
    | sol.FunctionDefinition
    | sol.VariableDeclaration
    | sol.ModifierInvocation
    | sol.Block
    | sol.UncheckedBlock
    | sol.ForStatement
    | sol.SourceUnit
    | sol.ContractDefinition;

export type SolTraceStep = BaseSolStep;
export type SolTrace = SolTraceStep[];
