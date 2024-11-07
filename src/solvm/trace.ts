import * as sol from "solc-typed-ast";
import { SolValue } from "./state";

export enum SolStepKind {
    PushScope = "push_scope",
    PopScope = "pop_scope",
    Eval = "eval",
    Exec = "exec",
    Assign = "assign",
    Copy = "copy"
}

export interface BaseSolStep {
    kind: SolStepKind;
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

export interface PushStep extends BaseSolStep {
    kind: SolStepKind.PushScope;
    node: ScopeNode;
}

export interface PopStep extends BaseSolStep {
    kind: SolStepKind.PopScope;
}

export interface EvalStep extends BaseSolStep {
    kind: SolStepKind.Eval;
    expr: sol.Expression;
    value: SolValue;
}

export interface ExecStep extends BaseSolStep {
    kind: SolStepKind.Exec;
    stmt: sol.Statement;
}

export interface CopyStep extends BaseSolStep {
    kind: SolStepKind.Copy;
}

export interface AssignStep extends BaseSolStep {
    kind: SolStepKind.Assign;
    to: sol.Expression;
}

export type SolTraceStep = PushStep | PopStep | EvalStep | ExecStep | CopyStep | AssignStep;
export type SolTrace = SolTraceStep[];
