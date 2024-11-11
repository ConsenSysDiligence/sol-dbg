import * as sol from "solc-typed-ast";
import { OPCODES } from "../debug";
import { SolBaseException } from "./exceptions";
import { SolMessage, SolValue } from "./state";

export abstract class BaseSolStep {}
/**
 * Mark steps that may be hidden as "internal". (e.g. compiler-generated constructor calls, etc)
 */
export abstract class InternalSolStep extends BaseSolStep {}

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

/**
 * This is a bit counter-intuitive, but due to modifiers we can execute multiple
 * 'Return' statements in the lifetime of a single internal function. However
 * there is only one real return, marked by `InternalReturnStep`
 */
export class InternalReturnStep extends InternalSolStep {
    constructor(public readonly values: SolValue[]) {
        super();
    }
}

/**
 * Marks an external call. Applies to both calls and contract deployments
 */
export class ExternalCallStep extends InternalSolStep {
    constructor(
        public readonly msg: SolMessage,
        public readonly opcode: OPCODES
    ) {
        super();
    }
}

/**
 * Marks a solidity exception
 */
export class ExceptionStep extends InternalSolStep {
    constructor(public readonly exc: SolBaseException) {
        super();
    }
}

/**
 * Marks the execution of a default (compiler-generated) constructor for contracts that don't have a real one
 */
export class DefaultConstructorStep extends InternalSolStep {
    constructor(public readonly contract: sol.ContractDefinition) {
        super();
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
