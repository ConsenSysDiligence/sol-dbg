import * as sol from "solc-typed-ast";
import {
    buildMsgDataViews,
    ContractInfo,
    DataLocationKind,
    IArtifactManager,
    Storage
} from "../debug";
import { stackTop, ZERO_ADDRESS } from "../utils";
import {
    nyi,
    SolBaseException,
    SolNoMethod,
    SolRawException,
    SolStructuredException
} from "./exceptions";
import { BaseScope, BlockScope, ContractScope, FunScope, GlobalScope, Scope } from "./scope";
import { LValue, noType, POISON, SolMessage, SolTuple, SolValue, VmDataView } from "./state";
import { EvalStep, ExecStep, PopScope, PushScope, ReturnStep, ScopeNode, SolTrace } from "./trace";
import { coerceToLValue } from "./utils";

export interface SolCallResult {
    reverted: boolean;
    data: Uint8Array;
}

export interface WorldInterface {
    call(msg: SolMessage): Promise<SolCallResult>;
    staticcall(msg: SolMessage): Promise<SolCallResult>;
    delegatecall(msg: SolMessage): Promise<SolCallResult>;
}

enum ControlFlow {
    Fallthrough = 0,
    Break = 1,
    Continue = 2,
    Return = 3
}

export class SolVM {
    protected infer: sol.InferType;
    protected scopes: Scope[] = [];
    protected temps = new Map<sol.Expression, SolValue>();
    protected compilerVersion: string;
    protected encoderVersion: sol.ABIEncoderVersion;
    protected contract: sol.ContractDefinition;
    protected trace: SolTrace = [];

    constructor(
        protected env: WorldInterface,
        protected artifactManager: IArtifactManager,
        protected info: ContractInfo,
        protected msg: SolMessage,
        protected storage: Storage
    ) {
        this.compilerVersion = info.artifact.compilerVersion;
        this.encoderVersion = info.artifact.abiEncoderVersion;
        this.infer = artifactManager.infer(this.compilerVersion);

        sol.assert(info.ast !== undefined, `Expected contract AST!`);

        this.contract = info.ast;

        this.scopes.push(new GlobalScope(this.contract.vScope));
        this.scopes.push(new ContractScope(this.contract, this.infer));
    }

    /**
     * Entry point for running the VM
     */
    run(): SolValue[] | SolBaseException {
        this.trace = [];

        try {
            if (this.msg.to === null || this.msg.to.equals(ZERO_ADDRESS)) {
                this._create();
                return [];
            }

            return this._call();
        } catch (e) {
            if (e instanceof SolBaseException) {
                return e;
            }

            throw e;
        }
    }

    getTrace(): SolTrace {
        return this.trace;
    }

    /**
     * Helper to handle contract creation
     */
    private _create(): void {
        nyi("contract creation");
    }

    /**
     * Helper to implement the logic of contract dispatch. Handles:
     *  - disaptching receive() and fallback()
     *  - dispatching a normal method
     *  @todo: Throw exception on non-payable fallback with money
     */
    private _call(): SolValue[] {
        const entry = this.artifactManager.findEntryPoint(this.msg.data, this.info);

        if (!entry) {
            throw new SolNoMethod(this.msg.data);
        }

        if (entry instanceof sol.VariableDeclaration) {
            nyi(`executing getter ${entry.name}`);
        }

        const views = buildMsgDataViews(
            entry,
            this.msg.data,
            DataLocationKind.CallData,
            this.infer,
            this.encoderVersion
        );

        return this.execFun(
            entry,
            views.map((v) =>
                v[1] === undefined ? { val: POISON, type: noType } : { val: v[1], type: v[1].type }
            )
        );
    }

    private pushScope(scope: Scope, node: ScopeNode): void {
        this.scopes.push(scope);
        this.trace.push(new PushScope(node));
    }

    private popScope(): void {
        this.scopes.pop();
        this.trace.push(new PopScope());
    }

    private topScopeByType<T extends ScopeNode>(
        constr: sol.ASTNodeConstructor<T>
    ): BaseScope | undefined {
        for (let i = this.scopes.length - 1; i >= 0; i--) {
            if (this.scopes[i].node instanceof constr) {
                return this.scopes[i];
            }
        }

        return undefined;
    }

    private lookup(name: string): VmDataView {
        for (let i = this.scopes.length - 1; i >= 0; i--) {
            const v = this.scopes[i].lookup(name);

            if (v) {
                return v;
            }
        }

        sol.assert(false, `Undeclared identifier ${name}`);
    }

    private execModifier(mod: sol.ModifierInvocation): void {
        const modArgs = mod.vArguments.map((arg) => this.evalExpression(arg));
        this.pushScope(new FunScope(mod, modArgs, this.infer), mod);

        const body = (mod.vModifier as sol.ModifierDefinition).vBody;
        sol.assert(body !== undefined, `NYI abstract modifiers`);

        this.execBlock(body);
        this.popScope();
    }

    /**
     * Execute a "callable". This includes FunctionDefinition, ModifierInvocation and VariableDeclaration (getters).
     * Sets up the callable scope, and figure out what to execute next:
     *  - for functions with modifiers, the first modifier
     *  - otherwise (fun with no modifier or a modifier) execute the body next
     *
     * @todo Handle getters
     */
    private execFun(fun: sol.FunctionDefinition, args: SolValue[]): SolValue[] {
        const scope = new FunScope(fun, args, this.infer);
        this.pushScope(scope, fun);

        // First check if this is a function with modifiers. If so we start execution at the first modifier
        let firstMod: sol.ModifierInvocation | undefined;
        for (const mod of fun.vModifiers) {
            if (!(mod.vModifier instanceof sol.ModifierDefinition)) {
                continue;
            }

            firstMod = mod;
            break;
        }

        // Execute either the first modifier (if any) or the function body
        if (firstMod) {
            this.execModifier(firstMod);
        } else {
            const body = fun.vBody;
            sol.assert(body !== undefined, `Expected a body`);

            this.execBlock(body);
        }

        this.popScope();

        return scope.returns();
    }

    /// ------------------------------------ Statements ----------------------------------------------------
    private execStatement(stmt: sol.Statement): ControlFlow {
        let res: ControlFlow;

        if (stmt instanceof sol.Block || stmt instanceof sol.UncheckedBlock) {
            res = this.execBlock(stmt);
        } else if (stmt instanceof sol.Break) {
            res = this.execBreak(stmt);
        } else if (stmt instanceof sol.Continue) {
            res = this.execContinue(stmt);
        } else if (stmt instanceof sol.DoWhileStatement) {
            res = this.execDoWhileStatement(stmt);
        } else if (stmt instanceof sol.EmitStatement) {
            res = this.execEmitStatement(stmt);
        } else if (stmt instanceof sol.ExpressionStatement) {
            res = this.execExpressionStatement(stmt);
        } else if (stmt instanceof sol.ForStatement) {
            res = this.execForStatement(stmt);
        } else if (stmt instanceof sol.IfStatement) {
            res = this.execIfStatement(stmt);
        } else if (stmt instanceof sol.InlineAssembly) {
            res = this.execInlineAssembly(stmt);
        } else if (stmt instanceof sol.PlaceholderStatement) {
            res = this.execPlaceholderStatement(stmt);
        } else if (stmt instanceof sol.Return) {
            res = this.execReturn(stmt);
        } else if (stmt instanceof sol.RevertStatement) {
            res = this.execRevertStatement(stmt);
        } else if (stmt instanceof sol.Throw) {
            res = this.execThrow(stmt);
        } else if (stmt instanceof sol.TryCatchClause) {
            res = this.execTryCatchClause(stmt);
        } else if (stmt instanceof sol.TryStatement) {
            res = this.execTryStatement(stmt);
        } else if (stmt instanceof sol.VariableDeclarationStatement) {
            res = this.execVariableDeclarationStatement(stmt);
        } else if (stmt instanceof sol.WhileStatement) {
            res = this.execWhileStatement(stmt);
        } else {
            nyi(`Stmt ${stmt.constructor.name}`);
        }

        if (!(stmt instanceof sol.Return)) {
            this.trace.push(new ExecStep(stmt));
        }

        return res;
    }

    /**
     * Execute a block. Pushes a block scope, and executes the statements of the block in order.
     * Checks the control flow after each statement to determine if we need to go out
     */
    private execBlock(block: sol.Block | sol.UncheckedBlock): ControlFlow {
        let flow: ControlFlow = ControlFlow.Fallthrough;

        this.pushScope(new BlockScope(block, this.infer), block);

        for (const stmt of block.vStatements) {
            flow = this.execStatement(stmt);

            if (flow !== ControlFlow.Fallthrough) {
                return flow;
            }
        }

        this.popScope();

        return flow;
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private execBreak(stmt: sol.Break): ControlFlow {
        return ControlFlow.Break;
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private execContinue(stmt: sol.Continue): ControlFlow {
        return ControlFlow.Continue;
    }

    private execDoWhileStatement(stmt: sol.DoWhileStatement): ControlFlow {
        let cond: boolean;
        let flow: ControlFlow;

        do {
            flow = this.execStatement(stmt.vBody);

            if (!(flow === ControlFlow.Fallthrough || flow === ControlFlow.Continue)) {
                break;
            }

            cond = this.evalBool(stmt.vCondition);
        } while (cond);

        return flow === ControlFlow.Return ? ControlFlow.Return : ControlFlow.Fallthrough;
    }

    private execEmitStatement(stmt: sol.EmitStatement): ControlFlow {
        stmt.vEventCall.vArguments.map((arg) => this.evalExpression(arg));
        return ControlFlow.Fallthrough;
    }

    private execExpressionStatement(stmt: sol.ExpressionStatement): ControlFlow {
        this.evalExpression(stmt.vExpression);
        return ControlFlow.Fallthrough;
    }

    private execForStatement(stmt: sol.ForStatement): ControlFlow {
        let cond: boolean;
        let flow: ControlFlow;

        this.pushScope(new BlockScope(stmt, this.infer), stmt);

        if (stmt.vInitializationExpression) {
            flow = this.execStatement(stmt.vInitializationExpression);
            if (flow !== ControlFlow.Fallthrough) {
                return flow;
            }
        }

        while (true) {
            cond = stmt.vCondition ? this.evalBool(stmt.vCondition) : true;

            if (!cond) {
                flow = ControlFlow.Fallthrough;
                break;
            }

            flow = this.execStatement(stmt.vBody);

            if (!(flow === ControlFlow.Continue || flow === ControlFlow.Fallthrough)) {
                break;
            }
        }

        this.popScope();

        return flow;
    }

    private execIfStatement(stmt: sol.IfStatement): ControlFlow {
        const cond: boolean = this.evalBool(stmt.vCondition);

        if (cond) {
            return this.execStatement(stmt.vTrueBody);
        }

        if (!stmt.vFalseBody) {
            return ControlFlow.Fallthrough;
        }

        return this.execStatement(stmt.vFalseBody);
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private execInlineAssembly(stmt: sol.InlineAssembly): ControlFlow {
        nyi("InlineAssembly");
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private execPlaceholderStatement(stmt: sol.PlaceholderStatement): ControlFlow {
        // Find the top modifier scope, and the function scope it belongs to
        const topModScope = this.topScopeByType(sol.ModifierInvocation);

        sol.assert(
            topModScope !== undefined && topModScope.node instanceof sol.ModifierInvocation,
            `Missing modifier scope`
        );

        const modInv = topModScope.node;
        const fun = modInv.parent as sol.FunctionDefinition;

        let funScope: FunScope | undefined;
        for (let idx = this.scopes.indexOf(topModScope) - 1; idx >= 0; idx--) {
            if (this.scopes[idx].node === fun) {
                funScope = this.scopes[idx] as FunScope;
                break;
            }
        }

        sol.assert(funScope !== undefined, `Missing fun scope for modifier invocation`);

        // Find the order of the current modifier in the moidifier list
        const modIdx = fun.vModifiers.indexOf(modInv);
        sol.assert(modIdx >= 0, ``);

        let nextModIdx: number;

        // Find the next executing modifier idx. If none, its time for the function body
        for (nextModIdx = modIdx + 1; nextModIdx < fun.vModifiers.length; nextModIdx++) {
            if (fun.vModifiers[nextModIdx].vModifier instanceof sol.ModifierDefinition) {
                break;
            }
        }

        const nextExecutable =
            nextModIdx === fun.vModifiers.length ? fun : fun.vModifiers[nextModIdx];

        /**
         * Scopes are weird in modifiers. Each modifier executes in its own scope. So before
         * a placeholder we take a snapshot of the scopes, reset them to the function scope, and execute the
         * next modifier or the body of the function. Afterwards we reset the scopes to the saved state for the remainder
         * of the execution of to modifier.
         */
        const savedScopes = [...this.scopes];
        this.scopes = this.scopes.slice(0, this.scopes.indexOf(funScope) + 1);

        if (nextExecutable instanceof sol.ModifierInvocation) {
            this.execModifier(nextExecutable);
        } else {
            const body = fun.vBody;
            sol.assert(body !== undefined, `Missing fun body`);
            this.execBlock(body);
        }

        this.scopes = savedScopes;

        return ControlFlow.Fallthrough;
    }

    private execReturn(stmt: sol.Return): ControlFlow {
        const funScope = this.topScopeByType(sol.FunctionDefinition);
        sol.assert(funScope !== undefined && funScope instanceof FunScope, `Missing fun scope`);

        const fun = funScope.node as sol.FunctionDefinition;

        let retVals: SolValue[];

        if (stmt.vExpression === undefined) {
            retVals = [];
        } else if (stmt.vExpression instanceof sol.TupleExpression) {
            retVals = this.evalExpression(stmt.vExpression) as SolValue[];
        } else {
            retVals = [this.evalExpression(stmt.vExpression)];
        }

        sol.assert(
            retVals.length === fun.vReturnParameters.vParameters.length,
            `Mismatch in number of ret vals and formal returns`
        );

        for (let i = 0; i < retVals.length; i++) {
            funScope.assign(i, retVals[i]);
        }

        this.trace.push(new ReturnStep(stmt, retVals));

        return ControlFlow.Return;
    }

    private execRevertStatement(stmt: sol.RevertStatement): ControlFlow {
        const args = stmt.errorCall.vArguments.map(this.evalExpression);
        // Push to the trace before we throw
        this.trace.push(new ExecStep(stmt));

        const errDef = stmt.errorCall.vReferencedDeclaration;
        sol.assert(
            errDef instanceof sol.ErrorDefinition,
            `Expected an errro definition not {0}`,
            errDef
        );

        throw new SolStructuredException(errDef, args);
    }

    private execThrow(stmt: sol.Throw): ControlFlow {
        // Push to the trace before we throw
        this.trace.push(new ExecStep(stmt));
        throw new SolRawException(new Uint8Array());
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private execTryCatchClause(stmt: sol.TryCatchClause): ControlFlow {
        nyi("TryCatchClause");
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private execTryStatement(stmt: sol.TryStatement): ControlFlow {
        nyi("TryStatement");
    }

    private execVariableDeclarationStatement(stmt: sol.VariableDeclarationStatement): ControlFlow {
        const curScope = stackTop(this.scopes) as BlockScope;
        if (!stmt.vInitialValue) {
            return ControlFlow.Fallthrough;
        }

        const rhs = this.evalExpression(stmt.vInitialValue);
        const rhsVs = rhs instanceof SolTuple ? rhs.components : [rhs];

        for (let i = 0, declIdx = 0; i < rhsVs.length; i++) {
            if (stmt.assignments[i] === null) {
                continue;
            }

            const decl = stmt.vDeclarations[declIdx];
            sol.assert(decl.id === stmt.assignments[i], `sanity`);
            curScope.assign(decl.name, rhsVs[i]);
            declIdx++;
        }

        return ControlFlow.Fallthrough;
    }
    private execWhileStatement(stmt: sol.WhileStatement): ControlFlow {
        let flow: ControlFlow = ControlFlow.Fallthrough;

        while (this.evalBool(stmt.vCondition)) {
            flow = this.execStatement(stmt.vBody);

            if (!(flow === ControlFlow.Fallthrough || flow === ControlFlow.Continue)) {
                break;
            }
        }

        return flow;
    }

    /// ------------------------------------ LValues----------------------------------------------------
    /**
     * LValues are a special case of expression evaluations meant for evaluating the LHS of assignments.
     */
    private evalLValue(expr: sol.Expression): LValue {
        let res: LValue;

        if (expr instanceof sol.Conditional) {
            nyi(`evalLVConditional(${expr.constructor.name})`);
        } else if (expr instanceof sol.FunctionCall) {
            res = coerceToLValue(this.evalFunctionCall(expr));
        } else if (expr instanceof sol.Identifier) {
            res = this.evalLVIdentifier(expr);
        } else if (expr instanceof sol.IndexAccess) {
            res = this.evalLVIndexAccess(expr);
        } else if (expr instanceof sol.MemberAccess) {
            nyi(`evalLVMemberAccess(${expr.constructor.name})`);
        } else if (expr instanceof sol.TupleExpression) {
            if (expr.vOriginalComponents.length === 1) {
                return this.evalLValue(expr.vOriginalComponents[0] as sol.Expression);
            }

            return expr.vOriginalComponents.map((c) => (c === null ? null : this.evalLValue(c)));
        } else {
            nyi(`evalLValue(${expr.constructor.name})`);
        }

        this.trace.push(new EvalStep(expr, res));

        return res;
    }

    private evalLVIdentifier(expr: sol.Identifier): LValue {
        return this.lookup(expr.name);
    }

    private evalLVIndexAccess(expr: sol.IndexAccess): LValue {
        sol.assert(expr.vIndexExpression !== undefined, `Missing index expr`);

        const baseT = this.infer.typeOf(expr.vBaseExpression);

        sol.assert(
            baseT instanceof sol.ArrayType || baseT instanceof sol.MappingType,
            `Expected an array, not {0}`,
            baseT
        );

        nyi("evalLVIndexAccess");
    }

    /// ------------------------------------ Expressions ----------------------------------------------------

    // Helper to eval an expression as bool or fail
    private evalBool(expr: sol.Expression): boolean {
        const res = this.evalExpression(expr);

        sol.assert(
            typeof res === "boolean",
            `Expected a boolean when evaluating {0} not {1} of type {2}`,
            expr,
            res as any,
            typeof res
        );

        return res;
    }

    private evalExpression(expr: sol.Expression): SolValue {
        let res: SolValue;

        if (expr instanceof sol.Assignment) {
            res = this.evalAssignment(expr);
        } else if (expr instanceof sol.BinaryOperation) {
            res = this.evalBinaryOperation(expr);
        } else if (expr instanceof sol.Conditional) {
            res = this.evalConditional(expr);
        } else if (expr instanceof sol.ElementaryTypeNameExpression) {
            res = this.evalElementaryTypeNameExpression(expr);
        } else if (expr instanceof sol.FunctionCallOptions) {
            res = this.evalFunctionCallOptions(expr);
        } else if (expr instanceof sol.FunctionCall) {
            res = this.evalFunctionCall(expr);
        } else if (expr instanceof sol.Identifier) {
            res = this.evalIdentifier(expr);
        } else if (expr instanceof sol.IndexAccess) {
            res = this.evalIndexAccess(expr);
        } else if (expr instanceof sol.IndexRangeAccess) {
            res = this.evalIndexRangeAccess(expr);
        } else if (expr instanceof sol.Literal) {
            res = this.evalLiteral(expr);
        } else if (expr instanceof sol.MemberAccess) {
            res = this.evalMemberAccess(expr);
        } else if (expr instanceof sol.NewExpression) {
            res = this.evalNewExpression(expr);
        } else if (expr instanceof sol.TupleExpression) {
            res = this.evalTupleExpression(expr);
        } else if (expr instanceof sol.UnaryOperation) {
            res = this.evalUnaryOperation(expr);
        } else {
            nyi(`evalExpression(${expr.constructor.name})`);
        }

        this.trace.push(new EvalStep(expr, res));

        return res;
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private assign(lhs: LValue, rhs: SolValue): void {
        nyi("assign");
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private evalAssignment(expr: sol.Assignment): SolValue {
        const rhs = this.evalExpression(expr.vRightHandSide);
        const lhs = this.evalLValue(expr.vLeftHandSide);

        this.assign(lhs, rhs);

        return rhs;
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private evalBinaryOperation(expr: sol.BinaryOperation): SolValue {
        nyi("BinaryOperation");
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private evalConditional(expr: sol.Conditional): SolValue {
        nyi("Conditional");
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private evalElementaryTypeNameExpression(expr: sol.ElementaryTypeNameExpression): SolValue {
        nyi("ElementaryTypeNameExpression");
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private evalFunctionCallOptions(expr: sol.FunctionCallOptions): SolValue {
        nyi("FunctionCallOptions");
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private evalFunctionCall(expr: sol.FunctionCall): SolValue {
        nyi("FunctionCall");
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private evalIdentifier(expr: sol.Identifier): SolValue {
        nyi("Identifier");
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private evalIndexAccess(expr: sol.IndexAccess): SolValue {
        nyi("IndexAccess");
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private evalIndexRangeAccess(expr: sol.IndexRangeAccess): SolValue {
        nyi("IndexRangeAccess");
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private evalLiteral(expr: sol.Literal): SolValue {
        if (expr.kind === sol.LiteralKind.Number) {
            return BigInt(expr.value);
        }

        nyi("Literal");
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private evalMemberAccess(expr: sol.MemberAccess): SolValue {
        nyi("MemberAccess");
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private evalNewExpression(expr: sol.NewExpression): SolValue {
        nyi("NewExpression");
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private evalTupleExpression(expr: sol.TupleExpression): SolValue {
        return new SolTuple(
            expr.vOriginalComponents.map((c) => {
                sol.assert(c !== null, `Unexpected eval of null tuple component`);
                return this.evalExpression(c);
            })
        );
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private evalUnaryOperation(expr: sol.UnaryOperation): SolValue {
        nyi("UnaryOperation");
    }
}
