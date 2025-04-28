import { Address } from "@ethereumjs/util";
import { concatBytes, hexToBytes } from "ethereum-cryptography/utils";
import { lt } from "semver";
import * as sol from "solc-typed-ast";
import {
    buildMsgDataViews,
    cd_decodeValue,
    ContractInfo,
    DataLocation,
    DataLocationKind,
    decodeIndexLoc,
    decodeValue,
    IArtifactManager,
    mem_decodeValue,
    Memory,
    OPCODES,
    StepState,
    stor_decodeIndexLoc,
    stor_decodeValue,
    stor_mustDecodeValue,
    Storage,
    StorageLocation
} from "../debug";
import { stackTop, ZERO_ADDRESS } from "../utils";
import { PanicCodes } from "./constants";
import {
    fail,
    nyi,
    SolBaseException,
    SolInvalid,
    SolNoMethod,
    SolPanic,
    SolRevert
} from "./exceptions";
import { BaseScope, BlockScope, ContractScope, FunScope, GlobalScope } from "./scope";
import {
    isTypedVMDataView,
    LValue,
    noType,
    POISON,
    SolMessage,
    SolTuple,
    SolValue,
    TypedVmDataView,
    VmDataView
} from "./state";
import {
    DefaultConstructorStep,
    EvalStep,
    ExceptionStep,
    ExecStep,
    ExternalCallStep,
    ExternalReturnStep,
    InternalReturnStep,
    ScopeNode,
    SolTrace
} from "./trace";
import {
    asyncMap,
    decodeReturns,
    encodeCall,
    encodeReturns,
    findConstructor,
    getFunCallTarget,
    grabInheritanceArgs,
    isTypePrimitive,
    stor_assignValue
} from "./utils";

export interface SolCallResult {
    reverted: boolean;
    data: Uint8Array;
}

export interface WorldInterface {
    create(msg: SolMessage): Promise<SolCallResult>;
    call(msg: SolMessage): Promise<SolCallResult>;
    staticcall(msg: SolMessage): Promise<SolCallResult>;
    delegatecall(msg: SolMessage): Promise<SolCallResult>;
    getStorage(): Storage;
}

enum ControlFlow {
    Fallthrough = 0,
    Break = 1,
    Continue = 2,
    Return = 3
}

export class SolVM {
    protected infer: sol.InferType;
    protected scopes: BaseScope[] = [];
    protected temps = new Map<sol.Expression, SolValue>();
    protected compilerVersion: string;
    protected encoderVersion: sol.ABIEncoderVersion;
    protected contract: sol.ContractDefinition;
    protected trace: SolTrace = [];
    protected storage: Storage;
    protected memory: Memory;
    protected cdArgBaseOff: number;

    constructor(
        protected env: WorldInterface,
        protected artifactManager: IArtifactManager,
        protected info: ContractInfo,
        protected msg: SolMessage
    ) {
        this.memory = new Uint8Array();
        this.storage = env.getStorage();
        this.compilerVersion = info.artifact.compilerVersion;
        this.encoderVersion = info.artifact.abiEncoderVersion;
        this.infer = artifactManager.infer(this.compilerVersion);

        sol.assert(info.ast !== undefined, `Expected contract AST!`);

        this.contract = info.ast;

        this.scopes.push(new GlobalScope(this.contract.vScope));
        this.scopes.push(new ContractScope(this.contract, this.infer));

        if (this.msg.to.equals(ZERO_ADDRESS)) {
            // For creation Txs the base cd offset is the length of the bytecode
            this.cdArgBaseOff = info.contractArtifact.evm.bytecode.object.length / 2;
        } else {
            this.cdArgBaseOff = 4;
        }
    }

    /**
     * Entry point for running the VM
     */
    async run(): Promise<SolValue[] | SolBaseException> {
        this.trace = [];

        try {
            if (this.msg.to === null || this.msg.to.equals(ZERO_ADDRESS)) {
                await this._create();
                return [];
            }

            return await this._call();
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

    getStorage(): Storage {
        return this.storage;
    }

    /**
     * Helper to handle contract creation. Handles constructor evaluation.
     * Called at most once per object lifetime.
     */
    private async _create(): Promise<void> {
        const info = this.artifactManager.getContractFromCreationBytecode(this.msg.data);
        sol.assert(info !== undefined, ``);
        const contract = info.ast;
        sol.assert(contract !== undefined, ``);
        const [argExprMap, parentScopeMap] = grabInheritanceArgs(contract);

        const argMap = new Map<sol.ContractDefinition, SolValue[]>();
        const firstConstructor = findConstructor(contract);

        if (firstConstructor) {
            const calldataViews = buildMsgDataViews(
                firstConstructor,
                this.msg.data,
                DataLocationKind.CallData,
                this.infer,
                this.encoderVersion
            );

            const calldataArgs = calldataViews.map((v) =>
                v[1] === undefined ? { val: POISON, type: noType } : { val: v[1], type: v[1].type }
            );

            argMap.set(firstConstructor.vScope as sol.ContractDefinition, calldataArgs);
        }

        for (const base of contract.vLinearizedBaseContracts) {
            // @todo add inline state var initializers

            if (!base.vConstructor) {
                this.trace.push(new DefaultConstructorStep(base));
                continue;
            }

            const args = argMap.get(base);
            sol.assert(
                args !== undefined,
                `Missing args for base ${base.name} of ${contract.name}`
            );

            // Evaluate any parent-contract args in the scope of this constructor
            for (const [subContract, baseContract] of parentScopeMap) {
                if (baseContract === base) {
                    const argExprs = argExprMap.get(subContract);
                    sol.assert(argExprs !== undefined, ``);

                    const savedScopes = this.scopes;
                    this.scopes = [
                        ...this.scopes.slice(0, 2),
                        new FunScope(base.vConstructor, args, this.infer)
                    ];

                    argMap.set(
                        subContract,
                        await asyncMap(argExprs, (e) => this.evalExpression(e))
                    );

                    this.scopes = savedScopes;
                }
            }

            // Execute this constructor (and any of its concrete modifiers)
            await this.execFun(base.vConstructor, args);
        }

        // This is invalid in the cse of immutable references
        // @todo: add test to fail this with immutable refs; fix broken test
        this.trace.push(
            new ExternalReturnStep(hexToBytes(info.contractArtifact.evm.deployedBytecode.object))
        );
    }

    private solException(e: SolBaseException): never {
        this.trace.push(new ExceptionStep(e));
        throw e;
    }

    /**
     * Helper to implement the logic of contract dispatch. Handles:
     *  - disaptching receive() and fallback()
     *  - dispatching a normal method
     * Called at most once per object lifetime.
     *  @todo: Throw exception on non-payable fallback with money
     */
    private async _call(): Promise<SolValue[]> {
        const entry = this.artifactManager.findEntryPoint(this.msg.data, this.info);

        if (!entry) {
            this.solException(new SolNoMethod(this.msg.data));
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

        const rets = await this.execFun(
            entry,
            views.map((v) =>
                v[1] === undefined ? { val: POISON, type: noType } : { val: v[1], type: v[1].type }
            )
        );

        const encodedSolidityRet = encodeReturns(entry, rets, this.infer, sol.ABIEncoderVersion.V2);
        this.trace.push(new ExternalReturnStep(encodedSolidityRet));

        return rets;
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private pushScope(scope: BaseScope, node: ScopeNode): void {
        this.scopes.push(scope);
        //this.trace.push(new PushScope(node));
    }

    private popScope(): void {
        this.scopes.pop();
        //this.trace.push(new PopScope());
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

        fail(`Undeclared identifier ${name}`);
    }

    private async execModifier(mod: sol.ModifierInvocation): Promise<ControlFlow> {
        const modArgs = await asyncMap(mod.vArguments, (arg) => this.evalExpression(arg));
        this.pushScope(new FunScope(mod, modArgs, this.infer), mod);

        const body = (mod.vModifier as sol.ModifierDefinition).vBody;
        sol.assert(body !== undefined, `NYI abstract modifiers`);

        const res = await this.execBlock(body);
        this.popScope();

        return res;
    }

    /**
     * Execute a FunctionDefinition.
     * Sets up the callable scope, and figure out what to execute next:
     *  - for functions with modifiers, the first modifier
     *  - otherwise (fun with no modifier or a modifier) execute the body next
     *
     * @todo Handle getters
     */
    private async execFun(fun: sol.FunctionDefinition, args: SolValue[]): Promise<SolValue[]> {
        // If we are being called from another function, then we remember its scopes here. We restore them at the end
        const savedScopes = this.scopes;

        const scope = new FunScope(fun, args, this.infer);
        this.scopes = [this.scopes[0]];

        // If this is a contract method add the contract scope. Otherwise this is a global function in the global scope
        if (fun.vScope instanceof sol.ContractDefinition) {
            this.pushScope(new ContractScope(fun.vScope, this.infer), fun.vScope);
        }

        // Add function scope
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
            await this.execModifier(firstMod);
        } else {
            const body = fun.vBody;
            sol.assert(body !== undefined, `Expected a body`);

            await this.execBlock(body);
        }

        this.scopes = savedScopes;
        const rets = scope.returns();

        this.trace.push(new InternalReturnStep(rets));
        return rets;
    }

    /// ------------------------------------ Statements ----------------------------------------------------
    private async execStatement(stmt: sol.Statement): Promise<ControlFlow> {
        let res: ControlFlow;

        if (stmt instanceof sol.Block || stmt instanceof sol.UncheckedBlock) {
            res = await this.execBlock(stmt);
        } else if (stmt instanceof sol.Break) {
            res = await this.execBreak(stmt);
        } else if (stmt instanceof sol.Continue) {
            res = await this.execContinue(stmt);
        } else if (stmt instanceof sol.DoWhileStatement) {
            res = await this.execDoWhileStatement(stmt);
        } else if (stmt instanceof sol.EmitStatement) {
            res = await this.execEmitStatement(stmt);
        } else if (stmt instanceof sol.ExpressionStatement) {
            res = await this.execExpressionStatement(stmt);
        } else if (stmt instanceof sol.ForStatement) {
            res = await this.execForStatement(stmt);
        } else if (stmt instanceof sol.IfStatement) {
            res = await this.execIfStatement(stmt);
        } else if (stmt instanceof sol.InlineAssembly) {
            res = await this.execInlineAssembly(stmt);
        } else if (stmt instanceof sol.PlaceholderStatement) {
            res = await this.execPlaceholderStatement(stmt);
        } else if (stmt instanceof sol.Return) {
            res = await this.execReturn(stmt);
        } else if (stmt instanceof sol.RevertStatement) {
            res = await this.execRevertStatement(stmt);
        } else if (stmt instanceof sol.Throw) {
            res = await this.execThrow(stmt);
        } else if (stmt instanceof sol.TryCatchClause) {
            res = await this.execTryCatchClause(stmt);
        } else if (stmt instanceof sol.TryStatement) {
            res = await this.execTryStatement(stmt);
        } else if (stmt instanceof sol.VariableDeclarationStatement) {
            res = await this.execVariableDeclarationStatement(stmt);
        } else if (stmt instanceof sol.WhileStatement) {
            res = await this.execWhileStatement(stmt);
        } else {
            nyi(`Stmt ${stmt.constructor.name}`);
        }

        this.trace.push(new ExecStep(stmt));

        return res;
    }

    /**
     * Execute a block. Pushes a block scope, and executes the statements of the block in order.
     * Checks the control flow after each statement to determine if we need to go out
     */
    private async execBlock(block: sol.Block | sol.UncheckedBlock): Promise<ControlFlow> {
        let flow: ControlFlow = ControlFlow.Fallthrough;

        this.pushScope(new BlockScope(block, this.infer), block);

        for (const stmt of block.vStatements) {
            flow = await this.execStatement(stmt);

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

    private async execDoWhileStatement(stmt: sol.DoWhileStatement): Promise<ControlFlow> {
        let cond: boolean;
        let flow: ControlFlow;

        do {
            flow = await this.execStatement(stmt.vBody);

            if (!(flow === ControlFlow.Fallthrough || flow === ControlFlow.Continue)) {
                break;
            }

            cond = await this.evalBool(stmt.vCondition);
        } while (cond);

        return flow === ControlFlow.Return ? ControlFlow.Return : ControlFlow.Fallthrough;
    }

    private async execEmitStatement(stmt: sol.EmitStatement): Promise<ControlFlow> {
        await asyncMap(stmt.vEventCall.vArguments, (arg) => this.evalExpression(arg));
        return ControlFlow.Fallthrough;
    }

    private async execExpressionStatement(stmt: sol.ExpressionStatement): Promise<ControlFlow> {
        await this.evalExpression(stmt.vExpression);
        return ControlFlow.Fallthrough;
    }

    private async execForStatement(stmt: sol.ForStatement): Promise<ControlFlow> {
        let cond: boolean;
        let flow: ControlFlow;

        this.pushScope(new BlockScope(stmt, this.infer), stmt);

        if (stmt.vInitializationExpression) {
            flow = await this.execStatement(stmt.vInitializationExpression);
            if (flow !== ControlFlow.Fallthrough) {
                return flow;
            }
        }

        while (true) {
            cond = stmt.vCondition ? await this.evalBool(stmt.vCondition) : true;

            if (!cond) {
                flow = ControlFlow.Fallthrough;
                break;
            }

            flow = await this.execStatement(stmt.vBody);

            if (!(flow === ControlFlow.Continue || flow === ControlFlow.Fallthrough)) {
                break;
            }
        }

        this.popScope();

        return flow;
    }

    private async execIfStatement(stmt: sol.IfStatement): Promise<ControlFlow> {
        const cond: boolean = await this.evalBool(stmt.vCondition);

        if (cond) {
            return await this.execStatement(stmt.vTrueBody);
        }

        if (!stmt.vFalseBody) {
            return ControlFlow.Fallthrough;
        }

        return await this.execStatement(stmt.vFalseBody);
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private execInlineAssembly(stmt: sol.InlineAssembly): ControlFlow {
        nyi("InlineAssembly");
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private async execPlaceholderStatement(stmt: sol.PlaceholderStatement): Promise<ControlFlow> {
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
            const flow = await this.execModifier(nextExecutable);

            sol.assert(flow === ControlFlow.Fallthrough, `NYI: Returning in a modifier`);
        } else {
            const body = fun.vBody;
            sol.assert(body !== undefined, `Missing fun body`);
            await this.execBlock(body);
        }

        this.scopes = savedScopes;

        return ControlFlow.Fallthrough;
    }

    private async execReturn(stmt: sol.Return): Promise<ControlFlow> {
        const funScope = this.topScopeByType(sol.FunctionDefinition);
        sol.assert(funScope !== undefined && funScope instanceof FunScope, `Missing fun scope`);

        const fun = funScope.node as sol.FunctionDefinition;

        let retVals: SolValue[];

        if (stmt.vExpression === undefined) {
            retVals = [];
        } else {
            const retVal = await this.evalExpression(stmt.vExpression);

            retVals = retVal instanceof SolTuple ? retVal.components : [retVal];
        }

        sol.assert(
            retVals.length === fun.vReturnParameters.vParameters.length || retVals.length === 0,
            `Mismatch in number of ret vals and formal returns`
        );

        if (retVals.length === 0) {
            return ControlFlow.Return;
        }

        for (let i = 0; i < retVals.length; i++) {
            funScope.assign(i, retVals[i]);
        }

        return ControlFlow.Return;
    }

    private async execRevertStatement(stmt: sol.RevertStatement): Promise<ControlFlow> {
        const args = await asyncMap(stmt.errorCall.vArguments, (arg) => this.evalExpression(arg));
        // Push to the trace before we throw
        this.trace.push(new ExecStep(stmt));

        const errDef = stmt.errorCall.vReferencedDeclaration;
        sol.assert(
            errDef instanceof sol.ErrorDefinition,
            `Expected an errro definition not {0}`,
            errDef
        );

        const payload = encodeCall(errDef, args, this.infer, this.encoderVersion);
        this.solException(new SolRevert(payload));
    }

    private execThrow(stmt: sol.Throw): ControlFlow {
        // Push to the trace before we throw
        this.trace.push(new ExecStep(stmt));
        this.solException(new SolRevert(new Uint8Array()));
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private execTryCatchClause(stmt: sol.TryCatchClause): ControlFlow {
        nyi("TryCatchClause");
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private execTryStatement(stmt: sol.TryStatement): ControlFlow {
        nyi("TryStatement");
    }

    private async execVariableDeclarationStatement(
        stmt: sol.VariableDeclarationStatement
    ): Promise<ControlFlow> {
        const curScope = stackTop(this.scopes) as BlockScope;
        if (!stmt.vInitialValue) {
            return ControlFlow.Fallthrough;
        }

        const rhs = await this.evalExpression(stmt.vInitialValue);
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

    private async execWhileStatement(stmt: sol.WhileStatement): Promise<ControlFlow> {
        let flow: ControlFlow = ControlFlow.Fallthrough;

        while (await this.evalBool(stmt.vCondition)) {
            flow = await this.execStatement(stmt.vBody);

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
    private async evalLValue(expr: sol.Expression): Promise<LValue> {
        let res: LValue;

        if (expr instanceof sol.Conditional) {
            nyi(`evalLVConditional(${expr.constructor.name})`);
        } else if (expr instanceof sol.FunctionCall) {
            //res = coerceToLValue(await this.evalFunctionCall(expr));
            nyi(`evalLValue(FunctionCall)`);
        } else if (expr instanceof sol.Identifier) {
            res = await this.evalLVIdentifier(expr);
        } else if (expr instanceof sol.IndexAccess) {
            res = await this.evalLVIndexAccess(expr);
        } else if (expr instanceof sol.MemberAccess) {
            nyi(`evalLVMemberAccess(${expr.constructor.name})`);
        } else if (expr instanceof sol.TupleExpression) {
            if (expr.vOriginalComponents.length === 1) {
                return await this.evalLValue(expr.vOriginalComponents[0] as sol.Expression);
            }

            const res: Array<LValue | null> = [];
            for (const c of expr.vOriginalComponents) {
                res.push(c === null ? null : await this.evalLValue(c));
            }

            return res;
        } else {
            nyi(`evalLValue(${expr.constructor.name})`);
        }

        this.trace.push(new EvalStep(expr, res));

        return res;
    }

    private async evalLVIdentifier(expr: sol.Identifier): Promise<LValue> {
        return { type: this.infer.typeOf(expr), view: this.lookup(expr.name) };
    }

    private async evalLVIndexAccess(expr: sol.IndexAccess): Promise<LValue> {
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
    private async evalBool(expr: sol.Expression): Promise<boolean> {
        const res = await this.evalExpression(expr);

        sol.assert(
            typeof res === "boolean",
            `Expected a boolean when evaluating {0} not {1} of type {2}`,
            expr,
            res as any,
            typeof res
        );

        return res;
    }

    // Helper to eval an expression as bigint or fail
    private async evalBigint(expr: sol.Expression): Promise<bigint> {
        const res = await this.evalExpression(expr);

        sol.assert(
            typeof res === "bigint",
            `Expected a boolean when evaluating {0} not {1} of type {2}`,
            expr,
            res as any,
            typeof res
        );

        return res;
    }

    // Helper to eval an expression as bytes
    private async evalBytes(expr: sol.Expression): Promise<Uint8Array> {
        const res = await this.evalExpression(expr);

        sol.assert(
            res instanceof Uint8Array,
            `Expected a boolean when evaluating {0} not {1} of type {2}`,
            expr,
            res as any,
            typeof res
        );

        return res;
    }

    // Helper to eval an expression as Address or fail
    private async evalAddress(expr: sol.Expression): Promise<Address> {
        const res = await this.evalExpression(expr);

        sol.assert(
            res instanceof Address,
            `Expected a boolean when evaluating {0} not {1} of type {2}`,
            expr,
            res as any,
            typeof res
        );

        return res;
    }

    private async evalExpression(expr: sol.Expression): Promise<SolValue> {
        let res: SolValue;

        if (expr instanceof sol.Assignment) {
            res = await this.evalAssignment(expr);
        } else if (expr instanceof sol.BinaryOperation) {
            res = await this.evalBinaryOperation(expr);
        } else if (expr instanceof sol.Conditional) {
            res = await this.evalConditional(expr);
        } else if (expr instanceof sol.ElementaryTypeNameExpression) {
            res = await this.evalElementaryTypeNameExpression(expr);
        } else if (expr instanceof sol.FunctionCall) {
            res = await this.evalFunctionCall(expr);
        } else if (expr instanceof sol.Identifier) {
            res = await this.evalIdentifier(expr);
        } else if (expr instanceof sol.IndexAccess) {
            res = await this.evalIndexAccess(expr);
        } else if (expr instanceof sol.IndexRangeAccess) {
            res = await this.evalIndexRangeAccess(expr);
        } else if (expr instanceof sol.Literal) {
            res = await this.evalLiteral(expr);
        } else if (expr instanceof sol.MemberAccess) {
            res = await this.evalMemberAccess(expr);
        } else if (expr instanceof sol.TupleExpression) {
            res = await this.evalTupleExpression(expr);
        } else if (expr instanceof sol.UnaryOperation) {
            res = await this.evalUnaryOperation(expr);
        } else {
            nyi(`evalExpression(${expr.constructor.name})`);
        }

        this.trace.push(new EvalStep(expr, res));
        //console.error(`eval(${sol.pp(expr)}) -> ${res}`);

        return res;
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private assign(lhs: LValue, rhs: SolValue): void {
        // Tuple assignment
        if (lhs instanceof Array) {
            sol.assert(
                rhs instanceof SolTuple && lhs.length === rhs.components.length,
                `Mismatch in tuple assignment`
            );

            for (let i = 0; i < lhs.length; i++) {
                const lhsC = lhs[i];

                if (lhsC !== null) {
                    this.assign(lhsC, rhs.components[i]);
                }
            }

            return;
        }

        const view = lhs.view;

        if (view.kind === "local") {
            view.scope.assign(view.name as any, rhs);
        } else if (view.kind === "storage") {
            this.storage = stor_assignValue(lhs.type, view, rhs, this.storage);
        } else if (view.kind === "memory") {
            nyi("Memory assignments");
        } else {
            nyi(`Assignment location ${view.kind}`);
        }
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private async evalAssignment(expr: sol.Assignment): Promise<SolValue> {
        let rhs = await this.evalExpression(expr.vRightHandSide);
        const lhs = await this.evalLValue(expr.vLeftHandSide);

        const op = expr.operator;

        if (op.length > 1) {
            sol.assert(!(lhs instanceof Array), `No operator assignments on tuples.`);

            const lhsV = await this.evalExpression(expr.vLeftHandSide);
            const type = this.infer.typeOfAssignment(expr);
            rhs = await this.evalBinaryOperationImpl(
                lhsV,
                op[0],
                rhs,
                type,
                this.getBinaryUserFunction(op[0], type, expr),
                this.isUnchecked(expr)
            );
        }

        this.assign(lhs, rhs);

        return rhs;
    }

    isUnchecked(expr: sol.Expression): boolean {
        // @todo add semver
        // @todo check if this.info.artifact.compilerVersion < 0.8.0
        return expr.getClosestParentByType(sol.UncheckedBlock) !== undefined;
    }

    private getBinaryUserFunction(
        // eslint-disable-next-line @typescript-eslint/no-unused-vars
        op: string,
        // eslint-disable-next-line @typescript-eslint/no-unused-vars
        exprT: sol.TypeNode,
        // eslint-disable-next-line @typescript-eslint/no-unused-vars
        ctx: sol.ASTNode
    ): sol.FunctionDefinition | undefined {
        // @todo - need to detect that a given (operator, type) has an associated user function attached
        return undefined;
    }

    private async evalBinaryOperationImpl(
        left: SolValue,
        operator: string,
        right: SolValue,
        type: sol.TypeNode,
        userFunction: sol.FunctionDefinition | undefined,
        unchecked: boolean
    ): Promise<SolValue> {
        // @todo - need to detect
        if (userFunction) {
            nyi("User-defined operators");
        }

        if (sol.BINARY_OPERATOR_GROUPS.Logical.includes(operator)) {
            if (!(typeof left === "boolean" && typeof right === "boolean")) {
                fail(`${operator} expects booleans not ${left} and ${right}`);
            }

            if (operator === "&&") {
                return left && right;
            }

            if (operator === "||") {
                return left || right;
            }

            fail(`Unknown logical operator ${operator}`);
        }

        if (sol.BINARY_OPERATOR_GROUPS.Equality.includes(operator)) {
            let isEqual: boolean;

            if (typeof left === "boolean" && typeof right === "boolean") {
                isEqual = left === right;
            } else if (typeof left === "bigint" && typeof right === "bigint") {
                isEqual = left === right;
            } else {
                nyi(`${left} ${operator} ${right}`);
            }

            if (operator === "==") {
                return isEqual;
            }

            if (operator === "!=") {
                return !isEqual;
            }

            fail(`Unknown equality operator ${operator}`);
        }

        if (sol.BINARY_OPERATOR_GROUPS.Comparison.includes(operator)) {
            if (!(typeof left === "bigint" && typeof right === "bigint")) {
                nyi(`${left} ${operator} ${right}`);
            }

            if (operator === "<") {
                return left < right;
            }

            if (operator === "<=") {
                return left <= right;
            }

            if (operator === ">") {
                return left > right;
            }

            if (operator === ">=") {
                return left >= right;
            }

            nyi(`Unknown comparison operator ${operator}`);
        }

        if (sol.BINARY_OPERATOR_GROUPS.Arithmetic.includes(operator)) {
            if (!(typeof left === "bigint" && typeof right === "bigint")) {
                nyi(`${left} ${operator} ${right}`);
            }

            let res: bigint;

            if (operator === "+") {
                res = left + right;
            } else if (operator === "-") {
                res = left - right;
            } else if (operator === "*") {
                res = left * right;
            } else if (operator === "/") {
                res = left / right;
            } else if (operator === "%") {
                res = left % right;
            } else if (operator === "**") {
                res = left ** right;
            } else {
                nyi(`Unknown arithmetic operator ${operator}`);
            }

            const clampedRes = this.clampIntToType(res, type);
            const overflow = clampedRes !== res;

            if (overflow && !unchecked) {
                nyi(`Exception on overflow`);
            }

            return res;
        }

        if (sol.BINARY_OPERATOR_GROUPS.Bitwise.includes(operator)) {
            if (!(typeof left === "bigint" && typeof right === "bigint")) {
                nyi(`${operator} between ${left} and ${right}`);
            }

            if (operator === "<<") {
                return left << right;
            }

            if (operator === ">>") {
                return left >> right;
            }

            if (operator === "|") {
                return left | right;
            }

            if (operator === "&") {
                return left & right;
            }

            if (operator === "^") {
                return left ^ right;
            }

            nyi(`Unknown bitwise operator ${operator}`);
        }

        nyi(`${left} ${operator} ${right}`);
    }

    private async evalBinaryOperation(expr: sol.BinaryOperation): Promise<SolValue> {
        const left = await this.evalExpression(expr.vLeftExpression);
        const right = await this.evalExpression(expr.vRightExpression);
        const operator = expr.operator;
        const type = this.infer.typeOf(expr);

        return await this.evalBinaryOperationImpl(
            left,
            operator,
            right,
            type,
            expr.vUserFunction,
            this.isUnchecked(expr)
        );
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private evalConditional(expr: sol.Conditional): SolValue {
        nyi("Conditional");
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private evalElementaryTypeNameExpression(expr: sol.ElementaryTypeNameExpression): SolValue {
        nyi("ElementaryTypeNameExpression");
    }

    private async evalAssert(expr: sol.FunctionCall): Promise<SolValue> {
        const cond = await this.evalBool(expr.vArguments[0]);

        if (cond) {
            return POISON;
        }

        if (lt(this.compilerVersion, "0.8.0")) {
            this.solException(new SolInvalid());
        }

        this.solException(new SolPanic(PanicCodes.AssertFail));
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private async evalBuiltinFunctionCall(expr: sol.FunctionCall): Promise<SolValue> {
        const callee = expr.vExpression;
        if (callee instanceof sol.Identifier) {
            if (callee.name === "assert") {
                return this.evalAssert(expr);
            }

            nyi(`evalBuiltinFunctionCall ${callee.name}`);
        }

        if (callee instanceof sol.MemberAccess) {
            if (callee.memberName === "push") {
                sol.assert(expr.vArguments.length <= 1, ``);
                const base = await this.evalLValue(callee.vExpression);
                let newEl: SolValue;

                if (expr.vArguments.length === 0) {
                    // 0-initialized case
                    // @todo 0-init of maps? (e.g. mapping(uint=>uint)[] arr, arr.push())
                    nyi("0-init push");
                } else {
                    newEl = await this.evalExpression(expr.vArguments[0]);
                }

                sol.assert(!(base instanceof Array), ``);

                console.error(`base: `, base, ` newEl: `, newEl);
                const len = stor_mustDecodeValue(
                    sol.types.uint256,
                    base.view as StorageLocation,
                    this.storage,
                    this.infer
                )[0];

                sol.assert(
                    typeof len === `bigint` &&
                        base.type instanceof sol.PointerType &&
                        base.type.to instanceof sol.ArrayType,
                    ``
                );

                // Increment length
                this.assign({ type: sol.types.uint256, view: base.view }, len + 1n);
                // Push new value
                const newValLoc = stor_decodeIndexLoc(
                    base.type,
                    base.view as StorageLocation,
                    len,
                    this.storage,
                    this.infer
                );

                sol.assert(newValLoc !== undefined, ``);

                const newValView: TypedVmDataView = {
                    type: base.type.to.elementT,
                    view: newValLoc
                };

                this.assign(newValView, newEl);

                return newValView;
            }
        }

        nyi(`evalBuiltinFunctionCall ${callee.constructor.name}`);
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private async evalInternalFunctionCall(expr: sol.FunctionCall): Promise<SolValue> {
        const args = await asyncMap(expr.vArguments, (arg) => this.evalExpression(arg));
        const callee = getFunCallTarget(expr, this.infer);
        const retVals = await this.execFun(callee, args);

        return retVals.length === 0
            ? POISON
            : retVals.length === 1
              ? retVals[0]
              : new SolTuple(retVals);
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private async evalExternalFunctionCall(
        expr: sol.FunctionCall,
        propagateRevert = true
    ): Promise<SolValue> {
        const [callee, value, gas, salt] = await this.decodeCallee(expr.vExpression);
        const args = await asyncMap(expr.vArguments, (e) => this.evalExpression(e));

        sol.assert(callee instanceof sol.MemberAccess, `Unexpected external callee {0}`, callee);

        const calleeDef = callee.vReferencedDeclaration;
        sol.assert(
            calleeDef instanceof sol.FunctionDefinition,
            `Unexpected external callee ${callee.constructor.name}`
        );
        const addr = await this.evalAddress(callee.vExpression);

        const data = encodeCall(calleeDef, args, this.infer, this.encoderVersion);

        const msg: SolMessage = {
            to: addr,
            data: data,
            value: value || 0n,
            gas: gas || 0n,
            salt
        };

        let res: SolCallResult;

        let opcode: OPCODES;
        if (calleeDef.stateMutability === sol.FunctionStateMutability.View) {
            opcode = OPCODES.STATICCALL;
        } else if (
            calleeDef.vScope instanceof sol.ContractDefinition &&
            calleeDef.vScope.kind === sol.ContractKind.Library
        ) {
            opcode = OPCODES.DELEGATECALL;
        } else {
            opcode = OPCODES.CALL;
        }

        this.trace.push(new ExternalCallStep(msg, opcode));

        if (calleeDef.stateMutability === sol.FunctionStateMutability.View) {
            res = await this.env.staticcall(msg);
        } else if (
            calleeDef.vScope instanceof sol.ContractDefinition &&
            calleeDef.vScope.kind === sol.ContractKind.Library
        ) {
            res = await this.env.delegatecall(msg);
        } else {
            res = await this.env.call(msg);
        }

        if (res.reverted && propagateRevert) {
            throw new SolRevert(res.data);
        }

        // @todo what happens when the callee encoder version is different from the caller?
        // 2 cases - older and newer. Need separate tests!!!
        const retVals = decodeReturns(calleeDef, res.data, this.infer, this.encoderVersion);

        return retVals.length === 0
            ? POISON
            : retVals.length === 1
              ? retVals[0]
              : new SolTuple(retVals);
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private async evalTypeConversion(expr: sol.FunctionCall): Promise<SolValue> {
        const exprT = this.infer.typeOf(expr.vExpression);
        sol.assert(expr.vArguments.length === 1 && exprT instanceof sol.TypeNameType, ``);

        const innerV = await this.evalExpression(expr.vArguments[0]);
        const toT = exprT.type;

        if (toT instanceof sol.IntType && typeof innerV === "bigint") {
            return sol.clampIntToType(innerV, toT);
        }

        nyi(`evalTypeConversion ${innerV} -> ${toT.pp()}`);
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private async evalStructConstructorCall(expr: sol.FunctionCall): Promise<SolValue> {
        nyi("evalStructConstructorCall");
    }

    /**
     * Helper to decode a function callee for an external call. Returns a tuple:
     * [actual callee, value passed, gas passed]
     */
    private async decodeCallee(
        expr: sol.Expression
    ): Promise<[sol.Expression, bigint | undefined, bigint | undefined, Uint8Array | undefined]> {
        let callee: sol.Expression;
        let value: bigint | undefined;
        let gas: bigint | undefined;
        let salt: Uint8Array | undefined;

        while (true) {
            if (expr instanceof sol.FunctionCallOptions) {
                for (const [name, optExpr] of expr.vOptionsMap) {
                    if (name === "value") {
                        value = await this.evalBigint(optExpr);
                    } else if (name === "gas") {
                        gas = await this.evalBigint(optExpr);
                    } else if (name === "salt") {
                        salt = await this.evalBytes(optExpr);
                    } else {
                        fail(`Unexpected call option ${name}`);
                    }
                }

                expr = expr.vExpression;
                continue;
            } else if (expr instanceof sol.FunctionCall) {
                if (expr.vFunctionName === "value") {
                    value = await this.evalBigint(expr.vArguments[0]);
                } else if (expr.vFunctionName === "gas") {
                    gas = await this.evalBigint(expr.vArguments[0]);
                } else if (expr.vFunctionName === "salt") {
                    salt = await this.evalBytes(expr.vArguments[0]);
                } else {
                    fail(`Unexpected call option ${expr.vFunctionName}`);
                }
                expr = expr.vExpression;
                continue;
            }

            callee = expr;
            break;
        }

        return [callee, value, gas, salt];
    }

    private async evalNewCall(expr: sol.FunctionCall, propagateRevert = true): Promise<SolValue> {
        const [callee, value, gas, salt] = await this.decodeCallee(expr.vCallee);

        sol.assert(callee instanceof sol.NewExpression, ``);

        const newT = this.infer.typeNameToTypeNode(callee.vTypeName);
        const args = await asyncMap(expr.vArguments, (arg) => this.evalExpression(arg));

        if (
            newT instanceof sol.UserDefinedType &&
            newT.definition instanceof sol.ContractDefinition
        ) {
            const constr = findConstructor(newT.definition);
            const constrArgs =
                constr === undefined
                    ? new Uint8Array()
                    : encodeCall(constr, args, this.infer, this.encoderVersion);

            const info = this.artifactManager.getContractInfo(newT.definition);

            sol.assert(info !== undefined, `Missing info for ${newT.definition.name}`);

            const data = concatBytes(
                hexToBytes(info.contractArtifact.evm.bytecode.object),
                constrArgs
            );

            const msg = {
                to: ZERO_ADDRESS,
                data,
                value: value || 0n,
                gas: gas || 0n,
                salt
            };

            this.trace.push(new ExternalCallStep(msg, msg.salt ? OPCODES.CREATE2 : OPCODES.CREATE));
            const res = await this.env.create(msg);

            if (res.reverted && propagateRevert) {
                throw new SolRevert(res.data);
            }

            sol.assert(res.data.length === 20, `Expected an address`);
            return new Address(res.data);
        }

        nyi(`new(${newT.pp()})`);
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private async evalFunctionCall(expr: sol.FunctionCall): Promise<SolValue> {
        if (expr.kind === sol.FunctionCallKind.TypeConversion) {
            return await this.evalTypeConversion(expr);
        } else if (expr.kind === sol.FunctionCallKind.StructConstructorCall) {
            return await this.evalStructConstructorCall(expr);
        } else {
            if (expr.vFunctionCallType === sol.ExternalReferenceType.Builtin) {
                const callee = expr.vCallee;

                if (callee instanceof sol.NewExpression) {
                    return await this.evalNewCall(expr);
                }

                return await this.evalBuiltinFunctionCall(expr);
            }

            if (this.infer.isFunctionCallExternal(expr)) {
                return await this.evalExternalFunctionCall(expr);
            }

            return await this.evalInternalFunctionCall(expr);
        }
    }

    /**
     * Given a typed view, return the corresponding value at that location.
     * If the view is of a primitive type, return the type at that location.
     * If the view is of a complex type, return the view itself.
     * If the view if of a local variable of a complex type (pointer) return the value inside.
     */
    private getValAtLoc(view: TypedVmDataView): SolValue {
        const type = view.type;
        const loc = view.view;

        // Case 1 - local "stack" variable - just return it
        if (loc.kind === "local") {
            // For local variables just return their value from the "stack". If this is a refernce type,
            // we will have stored a pointer on the stack
            const res = loc.scope.deref(loc);
            sol.assert(res !== undefined, `Failed deref`);

            return res;
        }

        sol.assert(
            loc.kind === DataLocationKind.Memory ||
                loc.kind === DataLocationKind.Storage ||
                loc.kind === DataLocationKind.CallData,
            ``
        );

        // Case 2 -  a primitive type from memory/storage/calldata - just read it from memory
        if (isTypePrimitive(type)) {
            if (loc.kind === DataLocationKind.Memory) {
                const res = mem_decodeValue(type, loc, this.memory, this.infer);
                sol.assert(
                    res !== undefined,
                    `Unable to decode value of type {0} from mem addr {1}`,
                    type,
                    loc.address
                );

                return res[0];
            } else if (loc.kind === DataLocationKind.Storage) {
                const res = stor_decodeValue(type, loc, this.storage, this.infer);
                sol.assert(
                    res !== undefined,
                    `Unable to decode value of type {0} from mem addr {1}`,
                    type,
                    loc.address
                );

                return res[0];
            } else if (loc.kind === DataLocationKind.CallData) {
                const abiType = this.infer.toABIEncodedType(type, sol.ABIEncoderVersion.V2);
                const res = cd_decodeValue(
                    abiType,
                    type,
                    loc,
                    this.msg.data,
                    BigInt(this.cdArgBaseOff),
                    this.infer
                );

                sol.assert(
                    res !== undefined,
                    `Unable to decode value of type {0} from mem addr {1}`,
                    type,
                    loc.address
                );

                return res[0];
            } else {
                nyi(`Identifier loc ${loc}`);
            }
        }

        // Case 3 - A complex type in storage/memory/calldata - evalutes to just the address
        sol.assert(loc.kind !== DataLocationKind.Storage || loc.endOffsetInWord === 32, ``);

        return view;
    }

    private evalIdentifier(expr: sol.Identifier): SolValue {
        const view = this.lookup(expr.name);
        const type = this.infer.typeOf(expr);

        return this.getValAtLoc({ type, view });
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private async evalIndexAccess(expr: sol.IndexAccess): Promise<SolValue> {
        const baseV = await this.evalExpression(expr.vBaseExpression);
        const baseT = this.infer.typeOf(expr.vBaseExpression);
        const resT = this.infer.typeOf(expr);
        sol.assert(expr.vIndexExpression !== undefined, ``);

        const idxV = await this.evalExpression(expr.vIndexExpression);

        sol.assert(isTypedVMDataView(baseV) && baseT instanceof sol.PointerType, ``);
        const state = { storage: this.storage, memory: this.memory } as any as StepState;

        const valLoc = decodeIndexLoc(
            { type: baseV.type, loc: baseV.view as DataLocation },
            idxV,
            state,
            this.infer
        );

        sol.assert(valLoc !== undefined, ``);

        if (isTypePrimitive(valLoc.type)) {
            return decodeValue(valLoc, state, this.infer);
        }
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

        if (expr.kind === sol.LiteralKind.Bool) {
            return expr.value === "true";
        }

        nyi("Literal");
    }

    private async evalBuiltinMemberAccess(expr: sol.MemberAccess): Promise<SolValue> {
        const base = await this.evalExpression(expr.vExpression);
        const baseT = this.infer.typeOf(expr.vExpression);

        if (
            baseT instanceof sol.PointerType &&
            baseT.to instanceof sol.ArrayType &&
            expr.memberName === "length"
        ) {
            sol.assert(isTypedVMDataView(base), ``);

            return decodeValue(
                { type: sol.types.uint256, loc: base.view as DataLocation },
                {
                    memory: this.memory,
                    storage: this.storage
                } as any as StepState,
                this.infer,
                new Map()
            );
        }

        nyi(`evalBuiltinMemberAccess(${expr.memberName})`);
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private async evalMemberAccess(expr: sol.MemberAccess): Promise<SolValue> {
        const def = expr.vReferencedDeclaration;

        if (def === undefined) {
            return this.evalBuiltinMemberAccess(expr);
        }
        nyi("MemberAccess");
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private async evalTupleExpression(expr: sol.TupleExpression): Promise<SolValue> {
        const components = await asyncMap(expr.vOriginalComponents, (c) => {
            sol.assert(c !== null, `Unexpected eval of null tuple component`);
            return this.evalExpression(c);
        });
        return components.length === 1 ? components[0] : new SolTuple(components);
    }

    clampIntToType(val: bigint, type: sol.TypeNode): bigint {
        sol.assert(
            type instanceof sol.IntType || type instanceof sol.IntLiteralType,
            `Unexpected int type {0}`,
            type
        );

        if (type instanceof sol.IntLiteralType) {
            return val;
        }

        return sol.clampIntToType(val, type);
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    private async evalUnaryOperation(expr: sol.UnaryOperation): Promise<SolValue> {
        const op = expr.operator;

        if (expr.vUserFunction) {
            nyi(`User-defined functions`);
        }

        if (op === "!") {
            return !(await this.evalBool(expr.vSubExpression));
        }

        if (op === "-") {
            const rawRes = -(await this.evalBigint(expr.vSubExpression));
            const type = this.infer.typeOf(expr);
            const clampedRes = this.clampIntToType(rawRes, type);

            if (rawRes !== clampedRes && !this.isUnchecked(expr)) {
                nyi(`Exception on overflow`);
            }

            return clampedRes;
        }

        if (op === "~") {
            nyi(`Unary bitwise not`);
        }

        if (op === "delete") {
            nyi("delete");
        }

        if (op === "++" || op === "--") {
            const type = this.infer.typeOf(expr) as sol.IntType;
            const inner = await this.evalBigint(expr.vSubExpression);
            const lv = await this.evalLValue(expr.vSubExpression);

            const newVal = await this.evalBinaryOperationImpl(
                inner,
                op[0],
                1n,
                type,
                this.getBinaryUserFunction(op[0], type, expr),
                this.isUnchecked(expr)
            );

            this.assign(lv, newVal);

            return expr.prefix ? newVal : inner;
        }

        nyi(`Unknown unary op ${expr.operator}`);
    }
}
