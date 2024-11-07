import * as sol from "solc-typed-ast";
import { ContractLayout, getContractLayout } from "../debug";
import { zip } from "../utils";
import { nyi } from "./exceptions";
import { SolValue, VmDataView } from "./state";
import { ScopeNode } from "./trace";
import { zeroValue } from "./utils";

export type Scope = GlobalScope | ContractScope | FunScope | BlockScope;

export interface ScopeI {
    lookup(name: string): VmDataView | undefined;
}

export abstract class BaseScope implements ScopeI {
    protected defs = new Map<string, VmDataView>();
    constructor(public readonly node: ScopeNode) {}

    lookup(name: string): VmDataView | undefined {
        return this.defs.get(name);
    }
}

export class FunScope extends BaseScope {
    private args = new Map<string, SolValue>();
    private rets: SolValue[] = [];
    private nameToIdx = new Map<string, number>();

    returns(): SolValue[] {
        return this.rets;
    }

    lookupVal(name: string | number): SolValue {
        if (typeof name === "number") {
            sol.assert(this.rets.length > name, `Return oob ${name}`);
            return this.rets[name];
        }

        if (this.nameToIdx.has(name)) {
            sol.assert(
                this.rets.length > (this.nameToIdx.get(name) as number),
                `Return oob ${name}`
            );
            return this.rets[this.nameToIdx.get(name) as number];
        }

        const res = this.args.get(name);

        sol.assert(res !== undefined, `Unknown local ${name}`);

        return res;
    }

    constructor(
        fun: sol.FunctionDefinition | sol.ModifierInvocation,
        args: SolValue[],
        infer: sol.InferType
    ) {
        super(fun);
        const formals = (
            fun instanceof sol.FunctionDefinition ? fun : (fun.vModifier as sol.ModifierDefinition)
        ).vParameters.vParameters;

        const rets = fun instanceof sol.FunctionDefinition ? fun.vReturnParameters.vParameters : [];

        for (const [decl, val] of zip(formals, args)) {
            this.defs.set(decl.name, { kind: "local", scope: this, name: decl.name });
            this.args.set(decl.name, val);
        }

        for (let i = 0; i < rets.length; i++) {
            const decl = rets[i];

            if (decl.name === "") {
                continue;
            }

            this.defs.set(decl.name, { kind: "local", scope: this, name: i });
            this.rets.push(zeroValue(infer.variableDeclarationToTypeNode(decl)));
            if (decl.name !== "") {
                this.nameToIdx.set(decl.name, i);
            }
        }
    }

    assign(name: string | number, value: SolValue): void {
        if (typeof name === "number") {
            this.rets[name] = value;
        } else {
            this.args.set(name, value);
        }
    }
}

export class BlockScope extends BaseScope {
    locals = new Map<string, SolValue>();

    constructor(node: sol.Block | sol.UncheckedBlock | sol.ForStatement, infer: sol.InferType) {
        super(node);
        let declStmts: sol.VariableDeclarationStatement[];

        if (node instanceof sol.ForStatement) {
            declStmts =
                node.vInitializationExpression instanceof sol.VariableDeclarationStatement
                    ? [node.vInitializationExpression]
                    : [];
        } else {
            declStmts = node.vStatements.filter(
                (s) => s instanceof sol.VariableDeclarationStatement
            ) as sol.VariableDeclarationStatement[];
        }

        for (const decl of declStmts) {
            for (const v of decl.vDeclarations) {
                const type = infer.variableDeclarationToTypeNode(v);
                this.locals.set(v.name, zeroValue(type));
                this.defs.set(v.name, { kind: "local", scope: this, name: v.name });
            }
        }
    }

    assign(name: string, v: SolValue): void {
        this.locals.set(name, v);
    }
}

export class ContractScope extends BaseScope {
    protected layout: ContractLayout;

    lookup(name: string): VmDataView | undefined {
        return this.defs.get(name);
    }

    constructor(contract: sol.ContractDefinition, infer: sol.InferType) {
        super(contract);

        const layout = getContractLayout(infer, contract);

        sol.assert(layout !== undefined, `Failed computing layout for ${contract.name}`);

        this.layout = layout;

        for (const [def, view] of layout) {
            this.defs.set(def.name, view.loc);
        }
    }
}

export class GlobalScope extends BaseScope {
    constructor(unit: sol.SourceUnit) {
        super(unit);

        for (const def of unit.vVariables) {
            sol.assert(
                def.vValue !== undefined,
                `Unexpected global constant without an initializer`
            );

            // @todo require isolating the evaulator for constant evaulation
            nyi(`evaluating global constants`);
        }
    }
}
