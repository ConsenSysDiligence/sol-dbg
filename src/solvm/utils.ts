import * as sol from "solc-typed-ast";
import { nyi } from "./exceptions";
import { LValue, SolValue } from "./state";

export function zeroValue(typ: sol.TypeNode): SolValue {
    if (typ instanceof sol.IntType) {
        return 0n;
    }

    if (typ instanceof sol.BoolType) {
        return false;
    }

    nyi(`NYI zeroValue`);
}

export function isExternal(call: sol.FunctionCall, infer: sol.InferType): boolean {
    const callee = call.vCallee;

    if (!(callee instanceof sol.MemberAccess)) {
        return false;
    }

    const baseT = infer.typeOf(callee.vExpression);

    return (
        baseT instanceof sol.UserDefinedType && baseT.definition instanceof sol.ContractDefinition
    );
}

export function getFunCallTarget(
    call: sol.FunctionCall,
    infer: sol.InferType
): sol.FunctionDefinition {
    const decl = call.vReferencedDeclaration;

    sol.assert(decl instanceof sol.FunctionDefinition, `NYI fun def {0}`, decl);

    const contractScope = call.getClosestParentByType(sol.ContractDefinition);

    const callee = contractScope ? sol.resolveCallable(contractScope, decl, infer) : decl;

    sol.assert(callee instanceof sol.FunctionDefinition, `NYI callee {0}`, callee);

    return callee;
}

export function coerceToLValue(val: SolValue): LValue {
    if (val instanceof Array) {
        return val.map(coerceToLValue);
    }

    sol.assert(typeof val === "object" && "kind" in val, ``);

    return val;
}

export function bigintToInt(v: bigint, min?: number, max?: number): number {
    min = min === undefined ? Number.MIN_SAFE_INTEGER : min;
    max = max === undefined ? Number.MAX_SAFE_INTEGER : max;
    sol.assert(v >= min && v <= max, `{0} outside of safe range [{1}, {2}]`, v, min, max);

    return Number(v);
}

export function checkOOB(
    v: bigint | number | undefined,
    min: number,
    max: number
): number | undefined;
export function checkOOB(
    v: bigint | number | undefined,
    min: bigint,
    max: bigint
): bigint | undefined;
export function checkOOB(
    v: bigint | number | undefined,
    min: number,
    max: bigint
): bigint | undefined;
export function checkOOB(
    v: bigint | number | undefined,
    min: bigint,
    max: number
): bigint | undefined;
export function checkOOB(
    v: bigint | number | undefined,
    min: number | bigint,
    max: number | bigint
): number | bigint | undefined {
    if (v === undefined) {
        return undefined;
    }

    if (typeof min === "number" && typeof max === "number") {
        if (v < min || v > max) {
            return undefined;
        }

        return Number(v);
    }

    if (v < min || v > max) {
        return undefined;
    }

    return v;
}
