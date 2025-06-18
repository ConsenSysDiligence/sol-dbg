import {
    ArrayType,
    PackedArrayType,
    PointerType,
    DataLocation as SolDataLocation,
    TypeNode
} from "solc-typed-ast";
import { MAX_ARR_DECODE_LIMIT, uint256 } from "../../utils/misc";
import { DataLocationKind, MemoryLocationKind, StackLocation, StepState } from "../types";
import { st_decodeInt } from "./stack";
import { Poison, Value } from "./value";

export function solLocToMemoryLocationKind(loc: SolDataLocation): MemoryLocationKind {
    if (loc === SolDataLocation.Default) {
        return DataLocationKind.Memory;
    }

    return loc as unknown as MemoryLocationKind;
}

/**
 * Array pointers to calldata in the stack are stored as 2 slots - offset and
 * length. This is to support array slices. Return the offset and length of the
 * array (slice).
 */
export function getCDArrayInStackOffAndLen(
    loc: StackLocation,
    state: StepState
): [bigint, number] | [undefined, undefined] {
    const off = st_decodeInt(uint256, loc, state.evmStack);

    if (off === undefined) {
        return [undefined, undefined];
    }

    const len = st_decodeInt(
        uint256,
        { kind: loc.kind, offsetFromTop: loc.offsetFromTop - 1 },
        state.evmStack
    );

    if (len === undefined) {
        return [undefined, undefined];
    }

    if (len > MAX_ARR_DECODE_LIMIT) {
        return [undefined, undefined];
    }

    return [off, Number(len)];
}

export function isCalldataArrayType(typ: TypeNode): boolean {
    return (
        typ instanceof PointerType &&
        ((typ.to instanceof ArrayType && typ.to.size === undefined) ||
            typ.to instanceof PackedArrayType) &&
        typ.location === SolDataLocation.CallData
    );
}

/**
 * Returns true IFF the  n >= min && n <= max
 *
 * @param n
 * @param min
 * @param max
 */
export function inRange(n: number | bigint, min: number | bigint, max: number | bigint): boolean {
    return BigInt(n) >= BigInt(min) && BigInt(n) <= BigInt(max);
}

/**
 * Return the sum of the give numbers.
 * @param ns
 * @returns
 */
export function sum(...ns: number[]): number {
    let res = 0;
    for (const n of ns) {
        res += n;
    }

    return res;
}

export function isFailure(t: Value): t is Poison {
    return t instanceof Poison;
}

export function isTypeStringDynamicArray(t: string): boolean {
    return t.endsWith("[]");
}

export function isTypeStringMapping(t: string): boolean {
    return t.startsWith("mapping(");
}
