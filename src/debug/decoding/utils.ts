import {
    ArrayType,
    PackedArrayType,
    PointerType,
    DataLocation as SolDataLocation,
    TypeNode
} from "solc-typed-ast";
import { DecodingFailure, Value } from "./value";
import { PointerView, View } from "./view";
import { PointerCalldataView } from "./calldata";
import { PointerMemView } from "./memory";
import { PointerStorageView } from "./storage";
import { PointerStackView } from "./stack";

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

export function isFailure(t: Value): t is DecodingFailure {
    return t instanceof DecodingFailure;
}

export function isTypeStringDynamicArray(t: string): boolean {
    return t.endsWith("[]");
}

export function isTypeStringMapping(t: string): boolean {
    return t.startsWith("mapping(");
}

export function isTypeStringStruct(t: string): boolean {
    return t.startsWith("struct ");
}

export function isTypeStringStatic32BytesInStorage(t: string): boolean {
    return isTypeStringDynamicArray(t) || isTypeStringMapping(t);
}

export function isPointerView(v: any): v is PointerView<any, View> {
    return (
        v instanceof PointerCalldataView ||
        v instanceof PointerMemView ||
        v instanceof PointerStorageView ||
        v instanceof PointerStackView
    );
}
