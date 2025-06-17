import { Address } from "@ethereumjs/util";
import { assert, FunctionDefinition, TypeNode, UserDefinedType } from "solc-typed-ast";
import { View } from "./view";

export class ExternalFunRef {
    constructor(
        public readonly address: Address,
        public readonly selector: Uint8Array
    ) {}
}

export class InternalFunRef {
    constructor(public readonly fun: FunctionDefinition) {}
}

export type FunctionValue = ExternalFunRef | InternalFunRef;

export class Struct {
    constructor(public readonly entries: Array<[string, Value]>) {}
    field(name: string): Value {
        // @todo optimize if we end up using this more
        for (const [fieldName, val] of this.entries) {
            if (name === fieldName) {
                return val;
            }
        }

        assert(false, `No field ${name} in ${this.entries}`);
    }
}

export class Slice {
    constructor(
        public readonly array: Value[],
        public readonly start: number,
        public readonly end: number
    ) {}
}

export abstract class Poison {
    abstract pp(): string;
}

export class MissingTypeInfo extends Poison {
    constructor(public readonly missingType: UserDefinedType) {
        super();
    }

    pp(): string {
        return `<missing info for ${this.missingType.name}>`;
    }
}

export class DecodingFailure extends Poison {
    constructor(public readonly reason: string) {
        super();
    }

    pp(): string {
        return `<err:${this.reason}>`;
    }
}

/**
 * Return true if `v` is a type of Poison.
 * @param v
 * @returns
 */
export function isPoison(v: Value): v is Poison {
    return v instanceof Poison;
}

/**
 * Return true if `v` contains any Poison inside.
 * @param v
 * @returns
 */
export function hasPoison(v: Value): boolean {
    if (isPoison(v)) {
        return true;
    }

    if (v instanceof Array) {
        for (const el of v) {
            if (hasPoison(el)) {
                return true;
            }
        }

        return false;
    }

    if (v instanceof Slice) {
        for (const el of v.array.slice(v.start, v.end)) {
            if (hasPoison(el)) {
                return true;
            }
        }

        return false;
    }

    if (v instanceof Struct) {
        for (const [, field] of v.entries) {
            if (hasPoison(field)) {
                return true;
            }
        }

        return false;
    }

    if (v instanceof Map) {
        for (const [key, val] of v.entries()) {
            if (hasPoison(key) || hasPoison(val)) {
                return true;
            }
        }

        return false;
    }

    return false;
}

/**
 * Typescript types corresponding to various Solidity types.
 * Includes both primitive (value) types and compound types
 */
export type Value =
    | bigint // int/uint
    | boolean // bool
    | Uint8Array // byte, bytesN, bytes
    | string // string
    | number // enum
    | Address // address
    | FunctionValue // function types
    //  | @todo Rationals?
    | Value[] // sized and unsized arrays
    | Slice // array slices
    | Struct // Structs
    | Map<Value, Value> // Mappings
    // Stack is the only state type that may have pointers into other areas.
    // Therefore decoding stack data may return a data view into another area
    | View<any, Value, any, TypeNode>
    | Poison;
