import { Address } from "@ethereumjs/util";
import { assert, FunctionDefinition } from "solc-typed-ast";

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
    | Map<Value, Value>; // Mappings
