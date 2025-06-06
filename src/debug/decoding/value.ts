import { Address } from "@ethereumjs/util";
import { FunctionDefinition } from "solc-typed-ast";

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
    | Struct; // Structs
