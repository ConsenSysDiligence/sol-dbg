import { Address } from "@ethereumjs/util";
import * as sol from "solc-typed-ast";
import { DataLocation, Memory, Storage } from "../debug";
import { BlockScope, FunScope } from "./scope";

export const noType = new sol.TupleType([]);
export class Poison {}
export const POISON = new Poison();

export type SolStorage = Storage;
export type SolMemory = Memory;

export interface VmLocalView {
    kind: "local";
    scope: FunScope | BlockScope;
    name: string | number;
}

export type VmDataView = DataLocation | VmLocalView;

export class SolTuple {
    constructor(public readonly components: SolValue[]) {}
}
export type SolValue =
    | bigint
    | boolean
    | string
    | Uint8Array
    | VmDataView
    | SolValue[]
    | SolTuple
    | Poison;

export type LValue = VmDataView | Array<LValue | null>;

export interface SolTypedValue {
    val: SolValue;
    type: sol.TypeNode;
}

export interface SolNamedTypedValue extends SolTypedValue {
    name: string;
}

export interface SolMessage {
    to: Address;
    data: Uint8Array;
    gas: bigint;
    value: bigint;
    salt: Uint8Array | undefined;
}
