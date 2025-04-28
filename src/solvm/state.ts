import { Address } from "@ethereumjs/util";
import * as sol from "solc-typed-ast";
import { DataLocation, Memory, Storage } from "../debug";
import { BlockScope, FunScope } from "./scope";

export const noType = new sol.TupleType([]);
export class Poison {}
export const POISON = new Poison();

export type SolStorage = Storage;
export type SolMemory = Memory;

/**
 * A view to a stack local variable in the VM
 */
export interface VmLocalView {
    kind: "local";
    scope: FunScope | BlockScope;
    name: string | number;
}

// View to a value anywhere in the vm - stack locals (scopes), memory, storage
export type VmDataView = DataLocation | VmLocalView;
export type TypedVmDataView = { type: sol.TypeNode; view: VmDataView };

export function isTypedVMDataView(a: any): a is TypedVmDataView {
    return typeof a === "object" && "type" in a && "view" in a;
}

export class SolTuple {
    constructor(public readonly components: SolValue[]) {}
}

export type SolValue =
    | bigint
    | boolean
    | Uint8Array
    | Address
    | SolTuple
    | TypedVmDataView
    | Poison;

export type LValue = TypedVmDataView | Array<LValue | null>;

export interface SolMessage {
    to: Address;
    data: Uint8Array;
    gas: bigint;
    value: bigint;
    salt: Uint8Array | undefined;
}
