import { InferType, MappingType, TypeNode, UserDefinedType } from "solc-typed-ast";
import { Memory, Stack, Storage } from "../types";

export type SolValue = any;
type ValueOrView = SolValue | View;
type PossibleStates = Storage | Memory | Stack;

// Generic View Classes
export abstract class View<T extends TypeNode = TypeNode> {
    constructor(
        public readonly type: T,
        public readonly infer: InferType
    ) {}
    abstract decode(state: PossibleStates): ValueOrView;
}

export abstract class BaseAddressView<T extends TypeNode = TypeNode> extends View<T> {
    protected _address: bigint;

    constructor(type: T, infer: InferType, address: bigint) {
        super(type, infer);
        this._address = address;
    }

    get address(): bigint {
        return this._address;
    }

    abstract decode(state: PossibleStates): ValueOrView;
}

export interface IndexableView extends View {
    decodeIdx(arg: PossibleStates, idx: SolValue): View | undefined;
}

export interface MapView extends View<MappingType> {
    decodeIdx(arg: PossibleStates, idx: Uint8Array): View;
}

export interface ArrayView extends IndexableView {
    decodeLen(arg: PossibleStates): bigint | undefined;
    decodeIdx(arg: PossibleStates, idx: bigint): View | undefined;
}

export interface StructView extends View<UserDefinedType> {
    decodeField(arg: PossibleStates, field: string): View | undefined;
}
