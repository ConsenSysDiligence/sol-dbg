import { DecodingFailure, Struct, Value } from "./value";
import { Memory, Stack, Storage } from "../types";
import { BaseRuntimeType } from "../runtime_types";

export type StateArea = Memory | Stack | Storage;

export class EncodingError extends Error {
    constructor(public readonly reason: string) {
        super();
    }
}

/**
 * Global switch for wether strings should be treated as bytes in all views.
 * Its hacky to use global switches for such deep cutting semantic behavior, but I
 * can't think of a cleaner way to do this.
 */
let treatStringsAsBytes = false;

export function shouldTreatStringsAsBytes(): boolean {
    return treatStringsAsBytes;
}

export function setTreatStringAsBytes(v: boolean): void {
    treatStringsAsBytes = v;
}

/**
 * Base class for all data {@link View}s. Views are parametrized by the {@link State} they project from (e.g. Memory, Storage, Calldata),
 * the Solidity {@link Type} of the element they are decoding as well as Typescript type of the {@link Value} they decode.
 */
export abstract class View<
    State = StateArea,
    Val extends Value = Value,
    Loc = any,
    Type extends BaseRuntimeType = BaseRuntimeType
> {
    constructor(
        public readonly type: Type,
        protected loc: Loc
    ) {}

    /**
     * Decode a value from the given State
     * @param state
     */
    abstract decode(state: State): Val | DecodingFailure;
    abstract pp(): string;
}

interface ViewI<State, Val> {
    decode(state: State): Val | DecodingFailure;
    pp(): string;
}

export interface IndexableView<
    IdxType extends Value,
    State extends StateArea,
    ValViewT extends View<State>
> extends ViewI<State, Value> {
    indexView(key: IdxType, state: State): ValViewT | DecodingFailure;
}

export interface ArrayLikeView<
    State extends StateArea,
    KeyViewT extends View<State>
> extends IndexableView<bigint, State, KeyViewT> {
    size(state: State): bigint | DecodingFailure;
}

export interface StructView<State, FieldViewT extends View<State>> extends ViewI<State, Struct> {
    fieldView(name: string): FieldViewT | DecodingFailure;
}

export interface PointerView<State, ToViewT extends View<State>> extends ViewI<State, Value> {
    toView(state: State): ToViewT | DecodingFailure;
}
