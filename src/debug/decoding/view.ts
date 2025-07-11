import { TypeNode } from "solc-typed-ast";
import { DecodingFailure, Value } from "./value";
import { Memory, Stack, Storage } from "../types";

export type StateArea = Memory | Stack | Storage;

export class EncodingError extends Error {
    constructor(public readonly reason: string) {
        super();
    }
}

/**
 * Base class for all data {@link View}s. Views are parametrized by the {@link State} they project from (e.g. Memory, Storage, Calldata),
 * the Solidity {@link Type} of the element they are decoding as well as Typescript type of the {@link Value} they decode.
 */
export abstract class View<
    State = StateArea,
    Val extends Value = Value,
    Loc = any,
    Type extends TypeNode = TypeNode
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

export interface IndexableView<
    IdxType extends Value,
    State extends StateArea,
    KeyViewT extends View<State>
> {
    indexView(key: IdxType, state: State): KeyViewT | DecodingFailure;
}

export interface StructView<State, FieldViewT extends View<State>> {
    fieldView(name: string): FieldViewT | DecodingFailure;
}

export interface PointerView<State, ToViewT extends View<State>> {
    toView(state: State): ToViewT | DecodingFailure;
}
