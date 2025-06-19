import { TypeNode } from "solc-typed-ast";
import { DecodingFailure, Value } from "./value";
import { Memory, Stack } from "../types";

export type StateArea = Memory | Stack | Storage;

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
    //abstract encode(state: State, value: Val)
}
