import { TypeNode } from "solc-typed-ast";
import { Value } from "./value";
import { Memory, Stack } from "../types";

export class DecodingError<State> extends Error {
    constructor(
        public readonly view: View<any, any, any, any>,
        public readonly state: State | undefined,
        msg: string = ""
    ) {
        super(`Error decoding ${view.pp()}: ${msg}`);
    }
}

export type StateArea = Memory | Stack | Storage;

/**
 * Base class for all data {@link View}s. Views are parametrized by the {@link State} they project from (e.g. Memory, Storage, Calldata),
 * the Solidity {@link Type} of the element they are decoding as well as Typescript type of the {@link Value} they decode.
 */
export abstract class View<State = StateArea, Val extends Value = Value, Loc = any, Type extends TypeNode = TypeNode> {
    constructor(
        public readonly type: Type,
        protected loc: Loc
    ) { }

    /**
     * Decode a value from the given State
     * @param state
     */
    abstract decode(state: State): Val;
    abstract pp(): string;

    protected fail(state: State | undefined, msg: string = ""): never {
        throw new DecodingError(this, state, msg);
    }

    //abstract encode(state: State, value: Val)
}