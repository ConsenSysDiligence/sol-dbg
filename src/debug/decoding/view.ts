import { InferType, TypeNode } from "solc-typed-ast";
import { Value } from "./value";
import { Memory, Stack, Storage } from "../types";

export class ViewError<State> extends Error {
    constructor(
        public readonly view: View<any, any, any, any>,
        public readonly state: State,
        msg: string = ""
    ) {
        super(`Error decoding ${view.pp()}: ${msg}`);
    }
}
/**
 * Base class for all data {@link View}s. Views are parametrized by the {@link State} they project from (e.g. Memory, Storage, Calldata),
 * the Solidity {@link Type} of the element they are decoding as well as Typescript type of the {@link Value} they decode.
 */
export abstract class View<State, Val extends Value, Loc, Type extends TypeNode = TypeNode> {
    constructor(
        public readonly type: Type,
        public readonly infer: InferType,
        protected loc: Loc
    ) {}

    /**
     * Decode a value from the given State
     * @param state
     */
    abstract decode(state: State): Val;
    abstract pp(): string;

    protected fail(state: State, msg: string = ""): never {
        throw new ViewError(this, state, msg);
    }

    //abstract encode(state: State, value: Val)
}

/**
 * Base class for all memory views (calldata/memory) and the storage view
 */
export abstract class MemView<Val extends Value, Type extends TypeNode = TypeNode> extends View<
    Memory | Storage,
    Val,
    bigint,
    Type
> {
    constructor(type: Type, infer: InferType, loc: bigint) {
        super(type, infer, loc);
    }

    get address(): bigint {
        return this.loc;
    }
}

/**
 * Base class for all stack views
 */
export abstract class StackView<Val extends Value, Type extends TypeNode = TypeNode> extends View<
    Stack,
    Val,
    number,
    Type
> {
    constructor(type: Type, infer: InferType, loc: number) {
        super(type, infer, loc);
    }

    get offset(): number {
        return this.loc;
    }
}
