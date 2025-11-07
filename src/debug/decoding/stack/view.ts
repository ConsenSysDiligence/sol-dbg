import { Stack } from "../../types";
import { DecodingFailure, Value } from "../value";
import { ArrayLikeView, EncodingError, View } from "../view";
import {
    bigEndianBufToBigint,
    byte,
    encodeBigintInBigEndianBuf,
    fits,
    nyi,
    uint256,
    uint8,
    wordToAddress
} from "../../../utils";
import { Address } from "@ethereumjs/util";
import { makeStorageView } from "../storage";
import { inRange, isCalldataArrayType, isFailure } from "../utils";
import {
    ArraySliceCalldataView,
    BaseCalldataView,
    BytesSliceCalldataView,
    makeCalldataView,
    StringSliceCalldataView
} from "../calldata/view";
import { BaseStorageView } from "../storage/view";
import { BaseMemoryView, makeMemoryView } from "../memory/view";
import {
    AddressType,
    ArrayType,
    BaseRuntimeType,
    BoolType,
    BytesType,
    FixedBytesType,
    IntType,
    PointerType,
    StringType
} from "../../runtime_types";
import * as sol from "solc-typed-ast";

export abstract class BaseStackView<Val extends Value, Type extends BaseRuntimeType> extends View<
    Stack,
    Val,
    number,
    Type
> {
    fetchStackWord(offsetFromTop: number, state: Stack): Uint8Array | DecodingFailure {
        const len = state.length;

        if (len <= offsetFromTop) {
            return new DecodingFailure(`${offsetFromTop} OoB in stack of length ${len}`);
        }

        return state[len - offsetFromTop - 1];
    }

    decodeIntAt(offsetFromTop: number, typ: IntType, state: Stack): bigint | DecodingFailure {
        const word = this.fetchStackWord(offsetFromTop, state);

        if (isFailure(word)) {
            return word;
        }

        let res = bigEndianBufToBigint(word);
        // Convert signed negative 2's complement values
        if (typ.signed && (res & (BigInt(1) << BigInt(typ.numBits - 1))) !== BigInt(0)) {
            // Mask out any 1's above the number's size
            res = res & ((BigInt(1) << BigInt(typ.numBits)) - BigInt(1));
            res = -((BigInt(1) << BigInt(typ.numBits)) - res);
        }

        if (!fits(res, typ)) {
            return new DecodingFailure(`${res} doesnt fit in type ${typ.pp()}`);
        }

        return res;
    }

    pp(): string {
        return `<${this.type.pp()}@${this.loc} in stack>`;
    }

    abstract encode(value: Val, state: Stack): void;

    encodeIntAt(value: bigint, type: IntType, offsetFromTop: number, state: Stack): void {
        if (state.length <= offsetFromTop) {
            throw new EncodingError(`OoB access at ${offsetFromTop} in stack`);
        }

        encodeBigintInBigEndianBuf(value, state[offsetFromTop], type.numBits / 8);
    }
}

export class IntStackView extends BaseStackView<bigint, IntType> {
    decode(state: Stack): bigint | DecodingFailure {
        return this.decodeIntAt(this.loc, this.type, state);
    }

    encode(value: bigint, state: Stack): void {
        this.encodeIntAt(value, this.type, this.loc, state);
    }
}

export class BoolStackView extends BaseStackView<boolean, BoolType> {
    decode(state: Stack): boolean | DecodingFailure {
        const res = this.decodeIntAt(this.loc, uint256, state);
        return isFailure(res) ? res : res === 1n;
    }

    encode(value: boolean, state: Stack): void {
        this.encodeIntAt(value ? 1n : 0n, uint8, this.loc, state);
    }
}

export class AddressStackView extends BaseStackView<Address, AddressType> {
    decode(state: Stack): Address | DecodingFailure {
        const w = this.fetchStackWord(this.loc, state);
        return isFailure(w) ? w : wordToAddress(w);
    }

    encode(value: Address, state: Stack): void {
        const w = this.fetchStackWord(this.loc, state);

        if (isFailure(w)) {
            throw new EncodingError(w.reason);
        }

        w.set(value.bytes, 12);
    }
}

export class SingleByteStackView extends BaseStackView<bigint, FixedBytesType> {
    constructor(
        loc: number,
        public readonly byteOffset: number
    ) {
        super(byte, loc);
    }

    decode(state: Stack): bigint | DecodingFailure {
        const w = this.fetchStackWord(this.loc, state);
        return isFailure(w) ? w : BigInt(w[this.byteOffset]);
    }

    encode(value: bigint, state: Stack): void {
        const w = this.fetchStackWord(this.loc, state);

        if (!inRange(value, 0n, 255n)) {
            throw new EncodingError(`${value} not in byte range [0, 255]`);
        }

        if (isFailure(w)) {
            throw new EncodingError(w.reason);
        }

        w[this.byteOffset] = Number(value);
    }
}

export class FixedBytesStackView
    extends BaseStackView<Uint8Array, FixedBytesType>
    implements ArrayLikeView<Stack, SingleByteStackView>
{
    decode(state: Stack): Uint8Array | DecodingFailure {
        const w = this.fetchStackWord(this.loc, state);
        return isFailure(w) ? w : w.slice(0, this.type.numBytes);
    }

    encode(value: Uint8Array, state: Stack): void {
        const w = this.fetchStackWord(this.loc, state);
        if (isFailure(w)) {
            throw new EncodingError(w.reason);
        }

        w.set(value);
    }

    size(): bigint | DecodingFailure {
        return BigInt(this.type.numBytes);
    }

    indexView(key: bigint): DecodingFailure | SingleByteStackView {
        if (!inRange(key, 0, this.type.numBytes - 1)) {
            return new DecodingFailure(`Invalid index ${key} in ${this.type.pp()}`);
        }

        return new SingleByteStackView(this.loc, Number(key));
    }
}

type PointerValue = View<any, Value, any, BaseRuntimeType>;

export class PointerStackView extends BaseStackView<PointerValue, PointerType> {
    decode(state: Stack): PointerValue | DecodingFailure {
        const off = this.decodeIntAt(this.loc, uint256, state);

        if (isFailure(off)) {
            return off;
        }

        if (isCalldataArrayType(this.type)) {
            // Calldata Array slice - fetch 2 words from stack
            const len = this.decodeIntAt(this.loc - 1, uint256, state);

            if (isFailure(len)) {
                return len;
            }

            if (this.type.toType instanceof ArrayType) {
                return new ArraySliceCalldataView(this.type.toType as ArrayType, off, len);
            } else if (this.type.toType instanceof BytesType) {
                return new BytesSliceCalldataView(off, len);
            } else {
                sol.assert(this.type.toType instanceof StringType, ``);
                return new StringSliceCalldataView(off, len);
            }
        }

        if (this.type.location === sol.DataLocation.CallData) {
            return makeCalldataView(this.type.toType, 0n, off);
        }

        if (this.type.location === sol.DataLocation.Memory) {
            return makeMemoryView(this.type.toType, off);
        }

        if (
            this.type.location === sol.DataLocation.Storage ||
            this.type.location === sol.DataLocation.Transient
        ) {
            return makeStorageView(this.type.toType, [off, 32]);
        }

        nyi(`Stack pointer to ${this.type.location}`);
    }

    encode(value: PointerValue, state: Stack): void {
        if (value instanceof BaseStorageView) {
            if (value.endOffsetInWord !== 32) {
                throw new EncodingError(
                    `Unexpected non-word-aligned storage pointer in stack encoding`
                );
            }

            this.encodeIntAt(value.key, uint256, this.loc, state);
        } else if (value instanceof BaseMemoryView || value instanceof BaseCalldataView) {
            this.encodeIntAt(value.offset, uint256, this.loc, state);
        }

        nyi(`Unexpected pointer ${value.pp()}`);
    }
}

export function makeStackView(
    type: BaseRuntimeType,
    loc: number
): BaseStackView<Value, BaseRuntimeType> {
    if (type instanceof IntType) {
        return new IntStackView(type, loc);
    }

    if (type instanceof AddressType) {
        return new AddressStackView(type, loc);
    }

    if (type instanceof FixedBytesType) {
        return new FixedBytesStackView(type, loc);
    }

    if (type instanceof BoolType) {
        return new BoolStackView(type, loc);
    }

    if (type instanceof PointerType) {
        return new PointerStackView(type, loc);
    }

    nyi(`makeStackView(${type.pp()}, ${loc})`);
}
