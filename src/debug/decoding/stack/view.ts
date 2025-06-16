import {
    AddressType,
    ArrayType,
    BoolType,
    DataLocation,
    FixedBytesType,
    IntType,
    PointerType,
    TypeNode
} from "solc-typed-ast";
import { Stack, StateArea } from "../../types";
import { Value } from "../value";
import { View } from "../view";
import { bigEndianBufToBigint, fits, nyi, uint256, wordToAddress } from "../../../utils";
import { Address } from "@ethereumjs/util";
import { makeStorageView } from "../storage";
import { isCalldataArrayType } from "../utils";
import { ArraySliceCalldataView, makeCalldataView } from "../calldata/view";
import { makeMemoryView } from "../memory";

export abstract class BaseStackView<Val extends Value, Type extends TypeNode> extends View<
    Stack,
    Val,
    number,
    Type
> {
    fetchStackWord(offsetFromTop: number, state: Stack): Uint8Array {
        const len = state.length;

        if (len <= offsetFromTop) {
            this.fail(state, `Offset from top ${offsetFromTop} is OoB the stack of size ${len}`);
        }

        return state[len - offsetFromTop - 1];
    }

    decodeIntAt(offsetFromTop: number, typ: IntType, state: Stack): bigint {
        const word = this.fetchStackWord(offsetFromTop, state);

        let res = bigEndianBufToBigint(word);
        // Convert signed negative 2's complement values
        if (typ.signed && (res & (BigInt(1) << BigInt(typ.nBits - 1))) !== BigInt(0)) {
            // Mask out any 1's above the number's size
            res = res & ((BigInt(1) << BigInt(typ.nBits)) - BigInt(1));
            res = -((BigInt(1) << BigInt(typ.nBits)) - res);
        }

        if (!fits(res, typ)) {
            this.fail(
                state,
                `Decoded value ${res} from ${this.loc} doesn't fit in expected type ${typ.pp()}`
            );
        }

        return res;
    }

    pp(): string {
        return `<${this.type.pp()}@${this.loc} in stack>`;
    }
}

export class IntStackView extends BaseStackView<bigint, IntType> {
    decode(state: Stack): bigint {
        return this.decodeIntAt(this.loc, this.type, state);
    }
}

export class BoolStackView extends BaseStackView<boolean, BoolType> {
    decode(state: Stack): boolean {
        return this.decodeIntAt(this.loc, uint256, state) === 1n;
    }
}

export class AddressStackView extends BaseStackView<Address, AddressType> {
    decode(state: Stack): Address {
        return wordToAddress(this.fetchStackWord(this.loc, state));
    }
}

export class FixedBytesStackView extends BaseStackView<Uint8Array, FixedBytesType> {
    decode(state: Stack): Uint8Array {
        return this.fetchStackWord(this.loc, state).slice(0, this.type.size);
    }
}

type PointerValue = View<StateArea, Value, any, TypeNode>;
export class PointerStackView extends BaseStackView<PointerValue, PointerType> {
    decode(state: Stack): PointerValue {
        const off = this.decodeIntAt(this.loc, uint256, state);

        if (isCalldataArrayType(this.type)) {
            // Calldata Array slice - fetch 2 words from stack
            const len = this.decodeIntAt(this.loc - 1, uint256, state);

            return new ArraySliceCalldataView(this.type.to as ArrayType, off, len);
        }

        if (this.type.location === DataLocation.CallData) {
            return makeCalldataView(this.type.to, off, 0n);
        }

        if (this.type.location === DataLocation.Memory) {
            return makeMemoryView(this.type.to, off);
        }

        if (
            this.type.location === DataLocation.Storage ||
            this.type.location === DataLocation.Transient
        ) {
            return makeStorageView(this.type.to, [off, 32]);
        }

        nyi(`Stack pointer to ${this.type.location}`);
    }
}

export function makeStackView(type: TypeNode, loc: number): BaseStackView<Value, TypeNode> {
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
