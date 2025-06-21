import {
    AddressType,
    ArrayType,
    BoolType,
    BytesType,
    FixedBytesType,
    IntType,
    PointerType,
    StringType,
    TypeNode
} from "solc-typed-ast";
import { Memory } from "../../types";
import { View } from "../view";
import { DecodingFailure, Struct, Value } from "../value";
import {
    bigEndianBufToBigint,
    fits,
    MAX_ARR_DECODE_LIMIT,
    nyi,
    readMem,
    uint256
} from "../../../utils";
import { Address, bytesToUtf8 } from "@ethereumjs/util";
import { ExpStructType, MissingType } from "../exp_types";
import { isFailure } from "../utils";

export abstract class BaseMemoryView<
    Val extends Value,
    Type extends TypeNode = TypeNode
> extends View<Memory, Val, bigint, Type> {
    protected readMemAt(
        off: bigint,
        calldata: Memory,
        len: bigint | number
    ): Uint8Array | DecodingFailure {
        const res = readMem(off, len, calldata);

        if (!res) {
            // OoB access
            return new DecodingFailure(
                `OoB access at ${off}:${off + BigInt(len)} in memory of size ${calldata.length}`
            );
        }

        return res;
    }

    protected decodeIntAt(off: bigint, type: IntType, state: Memory): bigint | DecodingFailure {
        const bytes = this.readMemAt(off, state, 32);

        if (isFailure(bytes)) {
            return bytes;
        }

        let res = bigEndianBufToBigint(bytes);

        // Convert signed negative 2's complement values
        if (type.signed && (res & (BigInt(1) << BigInt(type.nBits - 1))) !== BigInt(0)) {
            // Mask out any 1's above the number's size
            res = res & ((BigInt(1) << BigInt(type.nBits)) - BigInt(1));
            res = -((BigInt(1) << BigInt(type.nBits)) - res);
        }

        if (!fits(res, type)) {
            return new DecodingFailure(
                `Decoded value ${res} doesn't fit in expected type ${type.pp()}`
            );
        }
        return res;
    }

    protected decodeBytesAt(loc: bigint, state: Memory): Uint8Array | DecodingFailure {
        const len = this.decodeIntAt(loc, uint256, state);

        if (isFailure(len)) {
            return len;
        }

        if (len >= MAX_ARR_DECODE_LIMIT) {
            return new DecodingFailure(`Bytes to decode too large - ${len}`);
        }

        return this.readMemAt(loc + 32n, state, len);
    }

    pp(): string {
        return `<${this.type.pp()}@${this.loc} in memory>`;
    }

    get offset(): bigint {
        return this.loc;
    }
}

export class IntMemView extends BaseMemoryView<bigint, IntType> {
    decode(state: Memory): bigint | DecodingFailure {
        return this.decodeIntAt(this.loc, this.type, state);
    }
}

export class AddressMemView extends BaseMemoryView<Address, AddressType> {
    decode(state: Memory): Address | DecodingFailure {
        const m = this.readMemAt(this.loc + 12n, state, 20);
        return isFailure(m) ? m : new Address(m);
    }
}

export class BoolMemView extends BaseMemoryView<boolean, BoolType> {
    decode(state: Memory): boolean | DecodingFailure {
        const m = this.readMemAt(this.loc, state, 32);
        return isFailure(m) ? m : bigEndianBufToBigint(m) !== 0n;
    }
}

export class FixedBytesMemView extends BaseMemoryView<Uint8Array, FixedBytesType> {
    decode(state: Memory): Uint8Array | DecodingFailure {
        return this.readMemAt(this.loc, state, this.type.size);
    }
}

export class BytesMemView extends BaseMemoryView<Uint8Array, BytesType> {
    decode(state: Memory): Uint8Array | DecodingFailure {
        return this.decodeBytesAt(this.loc, state);
    }
}

export class StringMemView extends BaseMemoryView<string, StringType> {
    decode(state: Memory): string | DecodingFailure {
        const bytes = this.decodeBytesAt(this.loc, state);
        return isFailure(bytes) ? bytes : bytesToUtf8(bytes);
    }
}

export class ArrayMemView extends BaseMemoryView<Value[], ArrayType> {
    decode(state: Memory): Value[] | DecodingFailure {
        let sizeBigint: bigint | DecodingFailure;
        let addr = this.loc;

        if (this.type.size === undefined) {
            sizeBigint = this.decodeIntAt(addr, uint256, state);

            if (isFailure(sizeBigint)) {
                return sizeBigint;
            }

            addr += 32n;
        } else {
            sizeBigint = this.type.size;
        }

        if (sizeBigint >= MAX_ARR_DECODE_LIMIT) {
            return new DecodingFailure(`Array too large to decode: ${sizeBigint}`);
        }

        const size = Number(sizeBigint);
        const res: Value[] = [];

        for (let i = 0; i < size; i++) {
            const view = makeMemoryView(this.type.elementT, addr);
            res.push(view.decode(state));
            addr += 32n;
        }

        return res;
    }
}

export class StructMemView extends BaseMemoryView<Struct, ExpStructType> {
    decode(state: Memory): Struct {
        const entries: Array<[string, Value]> = [];

        let offset = this.loc;

        for (const [name, type] of this.type.fields) {
            const view = makeMemoryView(type, offset);
            entries.push([name, view.decode(state)]);
            offset += 32n;
        }

        return new Struct(entries);
    }
}

export class PointerMemView extends BaseMemoryView<Value, PointerType> {
    decode(state: Memory): Value | DecodingFailure {
        const offset = this.decodeIntAt(this.loc, uint256, state);

        if (isFailure(offset)) {
            return offset;
        }

        const view = makeMemoryView(this.type.to, offset);
        return view.decode(state);
    }
}

export class MissingMemView extends BaseMemoryView<Value, MissingType> {
    decode(): DecodingFailure {
        return new DecodingFailure(
            `${this.type.rawTypeName ? this.type.rawTypeName.type : "<unknown>"}`
        );
    }
}

export function makeMemoryView(type: TypeNode, loc: bigint): BaseMemoryView<Value, TypeNode> {
    if (type instanceof IntType) {
        return new IntMemView(type, loc);
    }

    if (type instanceof BoolType) {
        return new BoolMemView(type, loc);
    }

    if (type instanceof AddressType) {
        return new AddressMemView(type, loc);
    }

    if (type instanceof FixedBytesType) {
        return new FixedBytesMemView(type, loc);
    }

    if (type instanceof ExpStructType) {
        return new StructMemView(type, loc);
    }

    if (type instanceof BytesType) {
        return new BytesMemView(type, loc);
    }

    if (type instanceof StringType) {
        return new StringMemView(type, loc);
    }

    if (type instanceof ArrayType) {
        return new ArrayMemView(type, loc);
    }

    if (type instanceof PointerType) {
        return new PointerMemView(type, loc);
    }

    if (type instanceof MissingType) {
        return new MissingMemView(type, loc);
    }

    nyi(`makeMemoryView(${type.pp()})`);
}
