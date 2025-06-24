import {
    AddressType,
    ArrayType,
    assert,
    BoolType,
    BytesType,
    FixedBytesType,
    IntType,
    PackedArrayType,
    PointerType,
    StringType,
    TypeNode
} from "solc-typed-ast";
import { Memory } from "../../types";
import { EncodingError, View } from "../view";
import { DecodingFailure, Struct, Value } from "../value";
import {
    bigEndianBufToBigint,
    encodeBigintInBigEndianBuf,
    fits,
    MAX_ARR_DECODE_LIMIT,
    nyi,
    readMem,
    uint256,
    ZERO_BYTES32
} from "../../../utils";
import { Address, bytesToUtf8 } from "@ethereumjs/util";
import { ExpStructType, MissingType } from "../exp_types";
import { isFailure } from "../utils";
import { Allocator } from "./allocator";
import { utf8ToBytes } from "ethereum-cryptography/utils";

export abstract class BaseMemoryView<
    Val extends Value,
    Type extends TypeNode = TypeNode
> extends View<Memory, Val, bigint, Type> {
    protected writeMemAt(value: Uint8Array, off: bigint, mem: Memory): void {
        if (off < 0n || off + BigInt(value.length) > BigInt(mem.length)) {
            console.error(
                `OoB writing mem at ${off} of length ${value.length} in memory of size ${mem.length}`
            );
            throw new EncodingError(
                `OoB writing mem at ${off} of length ${value.length} in memory of size ${mem.length}`
            );
        }

        mem.set(value, Number(off));
    }

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

    pp(): string {
        return `<${this.type.pp()}@${this.loc} in memory>`;
    }

    get offset(): bigint {
        return this.loc;
    }

    protected encodeIntAt(value: bigint, off: bigint, state: Memory): void {
        const word = new Uint8Array(32);

        // Note nBytes is always 32 - an int always takes up the full word
        encodeBigintInBigEndianBuf(value, word, 32, 32);
        this.writeMemAt(word, off, state);
    }

    abstract encode(value: Val, state: Memory, alloc: Allocator): void;
}

export class IntMemView extends BaseMemoryView<bigint, IntType> {
    decode(state: Memory): bigint | DecodingFailure {
        return this.decodeIntAt(this.loc, this.type, state);
    }

    encode(value: bigint, state: Memory): void {
        this.encodeIntAt(value, this.loc, state);
    }
}

export class AddressMemView extends BaseMemoryView<Address, AddressType> {
    decode(state: Memory): Address | DecodingFailure {
        const m = this.readMemAt(this.loc + 12n, state, 20);
        return isFailure(m) ? m : new Address(m);
    }

    encode(value: Address, state: Memory): void {
        // We write a full word to make sure the lower bytes are 0-ed out
        const w = new Uint8Array(32);
        w.set(value.bytes, 12);
        this.writeMemAt(w, this.loc, state);
    }
}

const ONE_BYTES32 = new Uint8Array(32);
ONE_BYTES32[31] = 1;

export class BoolMemView extends BaseMemoryView<boolean, BoolType> {
    decode(state: Memory): boolean | DecodingFailure {
        const m = this.readMemAt(this.loc, state, 32);
        return isFailure(m) ? m : bigEndianBufToBigint(m) !== 0n;
    }

    encode(value: boolean, state: Memory): void {
        this.writeMemAt(value ? ONE_BYTES32 : ZERO_BYTES32, this.loc, state);
    }
}

export class FixedBytesMemView extends BaseMemoryView<Uint8Array, FixedBytesType> {
    decode(state: Memory): Uint8Array | DecodingFailure {
        return this.readMemAt(this.loc, state, this.type.size);
    }

    encode(value: Uint8Array<ArrayBufferLike>, state: Memory): void {
        const w = new Uint8Array(32);
        w.set(value);
        this.writeMemAt(w, this.loc, state);
    }
}

export abstract class PackedArrayMemView<
    V extends Value,
    T extends TypeNode
> extends BaseMemoryView<V, T> {
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

    protected encodeBytesAt(value: Uint8Array, off: bigint, state: Memory): void {
        this.encodeIntAt(BigInt(value.length), off, state);
        this.writeMemAt(value, off + 32n, state);
    }
}

export class BytesMemView extends PackedArrayMemView<Uint8Array, BytesType> {
    decode(state: Memory): Uint8Array | DecodingFailure {
        return this.decodeBytesAt(this.loc, state);
    }

    encode(value: Uint8Array, state: Memory): void {
        this.encodeBytesAt(value, this.loc, state);
    }
}

export class StringMemView extends PackedArrayMemView<string, StringType> {
    decode(state: Memory): string | DecodingFailure {
        const bytes = this.decodeBytesAt(this.loc, state);
        return isFailure(bytes) ? bytes : bytesToUtf8(bytes);
    }

    encode(value: string, state: Memory): void {
        this.encodeBytesAt(utf8ToBytes(value), this.loc, state);
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

    encode(value: Value[], state: Memory, alloc: Allocator): void {
        let off = this.loc;

        if (this.type.size === undefined) {
            this.encodeIntAt(BigInt(value.length), off, state);
            off += 32n;
        }

        for (const v of value) {
            const view = makeMemoryView(this.type.elementT, off);
            view.encode(v, state, alloc);
            off += 32n;
        }
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

    encode(value: Struct, state: Memory, alloc: Allocator): void {
        let offset = this.loc;

        assert(value.entries.length === this.type.fields.length, `Mismatch in encoding struct`);

        for (let i = 0; i < this.type.fields.length; i++) {
            const [, type] = this.type.fields[i];
            const view = makeMemoryView(type, offset);
            view.encode(value.entries[i][1], state, alloc);
            offset += 32n;
        }
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

    /**
     * Helper to compute how much memory we need to allocate for a pointed-to value of type t
     */
    static allocSize(v: Value | undefined, t: TypeNode): number {
        if (t instanceof ArrayType) {
            return (
                (v as Value[]).length * PointerMemView.allocSize(undefined, t.elementT) +
                (t.size !== undefined ? 0 : 32)
            );
        }

        if (t instanceof ExpStructType) {
            let size = 0;
            for (let i = 0; i < t.fields.length; i++) {
                size += PointerMemView.allocSize((v as Struct).entries[i][1], t.fields[i][1]);
            }

            return size;
        }

        if (t instanceof PackedArrayType) {
            return Buffer.from(v as Uint8Array | string).length + 32;
        }

        return 32;
    }

    encode(value: Value, state: Memory, alloc: Allocator): void {
        const ptr = alloc.alloc(PointerMemView.allocSize(value, this.type.to));
        this.encodeIntAt(ptr, this.loc, state);
        const view = makeMemoryView(this.type.to, ptr);
        view.encode(value, state, alloc);
    }
}

export class MissingMemView extends BaseMemoryView<Value, MissingType> {
    decode(): DecodingFailure {
        return new DecodingFailure(
            `${this.type.rawTypeName ? this.type.rawTypeName.type : "<unknown>"}`
        );
    }

    encode(): void {
        throw new EncodingError(`Cannot encode missing type ${this.type.pp()}`);
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
