import { assert } from "solc-typed-ast";
import { Memory } from "../../types";
import {
    ArrayLikeView,
    EncodingError,
    PointerView,
    shouldTreatStringsAsBytes,
    StructView,
    View
} from "../view";
import { DecodingFailure, Struct, Value } from "../value";
import {
    bigEndianBufToBigint,
    bigIntToNum,
    byte,
    encodeBigintInBigEndianBuf,
    fits,
    MAX_ARR_DECODE_LIMIT,
    nyi,
    readMem,
    roundUpToWordSize,
    uint256,
    ZERO_BYTES32
} from "../../../utils";
import { Address, bigIntToHex, bytesToUtf8 } from "@ethereumjs/util";
import { inRange, isFailure } from "../utils";
import { Allocator } from "./allocator";
import { utf8ToBytes } from "ethereum-cryptography/utils";
import {
    AddressType,
    ArrayType,
    BaseRuntimeType,
    BoolType,
    BytesType,
    FixedBytesType,
    IntType,
    MappingType,
    MissingTypeDef,
    PointerType,
    StringType,
    StructType
} from "../../runtime_types";

interface ArrayLikeMemView<
    ValViewT extends BaseMemoryView<Value, BaseRuntimeType>
> extends ArrayLikeView<Memory, ValViewT> {}

export function isArrayLikeMemView(
    v: any
): v is ArrayLikeMemView<BaseMemoryView<Value, BaseRuntimeType>> {
    return v instanceof FixedBytesMemView || v instanceof ArrayMemView || v instanceof BytesMemView;
}

export abstract class BaseMemoryView<
    Val extends Value,
    Type extends BaseRuntimeType = BaseRuntimeType
> extends View<Memory, Val, bigint, Type> {
    protected writeMemAt(value: Uint8Array, off: bigint, mem: Memory): void {
        if (!inRange(off, 0n, mem.length - value.length)) {
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
        if (type.signed && (res & (BigInt(1) << BigInt(type.numBits - 1))) !== BigInt(0)) {
            // Mask out any 1's above the number's size
            res = res & ((BigInt(1) << BigInt(type.numBits)) - BigInt(1));
            res = -((BigInt(1) << BigInt(type.numBits)) - res);
        }

        if (!fits(res, type)) {
            return new DecodingFailure(
                `Decoded value ${res} doesn't fit in expected type ${type.pp()}`
            );
        }
        return res;
    }

    pp(): string {
        return `<${this.type.pp()}@${bigIntToHex(this.loc)} in memory>`;
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

/**
 * We need a special SingleByteMemView for FixedBytesMemView's indexView() method.
 * We cannot just re-use FixedBytesMemView, since even for a single byte, that will
 * write 32 bytes with padded zeroes.
 */
export class SingleByteMemView extends BaseMemoryView<Uint8Array, FixedBytesType> {
    constructor(loc: bigint) {
        super(byte, loc);
    }

    decode(state: Memory): Uint8Array | DecodingFailure {
        if (!inRange(this.loc, 0, state.length)) {
            return new DecodingFailure(`OoB byte access at ${this.loc}`);
        }

        return state.slice(Number(this.loc), Number(this.loc + 1n));
    }

    encode(value: Uint8Array, state: Memory): void {
        this.writeMemAt(value, this.loc, state);
    }
}

export class FixedBytesMemView
    extends BaseMemoryView<Uint8Array, FixedBytesType>
    implements ArrayLikeMemView<SingleByteMemView>
{
    decode(state: Memory): Uint8Array | DecodingFailure {
        return this.readMemAt(this.loc, state, this.type.numBytes);
    }

    encode(value: Uint8Array<ArrayBufferLike>, state: Memory): void {
        const w = new Uint8Array(32);
        w.set(value);
        this.writeMemAt(w, this.loc, state);
    }

    indexView(key: bigint): DecodingFailure | SingleByteMemView {
        if (key >= this.type.numBytes || key < 0n) {
            return new DecodingFailure(`Invalid index ${key} in ${this.type.pp()}`);
        }

        return new SingleByteMemView(this.loc + key);
    }

    size(): bigint {
        return BigInt(this.type.numBytes);
    }
}

export abstract class PackedArrayMemView<
    V extends Value,
    T extends BaseRuntimeType
> extends BaseMemoryView<V, T> {
    protected decodeBytesAt(loc: bigint, state: Memory): Uint8Array | DecodingFailure {
        const len = this.decodeIntAt(loc, uint256, state);

        if (isFailure(len)) {
            return len;
        }

        if (!inRange(len, 0n, MAX_ARR_DECODE_LIMIT)) {
            return new DecodingFailure(`Bytes to decode too large - ${len}`);
        }

        return this.readMemAt(loc + 32n, state, len);
    }

    protected encodeBytesAt(value: Uint8Array, off: bigint, state: Memory): void {
        this.encodeIntAt(BigInt(value.length), off, state);
        this.writeMemAt(value, off + 32n, state);
    }
}

export class BytesMemView
    extends PackedArrayMemView<Uint8Array, BytesType | StringType>
    implements ArrayLikeMemView<SingleByteMemView>
{
    decode(state: Memory): Uint8Array | DecodingFailure {
        return this.decodeBytesAt(this.loc, state);
    }

    encode(value: Uint8Array, state: Memory): void {
        this.encodeBytesAt(value, this.loc, state);
    }

    indexView(key: bigint, state: Memory): DecodingFailure | SingleByteMemView {
        const len = this.decodeIntAt(this.loc, uint256, state);

        if (isFailure(len)) {
            return len;
        }

        if (key >= len || key < 0n) {
            return new DecodingFailure(`Invalid index ${key} in bytes of size ${len}`);
        }

        return new SingleByteMemView(this.loc + 32n + key);
    }

    size(state: Memory): bigint | DecodingFailure {
        return this.decodeIntAt(this.loc, uint256, state);
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

export class ArrayMemView
    extends BaseMemoryView<Value[], ArrayType>
    implements ArrayLikeMemView<BaseMemoryView<Value, BaseRuntimeType>>
{
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

        if (!inRange(sizeBigint, 0n, MAX_ARR_DECODE_LIMIT)) {
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

    indexView(
        key: bigint,
        state: Memory
    ): BaseMemoryView<Value, BaseRuntimeType> | DecodingFailure {
        let addr = this.loc;
        let size: bigint | DecodingFailure;

        if (this.type.size === undefined) {
            size = this.decodeIntAt(addr, uint256, state);

            if (isFailure(size)) {
                return size;
            }

            addr += 32n;
        } else {
            size = this.type.size;
        }

        if (key >= size || key < 0n) {
            return new DecodingFailure(`Invalid index ${key} to array of size ${size}`);
        }

        return makeMemoryView(this.type.elementT, addr + key * 32n);
    }

    size(state: Memory): bigint | DecodingFailure {
        if (this.type.size !== undefined) {
            return this.type.size;
        }

        return this.decodeIntAt(this.loc, uint256, state);
    }
}

export class StructMemView
    extends BaseMemoryView<Struct, StructType>
    implements StructView<Memory, BaseMemoryView<Value, BaseRuntimeType>>
{
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

    fieldView(name: string): BaseMemoryView<Value, BaseRuntimeType> | DecodingFailure {
        let offset = this.loc;

        for (const [fieldName, type] of this.type.fields) {
            if (fieldName === name) {
                return makeMemoryView(type, offset);
            }

            offset += 32n;
        }

        return new DecodingFailure(`No such field ${name} on struct ${this.type.pp()}`);
    }
}

export class PointerMemView
    extends BaseMemoryView<Value, PointerType>
    implements PointerView<Memory, BaseMemoryView<Value, BaseRuntimeType>>
{
    decode(state: Memory): Value | DecodingFailure {
        const view = this.toView(state);

        if (isFailure(view)) {
            return view;
        }

        return view.decode(state);
    }

    /**
     * Helper to compute how much memory we need to allocate for a pointed-to value of type t
     */
    static allocSize(v: Value | undefined, t: BaseRuntimeType): number {
        if (t instanceof ArrayType) {
            if (t.size !== undefined) {
                return bigIntToNum(t.size * 32n);
            }

            return (v as Value[]).length * 32 + 32;
        }

        if (t instanceof StructType) {
            return t.fields.length * 32;
        }

        if (t instanceof StringType || t instanceof BytesType) {
            return roundUpToWordSize(Buffer.from(v as Uint8Array | string).length) + 32;
        }

        if (t instanceof MappingType) {
            // Nothing gets allocated in memory for a Map
            return 0;
        }

        return 32;
    }

    public static allocMemFor(
        val: Value | undefined,
        type: BaseRuntimeType,
        allocator: Allocator
    ): BaseMemoryView<Value, typeof type> {
        const size = PointerMemView.allocSize(val, type);
        const addr = allocator.alloc(size);
        return makeMemoryView(type, addr);
    }

    encode(value: Value, state: Memory, alloc: Allocator): void {
        if (value instanceof BaseMemoryView) {
            if (value.type.pp() !== this.type.toType.pp()) {
                throw new EncodingError(
                    `Cannot assign a pointer from incompatible type ${value.type.pp()} to ${this.pp()}`
                );
            }

            this.encodeIntAt(value.offset, this.loc, state);
            return;
        }

        const toView = PointerMemView.allocMemFor(value, this.type.toType, alloc);
        toView.encode(value, state, alloc);
        this.encodeIntAt(toView.offset, this.loc, state);
    }

    toView(state: Memory): BaseMemoryView<Value, BaseRuntimeType> | DecodingFailure {
        const offset = this.decodeIntAt(this.loc, uint256, state);

        if (isFailure(offset)) {
            return offset;
        }

        return makeMemoryView(this.type.toType, offset);
    }
}

export class MissingMemView extends BaseMemoryView<Value, MissingTypeDef> {
    decode(): DecodingFailure {
        return new DecodingFailure(`<failed decoding ${this.type.pp()}>`);
    }

    encode(): void {
        throw new EncodingError(`Cannot encode missing type ${this.type.pp()}`);
    }
}

// In solidity <0.7.0 Map fields were allowed in memory structs. They were essentially a no-op:
// An empty map is decoded, and nothing happens when we assign structs with maps inside.
// For backwards compatibility allow Map views in memory.
export class MapMemView extends BaseMemoryView<Map<Value, Value>, MappingType> {
    encode(): void {
        // Nothing to do
    }
    decode(): Map<Value, Value> {
        return new Map();
    }
}

export function makeMemoryView(
    type: BaseRuntimeType,
    loc: bigint
): BaseMemoryView<Value, BaseRuntimeType> {
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

    if (type instanceof StructType) {
        return new StructMemView(type, loc);
    }

    if (type instanceof BytesType) {
        return new BytesMemView(type, loc);
    }

    if (type instanceof StringType) {
        return shouldTreatStringsAsBytes()
            ? new BytesMemView(type, loc)
            : new StringMemView(type, loc);
    }

    if (type instanceof ArrayType) {
        return new ArrayMemView(type, loc);
    }

    if (type instanceof PointerType) {
        return new PointerMemView(type, loc);
    }

    if (type instanceof MappingType) {
        return new MapMemView(type, loc);
    }

    if (type instanceof MissingTypeDef) {
        return new MissingMemView(type, loc);
    }

    nyi(`makeMemoryView(${type.pp()})`);
}
