import {
    AddressType,
    ArrayType,
    assert,
    BoolType,
    BytesType,
    FixedBytesType,
    IntType,
    PointerType,
    StringType,
    TupleType,
    TypeNode,
    types
} from "solc-typed-ast";
import { Memory } from "../../types";
import { DecodingFailure, Struct, Value } from "../value";
import { IndexableView, PointerView, StructView, View } from "../view";
import {
    bigEndianBufToBigint,
    fits,
    MAX_ARR_DECODE_LIMIT,
    nyi,
    readMem,
    uint256,
    zip
} from "../../../utils";
import { Address, bytesToUtf8 } from "@ethereumjs/util";
import { inRange, isFailure, isTypeStringDynamicArray } from "../utils";
import { ExpStructType, MissingType } from "../exp_types";

/**
 * Return true IFF the given type is "dynamic". I,e. its size is not statically known.
 * @param t
 * @param infer
 * @returns
 */
function isTypeDynamic(t: TypeNode): boolean {
    if (t instanceof BytesType || t instanceof StringType) {
        return true;
    }

    if (t instanceof ArrayType) {
        return t.size === undefined || isTypeDynamic(t.elementT);
    }

    if (t instanceof TupleType) {
        for (const elementT of t.elements) {
            if (elementT && isTypeDynamic(elementT)) {
                return true;
            }
        }
    }

    if (t instanceof ExpStructType) {
        for (const [, fieldT] of t.fields) {
            if (isTypeDynamic(fieldT)) {
                return true;
            }
        }
    }

    if (t instanceof PointerType) {
        return isTypeDynamic(t.to);
    }

    return false;
}

/**
 * Return the calldata head size for a given type. This is `len(head(X))` in the notation of
 * https://docs.soliditylang.org/en/latest/abi-spec.html#formal-specification-of-the-encoding
 *
 * @param t
 * @param infer
 * @returns
 */
function headSize(t: TypeNode): number | undefined {
    if (isTypeDynamic(t)) {
        return 32;
    }

    if (t instanceof TupleType) {
        let size = 0;

        for (const elT of t.elements) {
            const elSize = headSize(elT as TypeNode);

            if (elSize === undefined) {
                return undefined;
            }

            size += elSize;
        }

        return size;
    }

    if (t instanceof ArrayType) {
        assert(t.size !== undefined, `Statically sized array types must have a size`);
        const elSize = headSize(t.elementT);

        if (elSize === undefined) {
            return undefined;
        }

        return elSize * Number(t.size);
    }

    if (t instanceof ExpStructType) {
        let size = 0;
        for (const [, fieldT] of t.fields) {
            const fieldSize = headSize(fieldT);

            if (fieldSize === undefined) {
                return undefined;
            }

            size += fieldSize;
        }

        return size;
    }

    if (
        t instanceof IntType ||
        t instanceof BoolType ||
        t instanceof FixedBytesType ||
        t instanceof AddressType
    ) {
        return 32;
    }

    if (t instanceof PointerType) {
        return headSize(t.to);
    }

    if (t instanceof MissingType) {
        if (t.rawTypeName === undefined) {
            return undefined;
        }

        // Small optimization - if we can guess this is a dynamic array of an unknown
        // element type, we still know the head size is 32.
        if (isTypeStringDynamicArray(t.rawTypeName.typeString)) {
            return 32;
        }
    }

    nyi(`Statically sized type ${t.pp()}`);
}

export abstract class BaseCalldataView<
    Val extends Value,
    Type extends TypeNode = TypeNode
> extends View<Memory, Val, bigint, Type> {
    constructor(
        type: Type,
        loc: bigint,
        protected base: bigint
    ) {
        super(type, loc);
    }

    protected readMemAt(
        off: number | bigint,
        calldata: Memory,
        len: bigint | number
    ): Uint8Array | DecodingFailure {
        const actualOffset = this.base + BigInt(off);
        const res = readMem(actualOffset, len, calldata);

        if (!res) {
            // OoB access
            return new DecodingFailure(
                `OoB access at ${actualOffset}:${actualOffset + BigInt(len)} in memory of size ${calldata.length}`
            );
        }

        return res;
    }

    protected decodeIntAt(
        offset: bigint,
        typ: IntType,
        calldata: Memory
    ): bigint | DecodingFailure {
        const bytes = this.readMemAt(offset, calldata, 32);

        if (isFailure(bytes)) {
            return bytes;
        }

        let res = bigEndianBufToBigint(bytes);

        // Convert signed negative 2's complement values
        if (typ.signed && (res & (BigInt(1) << BigInt(typ.nBits - 1))) !== BigInt(0)) {
            // Mask out any 1's above the number's size
            res = res & ((BigInt(1) << BigInt(typ.nBits)) - BigInt(1));
            res = -((BigInt(1) << BigInt(typ.nBits)) - res);
        }

        if (!fits(res, typ)) {
            return new DecodingFailure(`Decoded value ${res} doesnt fit in type ${this.type.pp()}`);
        }

        return res;
    }

    decodeBytesAt(loc: bigint, state: Memory): Uint8Array | DecodingFailure {
        const len = this.decodeIntAt(loc, uint256, state);

        if (isFailure(len)) {
            return len;
        }

        if (len >= MAX_ARR_DECODE_LIMIT) {
            return new DecodingFailure(`Bytes to decode too large - ${len}`);
        }

        return this.readMemAt(loc + 32n, state, len);
    }

    decodeTupleAt(loc: bigint, base: bigint, type: TupleType, state: Memory): Value[] {
        const res: Value[] = [];
        let failRemaining = false;

        for (const t of type.elements) {
            if (failRemaining) {
                res.push(new DecodingFailure(`Failed due to earlier failure.`));
            }

            assert(t !== null, `Unexpected null element in tuple type {0}`, type);
            res.push(makeCalldataView(t, loc, base).decode(state));
            const tSize = headSize(t);

            if (tSize === undefined) {
                failRemaining = true;
            } else {
                loc += BigInt(tSize);
            }
        }

        return res;
    }

    pp(): string {
        return `<${this.type.pp()}@${this.loc + this.base} in calldata>`;
    }

    get offset(): bigint {
        return this.loc;
    }
}

/**
 * View to an IntType in calldata
 */
export class IntCalldataView extends BaseCalldataView<bigint, IntType> {
    decode(state: Memory): bigint | DecodingFailure {
        return this.decodeIntAt(this.loc, this.type, state);
    }
}

/**
 * View to a BoolType in calldata
 */
export class BoolCalldataView extends BaseCalldataView<boolean, BoolType> {
    decode(state: Memory): boolean | DecodingFailure {
        const intV = this.decodeIntAt(this.loc, uint256, state);

        if (isFailure(intV)) {
            return intV;
        }

        return intV !== 0n;
    }
}

/**
 * View to an Address in calldata
 */
export class AddressCalldataView extends BaseCalldataView<Address, AddressType> {
    decode(state: Memory): Address | DecodingFailure {
        const m = this.readMemAt(this.loc, state, 32);

        if (isFailure(m)) {
            return m;
        }

        return new Address(m.slice(12));
    }
}

/**
 * We need a special SingleByteMemView for FixedBytesMemView's indexView() method.
 * We cannot just re-use FixedBytesMemView, since even for a single byte, that will
 * write 32 bytes with padded zeroes.
 */
export class SingleByteCalldataView extends BaseCalldataView<number, FixedBytesType> {
    constructor(loc: bigint, base: bigint) {
        super(types.byte, loc, base);
    }

    decode(state: Memory): number | DecodingFailure {
        const off = this.loc + this.base;

        if (off < 0 || off > state.length) {
            return new DecodingFailure(`OoB byte access at ${off}`);
        }

        return state[Number(off)];
    }
}

/**
 * View to an FixedBytes in calldata
 */
export class FixedBytesCalldataView
    extends BaseCalldataView<Uint8Array, FixedBytesType>
    implements IndexableView<bigint, Memory, SingleByteCalldataView>
{
    decode(state: Memory): Uint8Array | DecodingFailure {
        return this.readMemAt(this.loc, state, this.type.size);
    }

    indexView(key: bigint): DecodingFailure | SingleByteCalldataView {
        if (key >= this.type.size || key < 0n) {
            return new DecodingFailure(`Invalid index ${key} in ${this.type.pp()}`);
        }

        return new SingleByteCalldataView(this.loc + key, this.base);
    }
}

export class BytesCalldataView
    extends BaseCalldataView<Uint8Array, BytesType>
    implements IndexableView<bigint, Memory, SingleByteCalldataView>
{
    decode(state: Memory): Uint8Array | DecodingFailure {
        return this.decodeBytesAt(this.loc, state);
    }

    indexView(key: bigint, state: Memory): DecodingFailure | SingleByteCalldataView {
        const len = this.decodeIntAt(this.loc, uint256, state);

        if (isFailure(len)) {
            return len;
        }

        if (key >= len || key < 0n) {
            return new DecodingFailure(`Invalid index ${key} in bytes of len ${len}`);
        }

        return new SingleByteCalldataView(this.loc + 32n + key, this.base);
    }
}

export class StringCalldataView extends BaseCalldataView<string, BytesType> {
    decode(state: Memory): string | DecodingFailure {
        const bytes = this.decodeBytesAt(this.loc, state);

        if (isFailure(bytes)) {
            return bytes;
        }

        return bytesToUtf8(bytes);
    }
}

export class TupleCalldataView extends BaseCalldataView<Value[], TupleType> {
    decode(state: Memory): Value[] | DecodingFailure {
        let offset: bigint | DecodingFailure = this.loc;
        let base = this.base;

        if (isTypeDynamic(this.type)) {
            offset = this.decodeIntAt(offset, uint256, state);
            if (isFailure(offset)) {
                return offset;
            }

            base = offset + this.base;
            offset = 0n;
        }

        return this.decodeTupleAt(offset, base, this.type, state);
    }
}

export abstract class BaseArrayCalldataView
    extends BaseCalldataView<Value[], ArrayType>
    implements IndexableView<bigint, Memory, BaseCalldataView<Value, TypeNode>>
{
    decodeArray(baseOff: bigint, bigIntSize: bigint, state: Memory): Value[] | DecodingFailure {
        if (!inRange(bigIntSize, 0, MAX_ARR_DECODE_LIMIT)) {
            return new DecodingFailure(`Array too large ${bigIntSize}`);
        }

        const size = Number(bigIntSize);
        const res: Value[] = [];

        let off: bigint = 0n;
        const newBase = baseOff + this.base;
        const hs = headSize(this.type.elementT);

        if (hs === undefined) {
            return new DecodingFailure(`Can't compute head size of ${this.type.elementT.pp()}`);
        }

        const elSize = BigInt(hs);

        for (let i = 0; i < size; i++) {
            const elView = makeCalldataView(this.type.elementT, off, newBase);
            res.push(elView.decode(state));
            off += elSize;
        }

        return res;
    }

    protected _indexView(
        key: bigint,
        baseOff: bigint,
        size: bigint
    ): BaseCalldataView<Value, TypeNode> | DecodingFailure {
        if (key >= size || key < 0n) {
            return new DecodingFailure(`Invalid index ${key} in array of size ${size}`);
        }

        const newBase = baseOff + this.base;
        const hs = headSize(this.type.elementT);

        if (hs === undefined) {
            return new DecodingFailure(`Can't compute head size of ${this.type.elementT.pp()}`);
        }

        const elSize = BigInt(hs);
        return makeCalldataView(this.type.elementT, elSize * key, newBase);
    }

    abstract indexView(
        key: bigint,
        state: Memory
    ): BaseCalldataView<Value, TypeNode> | DecodingFailure;
}

export class ArrayCalldataView extends BaseArrayCalldataView {
    decode(state: Memory): Value[] | DecodingFailure {
        let baseOff: bigint = this.loc;

        // Dynamic sized arrays have length at the start. Fixed sized arrays do
        // not.
        let bigintSize: bigint | DecodingFailure;
        if (this.type.size !== undefined) {
            bigintSize = this.type.size;
        } else {
            bigintSize = this.decodeIntAt(baseOff, uint256, state);

            if (isFailure(bigintSize)) {
                return bigintSize;
            }

            baseOff += 32n;
        }

        return this.decodeArray(baseOff, bigintSize, state);
    }

    indexView(key: bigint, state: Memory): BaseCalldataView<Value, TypeNode> | DecodingFailure {
        let baseOff: bigint = this.loc;

        // Dynamic sized arrays have length at the start. Fixed sized arrays do
        // not.
        let size: bigint | DecodingFailure;
        if (this.type.size !== undefined) {
            size = this.type.size;
        } else {
            size = this.decodeIntAt(baseOff, uint256, state);

            if (isFailure(size)) {
                return size;
            }

            baseOff += 32n;
        }

        return this._indexView(key, baseOff, size);
    }
}

/**
 * An ArraySliceView is only created from stack locations. It should not be created in makeCalldataView.
 */
export class ArraySliceCalldataView extends BaseArrayCalldataView {
    constructor(
        type: ArrayType,
        loc: bigint,
        protected len: bigint
    ) {
        // Note: The base is 0n on purpose here since this is created from stack values?
        super(type, loc, 0n);
    }

    decode(state: Memory): Value[] | DecodingFailure {
        return this.decodeArray(this.loc, this.len, state);
    }

    indexView(key: bigint): BaseCalldataView<Value, TypeNode> | DecodingFailure {
        return this._indexView(key, this.loc, this.len);
    }
}

export class StructCalldataView
    extends BaseCalldataView<Struct, ExpStructType>
    implements StructView<Memory, BaseCalldataView<Value, TypeNode>>
{
    decode(state: Memory): Struct {
        // A StructCalldataView should be wrapped in a PointerCalldataView. So translating
        // the base should be handled by PointerCalldataView.decode
        const values = this.decodeTupleAt(
            this.loc,
            this.base,
            new TupleType(this.type.fields.map(([, type]) => type)),
            state
        );
        return new Struct(
            zip(
                this.type.fields.map(([name]) => name),
                values
            )
        );
    }

    fieldView(name: string): DecodingFailure | BaseCalldataView<Value, TypeNode> {
        let base = this.base;
        let loc = this.loc;

        if (isTypeDynamic(this.type)) {
            base = loc + this.base;
            loc = 0n;
        }

        for (const [fieldName, fieldType] of this.type.fields) {
            if (name === fieldName) {
                return makeCalldataView(fieldType, loc, base);
            }

            const tSize = headSize(fieldType);

            if (tSize === undefined) {
                return new DecodingFailure(
                    `Couldn't compute head size for field ${fieldName} in ${this.type.name}`
                );
            }

            loc += BigInt(tSize);
        }

        return new DecodingFailure(`No field ${name} on type ${this.type.pp()}`);
    }
}

export class PointerCalldataView
    extends BaseCalldataView<Value, PointerType>
    implements PointerView<Memory, BaseCalldataView<Value, TypeNode>>
{
    decode(state: Memory): Value | DecodingFailure {
        const innerView = this.toView(state);

        if (isFailure(innerView)) {
            return innerView;
        }

        return innerView.decode(state);
    }

    toView(state: Memory): DecodingFailure | BaseCalldataView<Value, TypeNode> {
        let off: bigint | DecodingFailure = this.loc;

        if (isTypeDynamic(this.type.to)) {
            off = this.decodeIntAt(off, uint256, state);

            if (isFailure(off)) {
                return off;
            }

            return makeCalldataView(this.type.to, 0n, this.base + off);
        }

        return makeCalldataView(this.type.to, off, this.base);
    }
}

export class MissingCalldataView extends BaseCalldataView<DecodingFailure, MissingType> {
    decode(): DecodingFailure {
        return new DecodingFailure(
            `${this.type.rawTypeName ? this.type.rawTypeName.type : "<unknown>"}`
        );
    }
}

export function makeCalldataView(
    type: TypeNode,
    loc: bigint,
    base: bigint
): BaseCalldataView<Value, TypeNode> {
    if (type instanceof IntType) {
        return new IntCalldataView(type, loc, base);
    }

    if (type instanceof BoolType) {
        return new BoolCalldataView(type, loc, base);
    }

    if (type instanceof AddressType) {
        return new AddressCalldataView(type, loc, base);
    }

    if (type instanceof FixedBytesType) {
        return new FixedBytesCalldataView(type, loc, base);
    }

    if (type instanceof TupleType) {
        return new TupleCalldataView(type, loc, base);
    }

    if (type instanceof BytesType) {
        return new BytesCalldataView(type, loc, base);
    }

    if (type instanceof StringType) {
        return new StringCalldataView(type, loc, base);
    }

    if (type instanceof ArrayType) {
        return new ArrayCalldataView(type, loc, base);
    }

    if (type instanceof ExpStructType) {
        return new StructCalldataView(type, loc, base);
    }

    if (type instanceof TupleType) {
        return new TupleCalldataView(type, loc, base);
    }

    if (type instanceof PointerType) {
        return new PointerCalldataView(type, loc, base);
    }

    if (type instanceof MissingType) {
        return new MissingCalldataView(type, loc, base);
    }

    nyi(`makeCalldataView(${type.pp()})`);
}

export function makeCalldataViews(
    types: TypeNode[],
    base: bigint
): Array<BaseCalldataView<Value, TypeNode>> {
    let off = 0n;
    const res: Array<BaseCalldataView<Value, TypeNode>> = [];
    let failRemaining = false;

    for (const t of types) {
        // If we have missing type info at an earlier type, and we can't compute its head size,
        // we make all remaining views "missing"
        if (failRemaining) {
            res.push(new MissingCalldataView(new MissingType(undefined), off, base));
            continue;
        }

        const view = makeCalldataView(t, off, base);
        res.push(view);

        const hs = headSize(t);

        if (hs === undefined) {
            failRemaining = true;
        } else {
            off += BigInt(hs);
        }
    }

    return res;
}
