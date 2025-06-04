import { AddressType, ArrayType, assert, BoolType, BytesType, ContractDefinition, DataLocation, EnumDefinition, enumToIntType, FixedBytesType, InferType, IntType, PointerType, specializeType, StringType, StructDefinition, TupleType, TypeNode, UserDefinedType, UserDefinedValueTypeDefinition } from "solc-typed-ast";
import { Memory } from "../../types";
import { Struct, Value } from "../value";
import { View } from "../view";
import { bigEndianBufToBigint, fits, MAX_ARR_DECODE_LIMIT, nyi, readMem, uint256 } from "../../../utils";
import { Address, bytesToUtf8 } from "@ethereumjs/util";
import { inRange } from "../utils";

export abstract class BaseCalldataView<Val extends Value, Type extends TypeNode = TypeNode> extends View<Memory, Val, bigint, Type> {
    constructor(
        public readonly type: Type,
        public readonly infer: InferType,
        protected loc: bigint,
        protected base: bigint
    ) {
        super(type, infer, loc);
    }

    protected readMemAt(off: number | bigint, calldata: Memory, len: bigint | number): Uint8Array {
        const res = readMem(this.base + BigInt(off), len, calldata);

        if (!res) {
            // OoB access
            this.fail(calldata, `OoB access at ${this.loc}:${this.loc + BigInt(len)} in memory of size ${calldata.length}`)
        }

        return res;
    }

    protected decodeIntAt(offset: bigint, typ: IntType, calldata: Memory): bigint {
        const bytes = this.readMemAt(offset, calldata, 32);

        let res = bigEndianBufToBigint(bytes);

        // Convert signed negative 2's complement values
        if (typ.signed && (res & (BigInt(1) << BigInt(typ.nBits - 1))) !== BigInt(0)) {
            // Mask out any 1's above the number's size
            res = res & ((BigInt(1) << BigInt(typ.nBits)) - BigInt(1));
            res = -((BigInt(1) << BigInt(typ.nBits)) - res);
        }

        if (!fits(res, typ)) {
            this.fail(calldata, `Decoded value ${res} doesnt fit in type ${this.type.pp()}`)
        }

        return res;
    }

    decodeBytesAt(loc: bigint, state: Memory): Uint8Array {
        const bytesOffset = this.decodeIntAt(loc, uint256, state);
        const len = this.decodeIntAt(bytesOffset, uint256, state);

        if (len >= MAX_ARR_DECODE_LIMIT) {
            this.fail(state, `Bytes to decode too large - ${len}`)
        }

        return this.readMemAt(bytesOffset + 32n, state, len);
    }

    pp(): string {
        return `<${this.type.pp()}@${this.loc} in calldata>`
    }

    public staticSize(): number {
        return 32;
    }
}

/**
 * View to an IntType in calldata
 */
export class IntCalldataView extends BaseCalldataView<bigint, IntType> {
    decode(state: Memory): bigint {
        return this.decodeIntAt(this.loc, this.type, state)
    }
}

/**
 * View to a BoolType in calldata
 */
export class BoolCalldataView extends BaseCalldataView<boolean, BoolType> {
    decode(state: Memory): boolean {
        return this.decodeIntAt(this.loc, uint256, state) !== 0n;
    }
}

/**
 * View to an Address in calldata
 */
export class AddressCalldataView extends BaseCalldataView<Address, AddressType> {
    decode(state: Memory): Address {
        return new Address(this.readMemAt(this.loc, state, 32).slice(12));
    }
}

/**
 * View to an FixedBytes in calldata
 */
export class FixedBytesCalldataView extends BaseCalldataView<Uint8Array, FixedBytesType> {
    decode(state: Memory): Uint8Array {
        return this.readMemAt(this.loc, state, this.type.size);
    }
}

/**
 * View to an Enum in calldata
 */
export class EnumCalldataView extends BaseCalldataView<number, UserDefinedType> {
    innnerType: IntType

    constructor(
        public readonly type: UserDefinedType,
        public readonly infer: InferType,
        loc: bigint,
        base: bigint
    ) {
        super(type, infer, loc, base);
        assert(type.definition instanceof EnumDefinition, `Building an EnumCalldataView with user defined type {0}`, type.definition)
        this.innnerType = enumToIntType(this.type.definition as EnumDefinition)
        assert(this.innnerType.nBits < 32, `Unexpectedly large enum`);
    }

    decode(state: Memory): number {
        return Number(this.decodeIntAt(this.loc, this.innnerType, state));
    }
}

/**
 * View to a Contract in calldata (just an address)
 */
export class ContractCalldataView extends BaseCalldataView<Address, UserDefinedType> {
    constructor(
        public readonly type: UserDefinedType,
        public readonly infer: InferType,
        loc: bigint,
        base: bigint
    ) {
        super(type, infer, loc, base);
        assert(type.definition instanceof ContractDefinition, `Building an ContractCalldataView with user defined type {0}`, type.definition)
    }

    decode(state: Memory): Address {
        return new Address(this.readMemAt(this.loc, state, 32).slice(12));
    }
}

export class BytesCalldataView extends BaseCalldataView<Uint8Array, PointerType> {
    constructor(
        public readonly type: PointerType,
        public readonly infer: InferType,
        loc: bigint,
        base: bigint,
    ) {
        super(type, infer, loc, base);
        assert(type.to instanceof BytesType, `Building a BytesCalldataView with invalid type {0}`, type)
    }

    decode(state: Memory): Uint8Array {
        return this.decodeBytesAt(this.loc, state);
    }
}

export class StringCalldataView extends BaseCalldataView<string, PointerType> {
    constructor(
        public readonly type: PointerType,
        public readonly infer: InferType,
        loc: bigint,
        base: bigint
    ) {
        super(type, infer, loc, base);
        assert(type.to instanceof StringType, `Building a StringCalldataView with invalid type {0}`, type)
    }

    decode(state: Memory): string {
        const bytes = this.decodeBytesAt(this.loc, state);
        return bytesToUtf8(bytes);
    }
}

export class TupleCalldataView extends BaseCalldataView<Value[], TupleType> {
    fieldViews: BaseCalldataView<Value, TypeNode>[];
    _staticSize: number = 0

    constructor(
        public readonly type: TupleType,
        public readonly infer: InferType,
        loc: bigint,
        base: bigint
    ) {
        super(type, infer, loc, base);

        this.fieldViews = type.elements.map((elT, idx) => {
            assert(elT !== null, `Unexpected null element in tuple type {0}`, type);
            return makeCalldataView(elT, this.infer, BigInt(idx) * 32n, this.loc + this.base);
        })

        for (const view of this.fieldViews) {
            this._staticSize += view.staticSize();
        }
    }

    decode(state: Memory): Value[] {
        return this.fieldViews.map((view) => view.decode(state));
    }

    staticSize(): number {
        return this._staticSize;
    }
}

export class ArrayCalldataView extends BaseCalldataView<Value[], PointerType> {
    /**
     * Array fixed size (if any)
     */
    arrT: ArrayType;
    fixedSize: number | undefined;

    constructor(
        public readonly type: PointerType,
        public readonly infer: InferType,
        loc: bigint,
        base: bigint
    ) {
        super(type, infer, loc, base);
        assert(type.to instanceof ArrayType, `Building a ArrayCalldataView with invalid type {0}`, type)
        this.arrT = type.to;
        assert(this.arrT.size === undefined || this.arrT.size < MAX_ARR_DECODE_LIMIT, `Array too large {0}`, type);
        this.fixedSize = type.to.size === undefined ? undefined : Number(type.to.size);
    }

    decode(state: Memory): Value[] {
        let size: number;
        let baseOff: bigint;

        if (this.fixedSize !== undefined) {
            size = this.fixedSize;
            baseOff = this.loc
        } else {
            const arrOff = this.decodeIntAt(this.loc, uint256, state)
            const bigintSize = this.decodeIntAt(arrOff, uint256, state)

            if (!inRange(bigintSize, 0, MAX_ARR_DECODE_LIMIT)) {
                this.fail(state, `Array too large ${bigintSize}`)
            }

            size = Number(bigintSize);
            baseOff = arrOff + 32n;
        }

        console.error(`Array ${this.type.pp()} baseOff:${baseOff} len: ${size}`)
        const res: Value[] = [];

        let off: bigint = 0n;

        for (let i = 0; i < size; i++) {
            const elView = makeCalldataView(this.arrT.elementT, this.infer, off, baseOff + this.base)
            res.push(elView.decode(state))

            off += BigInt(elView.staticSize());
        }

        return res;
    }

    public staticSize(): number {
        // Constant sized arrays are laid out as tuples
        if (this.fixedSize !== undefined) {
            return 32 * this.fixedSize;
        }

        return 32;
    }
}

export class StructCalldataView extends BaseCalldataView<Struct, PointerType> {
    fieldViews: [string, BaseCalldataView<Value, TypeNode>][];
    _staticSize: number = 0

    constructor(
        public readonly type: PointerType,
        public readonly infer: InferType,
        loc: bigint,
        base: bigint
    ) {
        super(type, infer, loc, base);

        assert(type.to instanceof UserDefinedType, `Building an StructCalldataView with invalid type {0}`, type)
        assert(type.to.definition instanceof StructDefinition, `Building an StructCalldataView with invalid type {0}`, type)

        const def = type.to.definition;

        this.fieldViews = def.vMembers.map((mem, idx) => {
            const memT = specializeType(infer.variableDeclarationToTypeNode(mem), DataLocation.CallData);
            return [mem.name, makeCalldataView(memT, this.infer, BigInt(idx) * 32n, this.loc + this.base)];
        })

        for (const [, view] of this.fieldViews) {
            this._staticSize += view.staticSize();
        }
    }

    decode(state: Memory): Struct {
        const entries: [string, Value][] = this.fieldViews.map(([name, view]) => [name, view.decode(state)])
        return new Struct(entries);
    }

    staticSize(): number {
        return this._staticSize;
    }
}

export function makeCalldataView(type: TypeNode, infer: InferType, loc: bigint, base: bigint): BaseCalldataView<Value, TypeNode> {
    if (type instanceof IntType) {
        return new IntCalldataView(type, infer, loc, base);
    }

    if (type instanceof BoolType) {
        return new BoolCalldataView(type, infer, loc, base);
    }

    if (type instanceof AddressType) {
        return new AddressCalldataView(type, infer, loc, base);
    }

    if (type instanceof FixedBytesType) {
        return new FixedBytesCalldataView(type, infer, loc, base);
    }

    if (type instanceof UserDefinedType) {
        const def = type.definition;
        if (def instanceof EnumDefinition) {
            return new EnumCalldataView(type, infer, loc, base);
        }

        if (def instanceof ContractDefinition) {
            return new ContractCalldataView(type, infer, loc, base);
        }

        if (def instanceof UserDefinedValueTypeDefinition) {
            const innerT = infer.typeNameToTypeNode(def.underlyingType);
            return makeCalldataView(innerT, infer, loc, base);
        }
    }

    if (type instanceof TupleType) {
        return new TupleCalldataView(type, infer, loc, base);
    }

    if (type instanceof PointerType) {
        if (type.to instanceof BytesType) {
            return new BytesCalldataView(type, infer, loc, base);
        }

        if (type.to instanceof StringType) {
            return new StringCalldataView(type, infer, loc, base);
        }

        if (type.to instanceof ArrayType) {
            return new ArrayCalldataView(type, infer, loc, base);
        }

        if (type.to instanceof UserDefinedType) {
            const def = type.to.definition;

            if (def instanceof StructDefinition) {
                return new StructCalldataView(type, infer, loc, base);
            }
        }
    }

    nyi(`makeCalldataView(${type.pp()})`);
}
