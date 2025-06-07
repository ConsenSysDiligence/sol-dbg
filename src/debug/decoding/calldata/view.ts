import {
    AddressType,
    ArrayType,
    assert,
    BoolType,
    BytesType,
    ContractDefinition,
    DataLocation,
    EnumDefinition,
    enumToIntType,
    FixedBytesType,
    InferType,
    IntType,
    PointerType,
    StringType,
    StructDefinition,
    TupleType,
    TypeName,
    TypeNode,
    UserDefinedType,
    UserDefinedValueTypeDefinition
} from "solc-typed-ast";
import { Memory } from "../../types";
import { Struct, Value } from "../value";
import { View } from "../view";
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
import { inRange, sum } from "../utils";

/**
 * Return true IFF the given type is "dynamic". I,e. its size is not statically known.
 * @param t
 * @param infer
 * @returns
 */
function isTypeDynamic(t: TypeNode, infer: InferType): boolean {
    if (t instanceof BytesType || t instanceof StringType) {
        return true;
    }

    if (t instanceof ArrayType) {
        return t.size === undefined || isTypeDynamic(t.elementT, infer);
    }

    if (t instanceof TupleType) {
        for (const elementT of t.elements) {
            if (elementT && isTypeDynamic(elementT, infer)) {
                return true;
            }
        }
    }

    if (t instanceof UserDefinedType && t.definition instanceof StructDefinition) {
        for (const member of t.definition.vMembers) {
            if (isTypeDynamic(infer.variableDeclarationToTypeNode(member), infer)) {
                return true;
            }
        }
    }

    if (t instanceof PointerType) {
        return isTypeDynamic(t.to, infer);
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
function headSize(t: TypeNode, infer: InferType): number {
    if (isTypeDynamic(t, infer)) {
        return 32;
    }

    if (t instanceof TupleType) {
        return sum(...t.elements.map((elT) => headSize(elT as TypeNode, infer)));
    }

    if (t instanceof ArrayType) {
        assert(t.size !== undefined, `Statically sized array types must have a size`);
        return headSize(t.elementT, infer) * Number(t.size);
    }

    if (t instanceof UserDefinedType && t.definition instanceof StructDefinition) {
        return sum(
            ...t.definition.vMembers.map((memDecl) =>
                headSize(infer.variableDeclarationToTypeNode(memDecl), infer)
            )
        );
    }

    if (
        t instanceof IntType ||
        t instanceof BoolType ||
        t instanceof FixedBytesType ||
        t instanceof AddressType
    ) {
        return 32;
    }

    if (
        t instanceof UserDefinedType &&
        (t.definition instanceof EnumDefinition ||
            t.definition instanceof UserDefinedValueTypeDefinition ||
            t.definition instanceof ContractDefinition)
    ) {
        return 32;
    }

    if (t instanceof PointerType) {
        return headSize(t.to, infer);
    }

    nyi(`Statically sized type ${t.pp()}`);
}

export abstract class BaseCalldataView<
    Val extends Value,
    Type extends TypeNode = TypeNode
> extends View<Memory, Val, bigint, Type> {
    constructor(
        type: Type,
        infer: InferType,
        loc: bigint,
        protected base: bigint
    ) {
        super(type, infer, loc);
    }

    protected readMemAt(off: number | bigint, calldata: Memory, len: bigint | number): Uint8Array {
        const actualOffset = this.base + BigInt(off);
        const res = readMem(actualOffset, len, calldata);

        if (!res) {
            // OoB access
            this.fail(
                calldata,
                `OoB access at ${actualOffset}:${actualOffset + BigInt(len)} in memory of size ${calldata.length}`
            );
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
            this.fail(calldata, `Decoded value ${res} doesnt fit in type ${this.type.pp()}`);
        }

        return res;
    }

    decodeBytesAt(loc: bigint, state: Memory): Uint8Array {
        const bytesOffset = this.decodeIntAt(loc, uint256, state);
        const len = this.decodeIntAt(bytesOffset, uint256, state);

        if (len >= MAX_ARR_DECODE_LIMIT) {
            this.fail(state, `Bytes to decode too large - ${len}`);
        }

        return this.readMemAt(bytesOffset + 32n, state, len);
    }

    decodeTupleAt(loc: bigint, type: TupleType, state: Memory): Value[] {
        let offset;
        let base;

        // For tuples with a "dynamic" element, solidity just store a pointer at loc
        // Static tuples are laid out directly loc.
        if (isTypeDynamic(type, this.infer)) {
            offset = 0n;
            base = this.decodeIntAt(loc, uint256, state) + this.base;
        } else {
            offset = loc;
            base = this.base;
        }

        const res: Value[] = [];

        for (const t of type.elements) {
            assert(t !== null, `Unexpected null element in tuple type {0}`, type);
            res.push(makeCalldataView(t, this.infer, offset, base).decode(state));
            offset += BigInt(headSize(t, this.infer));
        }

        return res;
    }

    pp(): string {
        return `<${this.type.pp()}@${this.loc + this.base} in calldata>`;
    }
}

/**
 * View to an IntType in calldata
 */
export class IntCalldataView extends BaseCalldataView<bigint, IntType> {
    decode(state: Memory): bigint {
        return this.decodeIntAt(this.loc, this.type, state);
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
    innnerType: IntType;

    constructor(
        public readonly type: UserDefinedType,
        public readonly infer: InferType,
        loc: bigint,
        base: bigint
    ) {
        super(type, infer, loc, base);
        assert(
            type.definition instanceof EnumDefinition,
            `Building an EnumCalldataView with wrong user defined type {0}`,
            type.definition
        );
        this.innnerType = enumToIntType(this.type.definition as EnumDefinition);
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
        assert(
            type.definition instanceof ContractDefinition,
            `Building an ContractCalldataView with wrong user defined type {0}`,
            type.definition
        );
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
        base: bigint
    ) {
        super(type, infer, loc, base);
        assert(
            type.to instanceof BytesType,
            `Building a BytesCalldataView with invalid type {0}`,
            type
        );
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
        assert(
            type.to instanceof StringType,
            `Building a StringCalldataView with invalid type {0}`,
            type
        );
    }

    decode(state: Memory): string {
        const bytes = this.decodeBytesAt(this.loc, state);
        return bytesToUtf8(bytes);
    }
}

export class TupleCalldataView extends BaseCalldataView<Value[], TupleType> {
    decode(state: Memory): Value[] {
        return this.decodeTupleAt(this.loc, this.type, state);
    }
}

export class ArrayCalldataView extends BaseCalldataView<Value[], PointerType> {
    /**
     * Array type
     */
    arrT: ArrayType;

    constructor(
        public readonly type: PointerType,
        public readonly infer: InferType,
        loc: bigint,
        base: bigint
    ) {
        super(type, infer, loc, base);
        assert(
            type.to instanceof ArrayType,
            `Building a ArrayCalldataView with invalid type {0}`,
            type
        );
        this.arrT = type.to;
    }

    decode(state: Memory): Value[] {
        let baseOff: bigint;

        // Fixed-sized arrays with static element types are laid out directly as tuples.
        // For dynamic arrays we instead store a pointer at the head
        if (isTypeDynamic(this.type, this.infer)) {
            baseOff = this.decodeIntAt(this.loc, uint256, state);
        } else {
            baseOff = this.loc;
        }

        // Dynamic sized arrays have length at the statr. Fixed sized arrays do
        // not.
        let bigintSize: bigint;
        if (this.arrT.size !== undefined) {
            bigintSize = this.arrT.size;
        } else {
            bigintSize = this.decodeIntAt(baseOff, uint256, state);
            baseOff += 32n;
        }

        if (!inRange(bigintSize, 0, MAX_ARR_DECODE_LIMIT)) {
            this.fail(state, `Array too large ${bigintSize}`);
        }

        const size = Number(bigintSize);
        const res: Value[] = [];

        let off: bigint = 0n;
        const newBase = baseOff + this.base;
        const elSize = BigInt(headSize(this.arrT.elementT, this.infer));

        for (let i = 0; i < size; i++) {
            const elView = makeCalldataView(this.arrT.elementT, this.infer, off, newBase);
            res.push(elView.decode(state));
            off += elSize;
        }

        return res;
    }
}

export class StructCalldataView extends BaseCalldataView<Struct, PointerType> {
    members: Array<[string, TypeNode]>;

    constructor(
        public readonly type: PointerType,
        public readonly infer: InferType,
        loc: bigint,
        base: bigint
    ) {
        super(type, infer, loc, base);

        assert(
            type.to instanceof UserDefinedType && type.to.definition instanceof StructDefinition,
            `Building an StructCalldataView with invalid type {0}`,
            type
        );

        const def = type.to.definition;

        this.members = def.vMembers.map((mem) => {
            return [
                mem.name,
                infer.typeNameToSpecializedTypeNode(mem.vType as TypeName, DataLocation.CallData)
            ];
        });
    }

    decode(state: Memory): Struct {
        const values = this.decodeTupleAt(
            this.loc,
            new TupleType(this.members.map(([, type]) => type)),
            state
        );
        return new Struct(
            zip(
                this.members.map(([name]) => name),
                values
            )
        );
    }
}

export function makeCalldataView(
    type: TypeNode,
    infer: InferType,
    loc: bigint,
    base: bigint
): BaseCalldataView<Value, TypeNode> {
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

        if (type.to instanceof TupleType) {
            return new TupleCalldataView(type.to, infer, loc, base);
        }
    }

    nyi(`makeCalldataView(${type.pp()})`);
}

export function makeCalldataViews(
    types: TypeNode[],
    infer: InferType,
    base: bigint
): Array<BaseCalldataView<Value, TypeNode>> {
    let off = 0n;
    const res: Array<BaseCalldataView<Value, TypeNode>> = [];

    for (const t of types) {
        const view = makeCalldataView(t, infer, off, base);
        res.push(view);
        off += BigInt(headSize(t, infer));
    }

    return res;
}
