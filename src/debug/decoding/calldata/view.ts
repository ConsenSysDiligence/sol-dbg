import { Address, bytesToUtf8 } from "@ethereumjs/util";
import {
    AddressType,
    ArrayType,
    assert,
    BoolType,
    BytesType,
    ContractDefinition,
    EnumDefinition,
    enumToIntType,
    FixedBytesType,
    InferType,
    IntType,
    PackedArrayType,
    PointerType,
    StringType,
    StructDefinition,
    TupleType,
    TypeNode,
    types,
    UserDefinedType,
    UserDefinedValueTypeDefinition
} from "solc-typed-ast";
import {
    bigEndianBufToBigint,
    fits,
    MAX_ARR_DECODE_LIMIT,
    nyi,
    readMem,
    uint256
} from "../../../utils/misc";
import { isTypeEncodedInline } from "../../abi";
import { Memory } from "../../types";
import { ArrayView, BaseAddressView, SolValue, StructView } from "../view";

function calldataStaticSize(typ: TypeNode, infer: InferType): number {
    // Primitive types
    if (
        typ instanceof IntType ||
        typ instanceof BoolType ||
        typ instanceof FixedBytesType ||
        typ instanceof AddressType ||
        typ instanceof PackedArrayType
    ) {
        return 32;
    }

    if (typ instanceof UserDefinedType) {
        const def = typ.definition;

        if (
            def instanceof EnumDefinition ||
            def instanceof UserDefinedValueTypeDefinition ||
            def instanceof ContractDefinition
        ) {
            return 32;
        }
    }

    // For pointers just get the inner type static size
    if (typ instanceof PointerType) {
        return calldataStaticSize(typ.to, infer);
    }

    // Fixed size arrays are encoded as tuples. Others are 32
    if (typ instanceof ArrayType) {
        if (typ.size === undefined) {
            return 32;
        }

        return calldataStaticSize(typ.elementT, infer) * Number(typ.size);
    }

    // Structs are encoded as tuples
    if (typ instanceof UserDefinedType) {
        const def = typ.definition;

        if (def instanceof StructDefinition) {
            let size = 0;

            for (const member of def.vMembers) {
                const memberT = infer.variableDeclarationToTypeNode(member);
                size += calldataStaticSize(memberT, infer);
            }

            return size;
        }
    }

    if (typ instanceof TupleType) {
        let size = 0;
        for (const elT of typ.elements) {
            assert(elT !== null, ``);

            size += calldataStaticSize(elT, infer);
        }

        return size;
    }

    nyi(`calldataStaticSize(${typ.pp()})`);
}

export abstract class BaseCalldataView<T extends TypeNode = TypeNode> extends BaseAddressView<T> {
    abstract decode(calldata: Memory): any;
    staticSize(): number {
        return calldataStaticSize(this.type, this.infer);
    }

    protected readMem(calldata: Memory, len: Uint8Array | bigint | number): Uint8Array | undefined {
        return readMem(this.address, len, calldata);
    }

    protected decodeInt(typ: IntType, calldata: Memory): bigint | undefined {
        const bytes = this.readMem(calldata, 32);

        // OoB access
        if (bytes === undefined) {
            return undefined;
        }

        let res = bigEndianBufToBigint(bytes);

        // Convert signed negative 2's complement values
        if (typ.signed && (res & (BigInt(1) << BigInt(typ.nBits - 1))) !== BigInt(0)) {
            // Mask out any 1's above the number's size
            res = res & ((BigInt(1) << BigInt(typ.nBits)) - BigInt(1));
            res = -((BigInt(1) << BigInt(typ.nBits)) - res);
        }

        assert(
            fits(res, typ),
            `Decoded value ${res} from ${this.address} doesn't fit in expected type ${typ.pp()}`
        );

        return res;
    }

    protected decodeEnum(def: EnumDefinition, calldata: Memory): bigint | undefined {
        const typ = enumToIntType(def);
        return this.decodeInt(typ, calldata);
    }

    protected decodeBool(calldata: Memory): boolean | undefined {
        const bytes = this.readMem(calldata, 32);

        if (bytes === undefined) {
            return undefined;
        }

        return bigEndianBufToBigint(bytes) !== BigInt(0);
    }

    protected decodeAddress(calldata: Memory): Address | undefined {
        const bytes = this.readMem(calldata, 32);

        if (bytes === undefined) {
            return undefined;
        }

        return new Address(bytes.slice(12));
    }

    protected decodeFixedBytes(typ: FixedBytesType, calldata: Memory): Uint8Array | undefined {
        return this.readMem(calldata, typ.size);
    }

    protected decodeBytes(calldata: Memory): Uint8Array | undefined {
        const len = this.decodeInt(uint256, calldata);

        if (len == undefined || len >= MAX_ARR_DECODE_LIMIT) {
            return undefined;
        }

        return readMem(this.address + BigInt(32), Number(len), calldata);
    }

    protected decodeString(calldata: Memory): string | undefined {
        const bytes = this.decodeBytes(calldata);

        if (bytes === undefined) {
            return undefined;
        }

        return bytesToUtf8(bytes);
    }
}

export abstract class BaseCalldataArrayView<T extends PackedArrayType | ArrayType>
    extends BaseCalldataView<T>
    implements ArrayView
{
    abstract decodeIdx(calldata: Memory, idx: bigint): BaseCalldataView;
    abstract decodeLen(storage: Memory): bigint | undefined;
}

export abstract class BaseCalldataStructView
    extends BaseCalldataView<UserDefinedType>
    implements StructView
{
    abstract decodeField(calldata: Memory, field: string): BaseCalldataView;
}

export class SimpleCalldataView extends BaseCalldataView {
    decode(
        calldata: Memory,
        typ: TypeNode = this.type
    ): bigint | boolean | Address | Uint8Array | string | undefined {
        if (typ instanceof IntType) {
            return this.decodeInt(typ, calldata);
        }

        if (typ instanceof BoolType) {
            return this.decodeBool(calldata);
        }

        if (typ instanceof AddressType) {
            return this.decodeAddress(calldata);
        }

        if (typ instanceof FixedBytesType) {
            return this.decodeFixedBytes(typ, calldata);
        }

        if (typ instanceof UserDefinedType) {
            const def = typ.definition;

            if (def instanceof EnumDefinition) {
                return this.decodeEnum(def, calldata);
            } else if (def instanceof ContractDefinition) {
                return this.decodeAddress(calldata);
            } else if (def instanceof UserDefinedValueTypeDefinition) {
                const underlyingType = this.infer.typeNameToTypeNode(def.underlyingType);
                return this.decode(calldata, underlyingType);
            }
        }

        nyi(`Decode simple type ${this.type.pp()}`);
    }
}

export class ByteCalldataView extends BaseCalldataView<IntType> {
    staticSize(): number {
        return 1;
    }

    constructor(infer: InferType, address: bigint) {
        super(types.uint8, infer, address);
    }

    decode(calldata: Memory): number | undefined {
        const word = this.readMem(calldata, 8);

        if (word === undefined) {
            return undefined;
        }

        return word[0];
    }
}

export class PackedArrayCalldataView<
    T extends BytesType | StringType
> extends BaseCalldataArrayView<T> {
    decode(calldata: Memory): Uint8Array | string | undefined {
        if (this.type instanceof BytesType) {
            return this.decodeBytes(calldata);
        }

        return this.decodeString(calldata);
    }

    decodeLen(calldata: Memory): bigint | undefined {
        return this.decodeInt(uint256, calldata);
    }

    decodeIdx(calldata: Memory, idx: SolValue): BaseCalldataView {
        return new ByteCalldataView(this.infer, this.address + 32n + BigInt(idx));
    }
}

/**
 * Helper function to make a new BaseStorageView.
 * Selects the correct view class based on the type.
 */
function makeCalldataView(type: TypeNode, infer: InferType, address: bigint): BaseCalldataView {
    if (
        type instanceof IntType ||
        type instanceof BoolType ||
        type instanceof AddressType ||
        type instanceof FixedBytesType
    ) {
        return new SimpleCalldataView(type, infer, address);
    }

    if (type instanceof UserDefinedType) {
        const def = type.definition;

        if (
            def instanceof EnumDefinition ||
            def instanceof ContractDefinition ||
            def instanceof UserDefinedValueTypeDefinition
        ) {
            return new SimpleCalldataView(type, infer, address);
        }
    }

    if (type instanceof PointerType) {
        return new PointerCalldataView(type, infer, address);
    }

    if (type instanceof TupleType) {
        return new TupleCalldataView(type, infer, address);
    }

    nyi(`makeCalldataView(${type.pp()})`);
}

export class TupleCalldataView extends BaseCalldataView<TupleType> {
    static decodeElementAt(
        elT: TypeNode,
        infer: InferType,
        address: bigint,
        base: bigint,
        calldata: Memory
    ): BaseCalldataView {
        if (!isTypeEncodedInline(elT)) {
            return makeCalldataView(elT, infer, address);
        }

        const off = new SimpleCalldataView(uint256, infer, address).decode(calldata) as bigint;
        return makeCalldataView(elT, infer, base + off);
    }

    decode(calldata: Memory): any[] {
        console.error(`Tuple.decode @ ${this.address}`);
        const res: any[] = [];

        const base = this.address;
        let curAddr = this.address;

        for (const elT of this.type.elements) {
            assert(elT !== null, ``);

            const elView = TupleCalldataView.decodeElementAt(
                elT,
                this.infer,
                curAddr,
                base,
                calldata
            );
            console.error(
                `tupleElementDecode ${elT.pp()} at ${curAddr} base ${base} val: ? view: `,
                elView
            );
            const val = elView.decode(calldata);
            res.push(val);

            curAddr += BigInt(elView.staticSize());
        }

        return res;
    }

    decodeIdx(calldata: Memory, idx: number): BaseCalldataView {
        const elT = this.type.elements[idx];
        assert(elT !== null, ``);

        const base = this.address;
        const curAddr = this.address + BigInt(idx * 32);
        return TupleCalldataView.decodeElementAt(elT, this.infer, curAddr, base, calldata);
    }
}

export class StructCalldataView extends BaseCalldataStructView {
    fields: Array<[string, TypeNode]> = [];
    tupleView: TupleCalldataView;

    constructor(type: UserDefinedType, infer: InferType, address: bigint) {
        super(type, infer, address);
        assert(
            this.type.definition instanceof StructDefinition,
            `Unexpected pointer type {0}`,
            this.type
        );

        for (const decl of this.type.definition.vMembers) {
            this.fields.push([decl.name, this.infer.variableDeclarationToTypeNode(decl)]);
        }

        this.tupleView = new TupleCalldataView(
            new TupleType(this.fields.map((x) => x[1])),
            this.infer,
            this.address
        );
    }

    decode(calldata: Memory): any {
        const res: any = {};

        const vals = this.tupleView.decode(calldata);

        for (let i = 0; i < this.fields.length; i++) {
            res[this.fields[i][0]] = vals[i];
        }

        return res;
    }

    decodeField(calldata: Memory, field: string): BaseCalldataView {
        const idx = this.fields.findIndex(([name]) => name === field);
        assert(idx >= 0, `No field {0}`, field);
        return this.tupleView.decodeIdx(calldata, idx);
    }
}

export class ArrayCalldataView extends BaseCalldataArrayView<ArrayType> {
    decodeIdx(calldata: Memory, idx: bigint): BaseCalldataView {
        const base = this.address + 32n;
        const elAddr = this.address + 32n + idx * 32n;
        return TupleCalldataView.decodeElementAt(
            this.type.elementT,
            this.infer,
            elAddr,
            base,
            calldata
        );
    }

    decodeLen(calldata: Memory): bigint | undefined {
        return new SimpleCalldataView(uint256, this.infer, this.address).decode(calldata) as
            | bigint
            | undefined;
    }

    decode(calldata: Memory): any[] | undefined {
        console.error(`ArrayView.decode @ ${this.address}`);
        const res: any[] = [];

        const len = this.decodeLen(calldata);
        if (len == undefined || len >= MAX_ARR_DECODE_LIMIT) {
            return undefined;
        }

        const numLen = Number(len);
        const base = this.address + 32n;

        for (let i = 0, curAddr = this.address + 32n; i < numLen; i++) {
            console.error(`ArrayView.decode ${i}-th element @ ${curAddr}`);
            const elView = TupleCalldataView.decodeElementAt(
                this.type.elementT,
                this.infer,
                curAddr,
                base,
                calldata
            );

            res.push(elView.decode(calldata));
            curAddr += BigInt(elView.staticSize());
        }
        return res;
    }
}

export class PointerCalldataView extends BaseCalldataView<PointerType> {
    deref(): BaseCalldataView {
        if (this.type.to instanceof ArrayType) {
            assert(
                this.type.to.size === undefined,
                `Unexpected fixed array type {0}`,
                this.type.to
            );

            return new ArrayCalldataView(this.type.to, this.infer, this.address);
        }

        if (this.type.to instanceof PackedArrayType) {
            return new PackedArrayCalldataView(this.type.to, this.infer, this.address);
        }

        nyi(`Calldata pointer type ${this.type.pp()}`);
    }

    decode(calldata: Memory): any {
        return this.deref().decode(calldata);
    }
}
