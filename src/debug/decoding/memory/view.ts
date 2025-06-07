import {
    AddressType,
    ArrayType,
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
    TypeName,
    TypeNode,
    UserDefinedType,
    UserDefinedValueTypeDefinition
} from "solc-typed-ast";
import { Memory } from "../../types";
import { View } from "../view";
import { Struct, Value } from "../value";
import {
    bigEndianBufToBigint,
    fits,
    MAX_ARR_DECODE_LIMIT,
    nyi,
    readMem,
    uint256
} from "../../../utils";
import { Address, bytesToUtf8 } from "@ethereumjs/util";

export abstract class BaseMemoryView<
    Val extends Value,
    Type extends TypeNode = TypeNode
> extends View<Memory, Val, bigint, Type> {
    protected readMemAt(off: bigint, calldata: Memory, len: bigint | number): Uint8Array {
        const res = readMem(off, len, calldata);

        if (!res) {
            // OoB access
            this.fail(
                calldata,
                `OoB access at ${off}:${off + BigInt(len)} in memory of size ${calldata.length}`
            );
        }

        return res;
    }

    protected decodeIntAt(off: bigint, type: IntType, state: Memory): bigint {
        const bytes = this.readMemAt(off, state, 32);

        let res = bigEndianBufToBigint(bytes);

        // Convert signed negative 2's complement values
        if (type.signed && (res & (BigInt(1) << BigInt(type.nBits - 1))) !== BigInt(0)) {
            // Mask out any 1's above the number's size
            res = res & ((BigInt(1) << BigInt(type.nBits)) - BigInt(1));
            res = -((BigInt(1) << BigInt(type.nBits)) - res);
        }

        if (!fits(res, type)) {
            this.fail(state, `Decoded value ${res} doesn't fit in expected type ${type.pp()}`);
        }
        return res;
    }

    protected decodeAddressAt(off: bigint, state: Memory): Address {
        return new Address(this.readMemAt(off + 12n, state, 20));
    }

    protected decodeBytesAt(loc: bigint, state: Memory): Uint8Array {
        const len = this.decodeIntAt(loc, uint256, state);

        if (len >= MAX_ARR_DECODE_LIMIT) {
            this.fail(state, `Bytes to decode too large - ${len}`);
        }

        return this.readMemAt(loc + 32n, state, len);
    }

    pp(): string {
        return `<${this.type.pp()}@${this.loc} in memory>`;
    }
}

export class IntMemView extends BaseMemoryView<bigint, IntType> {
    decode(state: Memory): bigint {
        return this.decodeIntAt(this.loc, this.type, state);
    }
}

export class AddressMemView extends BaseMemoryView<Address, AddressType> {
    decode(state: Memory): Address {
        return new Address(this.readMemAt(this.loc + 12n, state, 20));
    }
}

export class BoolMemView extends BaseMemoryView<boolean, BoolType> {
    decode(state: Memory): boolean {
        return bigEndianBufToBigint(this.readMemAt(this.loc, state, 32)) !== 0n;
    }
}

export class FixedBytesMemView extends BaseMemoryView<Uint8Array, FixedBytesType> {
    decode(state: Memory): Uint8Array {
        return this.readMemAt(this.loc, state, this.type.size);
    }
}

export class EnumMemView extends BaseMemoryView<number, UserDefinedType> {
    innerType: IntType;
    constructor(type: UserDefinedType, infer: InferType, loc: bigint) {
        super(type, infer, loc);
        if (!(type.definition instanceof EnumDefinition)) {
            this.fail(new Uint8Array(), `Invalid type ${type.pp()} for EnumMemView`);
        }

        this.innerType = enumToIntType(type.definition);
    }

    decode(state: Memory): number {
        return Number(this.decodeIntAt(this.loc, this.innerType, state));
    }
}

export class ContractMemView extends BaseMemoryView<Address, UserDefinedType> {
    constructor(type: UserDefinedType, infer: InferType, loc: bigint) {
        super(type, infer, loc);
        if (!(type.definition instanceof ContractDefinition)) {
            this.fail(new Uint8Array(), `Invalid type ${type.pp()} for ContractMemView`);
        }
    }

    decode(state: Memory): Address {
        return this.decodeAddressAt(this.loc, state);
    }
}

export class BytesMemView extends BaseMemoryView<Uint8Array, BytesType> {
    decode(state: Memory): Uint8Array {
        return this.decodeBytesAt(this.loc, state);
    }
}

export class StringMemView extends BaseMemoryView<string, StringType> {
    decode(state: Memory): string {
        const bytes = this.decodeBytesAt(this.loc, state);
        return bytesToUtf8(bytes);
    }
}

export class ArrayMemView extends BaseMemoryView<Value[], ArrayType> {
    decode(state: Memory): Value[] {
        let sizeBigint;
        let addr = this.loc;

        if (this.type.size === undefined) {
            sizeBigint = this.decodeIntAt(addr, uint256, state);
            addr += 32n;
        } else {
            sizeBigint = this.type.size;
        }

        if (sizeBigint >= MAX_ARR_DECODE_LIMIT) {
            this.fail(state, `Array too large to decode: ${sizeBigint}`);
        }

        const size = Number(sizeBigint);
        const res: Value[] = [];

        for (let i = 0; i < size; i++) {
            const view = makeMemoryView(this.type.elementT, this.infer, addr);
            res.push(view.decode(state));
            addr += 32n;
        }

        return res;
    }
}

export class StructMemView extends BaseMemoryView<Struct, UserDefinedType> {
    fields: Array<[string, TypeNode]>;

    constructor(
        public readonly type: UserDefinedType,
        public readonly infer: InferType,
        loc: bigint
    ) {
        super(type, infer, loc);

        if (!(type instanceof UserDefinedType && type.definition instanceof StructDefinition)) {
            this.fail(new Uint8Array(), `Invalid type ${type.pp()} for StructMemView`);
        }

        this.fields = type.definition.vMembers.map((decl) => {
            return [
                decl.name,
                infer.typeNameToSpecializedTypeNode(decl.vType as TypeName, DataLocation.Memory)
            ];
        });
    }

    decode(state: Memory): Struct {
        const entries: Array<[string, Value]> = [];

        let offset = this.loc;

        for (const [name, type] of this.fields) {
            const view = makeMemoryView(type, this.infer, offset);
            entries.push([name, view.decode(state)]);
            offset += 32n;
        }

        return new Struct(entries);
    }
}

export class PointerMemView extends BaseMemoryView<Value, PointerType> {
    decode(state: Memory): Value {
        const offset = this.decodeIntAt(this.loc, uint256, state);
        const view = makeMemoryView(this.type.to, this.infer, offset);
        return view.decode(state);
    }
}

export function makeMemoryView(
    type: TypeNode,
    infer: InferType,
    loc: bigint
): BaseMemoryView<Value, TypeNode> {
    if (type instanceof IntType) {
        return new IntMemView(type, infer, loc);
    }

    if (type instanceof BoolType) {
        return new BoolMemView(type, infer, loc);
    }

    if (type instanceof AddressType) {
        return new AddressMemView(type, infer, loc);
    }

    if (type instanceof FixedBytesType) {
        return new FixedBytesMemView(type, infer, loc);
    }

    if (type instanceof UserDefinedType) {
        const def = type.definition;
        if (def instanceof EnumDefinition) {
            return new EnumMemView(type, infer, loc);
        }

        if (def instanceof ContractDefinition) {
            return new ContractMemView(type, infer, loc);
        }

        if (def instanceof UserDefinedValueTypeDefinition) {
            const innerT = infer.typeNameToTypeNode(def.underlyingType);
            return makeMemoryView(innerT, infer, loc);
        }

        if (def instanceof StructDefinition) {
            return new StructMemView(type, infer, loc);
        }
    }

    if (type instanceof BytesType) {
        return new BytesMemView(type, infer, loc);
    }

    if (type instanceof StringType) {
        return new StringMemView(type, infer, loc);
    }

    if (type instanceof ArrayType) {
        return new ArrayMemView(type, infer, loc);
    }

    if (type instanceof PointerType) {
        return new PointerMemView(type, infer, loc);
    }

    nyi(`makeMemoryView(${type.pp()})`);
}
