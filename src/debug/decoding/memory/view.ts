import { ArrayType, PackedArrayType, TypeNode, UserDefinedType } from "solc-typed-ast";
import { Memory } from "../../types";
import { ArrayView, BaseAddressView, StructView } from "../view";

// Generic Memory Classes
export abstract class BaseMemoryView<T extends TypeNode = TypeNode> extends BaseAddressView<T> {}

export abstract class BaseMemoryArrayView<T extends PackedArrayType | ArrayType>
    extends BaseMemoryView<T>
    implements ArrayView
{
    abstract decodeLen(storage: Memory): bigint;
    abstract decodeIdx(storage: Memory, idx: bigint): BaseMemoryView;
}

export abstract class BaseMemoryStructView
    extends BaseMemoryView<UserDefinedType>
    implements StructView
{
    abstract decodeField(arg: any, field: string): BaseMemoryView;
}
