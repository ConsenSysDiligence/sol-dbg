import {
    AddressType,
    BoolType,
    EnumDefinition,
    enumToIntType,
    FixedBytesType,
    InferType,
    IntType,
    PointerType,
    TypeNode,
    UserDefinedType,
    UserDefinedValueTypeDefinition
} from "solc-typed-ast";

/**
 * Compute the 'static' size that a value of type `typ` would take up in
 * calldata in bytes.
 */
export function cd_typeStaticSize(typ: TypeNode, infer: InferType): number {
    if (typ instanceof IntType) {
        return typ.nBits / 8;
    }

    if (typ instanceof FixedBytesType) {
        return typ.size;
    }

    if (typ instanceof BoolType) {
        return 1;
    }

    if (typ instanceof AddressType) {
        return 20;
    }

    if (typ instanceof UserDefinedType) {
        if (typ.definition instanceof EnumDefinition) {
            return enumToIntType(typ.definition).nBits / 8;
        }

        if (typ.definition instanceof UserDefinedValueTypeDefinition) {
            return cd_typeStaticSize(
                infer.typeNameToTypeNode(typ.definition.underlyingType),
                infer
            );
        }
    }

    if (typ instanceof PointerType) {
        return 32;
    }

    throw new Error(`NYI typStaticStorSize(${typ.pp()})`);
}
