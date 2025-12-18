import * as sol from "solc-typed-ast";
import * as rtt from "./ast";
import { isTypeUnknownContract, nyi } from "../../utils";

const addressT = new rtt.AddressType();
const bytesT = new rtt.BytesType();
const stringT = new rtt.StringType();
const boolT = new rtt.BoolType();

/**
 * Convert the given solc-typed-ast typeIdentifier to a runtime types. This does the following conversions:
 *
 * @param rawT
 */
export function typeIdToRuntimeType(
    rawT: sol.TypeIdentifier,
    ctx: sol.ASTContext,
    loc?: sol.DataLocation
): rtt.BaseRuntimeType {
    if (rawT instanceof sol.IntTypeId) {
        return new rtt.IntType(rawT.numBits, rawT.isSigned);
    }

    if (rawT instanceof sol.FixedBytesTypeId) {
        return new rtt.FixedBytesType(rawT.numBytes);
    }

    if (rawT instanceof sol.BoolTypeId) {
        return boolT;
    }

    if (rawT instanceof sol.AddressTypeId) {
        return addressT;
    }

    if (rawT instanceof sol.BytesTypeId) {
        return bytesT;
    }

    if (rawT instanceof sol.StringTypeId) {
        return stringT;
    }

    if (rawT instanceof sol.ArrayTypeId) {
        const expElT = typeIdToRuntimeType(rawT.elT, ctx, loc);

        return new rtt.ArrayType(expElT, rawT.size);
    }

    if (rawT instanceof sol.MappingTypeId) {
        const keyT = typeIdToRuntimeType(rawT.keyType, ctx, loc);
        const valueT = typeIdToRuntimeType(rawT.valueType, ctx, loc);

        return new rtt.MappingType(keyT, valueT);
    }

    if (rawT instanceof sol.TupleTypeId) {
        return new rtt.TupleType(
            rawT.components.map((elT) =>
                elT === null ? new rtt.MissingType(undefined) : typeIdToRuntimeType(elT, ctx, loc)
            )
        );
    }

    if (rawT instanceof sol.PointerTypeId) {
        sol.assert(rawT.location !== sol.DataLocation.Default, `Unexpected default location`);
        const toT = typeIdToRuntimeType(rawT.toType, ctx, rawT.location);
        return new rtt.PointerType(toT, rawT.location);
    }

    if (rawT instanceof sol.ContractTypeId) {
        return addressT;
    }

    if (rawT instanceof sol.EnumTypeId) {
        const def = ctx.locate(rawT.id);

        if (!(def instanceof sol.EnumDefinition)) {
            return new rtt.MissingType(`No EnumDef found for ${rawT.pp()}`);
        }

        return typeIdToRuntimeType(sol.enumToIntTypeId(def), ctx, loc);
    }

    if (rawT instanceof sol.UserDefinedValueTypeId) {
        const def = ctx.locate(rawT.id);

        if (
            !(
                def instanceof sol.UserDefinedValueTypeDefinition &&
                def.underlyingType.typeIdentifier !== undefined
            )
        ) {
            return new rtt.MissingType(`No EnumDef found for ${rawT.pp()}`);
        }

        return typeIdToRuntimeType(sol.typeOf(def.underlyingType), ctx, loc);
    }

    if (rawT instanceof sol.StructTypeId) {
        const def = ctx.locate(rawT.id);

        if (!(def instanceof sol.StructDefinition)) {
            return new rtt.MissingType(`No EnumDef found for ${rawT.pp()}`);
        }

        sol.assert(loc !== undefined, `Missing location in struct expansion {0}`, rawT);
        const fields: Array<[string, rtt.BaseRuntimeType]> = def.vMembers.map((decl) => [
            decl.name,
            typeIdToRuntimeType(sol.changeLocationTo(sol.typeOf(decl), loc), ctx, loc)
        ]);

        return new rtt.StructType(rawT.name, fields);
    }

    nyi(`typeIdToRuntimeType(${rawT.constructor.name})`);
}

/**
 * Helper for converting `TypeName`s to `TypeNode`s. In some cases when solc-typed-ast conversion fails,
 * it can try and guess the correct simplified type from the typeString
 *
 * - unknown contracts - retun address
 */
function typeNameToTypeNode(
    t: sol.TypeName,
    infer: sol.InferType,
    loc?: sol.DataLocation
): sol.TypeNode {
    try {
        return loc ? infer.typeNameToSpecializedTypeNode(t, loc) : infer.typeNameToTypeNode(t);
    } catch (e) {
        if (isTypeUnknownContract(t)) {
            return new sol.AddressType(false);
        }

        throw e;
    }
}

/**
 * Convert the given solc-typed-ast type to a runtime types. This does the following conversions:
 *
 * - Convert `UserDefinedType(StructDefinition)` to `ExpStructType`
 * - Convert `UserDefinedType(UserDefinedValueTypeDefinition)` to the underlying type
 * - Convert `UserDefinedType(ContractDefinition)` to address
 *
 * @param rawT
 */
export function astToRuntimeType(
    rawT: sol.TypeNode,
    infer: sol.InferType,
    loc: sol.DataLocation | undefined = undefined
): rtt.BaseRuntimeType {
    if (rawT instanceof sol.IntType) {
        return new rtt.IntType(rawT.nBits, rawT.signed);
    }

    if (rawT instanceof sol.FixedBytesType) {
        return new rtt.FixedBytesType(rawT.size);
    }

    if (rawT instanceof sol.BoolType) {
        return new rtt.BoolType();
    }

    if (rawT instanceof sol.AddressType) {
        return new rtt.AddressType();
    }

    if (rawT instanceof sol.BytesType) {
        return new rtt.BytesType();
    }

    if (rawT instanceof sol.StringType) {
        return new rtt.StringType();
    }

    if (rawT instanceof sol.ArrayType) {
        const expElT = astToRuntimeType(rawT.elementT, infer, loc);

        return new rtt.ArrayType(expElT, rawT.size);
    }

    if (rawT instanceof sol.MappingType) {
        const keyT = astToRuntimeType(rawT.keyType, infer, loc);
        const valueT = astToRuntimeType(rawT.valueType, infer, loc);

        return new rtt.MappingType(keyT, valueT);
    }

    if (rawT instanceof sol.TupleType) {
        return new rtt.TupleType(
            rawT.elements.map((elT) =>
                elT === null ? new rtt.MissingType(undefined) : astToRuntimeType(elT, infer, loc)
            )
        );
    }

    if (rawT instanceof sol.PointerType) {
        const ptrLoc = rawT.location === sol.DataLocation.Default ? loc : rawT.location;
        sol.assert(ptrLoc !== undefined, `Missing location in conversion of {0}`, rawT);

        const toT = astToRuntimeType(rawT.to, infer, ptrLoc);

        return new rtt.PointerType(toT, ptrLoc);
    }

    if (rawT instanceof sol.UserDefinedType) {
        if (rawT.definition === undefined) {
            return new rtt.MissingType(undefined);
        }

        if (rawT.definition instanceof sol.StructDefinition) {
            sol.assert(loc !== undefined, `Missing location in struct expansion {0}`, rawT);
            const fields: Array<[string, rtt.BaseRuntimeType]> = rawT.definition.vMembers.map(
                (decl) => {
                    let fieldT: sol.TypeNode;
                    try {
                        fieldT = typeNameToTypeNode(decl.vType as sol.TypeName, infer, loc);
                    } catch (e) {
                        return [
                            decl.name,
                            new rtt.MissingType(
                                decl.vType !== undefined ? decl.vType.typeString : undefined
                            )
                        ];
                    }

                    return [decl.name, astToRuntimeType(fieldT, infer, loc)];
                }
            );

            return new rtt.StructType(rawT.name, fields);
        }

        if (rawT.definition instanceof sol.UserDefinedValueTypeDefinition) {
            let underlyingType: sol.TypeNode;
            try {
                underlyingType = typeNameToTypeNode(rawT.definition.underlyingType, infer);
            } catch (e) {
                return new rtt.MissingType(rawT.definition.underlyingType.typeString);
            }

            return astToRuntimeType(underlyingType, infer, loc);
        }

        if (rawT.definition instanceof sol.ContractDefinition) {
            return new rtt.AddressType();
        }

        if (rawT.definition instanceof sol.EnumDefinition) {
            return astToRuntimeType(sol.enumToIntType(rawT.definition), infer);
        }
    }

    if (rawT instanceof sol.TypeNameType) {
        return new rtt.TypeType(rawT.type);
    }

    nyi(`Type ${rawT.constructor.name}`);
}

/**
 * Given a general runtime type 'pattern' that doesn't contain any data locations, and a data location,
 * produce a concrete instance of the general type for the target location.
 * This is the inverse of `specializeType()`
 *
 * @param type - general type "pattern"
 * @param loc - target location to specialize to
 * @returns specialized type
 */
export function specializeType(
    type: rtt.BaseRuntimeType,
    loc: sol.DataLocation
): rtt.BaseRuntimeType {
    sol.assert(
        !(type instanceof rtt.PointerType),
        "Unexpected pointer type {0} in concretization.",
        type
    );
    sol.assert(
        !(type instanceof rtt.TupleType),
        "Unexpected tuple type {0} in concretization.",
        type
    );

    // bytes and string
    if (type instanceof rtt.StringType || type instanceof rtt.BytesType) {
        return new rtt.PointerType(type, loc);
    }

    if (type instanceof rtt.ArrayType) {
        const concreteElT = specializeType(type.elementT, loc);

        return new rtt.PointerType(new rtt.ArrayType(concreteElT, type.size), loc);
    }

    if (type instanceof rtt.StructType) {
        return new rtt.PointerType(type, loc);
    }

    if (type instanceof rtt.MappingType) {
        // Always treat map keys as in-memory copies
        const concreteKeyT = specializeType(type.keyType, sol.DataLocation.Memory);
        // The result of map indexing is always a pointer to a value that lives in storage
        const concreteValueT = specializeType(type.valueType, sol.DataLocation.Storage); // @todo update when maps supported in transient
        // Maps always live in storage
        return new rtt.PointerType(
            new rtt.MappingType(concreteKeyT, concreteValueT),
            sol.DataLocation.Storage
        ); // @todo update when maps supported in transient
    }

    if (type instanceof rtt.TupleType) {
        return new rtt.TupleType(type.elementTypes.map((elT) => specializeType(elT, loc)));
    }

    return type;
}

/**
 * Given a `BaseRuntimeType` `type` that is specialized to some storage location,
 * compute the original 'general' type that is independent of location.
 * This is the inverse of `specializeType()`
 *
 * @param type - specialized type
 * @returns computed generalized type
 */
export function generalizeType(type: rtt.BaseRuntimeType): rtt.BaseRuntimeType {
    if (type instanceof rtt.PointerType) {
        return generalizeType(type.toType);
    }

    if (type instanceof rtt.ArrayType) {
        const innerT = generalizeType(type.elementT);

        return new rtt.ArrayType(innerT, type.size);
    }

    if (type instanceof rtt.MappingType) {
        const genearlKeyT = generalizeType(type.keyType);
        const generalValueT = generalizeType(type.valueType);

        return new rtt.MappingType(genearlKeyT, generalValueT);
    }

    if (type instanceof rtt.StructType) {
        return new rtt.StructType(
            type.name,
            type.fields.map(([fieldName, fieldT]) => [fieldName, generalizeType(fieldT)])
        );
    }

    if (type instanceof rtt.TupleType) {
        return new rtt.TupleType(type.elementTypes.map(generalizeType));
    }

    return type;
}
