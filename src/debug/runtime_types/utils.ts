import * as sol from "solc-typed-ast";
import * as rtt from "./ast";
import { isTypeUnknownContract, nyi } from "../../utils";

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
        const toT = astToRuntimeType(rawT.to, infer, rawT.location);

        return new rtt.PointerType(toT, rawT.location);
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

    nyi(`Type ${rawT.constructor.name}`);
}
