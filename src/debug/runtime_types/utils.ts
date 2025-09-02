import * as sol from "solc-typed-ast";
import * as rtt from "./ast";
import { isTypeStringStatic32BytesInStorage } from "../decoding/utils";

function isTypeUnknownContract(t: sol.TypeName | undefined): boolean {
    return (
        t instanceof sol.UserDefinedTypeName &&
        t.referencedDeclaration < 0 &&
        (t.typeString.startsWith("contract ") ||
            t.typeString.startsWith("interface ") ||
            t.typeString.startsWith("library "))
    );
}

const addressT = new sol.AddressType(false);

const missingT = new rtt.MissingType();

/**
 * Helper for converting `TypeName`s to `TypeNode`s. In some cases when solc-typed-ast conversion fails,
 * it can try and guess the correct simplified type from the typeString
 *
 * - unknown contracts - retun address
 */
function typeNameToTypeNode(t: sol.TypeName, infer: sol.InferType, loc?: sol.DataLocation): sol.TypeNode {
    try {
        return loc ? infer.typeNameToSpecializedTypeNode(t, loc) : infer.typeNameToTypeNode(t);
    } catch (e) {
        if (isTypeUnknownContract(t)) {
            return addressT;
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
            rawT.elements.map((elT) => (elT === null ? missingT : astToRuntimeType(elT, infer, loc)))
        );
    }

    if (rawT instanceof sol.PointerType) {
        const toT = astToRuntimeType(rawT.to, infer, rawT.location);

        return new rtt.PointerType(toT, rawT.location);
    }

    if (rawT instanceof sol.UserDefinedType) {
        if (rawT.definition === undefined) {
            return missingT;
        }

        if (rawT.definition instanceof sol.StructDefinition) {
            sol.assert(loc !== undefined, `Missing location in struct expansion {0}`, rawT);
            const fields: Array<[string, rtt.BaseRuntimeType]> = rawT.definition.vMembers.map((decl) => {
                let fieldT: sol.TypeNode;
                try {
                    fieldT = typeNameToTypeNode(decl.vType as sol.TypeName, infer, loc);
                } catch (e) {
                    return [decl.name, missingT];
                }

                return [decl.name, astToRuntimeType(fieldT, infer, loc)];
            });

            return new rtt.StructType(rawT.name, fields);
        }

        if (rawT.definition instanceof sol.UserDefinedValueTypeDefinition) {
            let underlyingType: sol.TypeNode;
            try {
                underlyingType = typeNameToTypeNode(rawT.definition.underlyingType, infer);
            } catch (e) {
                return missingT;
            }

            return astToRuntimeType(underlyingType, infer, loc);
        }

        if (rawT.definition instanceof sol.ContractDefinition) {
            return addressT;
        }

        if (rawT.definition instanceof sol.EnumDefinition) {
            return sol.enumToIntType(rawT.definition);
        }
    }

    return rawT;
}

/**
 * Helper for converting `VariableDeclartaion`s to `TypeNode`s. In some cases when solc-typed-ast conversion fails,
 * it can try and guess the correct simplified type from the typeString
 *
 * - unknown contracts - retun address
 */
function variableDeclarationToTypeNode(v: sol.VariableDeclaration, infer: sol.InferType): sol.TypeNode {
    try {
        return infer.variableDeclarationToTypeNode(v);
    } catch (e) {
        if (v.vType && isTypeUnknownContract(v.vType)) {
            return addressT;
        }

        throw e;
    }
}

/**
 * Given a `ContractDefinition` try and compute an `ExpStructType` struct that
 * describes the layout of the class.  This takes into account all base classes,
 * and simplifies types using `simplifyType`.
 *
 * Since we may be missing AST information for some user-defined types, or even
 * entire bases, the layout may be partial. It may be only up to a given base,
 * and it may be missing exact type information for certain fields.
 *
 * We return a tuple with the resulting layout, and a boolean specifying whether
 * the layout is complete.
 *
 * @param def
 * @param infer
 */
export function getContractLayoutType(
    contract: sol.ContractDefinition,
    infer: sol.InferType
): [rtt.StructType, boolean] {
    const stateVars: Array<[string, rtt.BaseRuntimeType]> = [];
    let complete = true;

    for (const base of [...contract.vLinearizedBaseContracts].reverse()) {
        if (base === null || base === undefined) {
            complete = false;
            break;
        }

        for (const varDecl of base.vStateVariables) {
            // Not part of layout
            if (
                varDecl.mutability === sol.Mutability.Constant ||
                varDecl.mutability === sol.Mutability.Immutable ||
                varDecl.storageLocation === sol.DataLocation.Transient
            ) {
                continue;
            }

            let typeNode: sol.TypeNode;

            try {
                typeNode = variableDeclarationToTypeNode(varDecl, infer);
            } catch (e) {
                /**
                 * Missing type info. If this is a:
                 *  - map type
                 *  - array type
                 *
                 * then we can continue decoding as it takes exactly 32 bytes
                 * statically in the layout. Otherwise we have to abort decoding
                 */
                complete = false;
                if (isTypeStringStatic32BytesInStorage(varDecl.typeString)) {
                    stateVars.push([varDecl.name, missingT]);
                    continue;
                } else {
                    break;
                }
            }

            stateVars.push([varDecl.name, astToRuntimeType(typeNode, infer, sol.DataLocation.Storage)]);
        }
    }

    return [new rtt.StructType(contract.name, stateVars), complete];
}
