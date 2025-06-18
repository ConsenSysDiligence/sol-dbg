import {
    ArrayType,
    assert,
    ContractDefinition,
    DataLocation,
    EnumDefinition,
    enumToIntType,
    InferType,
    MappingType,
    Mutability,
    PointerType,
    StructDefinition,
    TupleType,
    TypeName,
    TypeNode,
    UserDefinedType,
    UserDefinedTypeName,
    UserDefinedValueTypeDefinition,
    VariableDeclaration
} from "solc-typed-ast";
import { address } from "../../utils";
import { isTypeStringStatic32BytesInStorage } from "./utils";

/**
 * An internal struct type that converts all field VariableDeclaration(s) to
 * TypeNode(s). This way handling this type doesn't require an InferType
 * instance.
 */
export class ExpStructType extends TypeNode {
    constructor(
        public readonly name: string,
        public readonly fields: Array<[string, TypeNode]>,
        public readonly rawType: TypeNode | undefined = undefined
    ) {
        super();
    }

    pp(): string {
        return `struct ${this.name}{\n${this.fields.map(([name, type]) => `${name}: ${type.pp()}`).join("\n")}\n}`;
    }
}

function isTypeUnknownContract(t: TypeName | undefined): boolean {
    return (
        t instanceof UserDefinedTypeName &&
        t.referencedDeclaration < 0 &&
        (t.typeString.startsWith("contract ") ||
            t.typeString.startsWith("interface ") ||
            t.typeString.startsWith("library "))
    );
}

/**
 * Helper for converting `TypeName`s to `TypeNode`s. In some cases when solc-typed-ast conversion fails,
 * it can try and guess the correct simplified type from the typeString
 *
 * - unknown contracts - retun address
 * - @todo unknown enum - optimistically guess uint8 if we are in version @todo
 */
function typeNameToTypeNode(t: TypeName, infer: InferType, loc?: DataLocation): TypeNode {
    try {
        return loc ? infer.typeNameToSpecializedTypeNode(t, loc) : infer.typeNameToTypeNode(t);
    } catch (e) {
        if (isTypeUnknownContract(t)) {
            return address;
        }

        throw e;
    }
}

/**
 * Helper for converting `VariableDeclartaion`s to `TypeNode`s. In some cases when solc-typed-ast conversion fails,
 * it can try and guess the correct simplified type from the typeString
 *
 * - unknown contracts - retun address
 * - @todo unknown enum - optimistically guess uint8 if we are in version @todo
 */
function variableDeclarationToTypeNode(v: VariableDeclaration, infer: InferType): TypeNode {
    try {
        return infer.variableDeclarationToTypeNode(v);
    } catch (e) {
        if (v.vType && isTypeUnknownContract(v.vType)) {
            return address;
        }

        throw e;
    }
}

/**
 * Simplify the given solc-typed-ast types for use in encoding. This does the following conversions:
 *
 * - Convert `UserDefinedType(StructDefinition)` to `ExpStructType`
 * - Convert `UserDefinedType(UserDefinedValueTypeDefinition)` to the underlying type
 * - Convert `UserDefinedType(ContractDefinition)` to address
 *
 * in the given rawT
 * @param rawT
 */
export function simplifyType(
    rawT: TypeNode,
    infer: InferType,
    loc: DataLocation | undefined
): TypeNode {
    if (rawT instanceof ArrayType) {
        const expElT = simplifyType(rawT.elementT, infer, loc);

        return expElT === rawT.elementT ? rawT : new ArrayType(expElT, rawT.size, rawT.src);
    }

    if (rawT instanceof MappingType) {
        const keyT = simplifyType(rawT.keyType, infer, loc);
        const valueT = simplifyType(rawT.valueType, infer, loc);

        return keyT === rawT.keyType && valueT === rawT.valueType
            ? rawT
            : new MappingType(keyT, valueT, rawT.src);
    }

    if (rawT instanceof TupleType) {
        return new TupleType(
            rawT.elements.map((elT) => (elT === null ? elT : simplifyType(elT, infer, loc)))
        );
    }

    if (rawT instanceof PointerType) {
        const toT = simplifyType(rawT.to, infer, rawT.location);

        return toT === rawT.to ? rawT : new PointerType(toT, rawT.location, rawT.kind, rawT.src);
    }

    if (rawT instanceof UserDefinedType) {
        if (rawT.definition === undefined) {
            return new MissingType(undefined);
        }

        if (rawT.definition instanceof StructDefinition) {
            assert(loc !== undefined, `Missing location in struct expansion {0}`, rawT);
            const fields: Array<[string, TypeNode]> = rawT.definition.vMembers.map((decl) => {
                let fieldT: TypeNode;
                try {
                    fieldT = typeNameToTypeNode(decl.vType as TypeName, infer, loc);
                } catch (e) {
                    fieldT = new MissingType(decl.vType);
                }

                return [decl.name, simplifyType(fieldT, infer, loc)];
            });

            return new ExpStructType(rawT.name, fields, rawT);
        }

        if (rawT.definition instanceof UserDefinedValueTypeDefinition) {
            let underlyingType: TypeNode;
            try {
                underlyingType = typeNameToTypeNode(rawT.definition.underlyingType, infer);
            } catch (e) {
                underlyingType = new MissingType(rawT.definition.underlyingType);
            }

            return simplifyType(underlyingType, infer, loc);
        }

        if (rawT.definition instanceof ContractDefinition) {
            return address;
        }

        if (rawT.definition instanceof EnumDefinition) {
            return enumToIntType(rawT.definition);
        }
    }

    if (rawT instanceof MissingType) {
        return rawT;
    }
    return rawT;
}

export class MissingType extends TypeNode {
    constructor(public readonly rawTypeName: TypeName | undefined) {
        super();
    }

    pp(): string {
        return `<missing type info for ${this.rawTypeName?.print()}>`;
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
 * @todo does this belong in storage decoding?
 * @param def
 * @param infer
 */
export function getContractLayoutType(
    contract: ContractDefinition,
    infer: InferType
): [ExpStructType, boolean] {
    const stateVars: Array<[string, TypeNode]> = [];
    let complete = true;

    for (const base of [...contract.vLinearizedBaseContracts].reverse()) {
        if (base === null || base === undefined) {
            complete = false;
            break;
        }

        for (const varDecl of base.vStateVariables) {
            // Not part of layout
            if (
                varDecl.mutability === Mutability.Constant ||
                varDecl.mutability === Mutability.Immutable ||
                varDecl.storageLocation === DataLocation.Transient
            ) {
                continue;
            }

            let typeNode: TypeNode;

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
                    typeNode = new MissingType(varDecl.vType);
                } else {
                    break;
                }
            }

            stateVars.push([varDecl.name, simplifyType(typeNode, infer, DataLocation.Storage)]);
        }
    }

    return [new ExpStructType(contract.name, stateVars), complete];
}
