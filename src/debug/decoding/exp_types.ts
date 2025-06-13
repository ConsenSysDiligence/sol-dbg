import {
    ArrayType,
    assert,
    ContractDefinition,
    DataLocation,
    InferType,
    MappingType,
    PointerType,
    StructDefinition,
    TupleType,
    TypeNode,
    UserDefinedType,
    UserDefinedValueTypeDefinition
} from "solc-typed-ast";
import { address } from "../../utils";

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
        return `<internal struct ${this.name}>`;
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
        if (rawT.definition instanceof StructDefinition) {
            assert(loc !== undefined, `Missing location in struct expansion {0}`, rawT);
            const fields: Array<[string, TypeNode]> = rawT.definition.vMembers.map((decl) => [
                decl.name,
                infer.typeNameToSpecializedTypeNode(decl, loc)
            ]);

            return new ExpStructType(rawT.name, fields, rawT);
        }

        if (rawT.definition instanceof UserDefinedValueTypeDefinition) {
            return simplifyType(
                infer.typeNameToTypeNode(rawT.definition.underlyingType),
                infer,
                loc
            );
        }

        if (rawT.definition instanceof ContractDefinition) {
            return address;
        }
    }

    return rawT;
}
