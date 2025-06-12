import {
    ArrayType,
    assert,
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
 * Convert any `UserDefinedType(StructDefinition)` to an `ExpStructType`
 * in the given rawT
 * @param rawT
 */
export function expandType(
    rawT: TypeNode,
    infer: InferType,
    loc: DataLocation | undefined
): TypeNode {
    if (rawT instanceof ArrayType) {
        const expElT = expandType(rawT.elementT, infer, loc);

        return expElT === rawT.elementT ? rawT : new ArrayType(expElT, rawT.size, rawT.src);
    }

    if (rawT instanceof MappingType) {
        const keyT = expandType(rawT.keyType, infer, loc);
        const valueT = expandType(rawT.valueType, infer, loc);

        return keyT === rawT.keyType && valueT === rawT.valueType
            ? rawT
            : new MappingType(keyT, valueT, rawT.src);
    }

    if (rawT instanceof TupleType) {
        return new TupleType(
            rawT.elements.map((elT) => (elT === null ? elT : expandType(elT, infer, loc)))
        );
    }

    if (rawT instanceof PointerType) {
        const toT = expandType(rawT.to, infer, rawT.location);

        return toT === rawT.to ? rawT : new PointerType(toT, rawT.location, rawT.kind, rawT.src);
    }

    if (rawT instanceof UserDefinedType && rawT.definition instanceof StructDefinition) {
        assert(loc !== undefined, `Missing location in struct expansion {0}`, rawT);
        const fields: Array<[string, TypeNode]> = rawT.definition.vMembers.map((decl) => [
            decl.name,
            infer.typeNameToSpecializedTypeNode(decl, loc)
        ]);

        return new ExpStructType(rawT.name, fields, rawT);
    }

    if (
        rawT instanceof UserDefinedType &&
        rawT.definition instanceof UserDefinedValueTypeDefinition
    ) {
        return expandType(infer.typeNameToTypeNode(rawT.definition.underlyingType), infer, loc);
    }

    return rawT;
}
