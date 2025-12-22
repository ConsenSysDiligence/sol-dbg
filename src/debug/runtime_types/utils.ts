import * as sol from "solc-typed-ast";
import * as rtt from "./ast";
import { nyi } from "../../utils";

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
        return new rtt.TupleType(rawT.components.map((elT) => typeIdToRuntimeType(elT, ctx, loc)));
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
            return new rtt.MissingTypeDef(rawT);
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
            return new rtt.MissingTypeDef(rawT);
        }

        return typeIdToRuntimeType(sol.typeOf(def.underlyingType), ctx, loc);
    }

    if (rawT instanceof sol.StructTypeId) {
        const def = ctx.locate(rawT.id);

        if (!(def instanceof sol.StructDefinition)) {
            return new rtt.MissingTypeDef(rawT);
        }

        sol.assert(loc !== undefined, `Missing location in struct expansion {0}`, rawT);
        const fields: Array<[string, rtt.BaseRuntimeType]> = def.vMembers.map((decl) => [
            decl.name,
            typeIdToRuntimeType(sol.changeLocationTo(sol.typeOf(decl), loc), ctx, loc)
        ]);

        return new rtt.StructType(rawT.name, fields);
    }

    if (rawT instanceof sol.FunctionTypeId) {
        return new rtt.FunctionType(rawT);
    }

    nyi(`typeIdToRuntimeType(${rawT.constructor.name})`);
}
