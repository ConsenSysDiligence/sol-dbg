import * as sol from "solc-typed-ast";
import { nyi } from "./misc";

/**
 * Returns the receive function for a contract (if any). Note that it may be defined on a base class
 */
export function findReceiveFun(
    contract: sol.ContractDefinition
): sol.FunctionDefinition | undefined {
    for (const base of contract.vLinearizedBaseContracts) {
        for (const fun of base.vFunctions) {
            if (fun.kind === sol.FunctionKind.Receive) {
                return fun;
            }
        }
    }

    return undefined;
}

/**
 * Returns the fallback function for a contract (if any). Note that it may be defined on a base class
 */
export function findFallbackFun(
    contract: sol.ContractDefinition
): sol.FunctionDefinition | undefined {
    for (const base of contract.vLinearizedBaseContracts) {
        for (const fun of base.vFunctions) {
            if (fun.kind === sol.FunctionKind.Fallback) {
                return fun;
            }
        }
    }

    return undefined;
}

export function findContractDef(
    units: sol.SourceUnit[],
    fileName: string,
    contractName: string
): sol.ContractDefinition | undefined {
    for (const unit of units) {
        if (unit.sourceEntryKey !== fileName) {
            continue;
        }

        for (const contract of unit.vContracts) {
            if (contract.name === contractName) {
                return contract;
            }
        }
    }

    return undefined;
}

export function resolveConstructor(
    contract: sol.ContractDefinition
): sol.FunctionDefinition | undefined {
    for (const contr of contract.vLinearizedBaseContracts) {
        if (contr.vConstructor) {
            return contr.vConstructor;
        }
    }

    return undefined;
}

export function isTypePrimitive(t: sol.TypeNode): boolean {
    if (
        t instanceof sol.IntType ||
        t instanceof sol.BoolType ||
        t instanceof sol.AddressType ||
        t instanceof sol.FixedBytesType ||
        t instanceof sol.FunctionType
    ) {
        return true;
    }

    if (t instanceof sol.PointerType) {
        return false;
    }

    nyi(`isPrimitive(${t.pp()})`);
}

export function elType(t: sol.ArrayType | sol.PackedArrayType): sol.TypeNode {
    return t instanceof sol.PackedArrayType ? sol.types.byte : t.elementT;
}

export function changeToLocation(typ: sol.TypeNode, newLoc: sol.DataLocation): sol.TypeNode {
    if (typ instanceof sol.PointerType) {
        return new sol.PointerType(changeToLocation(typ.to, newLoc), newLoc, typ.kind);
    }

    if (typ instanceof sol.ArrayType) {
        return new sol.ArrayType(changeToLocation(typ.elementT, newLoc), typ.size);
    }

    if (typ instanceof sol.MappingType) {
        sol.assert(
            newLoc === sol.DataLocation.Storage,
            `Cannot change type of mapping ${typ.pp()} to ${newLoc}`
        );

        return typ;
    }

    if (typ instanceof sol.TupleType) {
        return new sol.TupleType(
            typ.elements.map((elT: sol.TypeNode | null) =>
                changeToLocation(elT as sol.TypeNode, newLoc)
            )
        );
    }

    if (
        typ instanceof sol.IntType ||
        typ instanceof sol.BoolType ||
        typ instanceof sol.AddressType ||
        typ instanceof sol.FixedBytesType ||
        typ instanceof sol.StringType ||
        typ instanceof sol.BytesType ||
        typ instanceof sol.UserDefinedType
    ) {
        return typ;
    }

    throw new Error(`Cannot change location of type ${typ.pp()}`);
}

/**
 * Return the static size that the type `typ` will take in the standard ABI encoding of
 * arguments.
 */
export function abiStaticTypeSize(typ: sol.TypeNode): number {
    if (
        typ instanceof sol.IntType ||
        typ instanceof sol.AddressType ||
        typ instanceof sol.FixedBytesType ||
        typ instanceof sol.BoolType ||
        typ instanceof sol.PointerType
    ) {
        return 32;
    }

    if (typ instanceof sol.UserDefinedType) {
        const def = typ.definition;

        if (
            def instanceof sol.EnumDefinition ||
            def instanceof sol.ContractDefinition ||
            def instanceof sol.UserDefinedValueTypeDefinition
        ) {
            return 32;
        }

        throw new Error(`NYI decoding user-defined type ${typ.pp()}`);
    }

    if (typ instanceof sol.TupleType) {
        let res = 0;

        for (const elT of typ.elements) {
            sol.assert(elT !== null, ``);
            res += abiStaticTypeSize(elT);
        }

        return res;
    }

    throw new Error(`NYI decoding type ${typ.pp()}`);
}

export function isABITypeStaticSized(type: sol.TypeNode): boolean {
    if (type instanceof sol.ArrayType) {
        return type.size !== undefined && isABITypeStaticSized(type.elementT);
    }

    if (type instanceof sol.PointerType) {
        return isABITypeStaticSized(type.to);
    }

    if (type instanceof sol.TupleType) {
        for (const elT of type.elements) {
            sol.assert(elT !== null, ``);
            if (!isABITypeStaticSized(elT)) {
                return false;
            }
        }

        return true;
    }

    if (type instanceof sol.StringType || type instanceof sol.BytesType) {
        return false;
    }

    if (
        type instanceof sol.IntType ||
        type instanceof sol.AddressType ||
        type instanceof sol.BoolType ||
        type instanceof sol.FixedBytesType
    ) {
        return true;
    }

    throw new Error(`NYI isABITypeStaticSized(${type.pp()})`);
}
