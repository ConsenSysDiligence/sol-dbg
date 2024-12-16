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
