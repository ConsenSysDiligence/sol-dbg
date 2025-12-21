import * as sol from "solc-typed-ast";

/**
 * Returns the receive function for a contract (if any). Note that it may be defined on a base class
 */
export function findReceiveFun(
    contract: sol.ContractDefinition
): sol.FunctionDefinition | undefined {
    for (const base of contract.vLinearizedBaseContracts) {
        if (base === undefined) {
            continue;
        }

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
        if (base === undefined) {
            continue;
        }

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

/**
 * Return a list with the names and types of the callable arguments
 * @param nd
 * @returns
 */
export function getArgs(
    nd: sol.FunctionDefinition | sol.VariableDeclaration
): Array<[string, sol.TypeIdentifier]> {
    if (nd instanceof sol.VariableDeclaration) {
        const [argTs] = sol.getterArgsAndReturn(nd);
        return argTs.map((argT, i) => [`ARG_${i}`, argT]);
    }

    return nd.vParameters.vParameters.map((d) => [d.name, sol.typeOf(d)]);
}

/**
 * Return a list with the names and types of the callable returns
 * @param nd
 * @returns
 */
export function getReturns(
    nd: sol.FunctionDefinition | sol.VariableDeclaration
): Array<[string, sol.TypeIdentifier]> {
    if (nd instanceof sol.VariableDeclaration) {
        const [, retT] = sol.getterArgsAndReturn(nd);
        return (retT instanceof sol.TupleTypeId ? retT.components : [retT]).map((argT, i) => [
            `RET_${i}`,
            argT
        ]);
    }

    return nd.vReturnParameters.vParameters.map((d) => [d.name, sol.typeOf(d)]);
}
