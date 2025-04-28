import { AbiCoder } from "@ethersproject/abi";
import { concatBytes, hexToBytes } from "ethereum-cryptography/utils";
import * as sol from "solc-typed-ast";
import { stor_fetchWord, Storage, StorageLocation } from "../debug";
import { bigIntToBigEndianBuf, ZERO_ADDRESS } from "../utils";
import { nyi } from "./exceptions";
import { SolValue } from "./state";

export const coder = new AbiCoder();

export function bytesSplice(target: Uint8Array, newBytes: Uint8Array, off: number): Uint8Array {
    const res = new Uint8Array(target);
    sol.assert(off >= 0 && off + newBytes.length <= res.length, `OoB in bytesSplice`);

    for (let i = 0; i < newBytes.length; i++) {
        res[i + off] = newBytes[i];
    }

    return res;
}

export function stor_assignValue(
    typ: sol.TypeNode,
    loc: StorageLocation,
    value: SolValue,
    storage: Storage
): Storage {
    let val = stor_fetchWord(loc.address, storage);

    if (typ instanceof sol.IntType) {
        sol.assert(typeof value === "bigint", `Unexpected value ${value}} in stor_assignValue`);
        const newBytes = bigIntToBigEndianBuf(value, typ);
        sol.assert(
            newBytes.length <= loc.endOffsetInWord,
            `Misaligned store ${value} of type ${typ.pp()} with bytes ${newBytes.length} assigned ending at off ${loc.endOffsetInWord}`
        );
        val = bytesSplice(val, newBytes, loc.endOffsetInWord - newBytes.length);
    } else {
        nyi(`stor_assignValue(${typ.pp()}, ${loc.address}, ${value})`);
    }

    return storage.set(loc.address, val);
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

    /*
    if (
        t instanceof sol.UserDefinedType &&
        (t.definition instanceof sol.ContractDefinition ||
            t.definition instanceof sol.EnumDefinition)
    ) {
        return true;
    }

    if (
        t instanceof sol.ArrayType ||
        t instanceof sol.MappingType ||
        (t instanceof sol.UserDefinedType && t.definition instanceof sol.StructDefinition)
    ) {
        return false;
    }
    */

    nyi(`isPrimitive(${t.pp()})`);
}

export async function asyncMap<T1, T2>(
    arr: T1[],
    fun: (arg: T1, idx: number) => Promise<T2>
): Promise<T2[]> {
    const res: T2[] = [];

    for (let i = 0; i < arr.length; i++) {
        res.push(await fun(arr[i], i));
    }

    return res;
}

export function encodeCall(
    target: sol.FunctionDefinition | sol.ErrorDefinition,
    args: SolValue[],
    infer: sol.InferType,
    abiVersion: sol.ABIEncoderVersion
): Uint8Array {
    const selector =
        target instanceof sol.FunctionDefinition && target.isConstructor
            ? new Uint8Array()
            : hexToBytes(infer.signatureHash(target));
    sol.assert(selector.length === 4 || selector.length === 0, ``);

    const abiArgTs = target.vParameters.vParameters.map((p) =>
        infer.toABIEncodedType(infer.variableDeclarationToTypeNode(p), abiVersion)
    );

    return concatBytes(
        selector,
        hexToBytes(
            coder.encode(
                abiArgTs.map((t) => t.pp()),
                args
            )
        )
    );
}

export function encodeReturns(
    target: sol.FunctionDefinition,
    rets: SolValue[],
    infer: sol.InferType,
    abiVersion: sol.ABIEncoderVersion
): Uint8Array {
    const abiRetTs = target.vReturnParameters.vParameters.map((p) =>
        infer.toABIEncodedType(infer.variableDeclarationToTypeNode(p), abiVersion)
    );

    return hexToBytes(
        coder.encode(
            abiRetTs.map((t) => t.pp()),
            rets
        )
    );
}

function abiCoderValToSolVal(v: any, typ: sol.TypeNode): SolValue {
    if (typ instanceof sol.IntType) {
        const res = BigInt(v.toString());
        const clamped = sol.clampIntToType(res, typ);
        sol.assert(res === clamped, `Decoded return {0} outside of range for type {1}`, res, typ);

        return res;
    }

    nyi(`Decoding ${v} of type ${typ.pp()}`);
}

export function decodeReturns(
    target: sol.FunctionDefinition,
    data: Uint8Array,
    infer: sol.InferType,
    abiVersion: sol.ABIEncoderVersion
): SolValue[] {
    if (target.vReturnParameters.vParameters.length === 0) {
        return [];
    }

    const abiRetTs = target.vReturnParameters.vParameters.map((p) =>
        infer.toABIEncodedType(infer.variableDeclarationToTypeNode(p), abiVersion)
    );

    return coder
        .decode(
            abiRetTs.map((t) => t.pp()),
            data
        )
        .map((v, i) => abiCoderValToSolVal(v, abiRetTs[i]));
}

export function zeroValue(typ: sol.TypeNode): SolValue {
    if (typ instanceof sol.IntType) {
        return 0n;
    }

    if (typ instanceof sol.BoolType) {
        return false;
    }

    if (typ instanceof sol.AddressType) {
        return ZERO_ADDRESS;
    }

    if (typ instanceof sol.UserDefinedType && typ.definition instanceof sol.ContractDefinition) {
        return ZERO_ADDRESS;
    }

    nyi(`NYI zeroValue(${typ.pp()})`);
}

export function findConstructor(
    contract: sol.ContractDefinition
): sol.FunctionDefinition | undefined {
    for (const base of contract.vLinearizedBaseContracts) {
        if (base.vConstructor) {
            return base.vConstructor;
        }
    }

    return undefined;
}

export function getConstructorABITypes(
    contract: sol.ContractDefinition,
    infer: sol.InferType
): sol.TypeNode[] {
    const constrDef = findConstructor(contract);
    return constrDef
        ? constrDef.vParameters.vParameters.map((param) =>
              infer.variableDeclarationToTypeNode(param)
          )
        : [];
}

export function isCallExternal(call: sol.FunctionCall, infer: sol.InferType): boolean {
    const callee = call.vCallee;

    if (!(callee instanceof sol.MemberAccess)) {
        return false;
    }

    const baseT = infer.typeOf(callee.vExpression);

    return (
        baseT instanceof sol.UserDefinedType && baseT.definition instanceof sol.ContractDefinition
    );
}

export function getFunCallTarget(
    call: sol.FunctionCall,
    infer: sol.InferType
): sol.FunctionDefinition {
    const decl = call.vReferencedDeclaration;

    sol.assert(decl instanceof sol.FunctionDefinition, `NYI fun def {0}`, decl);

    const contractScope = call.getClosestParentByType(sol.ContractDefinition);

    const callee = contractScope ? sol.resolveCallable(contractScope, decl, infer) : decl;

    sol.assert(callee instanceof sol.FunctionDefinition, `NYI callee {0}`, callee);

    return callee;
}

export function bigintToInt(v: bigint, min?: number, max?: number): number {
    min = min === undefined ? Number.MIN_SAFE_INTEGER : min;
    max = max === undefined ? Number.MAX_SAFE_INTEGER : max;
    sol.assert(v >= min && v <= max, `{0} outside of safe range [{1}, {2}]`, v, min, max);

    return Number(v);
}

export function checkOOB(
    v: bigint | number | undefined,
    min: number,
    max: number
): number | undefined;
export function checkOOB(
    v: bigint | number | undefined,
    min: bigint,
    max: bigint
): bigint | undefined;
export function checkOOB(
    v: bigint | number | undefined,
    min: number,
    max: bigint
): bigint | undefined;
export function checkOOB(
    v: bigint | number | undefined,
    min: bigint,
    max: number
): bigint | undefined;
export function checkOOB(
    v: bigint | number | undefined,
    min: number | bigint,
    max: number | bigint
): number | bigint | undefined {
    if (v === undefined) {
        return undefined;
    }

    if (typeof min === "number" && typeof max === "number") {
        if (v < min || v > max) {
            return undefined;
        }

        return Number(v);
    }

    if (v < min || v > max) {
        return undefined;
    }

    return v;
}

type ArgExprMap = Map<sol.ContractDefinition, sol.Expression[]>;
type ParentScopeMap = Map<sol.ContractDefinition, sol.ContractDefinition>;
/**
 * Detect inheritance specifier-style and modifier-style invocation arguments
 * of contract definition and creates corresponding entries in map.
 * Recursively descends for detection in base contract definitions too.
 *
 * @param contract  contract definition to detect args.
 * @param argsMap   mapping to collect corresponding arguments to call base constructors.
 */
export function grabInheritanceArgs(
    contract: sol.ContractDefinition,
    argsMap: ArgExprMap = new Map(),
    parentScopeMap: ParentScopeMap = new Map()
): [ArgExprMap, ParentScopeMap] {
    for (const inheritanceSpecifier of contract.vInheritanceSpecifiers) {
        const base = inheritanceSpecifier.vBaseType
            .vReferencedDeclaration as sol.ContractDefinition;

        grabInheritanceArgs(base, argsMap, parentScopeMap);

        argsMap.set(base, inheritanceSpecifier.vArguments);
        parentScopeMap.set(base, contract);
    }

    if (contract.vConstructor) {
        for (const invocation of contract.vConstructor.vModifiers) {
            const base = invocation.vModifier;

            /**
             * Prior to Solidity 0.6.0, it was allowed to specify current contract
             * as modifier to it's own constructor. Compiler ignores the call in such case.
             *
             * Since Solidity 0.6.0 such use is forbidden.
             */
            if (base instanceof sol.ContractDefinition && base !== contract) {
                grabInheritanceArgs(base, argsMap, parentScopeMap);

                /**
                 * Modifier-style invocations are taking precedence over
                 * inheritance specifier-style calls for Solc 0.4.21 and lower.
                 *
                 * Solc 0.4.22 and further will not compile at all.
                 *
                 * @see https://solidity.readthedocs.io/en/v0.4.21/contracts.html#arguments-for-base-constructors
                 * @see https://solidity.readthedocs.io/en/v0.4.22/contracts.html#arguments-for-base-constructors
                 */
                argsMap.set(base, invocation.vArguments);
                parentScopeMap.set(base, contract);
            }
        }
    }

    return [argsMap, parentScopeMap];
}
