import { DataLocation as SolDataLocation } from "solc-typed-ast";
import { DecodingFailure, Value } from "./value";
import { ArrayLikeView, IndexableView, PointerView, StateArea, StructView, View } from "./view";
import {
    BytesCalldataView,
    BytesSliceCalldataView,
    isArrayLikeCalldataView,
    PointerCalldataView,
    StringCalldataView,
    StringSliceCalldataView,
    StructCalldataView
} from "./calldata";
import {
    BytesMemView,
    isArrayLikeMemView,
    PointerMemView,
    StringMemView,
    StructMemView
} from "./memory";
import {
    BytesStorageView,
    isArrayLikeStorageView,
    MapStorageView,
    PointerStorageView,
    StringStorageView,
    StructStorageView
} from "./storage";
import { PointerStackView } from "./stack";
import {
    ArrayType,
    astToRuntimeType,
    BaseRuntimeType,
    BytesType,
    PointerType,
    StringType
} from "../runtime_types";
import * as sol from "solc-typed-ast";
import * as rtt from "../runtime_types/ast";
import { isTypeUnknownContract } from "../../utils";
import { BytecodeInfo, LinkMap } from "../artifact_manager";
import { Address } from "@ethereumjs/util";

export function isCalldataArrayType(typ: BaseRuntimeType): boolean {
    return (
        typ instanceof PointerType &&
        ((typ.toType instanceof ArrayType && typ.toType.size === undefined) ||
            typ.toType instanceof BytesType ||
            typ.toType instanceof StringType) &&
        typ.location === SolDataLocation.CallData
    );
}

/**
 * Returns true IFF the  n >= min && n <= max
 *
 * @param n
 * @param min
 * @param max
 */
export function inRange(n: number | bigint, min: number | bigint, max: number | bigint): boolean {
    return BigInt(n) >= BigInt(min) && BigInt(n) <= BigInt(max);
}

export function isFailure(t: Value): t is DecodingFailure {
    return t instanceof DecodingFailure;
}

export function isTypeStringDynamicArray(t: string): boolean {
    return t.endsWith("[]");
}

export function isTypeStringMapping(t: string): boolean {
    return t.startsWith("mapping(");
}

export function isTypeStringStruct(t: string): boolean {
    return t.startsWith("struct ");
}

export function isTypeStringStatic32BytesInStorage(t: string): boolean {
    return isTypeStringDynamicArray(t) || isTypeStringMapping(t);
}

export function isPointerView(v: any): v is PointerView<any, View> {
    return (
        v instanceof PointerCalldataView ||
        v instanceof PointerMemView ||
        v instanceof PointerStorageView ||
        v instanceof PointerStackView
    );
}

export function isArrayLikeView(v: any): v is ArrayLikeView<any, View> {
    return isArrayLikeMemView(v) || isArrayLikeCalldataView(v) || isArrayLikeStorageView(v);
}

export function isStructView(v: any): v is StructView<any, View> {
    return (
        v instanceof StructMemView ||
        v instanceof StructCalldataView ||
        v instanceof StructStorageView
    );
}

export function isIndexableView(v: any): v is IndexableView<any, StateArea, View> {
    return (
        isArrayLikeMemView(v) ||
        isArrayLikeCalldataView(v) ||
        isArrayLikeStorageView(v) ||
        v instanceof MapStorageView
    );
}

/**
 * Helper for converting `VariableDeclartaion`s to `TypeNode`s. In some cases when solc-typed-ast conversion fails,
 * it can try and guess the correct simplified type from the typeString
 *
 * - unknown contracts - retun address
 */
function variableDeclarationToTypeNode(
    v: sol.VariableDeclaration,
    infer: sol.InferType
): sol.TypeNode {
    try {
        return infer.variableDeclarationToTypeNode(v);
    } catch (e) {
        if (v.vType && isTypeUnknownContract(v.vType)) {
            return new sol.AddressType(false);
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
                    stateVars.push([varDecl.name, new rtt.MissingType(varDecl.typeString)]);
                    continue;
                } else {
                    break;
                }
            }

            stateVars.push([
                varDecl.name,
                astToRuntimeType(typeNode, infer, sol.DataLocation.Storage)
            ]);
        }
    }

    return [new rtt.StructType(contract.name, stateVars), complete];
}

/**
 * Given a `BytecodeInfo` and some actual linked bytecode, decode the `LinkMap` that was used to link the bytecode.
 * @param bytecodeInfo
 * @param actualBytecode
 * @returns
 */
export function decodeLinkMap(bytecodeInfo: BytecodeInfo, actualBytecode: Uint8Array): LinkMap {
    const res: LinkMap = new Map();

    if (!bytecodeInfo.linkReferences) {
        return res;
    }

    for (const [libId, ranges] of bytecodeInfo.linkReferences) {
        sol.assert(ranges.length > 0 && ranges[0].length == 20, ``);
        const rng = ranges[0];
        res.set(libId, new Address(actualBytecode.slice(rng.start, rng.start + rng.length)));
    }

    return res;
}

const bytesT = new rtt.BytesType();

export function castStringToBytes(
    v: StringMemView | StringCalldataView | StringStorageView | StringSliceCalldataView
): BytesMemView | BytesCalldataView | BytesStorageView | BytesSliceCalldataView {
    if (v instanceof StringMemView) {
        return new BytesMemView(bytesT, v.offset);
    }

    if (v instanceof StringCalldataView) {
        return new BytesCalldataView(bytesT, v.offset, v.base);
    }

    if (v instanceof StringStorageView) {
        return new BytesStorageView(bytesT, [v.key, v.endOffsetInWord]);
    }

    return new BytesSliceCalldataView(v.offset, v.len);
}
