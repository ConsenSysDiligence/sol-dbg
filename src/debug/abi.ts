import { bytesToHex } from "ethereum-cryptography/utils";
import {
    ArrayType,
    ContractDefinition,
    DataLocation,
    EnumDefinition,
    FunctionDefinition,
    InferType,
    PackedArrayType,
    PointerType,
    StructDefinition,
    TupleType,
    TypeName,
    TypeNode,
    UserDefinedType,
    UserDefinedTypeName,
    UserDefinedValueTypeDefinition,
    VariableDeclaration,
    assert,
    enumToIntType,
    isReferenceType,
    types
} from "solc-typed-ast";
import { ABIEncoderVersion } from "solc-typed-ast/dist/types/abi";
import { getFunctionSelector, repeat, zip } from "../utils/misc";
import { abiStaticTypeSize } from "../utils/solidity";
import { IArtifactManager } from "./artifact_manager";
import { cd_decodeValue } from "./decoding/calldata/decode";
import { TupleCalldataView } from "./decoding/calldata/view";
import { mem_decodeValue } from "./decoding/memory/decoding";
import { View } from "./decoding/view";
import {
    DataLocationKind,
    DataView,
    DecodedEventDesc,
    EventDefInfo,
    EventDesc,
    LinearMemoryLocation
} from "./types";

/**
 * Given a callee AST Node (FunctionDefinition or VariableDeclaration
 * corresponding to a getter), some msg `data`, that lives in location
 * determined by `kind`, a type inference class as well as an `encoderVersion`
 * try and decode the function arguments for that particular callee from the
 * data.
 */
export function decodeMethodArgs(
    callee: FunctionDefinition | VariableDeclaration,
    data: Uint8Array,
    kind: DataLocationKind.Memory | DataLocationKind.CallData,
    infer: InferType,
    encoderVersion: ABIEncoderVersion
): Array<[string, any]> {
    const dataViews = buildMsgDataViews(callee, data, kind, infer, encoderVersion);

    const res: Array<[string, any]> = [];

    for (let i = 0; i < dataViews.length; i++) {
        const name = dataViews[i][0];
        const view = dataViews[i][1];

        if (view === undefined) {
            res.push([name, undefined]);
            continue;
        }

        assert(view.abiType !== undefined && view.loc.kind === DataLocationKind.CallData, ``);

        const val = cd_decodeValue(view.abiType, view.type, view.loc, data, infer);

        res.push([name, val ? val[0] : val]);
    }

    return res;
}

// @todo migrate to solc-typed-ast
export function isTypeUnknownContract(t: TypeName | undefined): boolean {
    return (
        t instanceof UserDefinedTypeName &&
        t.referencedDeclaration < 0 &&
        (t.typeString.startsWith("contract ") ||
            t.typeString.startsWith("interface ") ||
            t.typeString.startsWith("library "))
    );
}

/**
 * TODO: remove
 * An ABI-decoder implementation that is resilient to failures in some arguments decoding.
 * This function will return partial decoding results. This is needed since the fuzzer may not
 * always produce inputs that decode in their entirety.
 */
export function buildMsgDataViews(
    callee: FunctionDefinition | VariableDeclaration,
    data: Uint8Array,
    kind: DataLocationKind.Memory | DataLocationKind.CallData,
    infer: InferType,
    encoderVersion: ABIEncoderVersion
): Array<[string, DataView | undefined]> {
    const res: Array<[string, DataView | undefined]> = [];
    let baseOff;

    if (callee instanceof FunctionDefinition && callee.isConstructor) {
        baseOff = 0;
    } else {
        baseOff = 4;
        const selector =
            callee instanceof FunctionDefinition
                ? getFunctionSelector(callee, infer)
                : infer.signatureHash(callee);

        assert(
            selector === bytesToHex(data.slice(0, 4)),
            `Expected selector ${selector} instead got ${data.slice(0, 4)}`
        );
    }

    const formals: Array<[string, TypeNode]> =
        callee instanceof FunctionDefinition
            ? callee.vParameters.vParameters.map((argDef: VariableDeclaration) => [
                  argDef.name,
                  isTypeUnknownContract(argDef.vType)
                      ? types.address
                      : infer.variableDeclarationToTypeNode(argDef)
              ])
            : infer
                  .getterArgsAndReturn(callee)[0]
                  .map((typ: TypeNode, i: number) => [`ARG_${i}`, typ]);

    let staticOff = 0;
    const len = data.length;

    for (const [name, originalType] of formals) {
        const typ = infer.toABIEncodedType(originalType, encoderVersion);
        const staticSize = abiStaticTypeSize(typ);
        const loc =
            baseOff + staticOff + staticSize <= len
                ? { kind, address: BigInt(staticOff), base: BigInt(baseOff) }
                : undefined;

        staticOff += staticSize;

        const val = loc ? { type: originalType, abiType: typ, loc } : undefined;

        res.push([name, val]);
    }

    return res;
}

/**
 * Determine if the specified type `typ` is dynamic or not. Dynamic means
 * that if we are trying to read `typ` at location `loc`, in `loc` there should be just a
 * uint256 offset into memory/storage/calldata, where the actual data lives. Otherwise
 * (if the type is "static"), the direct encoding of the data will start at `loc`.
 *
 * Usually "static" types are just the value types - i.e. anything of statically
 * known size that fits in a uint256. As per https://docs.soliditylang.org/en/latest/abi-spec.html#formal-specification-of-the-encoding
 * there are several exceptions to the rule when encoding types in calldata:
 *
 * 1. Fixed size arrays with fixed-sized element types
 * 2. Tuples where all the tuple elements are fixed-size
 *
 * TODO(dimo):
 *  1. Check again that its not possible for tuples in internal calls to somehow get encoded on the stack
 *  2. What happens with return tuples? Are they always in memory?
 */
export function isTypeEncodedInline(typ: TypeNode): boolean {
    if (!(typ instanceof PointerType)) {
        return false;
    }

    if (typ.to instanceof PackedArrayType) {
        return true;
    }

    if (typ.to instanceof ArrayType && typ.to.size === undefined) {
        return true;
    }

    return false;
}

/**
 * Given an `EventDefInfo` and a concrete event `EventDesc`, build `DataView`s for decoding the event arguments.
 * Supports decoding indexed arguments.
 */
function buildEventDataViews(
    evtDef: EventDefInfo,
    evtDesc: EventDesc,
    infer: InferType
): Array<[string, DataView | number | undefined]> {
    let staticOff = 0;
    const res: Array<[string, DataView | number | undefined]> = [];

    const len = evtDesc.payload.length;
    let topicIdx = evtDef.definition.anonymous ? 0 : 1;

    for (const [name, originalType, indexed] of evtDef.args) {
        if (indexed) {
            res.push([name, topicIdx < evtDesc.topics.length ? topicIdx++ : undefined]);
            continue;
        }

        // @todo (dimo) Is it ok to hardcode the encoder version here?
        const typ = infer.toABIEncodedType(originalType, ABIEncoderVersion.V2);
        const staticSize = abiStaticTypeSize(typ);
        const loc: LinearMemoryLocation | undefined =
            staticOff + staticSize <= len
                ? { kind: DataLocationKind.Memory, address: BigInt(staticOff) }
                : undefined;

        staticOff += staticSize;

        const val: DataView | undefined = loc
            ? { type: originalType, abiType: typ, loc }
            : undefined;

        res.push([name, val]);
    }

    return res;
}

/**
 * Decode a raw event. Currently only supports non-anonmyous events.
 */
export function decodeEvent(
    artifactManager: IArtifactManager,
    evt: EventDesc
): DecodedEventDesc | undefined {
    if (evt.topics.length === 0) {
        return undefined;
    }

    const defInfo = artifactManager.getEventDefInfo(evt.topics[0]);

    if (!defInfo) {
        return undefined;
    }

    const infer = artifactManager.infer(defInfo.artifact.compilerVersion);
    const dataViews = buildEventDataViews(defInfo, evt, infer);
    const argVals: Array<[string, any]> = [];

    for (const [[name, view], [, type]] of zip(dataViews, defInfo.args)) {
        if (view === undefined) {
            // Failed building a view
            argVals.push([name, undefined]);
            continue;
        }

        if (typeof view === "number") {
            if (isReferenceType(type)) {
                // For indexed reference types just a hash of the value is
                // stored in the topic. Can't decode.
                argVals.push([name, undefined]);
            }

            const decodedVal = mem_decodeValue(
                type,
                { kind: DataLocationKind.Memory, address: 0n },
                evt.topics[view],
                infer
            );

            argVals.push([name, decodedVal ? decodedVal[0] : undefined]);
            continue;
        }

        const decodedVal = mem_decodeValue(
            type,
            view.loc as LinearMemoryLocation,
            evt.payload,
            infer
        );

        argVals.push([name, decodedVal ? decodedVal[0] : undefined]);
    }

    return {
        def: defInfo,
        args: argVals
    };
}

function toABIEncodedType(
    type: TypeNode,
    encoderVersion: ABIEncoderVersion,
    normalizePointers = false,
    infer: InferType
): TypeNode {
    assert(
        infer.isABIEncodable(type, encoderVersion),
        'Can not ABI-encode type "{0}" with encoder "{1}"',
        type,
        encoderVersion
    );

    if (type instanceof ArrayType) {
        const elT = toABIEncodedType(type.elementT, encoderVersion, normalizePointers, infer);

        if (type.size !== undefined) {
            return new TupleType(repeat(elT, Number(type.size)));
        }

        return new ArrayType(elT, type.size);
    }

    if (type instanceof PointerType) {
        const toT = toABIEncodedType(type.to, encoderVersion, normalizePointers, infer);

        if (toT instanceof TupleType) {
            return toT;
        }

        if (toT instanceof PackedArrayType || toT instanceof ArrayType) {
            return new PointerType(toT, normalizePointers ? DataLocation.Memory : type.location);
        }

        assert(false, `Unexpected ABI pointer type {0}`, toT);
    }

    if (type instanceof UserDefinedType) {
        if (type.definition instanceof UserDefinedValueTypeDefinition) {
            return infer.typeNameToTypeNode(type.definition.underlyingType);
        }

        if (type.definition instanceof ContractDefinition) {
            return types.address;
        }

        if (type.definition instanceof EnumDefinition) {
            return enumToIntType(type.definition);
        }

        if (type.definition instanceof StructDefinition) {
            const fieldTs = type.definition.vMembers.map((fieldT) =>
                infer.variableDeclarationToTypeNode(fieldT)
            );

            return new TupleType(
                fieldTs.map((fieldT) =>
                    toABIEncodedType(fieldT, encoderVersion, normalizePointers, infer)
                )
            );
        }
    }

    return type;
}

/**
 * Lift a value corresponding to an ABI type to a value corresponding to the "original" type.
 * We only lift tuples back up to structs here
 */
export function liftABIValue(value: any, originalType: TypeNode, infer: InferType): any {
    if (originalType instanceof PointerType) {
        if (originalType.to instanceof ArrayType) {
            const elT = originalType.to.elementT;
            assert(value instanceof Array, ``);
            return value.map((el) => liftABIValue(el, elT, infer));
        }

        if (
            originalType.to instanceof UserDefinedType &&
            originalType.to.definition instanceof StructDefinition
        ) {
            const fields = originalType.to.definition.vMembers;
            const res: any = {};

            assert(value instanceof Array && value.length === fields.length, ``);
            for (let i = 0; i < fields.length; i++) {
                const field = fields[i];

                try {
                    const fieldT = infer.variableDeclarationToTypeNode(field);
                    res[field.name] = liftABIValue(value[i], fieldT, infer);
                } catch (e) {
                    res[field.name] = undefined;
                }
            }

            return res;
        }
    }

    if (originalType instanceof TupleType) {
        assert(value instanceof Array, ``);
        return value.map((el, i) => liftABIValue(el, originalType.elements[i] as TypeNode, infer));
    }

    return value;
}

/**
 * An ABI-decoder implementation that is resilient to failures in some arguments decoding.
 * This function will return partial decoding results. This is needed since the fuzzer may not
 * always produce inputs that decode in their entirety.
 */
export function buildCalldataViews(
    callee: FunctionDefinition | VariableDeclaration,
    calldata: Uint8Array,
    infer: InferType
): Array<[string, View | undefined, TypeNode]> {
    const res: Array<[string, View | undefined, TypeNode]> = [];
    let baseOff;

    if (callee instanceof FunctionDefinition && callee.isConstructor) {
        baseOff = 0;
    } else {
        baseOff = 4;
        const selector =
            callee instanceof FunctionDefinition
                ? getFunctionSelector(callee, infer)
                : infer.signatureHash(callee);

        assert(
            selector === bytesToHex(calldata.slice(0, 4)),
            `Expected selector ${selector} instead got ${calldata.slice(0, 4)}`
        );
    }

    const formals: Array<[string, TypeNode]> =
        callee instanceof FunctionDefinition
            ? callee.vParameters.vParameters.map((argDef: VariableDeclaration) => [
                  argDef.name,
                  isTypeUnknownContract(argDef.vType)
                      ? types.address
                      : infer.variableDeclarationToTypeNode(argDef)
              ])
            : infer
                  .getterArgsAndReturn(callee)[0]
                  .map((typ: TypeNode, i: number) => [`ARG_${i}`, typ]);

    let argStaticOff = 0;
    const len = calldata.length;

    for (const [name, originalType] of formals) {
        if (baseOff + argStaticOff + 32 > len) {
            res.push([name, undefined, originalType]);
        }

        try {
            const abiType = toABIEncodedType(originalType, ABIEncoderVersion.V2, false, infer);
            const view = TupleCalldataView.decodeElementAt(
                abiType,
                infer,
                BigInt(baseOff + argStaticOff),
                BigInt(baseOff),
                calldata
            );

            res.push([name, view, originalType]);
        } catch (e) {
            res.push([name, undefined, originalType]);
        }

        argStaticOff += 32;
    }

    return res;
}
