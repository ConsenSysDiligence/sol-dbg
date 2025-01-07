import { bytesToHex } from "ethereum-cryptography/utils";
import {
    AddressType,
    ArrayType,
    BytesType,
    ContractDefinition,
    EnumDefinition,
    FunctionDefinition,
    InferType,
    MappingType,
    PointerType,
    StateVariableVisibility,
    StringType,
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
import { getFunctionSelector, zip } from "../utils/misc";
import { abiStaticTypeSize } from "../utils/solidity";
import { IArtifactManager } from "./artifact_manager";
import { cd_decodeValue } from "./decoding/calldata/decode";
import { mem_decodeValue } from "./decoding/memory/decoding";
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
        const typ = toABIEncodedType(originalType, infer, encoderVersion);
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
function isTypeEncodingDynamic(typ: TypeNode): boolean {
    if (
        typ instanceof PointerType ||
        typ instanceof ArrayType ||
        typ instanceof StringType ||
        typ instanceof BytesType
    ) {
        return true;
    }

    // Tuples in calldata with static elements
    if (typ instanceof TupleType) {
        for (const elT of typ.elements) {
            assert(elT !== null, ``);

            if (isTypeEncodingDynamic(elT)) {
                return true;
            }
        }

        return false;
    }

    return false;
}

/**
 * Convert an internal TypeNode to the external TypeNode that would correspond to it
 * after ABI-encoding with encoder version `encoderVersion`. Follows the following rules:
 *
 * 1. Contract definitions turned to address.
 * 2. Enum definitions turned to uint of minimal fitting size.
 * 3. Any storage pointer types are converted to memory pointer types.
 * 4. Throw an error on any nested mapping types.
 * 5. Fixed-size arrays with fixed-sized element types are encoded as inlined tuples
 * 6. Structs with fixed-sized elements are encoded as inlined tuples
 *
 * @see https://docs.soliditylang.org/en/latest/abi-spec.html
 */
export function toABIEncodedType(
    type: TypeNode,
    infer: InferType,
    encoderVersion: ABIEncoderVersion
): TypeNode {
    if (type instanceof MappingType) {
        throw new Error("Cannot abi-encode mapping types");
    }

    if (type instanceof ArrayType) {
        const encodedElementT = toABIEncodedType(type.elementT, infer, encoderVersion);

        if (type.size !== undefined) {
            const elements = [];

            for (let i = 0; i < type.size; i++) {
                elements.push(encodedElementT);
            }

            return new TupleType(elements);
        }

        return new ArrayType(encodedElementT, type.size);
    }

    if (type instanceof PointerType) {
        const toT = toABIEncodedType(type.to, infer, encoderVersion);

        return isTypeEncodingDynamic(toT) ? new PointerType(toT, type.location) : toT;
    }

    if (type instanceof UserDefinedType) {
        if (type.definition instanceof UserDefinedValueTypeDefinition) {
            return infer.typeNameToTypeNode(type.definition.underlyingType);
        }

        if (type.definition instanceof ContractDefinition) {
            return new AddressType(false);
        }

        if (type.definition instanceof EnumDefinition) {
            return enumToIntType(type.definition);
        }

        if (type.definition instanceof StructDefinition) {
            assert(
                encoderVersion !== ABIEncoderVersion.V1,
                "Getters of struct return type are not supported by ABI encoder v1"
            );

            const fieldTs = type.definition.vMembers.map((fieldT: VariableDeclaration) =>
                infer.variableDeclarationToTypeNode(fieldT)
            );

            return new TupleType(
                fieldTs.map((fieldT: TypeNode) => toABIEncodedType(fieldT, infer, encoderVersion))
            );
        }
    }

    return type;
}

/**
 * Given a 4-byte selector, a target contract and an `InferType` object return
 * the ASTNode that corresponds to the target callee. Must be either a
 * `FunctionDefinition`, a `VariableDeclaration` (for public state var getters),
 * or undefined (if we cannot identify it)
 */
export function findMethodBySelector(
    selector: Uint8Array | string,
    contract: ContractDefinition,
    infer: InferType
): FunctionDefinition | VariableDeclaration | undefined {
    const strSelector = typeof selector === "string" ? selector : bytesToHex(selector);

    for (const base of contract.vLinearizedBaseContracts) {
        if (!base) {
            continue;
        }

        for (const fun of base.vFunctions) {
            const funSel = getFunctionSelector(fun, infer);
            if (funSel == strSelector) {
                return fun;
            }
        }

        for (const v of base.vStateVariables) {
            if (v.visibility !== StateVariableVisibility.Public) {
                continue;
            }

            let hash: string | undefined;

            try {
                hash = infer.signatureHash(v);
            } catch (e) {
                continue;
            }

            if (hash == strSelector) {
                return v;
            }
        }
    }
    return undefined;
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
        const typ = toABIEncodedType(originalType, infer, ABIEncoderVersion.V2);
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
