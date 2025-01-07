import {
    ABIEncoderVersion,
    ArrayType,
    assert,
    InferType,
    PackedArrayType,
    PointerType,
    DataLocation as SolDataLocation,
    TypeNode
} from "solc-typed-ast";
import { MAX_ARR_DECODE_LIMIT, nyi, uint256 } from "../../utils/misc";
import { topExtFrame } from "../tracers/transformers/ext_stack";
import {
    DataLocation,
    DataLocationKind,
    DataView,
    MemoryLocation,
    MemoryLocationKind,
    StackLocation,
    StepState,
    StorageLocation
} from "../types";
import { cd_decodeArrayContents, cd_indexInto } from "./calldata";
import { mem_indexInto } from "./memory";
import { st_decodeInt } from "./stack";
import { stor_indexInto } from "./storage";

export function solLocToMemoryLocationKind(loc: SolDataLocation): MemoryLocationKind {
    if (loc === SolDataLocation.Default) {
        return DataLocationKind.Memory;
    }

    return loc as unknown as MemoryLocationKind;
}

/**
 * Array pointers to calldata in the stack are stored as 2 slots - offset and
 * length. This is to support array slices. Return the offset and length of the
 * array (slice).
 */
export function getCDArrayInStackOffAndLen(
    loc: StackLocation,
    state: StepState
): [bigint, number] | [undefined, undefined] {
    const off = st_decodeInt(uint256, loc, state.evmStack);

    if (off === undefined) {
        return [undefined, undefined];
    }

    const len = st_decodeInt(
        uint256,
        { kind: loc.kind, offsetFromTop: loc.offsetFromTop - 1 },
        state.evmStack
    );

    if (len === undefined) {
        return [undefined, undefined];
    }

    if (len > MAX_ARR_DECODE_LIMIT) {
        return [undefined, undefined];
    }

    return [off, Number(len)];
}

export function indexInto_int(
    typ: PointerType,
    loc: DataLocation,
    index: any,
    state: StepState,
    infer: InferType
): any {
    if (loc.kind === DataLocationKind.Memory) {
        return mem_indexInto(typ, loc, index, state.memory, infer);
    } else if (loc.kind === DataLocationKind.CallData) {
        const lastExtFrame = topExtFrame(state);

        let abiType: TypeNode;

        try {
            abiType = infer.toABIEncodedType(typ, ABIEncoderVersion.V2);
        } catch (e) {
            return undefined;
        }

        assert(abiType instanceof PointerType, `Unexpected abi type {0}`, abiType);

        return cd_indexInto(abiType, typ, loc, index, lastExtFrame.msgData, infer);
    } else if (loc.kind === DataLocationKind.Storage) {
        return stor_indexInto(typ, loc, index, state.storage, infer);
    }

    nyi(`Indexing into ${loc.kind}`);
}

export function isCalldataArrayType(typ: TypeNode): boolean {
    return (
        typ instanceof PointerType &&
        ((typ.to instanceof ArrayType && typ.to.size === undefined) ||
            typ.to instanceof PackedArrayType) &&
        typ.location === SolDataLocation.CallData
    );
}

/**
 * Decode a generic value expressed as a `DataView` (i.e. a tuple of type and location) given
 * the dbg state `state` at some step.
 */
export function indexInto(view: DataView, index: any, state: StepState, infer: InferType): any {
    const typ = view.type;
    const loc = view.loc;

    assert(typ instanceof PointerType, `Unexpected type {0}`, typ);
    /**
     * The only case where a pointer from one area of the state crosses into another
     * area of the state are pointers in the stack. All other pointers stay in their
     * own memory region
     */
    if (typ instanceof PointerType && loc.kind === DataLocationKind.Stack) {
        if (isCalldataArrayType(typ)) {
            const [off, len] = getCDArrayInStackOffAndLen(loc, state);

            if (off === undefined || len === undefined) {
                return undefined;
            }

            const lastExtFrame = topExtFrame(state);

            let abiType: TypeNode;

            try {
                abiType = infer.toABIEncodedType(typ, ABIEncoderVersion.V2);
            } catch (e) {
                return undefined;
            }

            assert(
                abiType instanceof PointerType && abiType.to instanceof ArrayType,
                `InternalError`
            );

            const res = cd_decodeArrayContents(
                abiType.to,
                typ.to as ArrayType,
                off,
                Number(len),
                lastExtFrame.msgData,
                infer
            );

            return res === undefined ? res : res[0];
        }

        const off = st_decodeInt(uint256, loc, state.evmStack);

        if (off === undefined) {
            return undefined;
        }

        const kind: MemoryLocationKind = solLocToMemoryLocationKind(typ.location);

        let pointedToLoc: MemoryLocation;

        if (kind === DataLocationKind.Storage) {
            pointedToLoc = {
                kind,
                address: off,
                endOffsetInWord: 32
            } as StorageLocation;
        } else if (kind === DataLocationKind.Memory) {
            pointedToLoc = {
                kind,
                address: off
            };
        } else if (kind === DataLocationKind.CallData) {
            pointedToLoc = {
                kind,
                address: off,
                base: 0n
            };
        } else {
            nyi(`NYI data location kind ${kind}`);
        }

        const res = indexInto_int(typ, pointedToLoc, index, state, infer);

        //console.error(`indexInto: res ${res}`);
        return res;
    }

    const res = indexInto_int(typ, loc, index, state, infer);
    //console.error(`indexInto: res ${res}`);

    return res;
}
