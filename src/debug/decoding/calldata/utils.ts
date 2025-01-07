import {
    ArrayType,
    assert,
    InferType,
    PackedArrayType,
    PointerType,
    TypeNode,
    types
} from "solc-typed-ast";
import { MAX_ARR_DECODE_LIMIT, uint256 } from "../../../utils/misc";
import { elType, isTypePrimitive } from "../../../utils/solidity";
import { CalldataLocation, DataLocationKind, Memory } from "../../types";
import { cd_decodeInt, cd_decodeValue } from "./decode";
import { cd_typeStaticSize } from "./size";

function cd_decodeValOrLoc(
    abiType: TypeNode,
    origType: TypeNode | undefined,
    loc: CalldataLocation,
    calldata: Memory,
    infer: InferType
): any {
    // For primitive types just decode the value
    if (isTypePrimitive(abiType)) {
        return cd_decodeValue(abiType, origType, loc, calldata, infer);
    }

    // For reference types, read the pointer and adjust by the current base offset
    const ptr = cd_decodeInt(types.uint256, loc, calldata);

    if (ptr === undefined) {
        return undefined;
    }

    return {
        kind: DataLocationKind.CallData,
        address: ptr[0],
        base: loc.base
    } as CalldataLocation;
}

export function cd_indexInto(
    abiType: PointerType,
    origType: PointerType | undefined,
    loc: CalldataLocation,
    index: number,
    calldata: Memory,
    infer: InferType
): any {
    let arrOffset = loc.address + loc.base;

    let len: [bigint, number] | undefined;
    const arrType = abiType.to;

    assert(
        arrType instanceof ArrayType || arrType instanceof PackedArrayType,
        `Unexpected abi type {0}`,
        abiType
    );

    if (arrType instanceof ArrayType && arrType.size !== undefined) {
        len = [arrType.size, 0];
    } else {
        // @todo are short strings encoded in a single word in cd?
        len = cd_decodeInt(uint256, loc, calldata);
    }

    if (len == undefined || len[0] >= MAX_ARR_DECODE_LIMIT) {
        return undefined;
    }

    console.error(`Loc: `, loc);
    console.error(`Arr base off: `, arrOffset);

    const numLen = Number(len[0]);
    console.error(`Arr len: `, len);

    arrOffset += BigInt(len[1]);

    if (index < 0 || index >= numLen) {
        return undefined;
    }

    const abiElT = elType(arrType);
    const elSize = cd_typeStaticSize(abiElT, infer);

    console.error(`El size: `, elSize);

    let origElT: TypeNode | undefined;

    if (origType) {
        // @todo support returned structs here...
        assert(
            origType.to instanceof ArrayType || origType.to instanceof PackedArrayType,
            `Unexpected original type {0}`,
            origType
        );

        origElT = elType(origType);
    }

    const elOff = BigInt(elSize * index);

    console.error(`El off: `, elOff, `El idx: `, index);

    return cd_decodeValOrLoc(
        abiElT,
        origElT,
        { kind: DataLocationKind.CallData, address: elOff, base: arrOffset },
        calldata,
        infer
    );
}
