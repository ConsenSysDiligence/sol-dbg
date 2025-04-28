import { Address, bytesToUtf8 } from "@ethereumjs/util";
import { keccak256 } from "ethereum-cryptography/keccak";
import {
    AddressType,
    ArrayType,
    assert,
    BoolType,
    BytesType,
    ContractDefinition,
    DataLocation,
    EnumDefinition,
    enumToIntType,
    FixedBytesType,
    InferType,
    IntType,
    MappingType,
    PackedArrayType,
    PointerType,
    StringType,
    StructDefinition,
    TypeNode,
    types,
    UserDefinedType,
    UserDefinedValueTypeDefinition
} from "solc-typed-ast";
import {
    bigEndianBufToBigint,
    bigIntToBuf,
    fits,
    MAX_ARR_DECODE_LIMIT,
    nyi
} from "../../../utils/misc";
import { changeToLocation } from "../../../utils/solidity";
import { Storage } from "../../types";
import { ArrayView, BaseAddressView, IndexableView, SolValue, StructView } from "../view";

/**
 * Helper function to determine the "static size" a type will take in Storage (in bytes)
 */
function staticSize(typ: TypeNode, infer: InferType): number {
    if (typ instanceof IntType) {
        return typ.nBits / 8;
    } else if (typ instanceof BoolType) {
        return 1;
    } else if (typ instanceof FixedBytesType) {
        return typ.size;
    } else if (typ instanceof Address) {
        return 20;
    } else if (typ instanceof UserDefinedType) {
        const def = typ.definition;

        if (def instanceof EnumDefinition) {
            return enumToIntType(def).nBits / 8;
        } else if (def instanceof ContractDefinition) {
            return 20;
        } else if (def instanceof UserDefinedValueTypeDefinition) {
            const underlyingType = infer.typeNameToTypeNode(def.underlyingType);
            return staticSize(underlyingType, infer);
        }
    }

    if (typ instanceof PointerType) {
        const toT = typ.to;

        if (toT instanceof PackedArrayType) {
            return 32;
        }

        if (toT instanceof ArrayType) {
            if (toT.size === undefined) {
                return 32;
            }

            assert(toT.size < MAX_ARR_DECODE_LIMIT, `Static sized array too large {0}`, toT);
            const elSize = staticSize(toT.elementT, infer);
            let arrSize = Number(toT.size) * elSize;

            // Values after an array always start in a new word
            arrSize += arrSize % 32 !== 0 ? 32 - (arrSize % 32) : 0;
            return arrSize;
        }

        if (toT instanceof MappingType) {
            return 32;
        }

        if (toT instanceof UserDefinedType) {
            const def = toT.definition;

            if (def instanceof StructDefinition) {
                let endOffsetInWord = 32;
                let size = 0;

                for (const member of def.vMembers) {
                    const memberT = infer.variableDeclarationToTypeNode(member);
                    const memberStaticSize = staticSize(memberT, infer);
                    size += memberStaticSize;

                    if (memberStaticSize > endOffsetInWord) {
                        // We don't fit in the remaining space in the word
                        size += endOffsetInWord;
                        endOffsetInWord = 32 - memberStaticSize;
                    } else {
                        endOffsetInWord -= memberStaticSize;
                    }
                }

                // Values after structs always start in a new slot
                if (endOffsetInWord % 32 !== 0) {
                    size += endOffsetInWord;
                }

                return size;
            }
        }
    }

    nyi(`NYI staticSize(${typ.pp()})`);
}

/**
 * Helper function to determine if type `type` can start at some storage address
 * with `endOffsetInWord` reamining in the word.
 */
function typeCanStartInLoc(type: TypeNode, infer: InferType, endOffsetInWord: number): boolean {
    if (type instanceof PointerType) {
        if (
            type.to instanceof ArrayType ||
            type.to instanceof BytesType ||
            type.to instanceof StringType ||
            (type.to instanceof UserDefinedType &&
                type.to.definition instanceof StructDefinition) ||
            type.to instanceof MappingType
        ) {
            return endOffsetInWord === 32;
        }

        nyi(`typeeFitsInLoc() for ${type.pp()}`);
    }

    const size = staticSize(type, infer);

    assert(size <= 32, `Unexpected type {0} spanning more than a single word`, type);

    return size <= endOffsetInWord;
}

function shiftBy(address: bigint, endOffsetInWord: number, nBytes: number): [bigint, number] {
    if (nBytes < endOffsetInWord) {
        endOffsetInWord -= nBytes;
    } else {
        nBytes -= endOffsetInWord;
        address += BigInt(1 + nBytes / 32);
        endOffsetInWord = 32 - (nBytes % 32);
    }

    return [address, endOffsetInWord];
}

function roundUpForType(
    address: bigint,
    endOffsetInWord: number,
    type: TypeNode,
    infer: InferType
): [bigint, number] {
    if (!typeCanStartInLoc(type, infer, endOffsetInWord)) {
        address += 1n;
        endOffsetInWord = 32;
    }

    return [address, endOffsetInWord];
}

/**
 * Helper function to make a new BaseStorageView.
 * Selects the correct view class based on the type.
 */
function makeStorageView(
    type: TypeNode,
    infer: InferType,
    address: bigint,
    endOffsetInWord: number
): BaseStorageView {
    if (
        type instanceof IntType ||
        type instanceof BoolType ||
        type instanceof AddressType ||
        type instanceof FixedBytesType
    ) {
        return new SimpleStorageView(type, infer, address, endOffsetInWord);
    }

    if (type instanceof UserDefinedType) {
        const def = type.definition;

        if (
            def instanceof EnumDefinition ||
            def instanceof ContractDefinition ||
            def instanceof UserDefinedValueTypeDefinition
        ) {
            return new SimpleStorageView(type, infer, address, endOffsetInWord);
        }
    }

    if (type instanceof PointerType) {
        return new PointerStorageView(type.to, infer, address, endOffsetInWord);
    }

    nyi(`makeStorageView(${type.pp()})`);
}

/**
 * Helper to fetch the word residing at key `key` from `storage`.  Note that
 * this always succeeds as all uninitialized values in storage are defined to
 * contain 0.
 */
function fetchWord(key: bigint, storage: Storage): Uint8Array {
    const keyHash = bigEndianBufToBigint(keccak256(bigIntToBuf(key, 32, "big")));
    const res = storage.get(keyHash);

    if (res === undefined) {
        return new Uint8Array(32);
    }

    return res;
}

/**
 * Helper to fetch `numBytes` bytes from `storage` starting at offset `off`.
 * Note that this always succeeds as all uninitialized values in storage are
 * defined to contain 0.
 */
function fetchBytes(
    wordOff: bigint,
    offInWord: number,
    numBytes: number,
    storage: Storage
): Uint8Array {
    let curBuf = fetchWord(wordOff, storage);
    const res = new Uint8Array(numBytes);

    for (let i = 0; i < numBytes; i++) {
        res[i] = curBuf[offInWord];

        offInWord = (offInWord + 1) % 32;

        if (offInWord === 0 && i < numBytes - 1) {
            wordOff++;

            curBuf = fetchWord(wordOff, storage);
        }
    }

    return res;
}

function keccakOfAddr(addr: bigint): bigint {
    const addrBuf = bigIntToBuf(addr, 32, "big");
    const hashBuf = keccak256(addrBuf);

    return bigEndianBufToBigint(hashBuf);
}

export abstract class BaseStorageView<T extends TypeNode = TypeNode> extends BaseAddressView<T> {
    protected _endOffsetInWord: number;

    get endOffsetInWord(): number {
        return this._endOffsetInWord;
    }

    constructor(type: T, infer: InferType, address: bigint, endOffsetInWord: number) {
        super(type, infer, address);
        this._endOffsetInWord = endOffsetInWord;
    }

    /**
     * Shift the location forward by nBytes. If the resulting location does not fit this type,
     * round to the next location
     * @param nBytes
     */
    shiftBy(nBytes: number): void {
        [this._address, this._endOffsetInWord] = roundUpForType(
            ...shiftBy(this.address, this.endOffsetInWord, nBytes),
            this.type,
            this.infer
        );
    }

    abstract decode(storage: Storage): SolValue;
    abstract staticSize(): number | undefined;

    // Decoding Helpers for primitive types
    protected decodeInt(typ: IntType, storage: Storage): bigint {
        const size = typ.nBits / 8;

        assert(
            this.endOffsetInWord >= size,
            `Internal Error: Can't decode {0} starting at offset {1} in word {2}`,
            typ,
            this.endOffsetInWord,
            this.address
        );

        const rawBytes = fetchBytes(this.address, this.endOffsetInWord - size, size, storage);

        let res = bigEndianBufToBigint(rawBytes);
        //console.error(`stor_decodeInt rawBytes=${rawBytes.toString(`hex`)} res=${res}`);

        // Convert signed negative 2's complement values
        if (typ.signed && (res & (BigInt(1) << BigInt(typ.nBits - 1))) !== BigInt(0)) {
            // Mask out any 1's above the number's size
            res = res & ((BigInt(1) << BigInt(typ.nBits)) - BigInt(1));
            res = -((BigInt(1) << BigInt(typ.nBits)) - res);
        }

        assert(
            fits(res, typ),
            `Decoded value ${res} from ${this} doesn't fit in expected type ${typ.pp()}`
        );

        return res;
    }

    protected decodeBool(storage: Storage): boolean {
        return this.decodeInt(types.uint8, storage) !== 0n;
    }

    protected decodeEnum(def: EnumDefinition, storage: Storage): bigint {
        const typ = enumToIntType(def);
        return this.decodeInt(typ, storage);
    }

    protected decodeFixedBytes(typ: FixedBytesType, storage: Storage): Uint8Array {
        assert(
            this.endOffsetInWord >= typ.size,
            `Internal Error: Can't decode {0} starting at offset {1} in word {2}`,
            typ,
            this.endOffsetInWord,
            this.address
        );

        return fetchBytes(this.address, this.endOffsetInWord - typ.size, typ.size, storage);
    }

    protected decodeAddress(storage: Storage): Address {
        assert(
            this.endOffsetInWord >= 20,
            `Internal Error: Can't decode address starting at offset {0} in word {1}`,
            this.endOffsetInWord,
            this.address
        );

        const bytes = fetchBytes(this.address, this.endOffsetInWord - 20, 20, storage);
        return new Address(bytes);
    }
}

export abstract class BaseStorageIndexableView<T extends TypeNode = TypeNode>
    extends BaseStorageView<T>
    implements IndexableView
{
    abstract decodeIdx(storage: Storage, idx: SolValue): BaseStorageView | undefined;
}

export abstract class BaseStorageArrayView<T extends TypeNode = TypeNode>
    extends BaseStorageIndexableView<T>
    implements ArrayView
{
    abstract decodeLen(storage: Storage): bigint;
}

export abstract class BaseStorageMapView extends BaseStorageIndexableView {}

// Concrete Implementations

/**
 * Storage view for simple types (i.e. single cell types) - int, bool, address,
 * enum, fixed bytes, contract reference, user-defined value type.
 *
 * For these types `deocode()` is equivalent to `decodeSingle()` as there is only one step to take.
 */
export class SimpleStorageView extends BaseStorageView {
    decode(storage: Storage): SolValue {
        return this.decodeInner(this.type, storage);
    }

    staticSize(typ: TypeNode = this.type): number {
        if (typ instanceof IntType) {
            return typ.nBits / 8;
        } else if (typ instanceof BoolType) {
            return 1;
        } else if (typ instanceof FixedBytesType) {
            return typ.size;
        } else if (typ instanceof Address) {
            return 20;
        } else if (typ instanceof UserDefinedType) {
            const def = typ.definition;

            if (def instanceof EnumDefinition) {
                return enumToIntType(def).nBits / 8;
            } else if (def instanceof ContractDefinition) {
                return 20;
            } else if (def instanceof UserDefinedValueTypeDefinition) {
                const underlyingType = this.infer.typeNameToTypeNode(def.underlyingType);
                return this.staticSize(underlyingType);
            }
        }

        nyi(`NYI staticSize(${typ.pp()})`);
    }

    private decodeInner(typ: TypeNode, storage: Storage): SolValue {
        if (typ instanceof IntType) {
            return this.decodeInt(typ, storage);
        } else if (typ instanceof BoolType) {
            return this.decodeBool(storage);
        } else if (typ instanceof FixedBytesType) {
            return this.decodeFixedBytes(typ, storage);
        } else if (typ instanceof Address) {
            return this.decodeAddress(storage);
        } else if (typ instanceof UserDefinedType) {
            const def = typ.definition;

            if (def instanceof EnumDefinition) {
                return this.decodeEnum(def, storage);
            } else if (def instanceof ContractDefinition) {
                return this.decodeAddress(storage);
            } else if (def instanceof UserDefinedValueTypeDefinition) {
                const underlyingType = this.infer.typeNameToTypeNode(def.underlyingType);
                return this.decodeInner(underlyingType, storage);
            }
        }

        nyi(`NYI storage decode for simple type ${typ.pp()}`);
    }
}

export class PackedArrayStorageView extends BaseStorageArrayView<BytesType | StringType> {
    staticSize(): number {
        return 32;
    }

    private decodeContentsAddrAndLen(storage: Storage): [bigint, bigint] {
        assert(
            this.endOffsetInWord === 32,
            `Internal Error: Decoding bytes in the middle (off {0}) of word {1}`,
            this.endOffsetInWord,
            this.address
        );

        const word = fetchWord(this.address, storage);
        const lByte = word[31];

        if (lByte % 2 === 0) {
            /// Less than 31 bytes - length * 2 stored in lowest byte
            const len = lByte / 2;
            return [this.address, BigInt(len)];
        }

        const len = this.decodeInt(types.uint256, storage);
        return [keccakOfAddr(this.address), (len - BigInt(1)) / BigInt(2)];
    }

    decodeLen(storage: Storage): bigint {
        return this.decodeContentsAddrAndLen(storage)[1];
    }

    decodeIdx(storage: Storage, idx: bigint): BaseStorageView {
        const [contentsAddr, len] = this.decodeContentsAddrAndLen(storage);

        assert(idx < len, `OoB index {0} in bytes arr of len {1}`, idx, len);

        return new SimpleStorageView(
            types.byte,
            this.infer,
            contentsAddr + idx / 32n,
            Number(idx % 32n)
        );
    }

    private decodeInner(storage: Storage): Uint8Array | undefined {
        const [contentsAddr, len] = this.decodeContentsAddrAndLen(storage);
        // In both encodings the bytes contents start in the lowest bytes
        return fetchBytes(contentsAddr, 0, Number(len), storage);
    }

    decode(storage: Storage): Uint8Array | string | undefined {
        const bytes = this.decodeInner(storage);

        if (bytes === undefined || this.type instanceof BytesType) {
            return bytes;
        }

        return bytesToUtf8(bytes);
    }
}

export class ArrayStorageView extends BaseStorageArrayView<ArrayType> {
    staticSize(): number | undefined {
        if (this.type.size === undefined) {
            return 32;
        }

        const elView = makeStorageView(
            this.type.elementT,
            this.infer,
            this.address,
            this.endOffsetInWord
        );

        assert(
            this.type.size < MAX_ARR_DECODE_LIMIT,
            `Static sized array too large {0}`,
            this.type
        );

        const elSize = elView.staticSize();

        if (elSize === undefined) {
            return undefined;
        }

        let arrSize = Number(this.type.size) * elSize;
        // Values after an array always start in a new word
        arrSize += arrSize % 32 !== 0 ? 32 - (arrSize % 32) : 0;
        return arrSize;
    }

    decodeLenAndFirstElView(storage: Storage): [bigint, BaseStorageView] {
        let len: bigint;
        let elView: BaseStorageView;

        if (this.type.size !== undefined) {
            len = BigInt(this.type.size);
            elView = makeStorageView(
                this.type.elementT,
                this.infer,
                keccakOfAddr(this.address),
                32
            );
        } else {
            len = this.decodeInt(types.uint256, storage);
            elView = makeStorageView(
                this.type.elementT,
                this.infer,
                this.address,
                this.endOffsetInWord
            );
        }

        return [len, elView];
    }

    decodeLen(storage: Storage): bigint {
        return this.decodeLenAndFirstElView(storage)[0];
    }

    decodeIdx(storage: Storage, idx: bigint): BaseStorageView | undefined {
        const [len, elView] = this.decodeLenAndFirstElView(storage);
        const elSize = elView.staticSize();

        if (elSize === undefined) {
            return undefined;
        }

        // TODO: Compute actual elSize including rounding up to next one
        // This is an n-to-m mapping! E.g. fit 10 uint3 into 1 word
        let nEls;
        let nWords;

        if (elSize <= 32) {
            nEls = 32 / elSize;
            nWords = 1;
        } else {
            nEls = 1;
            nWords = elSize / 32 + (elSize % 32) === 0 ? 0 : 1;
        }

        assert(idx < len, `Oob lookup {0} in array of length {1}`, idx, len);
        assert(idx < MAX_ARR_DECODE_LIMIT, `TODO: Replace this with optional return`);

        const numIdx = Number(idx);
        const off = (numIdx / nEls) * nWords + (numIdx % nEls) * elSize;
        elView.shiftBy(off);

        return elView;
    }

    decode(storage: Storage): SolValue {
        let numLen: number;
        let elView: BaseStorageView;

        const res: any[] = [];

        if (this.type.size === undefined) {
            const len = this.decodeLen(storage);

            //console.error(`stor_decodeArray: Decoded len ${len} at loc ${ppLoc(loc)}`);
            if (len > MAX_ARR_DECODE_LIMIT) {
                return undefined;
            }

            numLen = Number(len);
            elView = makeStorageView(
                this.type.elementT,
                this.infer,
                keccakOfAddr(this.address),
                32
            );
        } else {
            if (this.type.size > MAX_ARR_DECODE_LIMIT) {
                return undefined;
            }

            numLen = Number(this.type.size);
            elView = makeStorageView(
                this.type.elementT,
                this.infer,
                this.address,
                this.endOffsetInWord
            );
        }

        const elSize = elView.staticSize();

        if (elSize === undefined) {
            return undefined;
        }

        for (let i = 0; i < numLen; i++) {
            const elRes = elView.decode(storage);

            if (elRes === undefined) {
                return undefined;
            }

            res.push(elRes[0]);

            elView.shiftBy(elSize);
        }

        return res;
    }
}

export class StructStorageView extends BaseStorageView<UserDefinedType> implements StructView {
    private decodeFieldsAndSize(): [BaseStorageView[], number] | undefined {
        const def = this.type.definition;

        assert(
            def instanceof StructDefinition,
            `stor_decodeStruct expects a struct, not {0}`,
            this.type
        );

        assert(
            this.endOffsetInWord === 32,
            `Internal Error: thisation of struct of type {0} doesn't start at the end of word {1} - instead at off {2}`,
            this.type,
            this.address,
            this.endOffsetInWord
        );

        const fields: BaseStorageView[] = [];

        let address: bigint = this.address;
        let endOffsetInWord: number = this.endOffsetInWord;
        let lastFieldSize: number | undefined = 0;
        let size = 0;

        for (const field of def.vMembers) {
            let fieldGenT: TypeNode;

            try {
                fieldGenT = this.infer.variableDeclarationToTypeNode(field);
            } catch (e) {
                return undefined;
            }

            const fieldT = changeToLocation(fieldGenT, DataLocation.Storage);

            if (lastFieldSize !== 0) {
                [address, endOffsetInWord] = shiftBy(address, endOffsetInWord, lastFieldSize);

                if (!typeCanStartInLoc(fieldT, this.infer, endOffsetInWord)) {
                    address += 1n;
                    size += endOffsetInWord;
                    endOffsetInWord = 32;
                }
            }

            const fieldView = makeStorageView(fieldT, this.infer, address, endOffsetInWord);
            fields.push(fieldView);
            lastFieldSize = fieldView.staticSize();

            if (lastFieldSize === undefined) {
                return undefined;
            }

            size += lastFieldSize;
        }

        // Values after a struct must start in a new slot
        if (endOffsetInWord % 32 !== 0) {
            size += endOffsetInWord;
        }

        return [fields, size];
    }

    decode(storage: Storage): object | undefined {
        const def = this.type.definition;

        assert(
            def instanceof StructDefinition,
            `stor_decodeStruct expects a struct, not {0}`,
            this.type
        );

        const fieldsAndSize = this.decodeFieldsAndSize();

        if (fieldsAndSize === undefined) {
            return undefined;
        }

        const [fields] = fieldsAndSize;
        const res: any = {};

        for (let i = 0; i < fields.length; i++) {
            const fieldDecl = def.vMembers[i];
            const fieldView = fields[i];

            const fieldVal = fieldView.decode(storage);

            if (fieldVal === undefined) {
                return undefined;
            }

            res[fieldDecl.name] = fieldVal;
        }

        return res;
    }

    decodeField(arg: any, field: string): BaseStorageView | undefined {
        const def = this.type.definition;

        assert(
            def instanceof StructDefinition,
            `stor_decodeStruct expects a struct, not {0}`,
            this.type
        );

        const fieldIdx = def.vMembers.findIndex((decl) => decl.name === field);

        if (fieldIdx === -1) {
            return undefined;
        }

        const fieldsAndSize = this.decodeFieldsAndSize();

        if (fieldsAndSize === undefined) {
            return undefined;
        }

        return fieldsAndSize[0][fieldIdx];
    }

    staticSize(): number | undefined {
        const fieldsAndSize = this.decodeFieldsAndSize();

        if (fieldsAndSize === undefined) {
            return undefined;
        }

        return fieldsAndSize[1];
    }
}

export class PointerStorageView extends BaseStorageView {
    staticSize(): number | undefined {
        // In storage we don't store explicit pointers. Instead we have several
        // cases for the "pointer" slot:
        // - for dynamically sized arrays just the length (32 bytes)
        // - for statically sized arrays - the array begins immediately in the pointer slot (at least 32 bytes)
        // - for maps the slot is empty - (32 bytes)
        // - for structs the first element of the struct is there
        //
        // In all cases we leave the static size computation to the underlying type.
        return this.decodeInnerView().staticSize();
    }

    decode(storage: Storage): SolValue {
        return this.decodeInnerView().decode(storage);
    }

    decodeInnerView(): BaseStorageView {
        if (!(this.type instanceof PointerType)) {
            nyi(`Unexpected pointer type.`);
        }

        if (this.type.to instanceof BytesType || this.type.to instanceof StringType) {
            return new PackedArrayStorageView(
                this.type.to,
                this.infer,
                this.address,
                this.endOffsetInWord
            );
        }

        if (this.type.to instanceof UserDefinedType) {
            if (this.type.to.definition instanceof StructDefinition) {
                return new StructStorageView(
                    this.type.to,
                    this.infer,
                    this.address,
                    this.endOffsetInWord
                );
            }
        }

        if (this.type.to instanceof ArrayType) {
            return new ArrayStorageView(
                this.type.to,
                this.infer,
                this.address,
                this.endOffsetInWord
            );
        }

        nyi(`Pointer type ${this.type.pp()}`);
    }
}
