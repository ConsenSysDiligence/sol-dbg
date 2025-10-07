import { assert, stringToBytes } from "solc-typed-ast";
import { DecodingFailure, Struct, Value } from "../value";
import {
    ArrayLikeView,
    EncodingError,
    IndexableView,
    PointerView,
    StructView,
    View
} from "../view";
import { Storage } from "../../types";
import {
    address,
    bigEndianBufToBigint,
    bigIntToBuf,
    bool,
    byte,
    encodeBigintInBigEndianBuf,
    fits,
    MAX_ARR_DECODE_LIMIT,
    min,
    nyi,
    uint256,
    uint8,
    ZERO_BYTES32
} from "../../../utils";
import { keccak256 } from "ethereum-cryptography/keccak";
import { Address, bytesToUtf8, concatBytes } from "@ethereumjs/util";
import { MapKeys } from "../../tracers";
import { makeMemoryView } from "../memory";
import { inRange, isFailure, isTypeStringStatic32BytesInStorage } from "../utils";
import { BaseMemoryView, IntMemView } from "../memory/view";
import { bytesToHex, equalsBytes, utf8ToBytes } from "ethereum-cryptography/utils";
import {
    AddressType,
    ArrayType,
    BaseRuntimeType,
    BoolType,
    BytesType,
    FixedBytesType,
    IntType,
    MappingType,
    MissingType,
    PointerType,
    StringType,
    StructType
} from "../../runtime_types";

interface ArrayLikeStorageView<ValViewT extends BaseStorageView<Value, BaseRuntimeType>>
    extends ArrayLikeView<Storage, ValViewT> {}
type StorageLocation = [bigint, number];

export function isArrayLikeStorageView(
    v: any
): v is ArrayLikeStorageView<BaseStorageView<Value, BaseRuntimeType>> {
    return (
        v instanceof FixedBytesStorageView ||
        v instanceof ArrayStorageView ||
        v instanceof BytesStorageView
    );
}

function move(loc: StorageLocation, byBytes: number): StorageLocation {
    const [key, endOffsetInWord] = loc;
    assert(endOffsetInWord >= byBytes, ``);
    return endOffsetInWord === byBytes ? [key + 1n, 32] : [key, endOffsetInWord - byBytes];
}

export abstract class BaseStorageView<
    Val extends Value,
    Type extends BaseRuntimeType = BaseRuntimeType
> extends View<Storage, Val, [bigint, number], Type> {
    key: bigint;
    endOffsetInWord: number;

    constructor(type: Type, loc: [bigint, number]) {
        super(type, loc);
        [this.key, this.endOffsetInWord] = loc;
    }

    /**
     * Helper to fetch the word residing at key `key` from `storage`. If the key is missing return an all-0 value.
     */
    protected fetchWord(key: bigint, storage: Storage): Uint8Array {
        const keyHash = bigEndianBufToBigint(keccak256(bigIntToBuf(key, 32, "big")));
        const res = storage.get(keyHash);

        if (res === undefined) {
            return new Uint8Array(32);
        }

        return res;
    }

    /**
     * Helper to fetch the word residing at key `key` from `storage`. If the key is missing return an all-0 value.
     * Unlike `fetchWord` the returned Uint8Array does not alias the array in storage.
     */
    protected fetchWordCopy(key: bigint, storage: Storage): Uint8Array {
        const keyHash = bigEndianBufToBigint(keccak256(bigIntToBuf(key, 32, "big")));
        const res = storage.get(keyHash);

        if (res === undefined) {
            return new Uint8Array(32);
        }

        return new Uint8Array(res);
    }

    protected setWord(key: bigint, value: Uint8Array, storage: Storage): Storage {
        const keyHash = bigEndianBufToBigint(keccak256(bigIntToBuf(key, 32, "big")));

        if (equalsBytes(value, ZERO_BYTES32)) {
            return storage.delete(keyHash);
        }

        return storage.set(keyHash, value);
    }

    /**
     * Helper to fetch `numBytes` bytes from `storage` starting at offset `off`.
     */
    protected fetchBytes(
        wordOff: bigint,
        offInWord: number,
        numBytes: number,
        state: Storage
    ): Uint8Array {
        let curBuf = this.fetchWord(wordOff, state);
        const res = new Uint8Array(numBytes);

        /**
         * @todo(dimo) One byte at a time copying is inefficient.
         * See if we can copy slices
         */
        for (let i = 0; i < numBytes; i++) {
            res[i] = curBuf[offInWord];

            offInWord = (offInWord + 1) % 32;

            if (offInWord === 0 && i < numBytes - 1) {
                wordOff++;

                curBuf = this.fetchWord(wordOff, state);
            }
        }

        return res;
    }

    /**
     * Helper to decode an int type at a given location in storage.
     */
    protected decodeIntAt(
        key: bigint,
        endOffsetInWord: number,
        type: IntType,
        state: Storage
    ): bigint | DecodingFailure {
        const size = type.numBits / 8;

        if (endOffsetInWord < size) {
            return new DecodingFailure(
                `Internal Error: Can't decode ${type.pp()} starting at offset ${endOffsetInWord} in word ${key}`
            );
        }

        const rawBytes = this.fetchBytes(key, endOffsetInWord - size, size, state);

        let res = bigEndianBufToBigint(rawBytes);

        // Convert signed negative 2's complement values
        if (type.signed && (res & (BigInt(1) << BigInt(type.numBits - 1))) !== BigInt(0)) {
            // Mask out any 1's above the number's size
            res = res & ((BigInt(1) << BigInt(type.numBits)) - BigInt(1));
            res = -((BigInt(1) << BigInt(type.numBits)) - res);
        }

        if (!fits(res, type)) {
            return new DecodingFailure(
                `Decoded value ${res} from ${[key, endOffsetInWord]} doesn't fit in expected typee ${type.pp()}`
            );
        }

        return res;
    }

    protected encodeIntAt(
        val: bigint,
        key: bigint,
        endOffsetInWord: number,
        type: IntType,
        state: Storage
    ): Storage {
        const size = type.numBits / 8;

        if (endOffsetInWord < size) {
            throw new EncodingError(
                `Internal Error: Can't decode ${type.pp()} starting at offset ${endOffsetInWord} in word ${key}`
            );
        }

        if (!fits(val, type)) {
            throw new EncodingError(
                `Decoded value ${val} from ${[key, endOffsetInWord]} doesn't fit in expected typee ${type.pp()}`
            );
        }

        const word = this.fetchWordCopy(key, state);
        encodeBigintInBigEndianBuf(val, word, type.numBits / 8, endOffsetInWord);
        return this.setWord(key, word, state);
    }

    pp(): string {
        return `<${this.type.pp()}@${this.loc} in storage>`;
    }

    /**
     * The first location after the end of this view
     */
    abstract nextLoc(): StorageLocation | undefined;
    abstract decode(state: Storage, mapKeys?: MapKeys): Val | DecodingFailure;

    abstract encode(value: Val, state: Storage): Storage;
}

export class IntStorageView extends BaseStorageView<bigint, IntType> {
    decode(state: Storage): bigint | DecodingFailure {
        return this.decodeIntAt(this.key, this.endOffsetInWord, this.type, state);
    }

    nextLoc(): StorageLocation {
        return move(this.loc, this.type.numBits / 8);
    }

    encode(value: bigint, state: Storage): Storage {
        return this.encodeIntAt(value, this.key, this.endOffsetInWord, this.type, state);
    }
}

export class BoolStorageView extends BaseStorageView<boolean, BoolType> {
    decode(state: Storage): boolean | DecodingFailure {
        const byte = this.decodeIntAt(this.key, this.endOffsetInWord, uint8, state);

        if (isFailure(byte)) {
            return byte;
        }

        return byte !== BigInt(0);
    }

    encode(value: boolean, state: Storage): Storage {
        return this.encodeIntAt(value ? 1n : 0n, this.key, this.endOffsetInWord, uint8, state);
    }

    nextLoc(): StorageLocation {
        return move(this.loc, 1);
    }
}

export class AddressStorageView extends BaseStorageView<Address, AddressType> {
    decode(state: Storage): Address | DecodingFailure {
        if (this.endOffsetInWord < 20) {
            return new DecodingFailure(
                `Unalighed read: Can't decode ${this.type.pp()} starting at offset ${this.endOffsetInWord} in word ${this.key}`
            );
        }

        const bytes = this.fetchBytes(this.key, this.endOffsetInWord - 20, 20, state);
        return new Address(bytes);
    }

    encode(value: Address, state: Storage): Storage {
        if (this.endOffsetInWord < 20) {
            throw new EncodingError(
                `Unalighed read: Can't decode ${this.type.pp()} starting at offset ${this.endOffsetInWord} in word ${this.key}`
            );
        }

        const word = this.fetchWordCopy(this.key, state);
        word.set(value.bytes, this.endOffsetInWord - 20);
        return this.setWord(this.key, word, state);
    }

    nextLoc(): StorageLocation {
        return move(this.loc, 20);
    }
}

/**
 * For consisntency with Memory views, add a SingleByteStorage view that returns a number
 */
export class SingleByteStorageView extends BaseStorageView<bigint, FixedBytesType> {
    constructor(loc: [bigint, number]) {
        super(byte, loc);
    }

    decode(state: Storage): bigint | DecodingFailure {
        if (this.endOffsetInWord < this.type.numBytes) {
            return new DecodingFailure(
                `Unalighed Read: Can't decode ${this.type.pp()} starting at offset ${this.endOffsetInWord} in word ${this.key}`
            );
        }

        const byte = this.fetchBytes(
            this.key,
            this.endOffsetInWord - this.type.numBytes,
            this.type.numBytes,
            state
        );

        return BigInt(byte[0]);
    }

    encode(value: bigint, state: Storage): Storage {
        const word = this.fetchWordCopy(this.key, state);

        if (!inRange(value, 0, 255)) {
            throw new EncodingError(`${value} not in byte range [0, 255]`);
        }

        word[this.endOffsetInWord - this.type.numBytes] = Number(value);
        return this.setWord(this.key, word, state);
    }

    nextLoc(): StorageLocation {
        return move(this.loc, this.type.numBytes);
    }
}

export class FixedBytesStorageView
    extends BaseStorageView<Uint8Array, FixedBytesType>
    implements ArrayLikeStorageView<SingleByteStorageView>
{
    decode(state: Storage): Uint8Array | DecodingFailure {
        if (this.endOffsetInWord < this.type.numBytes) {
            return new DecodingFailure(
                `Unalighed Read: Can't decode ${this.type.pp()} starting at offset ${this.endOffsetInWord} in word ${this.key}`
            );
        }

        return this.fetchBytes(
            this.key,
            this.endOffsetInWord - this.type.numBytes,
            this.type.numBytes,
            state
        );
    }

    encode(value: Uint8Array, state: Storage): Storage {
        if (this.endOffsetInWord < this.type.numBytes) {
            throw new EncodingError(
                `Unalighed read: Can't decode ${this.type.pp()} starting at offset ${this.endOffsetInWord} in word ${this.key}`
            );
        }

        const word = this.fetchWordCopy(this.key, state);
        word.set(value, this.endOffsetInWord - this.type.numBytes);
        return this.setWord(this.key, word, state);
    }

    indexView(key: bigint): DecodingFailure | SingleByteStorageView {
        if (!inRange(key, 0, this.type.numBytes)) {
            return new DecodingFailure(`Invalid index ${key} in ${this.type.pp()}`);
        }

        return new SingleByteStorageView([
            this.key,
            this.endOffsetInWord - this.type.numBytes + Number(key) + 1
        ]);
    }

    nextLoc(): StorageLocation {
        return move(this.loc, this.type.numBytes);
    }

    size(): bigint | DecodingFailure {
        return BigInt(this.type.numBytes);
    }
}

export class PointerStorageView
    extends BaseStorageView<Value, PointerType>
    implements PointerView<Storage, BaseStorageView<Value, BaseRuntimeType>>
{
    innerView: BaseStorageView<Value, BaseRuntimeType>;
    constructor(type: PointerType, loc: [bigint, number]) {
        super(type, loc);
        this.innerView = makeStorageView(type.toType, loc);
    }

    decode(state: Storage, mapKeys?: MapKeys): Value {
        return this.innerView.decode(state, mapKeys);
    }

    encode(value: Value, state: Storage): Storage {
        return this.innerView.encode(value, state);
    }

    nextLoc(): StorageLocation | undefined {
        return this.innerView.nextLoc();
    }

    toView(): BaseStorageView<Value, BaseRuntimeType> {
        return this.innerView;
    }
}

function keccakOfAddr(addr: bigint): bigint {
    const addrBuf = bigIntToBuf(addr, 32, "big");
    const hashBuf = keccak256(addrBuf);

    return bigEndianBufToBigint(hashBuf);
}

export class ArrayStorageView
    extends BaseStorageView<Value[], ArrayType>
    implements ArrayLikeStorageView<BaseStorageView<Value, BaseRuntimeType>>
{
    private _nextLoc: StorageLocation | undefined;

    /**
     * Helper to compute how many words elements "take". This is not straightforward, as we may pack multiple
     * elements per word, or one element may take multiple words. So we return two values - nEls nad nWords.
     * I.e. we state that we can fit `nEls` elements in `nWords`, where this is the smalles "package" possible.
     * @param elT
     */
    static computeElmentSize(elT: BaseRuntimeType): [bigint, bigint] | undefined {
        let tmpL: StorageLocation | undefined = [0n, 32];
        let nEls = 0n;

        while (typeFitsInLoc(elT, tmpL) && tmpL[0] === 0n) {
            nEls++;
            const elView = makeStorageView(elT, tmpL);
            tmpL = elView.nextLoc();

            if (tmpL === undefined) {
                break;
            }
        }

        if (tmpL === undefined) {
            return undefined;
        }

        // Number of words needed for nEls elements
        const nWords = tmpL[0] + (tmpL[1] === 32 ? 0n : 1n);
        return [nEls, nWords];
    }

    constructor(type: ArrayType, loc: [bigint, number]) {
        super(type, loc);

        if (type.size === undefined) {
            this._nextLoc = nextWord(loc);
        } else {
            // Dirty way to compute how many elements fit in how many words
            const elSizeDesc = ArrayStorageView.computeElmentSize(this.type.elementT);

            if (elSizeDesc === undefined) {
                this._nextLoc = undefined;
            } else {
                const [nEls, nWords] = elSizeDesc;
                // Number of groups of "nEls" needed to fit in size
                const nGroups = type.size / nEls + (type.size % nEls === 0n ? 0n : 1n);
                // Number of words for the entire fixed sized array.
                // Note that the last groups may have fewer than nEls elements.
                // But it will still consume the same number of words. Its either 1 word,
                // Or its a 1-element group that takes multiple words.
                const nWordsPerArray = nGroups * nWords;
                this._nextLoc = [this.loc[0] + nWordsPerArray, 32];
            }
        }
    }

    nextLoc(): StorageLocation | undefined {
        return this._nextLoc;
    }

    decode(state: Storage, mapKeys?: MapKeys): Value[] | DecodingFailure {
        let sizeBigint: bigint | DecodingFailure;
        let contentsKey: bigint;

        if (this.type.size) {
            sizeBigint = this.type.size;
            contentsKey = this.key;
        } else {
            sizeBigint = this.decodeIntAt(this.key, this.endOffsetInWord, uint256, state);

            if (isFailure(sizeBigint)) {
                return sizeBigint;
            }

            contentsKey = keccakOfAddr(this.key);
        }

        if (!inRange(sizeBigint, 0n, MAX_ARR_DECODE_LIMIT)) {
            return new DecodingFailure(`Array too large to decode ${sizeBigint}`);
        }

        const size = Number(sizeBigint);
        const res: Value[] = [];
        let elLoc: StorageLocation | undefined = [contentsKey, 32];
        const elT = this.type.elementT;

        for (let i = 0; i < size; i++) {
            if (elLoc === undefined) {
                res.push(new DecodingFailure(`Failed earlier in array`));
            } else {
                const view = makeStorageView(elT, elLoc);
                res.push(view.decode(state, mapKeys));
                elLoc = view.nextLoc();
            }
        }

        return res;
    }

    encode(value: Value[], state: Storage): Storage {
        let s: Storage = state;

        if (this.type.size !== undefined && BigInt(value.length) !== this.type.size) {
            throw new EncodingError(
                `Invalid length ${value.length} for encoding an array of type ${this.type.pp()}`
            );
        }

        const size = BigInt(value.length);
        let baseKey = this.key;
        let stateAfterFirstAssign: Storage | undefined;

        if (this.type.size === undefined) {
            s = this.encodeIntAt(size, this.key, this.endOffsetInWord, uint256, s);
            stateAfterFirstAssign = s;
            baseKey = keccakOfAddr(baseKey);
        }

        let elLoc: StorageLocation | undefined = [baseKey, 32];

        for (let i = 0; i < size; i++) {
            const view = makeStorageView(this.type.elementT, elLoc);
            s = view.encode(value[i], s);

            if (stateAfterFirstAssign === undefined) {
                stateAfterFirstAssign = s;
            }

            elLoc = view.nextLoc();
            assert(
                elLoc !== undefined,
                `Internal error: elLoc shouldnt be undefined in ArrayStorageView.encode`
            );
        }

        return stateAfterFirstAssign !== undefined ? s.collapseUntil(stateAfterFirstAssign) : s;
    }

    indexView(
        key: bigint,
        state: Storage
    ): DecodingFailure | BaseStorageView<Value, BaseRuntimeType> {
        let size: bigint | DecodingFailure;
        let addr: bigint;

        if (this.type.size) {
            size = this.type.size;
            addr = this.key;
        } else {
            size = this.decodeIntAt(this.key, this.endOffsetInWord, uint256, state);

            if (isFailure(size)) {
                return size;
            }

            addr = keccakOfAddr(this.key);
        }

        if (!inRange(key, 0n, size - 1n)) {
            return new DecodingFailure(`Invalid index ${key} in array of size ${size}`);
        }

        // @todo I should cache this
        const elSizeDesc = ArrayStorageView.computeElmentSize(this.type.elementT);

        if (elSizeDesc === undefined) {
            return new DecodingFailure(`Couldnt determine element size`);
        }

        const [nEls, nWords] = elSizeDesc;

        const word = addr + (key / nEls) * nWords;

        const endOffsetInWord = typeStartsInNewWord(this.type.elementT)
            ? 32
            : 32 - Number(key % nEls) * staticSize(this.type.elementT);
        return makeStorageView(this.type.elementT, [word, endOffsetInWord]);
    }

    size(state: Storage): bigint | DecodingFailure {
        if (this.type.size) {
            return this.type.size;
        }

        return this.decodeIntAt(this.key, this.endOffsetInWord, uint256, state);
    }
}

export class StructStorageView
    extends BaseStorageView<Struct, StructType>
    implements StructView<Storage, BaseStorageView<Value, BaseRuntimeType>>
{
    fieldViews: Array<[string, BaseStorageView<Value, BaseRuntimeType>]> = [];
    private _nextLoc: StorageLocation | undefined;

    constructor(type: StructType, loc: StorageLocation) {
        super(type, loc);
        assert(this.endOffsetInWord === 32, `Structs must start at 32 byte boundaries`);

        let fieldLoc: StorageLocation | undefined = this.loc;

        for (const [name, fieldT] of this.type.fields) {
            const fieldView: BaseStorageView<Value, BaseRuntimeType> =
                fieldLoc === undefined
                    ? new MissingStorageView(new MissingType(undefined), [-1n, 32])
                    : makeStorageView(fieldT, fieldLoc);
            this.fieldViews.push([name, fieldView]);
            fieldLoc = fieldView.nextLoc();
        }

        this._nextLoc =
            fieldLoc === undefined ? undefined : fieldLoc[1] === 32 ? fieldLoc : nextWord(fieldLoc);
    }

    nextLoc(): StorageLocation | undefined {
        return this._nextLoc;
    }

    decode(state: Storage, mapKeys?: MapKeys): Struct {
        const entries: Array<[string, Value]> = this.fieldViews.map(([name, view]) => [
            name,
            view.decode(state, mapKeys)
        ]);
        return new Struct(entries);
    }

    encode(value: Struct, state: Storage): Storage {
        if (value.entries.length !== this.fieldViews.length) {
            throw new EncodingError(
                `Mismatch in number of fields in encoding of ${value} to ${this.type.pp()}`
            );
        }

        let s = state;
        let stateAfterFirstAssign: Storage | undefined;

        for (let i = 0; i < value.entries.length; i++) {
            s = this.fieldViews[i][1].encode(value.entries[i][1], s);

            if (stateAfterFirstAssign === undefined) {
                stateAfterFirstAssign = s;
            }
        }

        return stateAfterFirstAssign !== undefined ? s.collapseUntil(stateAfterFirstAssign) : s;
    }

    fieldView(name: string): BaseStorageView<Value, BaseRuntimeType> | DecodingFailure {
        for (const [fieldName, fieldView] of this.fieldViews) {
            if (name === fieldName) {
                return fieldView;
            }
        }
        return new DecodingFailure(`No field ${name} on type ${this.type.pp()}`);
    }
}

function decodeMapRefKey(type: BaseRuntimeType, data: Uint8Array): string {
    if (!(type instanceof StringType || type instanceof BytesType)) {
        throw new Error(`Invalid map reference key type ${type.pp()}`);
    }

    return type instanceof StringType ? bytesToUtf8(data) : bytesToHex(data);
}

function encodeMapKey(keyT: BaseRuntimeType, value: Value): Uint8Array {
    if (keyT instanceof PointerType) {
        if (!(keyT.toType instanceof StringType || keyT.toType instanceof BytesType)) {
            throw new Error(`Invalid map reference key type ${keyT.pp()}`);
        }

        return keyT.toType instanceof StringType
            ? utf8ToBytes(value as string)
            : (value as Uint8Array);
    }

    const buf = new Uint8Array(32);
    const keyV = makeMemoryView(keyT, 0n);
    keyV.encode(value, buf, undefined as any);
    return buf;
}

export class MapStorageView
    extends BaseStorageView<Map<Value, Value>, MappingType>
    implements IndexableView<Value, Storage, BaseStorageView<Value, BaseRuntimeType>>
{
    constructor(type: MappingType, loc: StorageLocation) {
        super(type, loc);
    }

    nextLoc(): StorageLocation {
        return nextWord(this.loc);
    }

    decode(state: Storage, mapKeys?: MapKeys): Map<Value, Value> {
        if (mapKeys === undefined) {
            return new Map();
        }

        const candidateKeys = mapKeys.get(this.loc[0]);
        const res = new Map<Value, Value>();

        if (candidateKeys === undefined) {
            return res;
        }

        let keyView: BaseMemoryView<Value, BaseRuntimeType> | undefined;

        if (!(this.type.keyType instanceof PointerType)) {
            keyView = makeMemoryView(this.type.keyType, 0n);
        }

        // @todo(dimo) Would it be better here to check that `candidateSlot` is an explicitly defined in storage, and not just a 0 by default?
        for (const [candidateKey, candidateSlot] of candidateKeys) {
            let decodedKey: Value;

            if (keyView !== undefined) {
                decodedKey = keyView.decode(candidateKey);
            } else {
                decodedKey = decodeMapRefKey(
                    (this.type.keyType as PointerType).toType,
                    candidateKey
                );
            }

            const valueView = makeStorageView(this.type.valueType, [candidateSlot, 32]);
            const decodedValue = valueView.decode(state, mapKeys);

            if (!isFailure(decodedKey) && !isFailure(decodedValue)) {
                res.set(decodedKey, decodedValue);
            }
        }

        return res;
    }

    encode(value: Map<Value, Value>, state: Storage): Storage {
        // Encode the current slot in the buffer `slot`
        const valueT = this.type.valueType;
        const slotBuf = new Uint8Array(32);
        const memView = new IntMemView(uint256, 0n);
        memView.encode(this.loc[0], slotBuf);

        for (const [k, v] of value) {
            // Compute the concatenation h(k) . p as per
            // https://docs.soliditylang.org/en/latest/internals/layout_in_storage.html#mappings-and-dynamic-arrays
            const keyBuf = encodeMapKey(this.type.keyType, k);
            const combinedBuf = concatBytes(keyBuf, slotBuf);
            const keySlot = bigEndianBufToBigint(keccak256(combinedBuf));

            const valueView = makeStorageView(valueT, [keySlot, 32]);
            state = valueView.encode(v, state);
        }

        return state;
    }

    indexView(key: Value): DecodingFailure | BaseStorageView<Value, BaseRuntimeType> {
        const slotBuf = new Uint8Array(32);
        const memView = new IntMemView(uint256, 0n);
        memView.encode(this.loc[0], slotBuf);
        const keyBuf = encodeMapKey(this.type.keyType, key);
        const combinedBuf = concatBytes(keyBuf, slotBuf);
        const keySlot = bigEndianBufToBigint(keccak256(combinedBuf));
        return makeStorageView(this.type.valueType, [keySlot, 32]);
    }
}

export abstract class PackedArrayStorageView<
    V extends Value,
    T extends BaseRuntimeType
> extends BaseStorageView<V, T> {
    nextLoc(): StorageLocation {
        return nextWord(this.loc);
    }

    protected getSize(state: Storage): bigint | DecodingFailure {
        const word = this.fetchWord(this.key, state);
        const lByte = word[31];

        if (lByte % 2 === 0) {
            /// Less than 31 bytes - length * 2 stored in lowest byte
            return BigInt(lByte / 2);
        }

        const len = this.decodeIntAt(this.key, this.endOffsetInWord, uint256, state);

        if (isFailure(len)) {
            return len;
        }

        return (len - 1n) / 2n;
    }

    decodeBytes(state: Storage): Uint8Array | DecodingFailure {
        const word = this.fetchWord(this.key, state);
        const lByte = word[31];

        if (lByte % 2 === 0) {
            /// Less than 31 bytes - length * 2 stored in lowest byte
            const len = lByte / 2;
            assert(len <= 31, `Unexpected length of more than 31`);

            return word.slice(0, len);
        }

        let len = this.decodeIntAt(this.key, this.endOffsetInWord, uint256, state);

        if (isFailure(len)) {
            return len;
        }

        len = (len - 1n) / 2n;

        if (!inRange(len, 0n, MAX_ARR_DECODE_LIMIT)) {
            return new DecodingFailure(`${this.type.pp()} too large - ${len}`);
        }

        const numLen = Number(len);
        const addr = keccakOfAddr(this.key);

        return this.fetchBytes(addr, 0, numLen, state);
    }

    encodeBytesAt(bytes: Uint8Array, slot: bigint, state: Storage): Storage {
        if (bytes.length < 32) {
            const w = this.fetchWordCopy(slot, state);
            w[31] = 2 * bytes.length;
            w.set(bytes, 0);
            return this.setWord(slot, w, state);
        }

        let s = this.encodeIntAt(BigInt(2 * bytes.length + 1), slot, 32, uint256, state);
        const stateAfterLen = s;
        let addr = keccakOfAddr(this.key);
        let srcOff = 0;
        while (srcOff < bytes.length) {
            const end = min(srcOff + 32, bytes.length);
            let w: Uint8Array;

            if (end - srcOff === 32) {
                w = bytes.slice(srcOff, end);
            } else {
                w = this.fetchWordCopy(addr, s);
                w.set(bytes.slice(srcOff, end));
            }

            s = this.setWord(addr, w, s);
            addr++;
            srcOff += 32;
        }

        return s.collapseUntil(stateAfterLen);
    }
}

export class BytesStorageView
    extends PackedArrayStorageView<Uint8Array, BytesType>
    implements ArrayLikeStorageView<SingleByteStorageView>
{
    decode(state: Storage): Uint8Array | DecodingFailure {
        return this.decodeBytes(state);
    }

    encode(value: Uint8Array, state: Storage): Storage {
        return this.encodeBytesAt(value, this.key, state);
    }

    indexView(key: bigint, state: Storage): DecodingFailure | SingleByteStorageView {
        const size = this.getSize(state);

        if (isFailure(size)) {
            return size;
        }

        if (key < 0n || key >= size) {
            return new DecodingFailure(`Invalid index ${key} in bytes of size ${size}`);
        }

        if (size < 32) {
            return new SingleByteStorageView([this.key, Number(key) + 1]);
        }

        const base = keccakOfAddr(this.key);
        const word = base + key / 32n;
        const endOffsetInWord = Number(key % 32n) + 1;

        return new SingleByteStorageView([word, endOffsetInWord]);
    }

    size(state: Storage): bigint | DecodingFailure {
        return this.getSize(state);
    }
}

export class StringStorageView extends PackedArrayStorageView<string, StringType> {
    decode(state: Storage): string | DecodingFailure {
        const bytes = this.decodeBytes(state);

        if (isFailure(bytes)) {
            return bytes;
        }

        return bytesToUtf8(bytes);
    }

    encode(value: string, state: Storage): Storage {
        return this.encodeBytesAt(stringToBytes(value), this.key, state);
    }
}

export class MissingStorageView extends BaseStorageView<DecodingFailure, MissingType> {
    constructor(type: MissingType, loc: StorageLocation) {
        super(type, loc);

        if (this.type.typeString !== undefined) {
            if (isTypeStringStatic32BytesInStorage(this.type.typeString)) {
                assert(
                    this.endOffsetInWord === 32,
                    `Unexpected non-word aligned {0} in storage`,
                    this.type.typeString
                );
            }
        }
    }

    decode(): DecodingFailure {
        const typeStr = this.type.typeString;
        return new DecodingFailure(`missing ${typeStr ? typeStr : "<unknown>"}`);
    }

    encode(): Storage {
        throw new EncodingError(`Cannot encode a missing value`);
    }

    nextLoc(): StorageLocation | undefined {
        if (this.type.typeString === undefined) {
            return undefined;
        }

        const typeString = this.type.typeString;
        // If we can guess this is a dynamic array or mapping from the typestring, then we know the nextLoc
        if (isTypeStringStatic32BytesInStorage(typeString)) {
            return nextWord(this.loc);
        }

        return undefined;
    }
}

/**
 * Return true if the given type `typ` fits in the storage word location pointed by
 * `loc`. This checks that the type actually fits, and that its not one of the types
 * that need to start in their own word (Arrays and Structs)
 * @param typ
 * @param loc
 * @param infer
 * @returns
 */
function typeFitsInLoc(typ: BaseRuntimeType, loc: StorageLocation): boolean {
    const [, endOffsetInWord] = loc;

    if (typeStartsInNewWord(typ)) {
        return endOffsetInWord == 32;
    }

    return staticSize(typ) <= endOffsetInWord;
}

export function nextWord(loc: StorageLocation): StorageLocation {
    return [loc[0] + 1n, 32];
}

function typeStartsInNewWord(t: BaseRuntimeType): boolean {
    if (t instanceof PointerType) {
        return typeStartsInNewWord(t.toType);
    }

    return t instanceof ArrayType || t instanceof MappingType || t instanceof StructType;
}

/**
 * Compute the 'static' size that a variable of primitive type `typ` would take
 * up in storage.  Primitive here means not an array, struct or a map. Those
 * have special layout rules (always start in a new word, next item in layout
 * starts in own word too).
 */
function staticSize(typ: BaseRuntimeType): number {
    if (typ instanceof IntType) {
        return typ.numBits / 8;
    }

    if (typ instanceof FixedBytesType) {
        return typ.numBytes;
    }

    if (typ instanceof BoolType) {
        return 1;
    }

    if (typ instanceof AddressType) {
        return 20;
    }

    if (typ instanceof BytesType || typ instanceof StringType) {
        return 32;
    }

    if (typ instanceof PointerType) {
        return staticSize(typ.toType);
    }

    if (
        typ instanceof MissingType &&
        typ.typeString !== undefined &&
        isTypeStringStatic32BytesInStorage(typ.typeString)
    ) {
        return 32;
    }

    nyi(`NYI staticSize(${typ.pp()})`);
}

export function makeStorageView(
    type: BaseRuntimeType,
    loc: StorageLocation
): BaseStorageView<Value, BaseRuntimeType> {
    if (type instanceof MissingType) {
        if (type.typeString !== undefined) {
            if (isTypeStringStatic32BytesInStorage(type.typeString)) {
                loc = loc[1] === 32 ? loc : nextWord(loc);
            }
        }

        return new MissingStorageView(type, loc);
    }

    if (!typeFitsInLoc(type, loc)) {
        loc = nextWord(loc);
    }

    if (type instanceof IntType) {
        return new IntStorageView(type, loc);
    }

    if (type instanceof BoolType) {
        return new BoolStorageView(bool, loc);
    }

    if (type instanceof AddressType) {
        return new AddressStorageView(address, loc);
    }

    if (type instanceof FixedBytesType) {
        return new FixedBytesStorageView(type, loc);
    }

    if (type instanceof BytesType) {
        return new BytesStorageView(type, loc);
    }

    if (type instanceof StringType) {
        return new StringStorageView(type, loc);
    }

    if (type instanceof ArrayType) {
        return new ArrayStorageView(type, loc);
    }

    if (type instanceof PointerType) {
        return new PointerStorageView(type, loc);
    }

    if (type instanceof StructType) {
        return new StructStorageView(type, loc);
    }

    if (type instanceof MappingType) {
        return new MapStorageView(type, loc);
    }

    nyi(`makeStoragView(${type.pp()})`);
}
