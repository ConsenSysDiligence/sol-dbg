import {
    AddressType,
    ArrayType,
    BoolType,
    BytesType,
    ContractDefinition,
    EnumDefinition,
    enumToIntType,
    FixedBytesType,
    InferType,
    IntType,
    MappingType,
    PointerType,
    StringType,
    TypeNode,
    UserDefinedType,
    UserDefinedValueTypeDefinition
} from "solc-typed-ast";
import { Struct, Value } from "../value";
import { View } from "../view";
import { Storage } from "../../types";
import {
    address,
    bigEndianBufToBigint,
    bigIntToBuf,
    bool,
    fits,
    MAX_ARR_DECODE_LIMIT,
    nyi,
    uint256,
    uint8
} from "../../../utils";
import { keccak256 } from "ethereum-cryptography/keccak";
import { Address } from "@ethereumjs/util";
import { ExpStructType } from "../exp_types";
import { assert } from "console";
import { MapKeys } from "../../tracers";
import { makeMemoryView } from "../memory";

type StorageLocation = [bigint, number];

function move(loc: StorageLocation, byBytes: number): StorageLocation {
    const [key, endOffsetInWord] = loc;
    assert(endOffsetInWord >= byBytes, ``);
    return endOffsetInWord === byBytes ? [key + 1n, 32] : [key, endOffsetInWord - byBytes];
}

export function roundLocToType(
    loc: StorageLocation,
    typ: TypeNode,
    infer: InferType
): StorageLocation {
    if (typeFitsInLoc(typ, loc, infer)) {
        return loc;
    }

    return nextWord(loc);
}

const defaultInfer = new InferType("0.8.30");

export abstract class BaseStorageView<
    Val extends Value,
    Type extends TypeNode = TypeNode
> extends View<Storage, Val, [bigint, number], Type> {
    key: bigint;
    endOffsetInWord: number;

    constructor(type: Type, infer: InferType, loc: [bigint, number]) {
        super(type, infer, loc);
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
    ): bigint {
        const size = type.nBits / 8;

        if (endOffsetInWord < size) {
            this.fail(
                state,
                `Internal Error: Can't decode ${type.pp()} starting at offset ${endOffsetInWord} in word ${key}`
            );
        }

        const rawBytes = this.fetchBytes(key, endOffsetInWord - size, size, state);

        let res = bigEndianBufToBigint(rawBytes);

        // Convert signed negative 2's complement values
        if (type.signed && (res & (BigInt(1) << BigInt(type.nBits - 1))) !== BigInt(0)) {
            // Mask out any 1's above the number's size
            res = res & ((BigInt(1) << BigInt(type.nBits)) - BigInt(1));
            res = -((BigInt(1) << BigInt(type.nBits)) - res);
        }

        if (!fits(res, type)) {
            this.fail(
                state,
                `Decoded value ${res} from ${[key, endOffsetInWord]} doesn't fit in expected typee ${type.pp()}`
            );
        }

        return res;
    }

    pp(): string {
        return `<${this.type.pp()}@${this.loc} in storage>`;
    }

    /**
     * The first location after the end of this view
     */
    abstract nextLoc(): StorageLocation;
}

export class IntStorageView extends BaseStorageView<bigint, IntType> {
    constructor(type: IntType, loc: [bigint, number]) {
        super(type, defaultInfer, loc);
    }

    decode(state: Storage): bigint {
        return this.decodeIntAt(this.key, this.endOffsetInWord, this.type, state);
    }

    nextLoc(): StorageLocation {
        return move(this.loc, this.type.nBits / 8);
    }
}

export class BoolStorageView extends BaseStorageView<boolean, BoolType> {
    constructor(loc: [bigint, number]) {
        super(bool, defaultInfer, loc);
    }

    decode(state: Storage): boolean {
        return this.decodeIntAt(this.key, this.endOffsetInWord, uint8, state) !== BigInt(0);
    }

    nextLoc(): StorageLocation {
        return move(this.loc, 1);
    }
}

export class AddressStorageView extends BaseStorageView<Address, AddressType> {
    constructor(loc: [bigint, number]) {
        super(address, defaultInfer, loc);
    }

    decode(state: Storage): Address {
        const bytes = this.fetchBytes(this.key, this.endOffsetInWord - 20, 20, state);
        return new Address(bytes);
    }

    nextLoc(): StorageLocation {
        return move(this.loc, 20);
    }
}

export class FixedBytesStorageView extends BaseStorageView<Uint8Array, FixedBytesType> {
    constructor(type: FixedBytesType, loc: [bigint, number]) {
        super(type, defaultInfer, loc);
    }

    decode(state: Storage): Uint8Array {
        if (this.endOffsetInWord < this.type.size) {
            this.fail(
                undefined,
                `Internal Error: Can't decode ${this.type.pp()} starting at offset ${this.endOffsetInWord} in word ${this.key}`
            );
        }

        return this.fetchBytes(
            this.key,
            this.endOffsetInWord - this.type.size,
            this.type.size,
            state
        );
    }

    nextLoc(): StorageLocation {
        return move(this.loc, this.type.size);
    }
}

export class PointerStorageView extends BaseStorageView<Value, PointerType> {
    innerView: BaseStorageView<Value, TypeNode>;
    constructor(type: PointerType, infer: InferType, loc: [bigint, number], mapKeys?: MapKeys) {
        super(type, infer, loc);
        this.innerView = makeStorageView(type.to, infer, loc, mapKeys);
    }

    decode(state: Storage): Value {
        return this.innerView.decode(state);
    }

    nextLoc(): StorageLocation {
        return this.innerView.nextLoc();
    }
}

function keccakOfAddr(addr: bigint): bigint {
    const addrBuf = bigIntToBuf(addr, 32, "big");
    const hashBuf = keccak256(addrBuf);

    return bigEndianBufToBigint(hashBuf);
}

export class ArrayStorageView extends BaseStorageView<Value[], ArrayType> {
    private _nextLoc: StorageLocation;

    constructor(
        type: ArrayType,
        infer: InferType,
        loc: [bigint, number],
        private readonly mapKeys: MapKeys | undefined = undefined
    ) {
        super(type, infer, loc);

        if (type.size === undefined) {
            this._nextLoc = nextWord(loc);
        } else {
            // Dirty way to compute how many elements fit in how many words
            let tmpL: StorageLocation = [0n, 32];
            let nEls = 0n;

            while (tmpL[0] === 0n) {
                nEls++;
                const elView = makeStorageView(type.elementT, infer, tmpL);
                tmpL = elView.nextLoc();
            }

            const nWords = (type.size / nEls + (type.size % nEls === 0n ? 0n : 1n)) * tmpL[0];
            this._nextLoc = [this.loc[0] + nWords, 32];
        }
    }

    nextLoc(): StorageLocation {
        return this._nextLoc;
    }

    decode(state: Storage): Value[] {
        let sizeBigint: bigint;
        let contentsKey: bigint;
        const contentsOff: number = 32;

        if (this.type.size) {
            sizeBigint = this.type.size;
            contentsKey = this.key;
        } else {
            sizeBigint = this.decodeIntAt(this.key, this.endOffsetInWord, uint256, state);
            contentsKey = keccakOfAddr(this.key);
        }

        if (sizeBigint > MAX_ARR_DECODE_LIMIT) {
            this.fail(state, `Array too large to decode ${sizeBigint}`);
        }

        const size = Number(sizeBigint);
        const res: Value[] = [];
        let elLoc: StorageLocation = [contentsKey, contentsOff];
        const elT = this.type.elementT;

        for (let i = 0; i < size; i++) {
            const view = makeStorageView(elT, this.infer, elLoc, this.mapKeys);
            res.push(view.decode(state));
            elLoc = view.nextLoc();
        }

        return res;
    }
}

export class StructStorageView extends BaseStorageView<Struct, ExpStructType> {
    fieldViews: Array<[string, BaseStorageView<Value, TypeNode>]> = [];
    private _nextLoc: StorageLocation;

    constructor(type: ExpStructType, infer: InferType, loc: StorageLocation, mapKeys?: MapKeys) {
        super(type, infer, loc);
        assert(this.endOffsetInWord === 32, `Structs must start at 32 byte boundaries`);

        let fieldLoc = this.loc;

        for (const [name, fieldT] of this.type.fields) {
            const fieldView = makeStorageView(fieldT, this.infer, fieldLoc, mapKeys);
            this.fieldViews.push([name, fieldView]);
            fieldLoc = fieldView.nextLoc();
        }

        this._nextLoc = fieldLoc[1] === 32 ? fieldLoc : nextWord(fieldLoc);
    }

    nextLoc(): StorageLocation {
        return this._nextLoc;
    }

    decode(state: Storage): Struct {
        const entries: Array<[string, Value]> = this.fieldViews.map(([name, view]) => [
            name,
            view.decode(state)
        ]);
        return new Struct(entries);
    }
}

export class MapStorageView extends BaseStorageView<Map<Value, Value>, MappingType> {
    constructor(
        type: MappingType,
        infer: InferType,
        loc: StorageLocation,
        private readonly mapKeys: MapKeys | undefined = undefined
    ) {
        super(type, infer, loc);
    }

    nextLoc(): StorageLocation {
        return nextWord(this.loc);
    }

    decode(state: Storage): Map<Value, Value> {
        if (!this.mapKeys) {
            return new Map();
        }

        const candidateKeys = this.mapKeys.get(this.loc[0]);
        const res = new Map<Value, Value>();

        if (candidateKeys === undefined) {
            return res;
        }

        let keyView;

        if (this.type.keyType instanceof PointerType) {
            const toT = this.type.keyType.to;
            assert(
                toT instanceof StringType || toT instanceof BytesType,
                `Unexpected mapping key type {0}`,
                this.type.keyType
            );
            keyView = makeMemoryView(this.type.keyType.to, this.infer, 0n);
        } else {
            keyView = makeMemoryView(this.type.keyType, this.infer, 0n);
        }

        for (const [candidateKey, candidateSlot] of candidateKeys) {
            let decodedKey;
            let decodedValue;

            try {
                decodedKey = keyView.decode(candidateKey);
                const valueView = makeStorageView(
                    this.type.valueType,
                    this.infer,
                    [candidateSlot, 32],
                    this.mapKeys
                );
                decodedValue = valueView.decode(state);
            } catch (DecodingError) {
                continue;
            }

            if (decodedKey !== undefined && decodedValue !== undefined) {
                res.set(decodedKey, decodedValue);
            }
        }

        return res;
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
function typeFitsInLoc(typ: TypeNode, loc: StorageLocation, infer: InferType): boolean {
    const [, endOffsetInWord] = loc;

    if (typeStartsInNewWord(typ)) {
        return endOffsetInWord == 32;
    }

    return staticSize(typ, infer) <= endOffsetInWord;
}

export function nextWord(loc: StorageLocation): StorageLocation {
    return [loc[0] + 1n, 32];
}

function typeStartsInNewWord(t: TypeNode): boolean {
    if (t instanceof PointerType) {
        return typeStartsInNewWord(t.to);
    }

    return (t instanceof ArrayType || t instanceof MappingType || t instanceof
        ExpStructType);
}

/**
 * Compute the 'static' size that a variable of primitive type `typ` would take
 * up in storage.  Primitive here means not an array, struct or a map. Those
 * have special layout rules (always start in a new word, next item in layout
 * starts in own word too).
 */
function staticSize(typ: TypeNode, infer: InferType): number {
    if (typ instanceof IntType) {
        return typ.nBits / 8;
    }

    if (typ instanceof FixedBytesType) {
        return typ.size;
    }

    if (typ instanceof BoolType) {
        return 1;
    }

    if (typ instanceof AddressType) {
        return 20;
    }

    // @todo maybe remove all these in a pre-pass
    if (typ instanceof UserDefinedType) {
        if (typ.definition instanceof EnumDefinition) {
            return enumToIntType(typ.definition).nBits / 8;
        }

        // @todo remove this in a pre-pass
        if (typ.definition instanceof UserDefinedValueTypeDefinition) {
            return staticSize(infer.typeNameToTypeNode(typ.definition.underlyingType), infer);
        }

        if (typ.definition instanceof ContractDefinition) {
            return 20;
        }
    }

    if (
        typ instanceof PointerType &&
        (typ.to instanceof BytesType || typ.to instanceof StringType)
    ) {
        return 32;
    }

    nyi(`NYI staticSize(${typ.pp()})`);
}

export function makeStorageView(
    type: TypeNode,
    infer: InferType,
    loc: StorageLocation,
    mapKeys?: MapKeys
): BaseStorageView<Value, TypeNode> {
    if (!typeFitsInLoc(type, loc, infer)) {
        loc = nextWord(loc);
    }

    if (type instanceof IntType) {
        return new IntStorageView(type, loc);
    }

    if (type instanceof BoolType) {
        return new BoolStorageView(loc);
    }

    if (type instanceof AddressType) {
        return new AddressStorageView(loc);
    }

    if (type instanceof FixedBytesType) {
        return new FixedBytesStorageView(type, loc);
    }

    /*
    if (type instanceof BytesType) {
        return new BytesStorageView(type, infer, loc);
    }

    if (type instanceof StringType) {
        return new StringStorageView(type, infer, loc);
    }

    */
    if (type instanceof ArrayType) {
        return new ArrayStorageView(type, infer, loc, mapKeys);
    }

    if (type instanceof PointerType) {
        return new PointerStorageView(type, infer, loc, mapKeys);
    }

    if (type instanceof ExpStructType) {
        return new StructStorageView(type, infer, loc, mapKeys);
    }

    if (type instanceof MappingType) {
        return new MapStorageView(type, infer, loc, mapKeys);
    }

    nyi(`makeStoragView(${type.pp()})`);
}
