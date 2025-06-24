import { assert } from "solc-typed-ast";
import { Memory } from "../../types";
import { IntMemView, makeMemoryView } from "./view";
import { uint256 } from "../../../utils";

export interface Allocator {
    alloc(size: number): bigint
}

const MAX_MEM_LIMIT = 64 * 1024 * 1024;
const FREE_POINTER_START = 0x40
const RESERVED_AREA_SIZE = 0x80
const INITIAL_SIZE = RESERVED_AREA_SIZE;

/**
 * Default allocator implementing the free-pointer allocation model of Solidity.
 * Note: The allocator owns the Memory object since it initializes it internally with a resizable ArrayBuffer
 */
export class DefaultAllocator implements Allocator {
    public readonly buf: ArrayBuffer
    public readonly memory: Memory
    private _freeMemPtr: IntMemView;

    constructor() {
        // Initialize memory
        this.buf = new ArrayBuffer(INITIAL_SIZE, { maxByteLength: MAX_MEM_LIMIT });
        this.memory = new Uint8Array(this.buf);
        this._freeMemPtr = makeMemoryView(uint256, BigInt(FREE_POINTER_START)) as IntMemView
        this._freeMemPtr.encode(BigInt(RESERVED_AREA_SIZE), this.memory);
    }

    get freeMemPtr(): bigint {
        const res = this._freeMemPtr.decode(this.memory)
        assert(typeof res === "bigint", `Internal error getting free mem pointer: ${res}`)
        return res

    }

    /**
     * Grow the memory to at least `newMemSize`
     * @param newMemSize 
     */
    grow(newMemSize: number | bigint) {
        assert(BigInt(newMemSize) <= MAX_MEM_LIMIT, `Memory grew too large: ${newMemSize}`)
        this.buf.resize(Number(newMemSize))
    }

    alloc(size: number): bigint {
        const res = this.freeMemPtr
        const newRegionEnd = res + BigInt(size);

        if (newRegionEnd > BigInt(this.memory.length)) {
            this.grow(newRegionEnd)
        }

        this._freeMemPtr.encode(newRegionEnd, this.memory);
        return res;
    }
}