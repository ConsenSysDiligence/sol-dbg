import { concatBytes, hexToBytes } from "ethereum-cryptography/utils";
import * as sol from "solc-typed-ast";
import { coder } from "./utils";

export class NYI extends Error {}

export function nyi(msg: string): never {
    throw new NYI(`NYI: ${msg}`);
}

export function fail(msg: string): never {
    sol.assert(false, msg);
}

export abstract class SolBaseException {
    constructor(public readonly data: Uint8Array) {}
}

/**
 * Base class for all compiler-generated exceptions (e.g. overflow, div by 0, no method, etc..)
 */
export abstract class SolCompilerException extends SolBaseException {}

/**
 * Base class for all source-generate exceptions (throw, revert,)
 */
export abstract class SolSourceException extends SolBaseException {}

/**
 * Revert generated exceptions (corresponding to 0xFD)
 */
export class SolRevert extends SolSourceException {}

export class SolPanic extends SolRevert {
    constructor(code: number) {
        super(
            concatBytes(
                new Uint8Array([0x2e, 0x36, 0xa7, 0x09]),
                hexToBytes(coder.encode(["uint"], [code]))
            )
        );
    }
}

/**
 * Invalid instruction based exceptions (corresponding to 0xFE - used to be the old way of doing assert(false) prior to 0.8.0)
 */
export class SolInvalid extends SolSourceException {
    constructor() {
        super(new Uint8Array());
    }
}

// Compiler exceptions
export class SolNoMethod extends SolCompilerException {}
