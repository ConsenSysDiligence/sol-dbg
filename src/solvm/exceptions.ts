import * as sol from "solc-typed-ast";
import { SolValue } from "./state";

export class NYI extends Error {}

export function nyi(msg: string): never {
    throw new NYI(`NYI: ${msg}`);
}

export function fail(msg: string): never {
    sol.assert(false, msg);
}

export abstract class SolBaseException {}

export abstract class SolInternalException extends SolBaseException {}

export class SolNoMethod extends SolInternalException {
    constructor(public readonly data: Uint8Array) {
        super();
    }
}
export class SolRawException extends SolBaseException {
    constructor(public readonly payload: Uint8Array) {
        super();
    }
}

export class SolStructuredException extends SolBaseException {
    constructor(
        public readonly type: sol.ErrorDefinition,
        public readonlyargs: SolValue[]
    ) {
        super();
    }
}
