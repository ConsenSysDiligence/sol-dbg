import * as sol from "solc-typed-ast";
import { BaseRuntimeType } from "./base_type";

/**
 * The type of a 'type expression'. Strictly speaking this shouldnt appear as a runtime value, however its convenient
 * to have it here as it appears as an argument to `abi.decode`.
 */
export class TypeType extends BaseRuntimeType {
    constructor(
        public readonly rawT: sol.TypeNode,
    ) {
        super();
    }

    pp(): string {
        return `<type: ${this.rawT.pp()}>`;
    }
}
