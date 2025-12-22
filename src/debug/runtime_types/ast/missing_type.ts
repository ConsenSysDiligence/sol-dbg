import { TypeIdentifier } from "solc-typed-ast";
import { BaseRuntimeType } from "./base_type";

/**
 * The local version of sol-dbg's MissingType.
 * @todo may deprecate this in the future
 */
export class MissingTypeDef extends BaseRuntimeType {
    constructor(public readonly type: TypeIdentifier) {
        super();
    }

    pp(): string {
        return `<missing type def for ${this.type.pp()}"}>`;
    }
}
