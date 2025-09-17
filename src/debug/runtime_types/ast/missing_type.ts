import { BaseRuntimeType } from "./base_type";

/**
 * The local version of sol-dbg's MissingType.
 * @todo may deprecate this in the future
 */
export class MissingType extends BaseRuntimeType {
    constructor(public readonly typeString: string | undefined) {
        super();
    }

    pp(): string {
        return `<mssing type${this.typeString !== undefined ? `: ${this.typeString}` : ""}>`;
    }
}
