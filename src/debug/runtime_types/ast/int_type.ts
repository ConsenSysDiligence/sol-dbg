import { BaseRuntimeType } from "./base_type";

export class IntType extends BaseRuntimeType {
    constructor(
        public readonly numBits: number,
        public readonly signed: boolean
    ) {
        super();
    }

    pp(): string {
        return `${this.signed ? "" : "u"}int${this.numBits}`;
    }

    /// Maximum value (inclusive) representable by this int type.
    max(): bigint {
        return 2n ** BigInt(this.signed ? this.numBits - 1 : this.numBits) - 1n;
    }

    /// Minimum value (inclusive) representable by this int type.
    min(): bigint {
        return this.signed ? -(2n ** BigInt(this.numBits - 1)) : 0n;
    }
}
