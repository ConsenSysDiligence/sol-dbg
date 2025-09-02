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
}
