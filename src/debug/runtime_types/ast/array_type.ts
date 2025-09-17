import { BaseRuntimeType } from "./base_type";

export class ArrayType extends BaseRuntimeType {
    constructor(
        public readonly elementT: BaseRuntimeType,
        public readonly size?: bigint
    ) {
        super();
    }

    pp(): string {
        return `${this.elementT.pp()}[${this.size !== undefined ? this.size : ""}]`;
    }
}
