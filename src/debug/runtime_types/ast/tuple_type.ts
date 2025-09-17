import { BaseRuntimeType } from "./base_type";

export class TupleType extends BaseRuntimeType {
    constructor(public readonly elementTypes: BaseRuntimeType[]) {
        super();
    }

    pp(): string {
        return `(${this.elementTypes.map((t) => t.pp()).join(", ")})`;
    }
}
