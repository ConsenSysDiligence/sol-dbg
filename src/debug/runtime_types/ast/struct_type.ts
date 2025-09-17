import { BaseRuntimeType } from "./base_type";

export class StructType extends BaseRuntimeType {
    constructor(
        public readonly name: string,
        public readonly fields: Array<[string, BaseRuntimeType]>
    ) {
        super();
    }

    pp(): string {
        return `struct ${this.name} {${this.fields.map(([name, type]) => `${name}: ${type.pp()}`).join(";\n")}}`;
    }
}
