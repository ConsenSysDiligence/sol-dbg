import { assert } from "solc-typed-ast";
import { BaseRuntimeType } from "./base_type";

export class StructType extends BaseRuntimeType {
    filedM: Map<string, BaseRuntimeType>

    constructor(
        public readonly name: string,
        public readonly fields: Array<[string, BaseRuntimeType]>
    ) {
        super();
        this.filedM = new Map(fields);
    }

    pp(): string {
        return `struct ${this.name} {${this.fields.map(([name, type]) => `${name}: ${type.pp()}`).join(";\n")}}`;
    }

    field(name: string): BaseRuntimeType {
        const res = this.filedM.get(name);
        assert(res !== undefined, `Unknown field ${name}`);
        return res;
    }
}
