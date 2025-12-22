import { FunctionTypeId } from "solc-typed-ast";
import { BaseRuntimeType } from "./base_type";

export class FunctionType extends BaseRuntimeType {
    constructor(public readonly solType: FunctionTypeId) {
        super();
    }

    pp(): string {
        return `<function ${this.solType.pp()}>`;
    }
}
