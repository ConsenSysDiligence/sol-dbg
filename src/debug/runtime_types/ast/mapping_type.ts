import { BaseRuntimeType } from "./base_type";

export class MappingType extends BaseRuntimeType {
    constructor(
        public readonly keyType: BaseRuntimeType,
        public readonly valueType: BaseRuntimeType
    ) {
        super();
    }

    pp(): string {
        return `mapping(${this.keyType.pp()} => ${this.valueType.pp()})`;
    }
}
