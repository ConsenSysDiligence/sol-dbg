import { BaseRuntimeType } from "./base_type";

export class BytesType extends BaseRuntimeType {
    pp(): string {
        return `bytes`;
    }
}
