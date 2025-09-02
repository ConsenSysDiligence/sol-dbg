import { BaseRuntimeType } from "./base_type";

export class StringType extends BaseRuntimeType {
    pp(): string {
        return `strings`;
    }
}
