import { BaseRuntimeType } from "./base_type";

export class BoolType extends BaseRuntimeType {
    pp(): string {
        return "bool";
    }
}
