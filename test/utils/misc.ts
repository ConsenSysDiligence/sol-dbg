import { SourceUnit } from "solc-typed-ast";
import { BaseRuntimeType } from "../../src/debug/runtime_types";

export type TypeGenerator = (unit: SourceUnit) => BaseRuntimeType;

export function ppType(t: BaseRuntimeType | TypeGenerator): string {
    if (t instanceof BaseRuntimeType) {
        return t.pp();
    }

    return "<type-generator>";
}
