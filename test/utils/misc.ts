import { SourceUnit, TypeNode } from "solc-typed-ast";
import { ExpStructType } from "../../src";

export type TypeGenerator = (unit: SourceUnit) => TypeNode;

export function ppType(t: TypeNode | TypeGenerator): string {
    if (t instanceof ExpStructType) {
        return `<internal struct ${t.name}>`;
    }
    if (t instanceof TypeNode) {
        return t.pp();
    }

    return "<type-generator>";
}
