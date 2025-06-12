import { SourceUnit, TypeNode } from "solc-typed-ast";

export type TypeGenerator = (unit: SourceUnit) => TypeNode;

export function ppType(t: TypeNode | TypeGenerator): string {
    if (t instanceof TypeNode) {
        return t.pp();
    }

    return "<type-generator>";
}
