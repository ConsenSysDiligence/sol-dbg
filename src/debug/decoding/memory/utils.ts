import { InferType, PointerType } from "solc-typed-ast";
import { nyi } from "../../../utils/misc";
import { LinearMemoryLocation, Memory } from "../../types";

export function mem_indexInto(
    typ: PointerType,
    loc: LinearMemoryLocation,
    index: bigint,
    memory: Memory,
    infer: InferType
): undefined | any {
    nyi(`mem_indexInto(${typ}, ${loc}, ${index}, ${memory}, ${infer})`);
}
