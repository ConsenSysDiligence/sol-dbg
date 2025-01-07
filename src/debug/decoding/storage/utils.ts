import { InferType, PointerType } from "solc-typed-ast";
import { nyi } from "../../../utils/misc";
import { Storage, StorageLocation } from "../../types";

export function stor_indexInto(
    typ: PointerType,
    loc: StorageLocation,
    index: any,
    storage: Storage,
    infer: InferType
): undefined | any {
    nyi(`stor_indexInto(${typ}, ${loc}, ${index} ${storage}, ${infer})`);
}
