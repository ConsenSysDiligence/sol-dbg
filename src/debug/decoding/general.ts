import { nyi } from "../../utils/misc";
import { topExtFrame } from "../tracers/transformers/ext_stack";
import { MapKeys } from "../tracers/transformers/keccak256_invert";
import { StepState } from "../types";
import { View } from "./view";
import { Value } from "./value";
import { BaseStackView } from "./stack/view";
import { BaseMemoryView } from "./memory/view";
import { BaseCalldataView } from "./calldata/view";
import { BaseStorageView } from "./storage/view";

export function decodeView(v: View, state: StepState, mapKeys?: MapKeys): Value {
    if (v instanceof BaseStackView) {
        const res = v.decode(state.evmStack);

        if (res instanceof View) {
            return decodeView(res, state, mapKeys);
        }

        return res;
    }

    if (v instanceof BaseMemoryView) {
        return v.decode(state.memory);
    }

    if (v instanceof BaseCalldataView) {
        const lastExtFrame = topExtFrame(state);
        const res = v.decode(lastExtFrame.msgData);

        if (res instanceof View) {
            return decodeView(res, state, mapKeys);
        }

        return res;
    }

    if (v instanceof BaseStorageView) {
        return v.decode(state.storage, mapKeys);
    }

    nyi(`View ${v.pp()}`);
}
