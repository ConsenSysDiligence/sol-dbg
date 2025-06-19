import { EVMStateManagerInterface } from "@ethereumjs/common";
import { Address } from "@ethereumjs/util";
import { assert, ContractDefinition, InferType } from "solc-typed-ast";
import { IArtifactManager } from "./artifact_manager";
import { getStorage } from "./tracers/transformers/basic_info";
import { getMapKeys, KeccakPreimageMap, MapKeys } from "./tracers/transformers/keccak256_invert";
import { Storage } from "./types";
import { getContractLayoutType, makeStorageView } from "./decoding";
import { isFailure } from "./decoding/utils";
import { Struct } from "./decoding/value";

export type ContractStates = { [addres: string]: Struct };

export async function decodeContractStates(
    artifactManager: IArtifactManager,
    contracts: Iterable<Address>,
    state: EVMStateManagerInterface,
    preimages: KeccakPreimageMap
): Promise<ContractStates> {
    const res: ContractStates = {};
    const mapKeys = getMapKeys(preimages);

    for (const addr of contracts) {
        const code = await state.getContractCode(addr);
        const info = artifactManager.getContractFromDeployedBytecode(code);

        if (!info || !info.ast) {
            continue;
        }

        const infer = artifactManager.infer(info.artifact.compilerVersion);
        const storage = await getStorage(state, addr);

        const contractState = decodeContractState(infer, info.ast, storage, mapKeys);

        if (contractState) {
            res[addr.toString()] = contractState;
        }
    }

    return res;
}

export function decodeContractState(
    infer: InferType,
    contract: ContractDefinition,
    storage: Storage,
    mapKeys?: MapKeys
): Struct {
    const [layout] = getContractLayoutType(contract, infer);
    const view = makeStorageView(layout, [0n, 32]);
    const structState = view.decode(storage, mapKeys);

    assert(!isFailure(structState) && structState instanceof Struct, ``);

    return structState;
}
