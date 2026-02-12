import { Block, createBlock } from "@ethereumjs/block";
import { Common, StateManagerInterface, Hardfork } from "@ethereumjs/common";
import { MerkleStateManager } from "@ethereumjs/statemanager";
import { TypedTransaction, TypedTxData } from "@ethereumjs/tx";
import {
    Address,
    PrefixedHexString,
    createAccount,
    createAddressFromString,
    hexToBigInt
} from "@ethereumjs/util";
import { bytesToHex, hexToBytes } from "ethereum-cryptography/utils";
import { assert } from "solc-typed-ast";
import { IArtifactManager } from "../debug/artifact_manager/artifact_manager";
import { ContractStates, decodeContractStates } from "../debug/layout";
import { BaseSolTxTracer, FoundryTxResult } from "../debug/tracers/base_tracer";
import { StorageDecodeTracer } from "../debug/tracers/storage_decode_tracer";
import { SupportTracer } from "../debug/tracers/support_tracer";
import { getContractGenKillSet } from "../debug/tracers/transformers/contract_lifetime";
import {
    getKeccakPreimages,
    KeccakPreimageMap
} from "../debug/tracers/transformers/keccak256_invert";
import { map_add } from "./map";
import { hexStrToBuf32, makeFakeTransaction, ZERO_ADDRESS_STRING } from "./misc";
import { set_add, set_subtract } from "./set";
import { HexString } from "../debug";

export interface TxDesc {
    address: HexString;
    gasLimit: HexString;
    gasPrice: HexString;
    input: HexString;
    origin: HexString;
    value: HexString;
    blockCoinbase: HexString;
    blockDifficulty: HexString;
    blockGasLimit: HexString;
    blockNumber: HexString;
    blockTime: HexString;
    nonce: number;
}

export interface AccountDescription {
    nonce: number;
    balance: HexString;
    code: HexString;
    storage: {
        [storageAddr: HexString]: HexString;
    };
}

export interface InitialState {
    accounts: {
        [address: HexString]: AccountDescription;
    };
}

export interface Scenario {
    initialState: InitialState;
    steps: TxDesc[];
}

export function txDescToTx(step: TxDesc, common: Common): TypedTransaction {
    const txData: TypedTxData = {
        value: hexToBigInt(step.value),
        gasLimit: hexToBigInt(step.gasLimit),
        gasPrice: 8,
        data: hexToBytes(step.input),
        nonce: step.nonce
    };

    if (step.address !== ZERO_ADDRESS_STRING) {
        txData.to = createAddressFromString(step.address);
    }

    return makeFakeTransaction(txData, step.origin, common);
}

export function blockFromTxDesc(step: TxDesc, common: Common): Block {
    return createBlock(
        {
            header: {
                coinbase: step.origin,
                difficulty: common.hardfork() === Hardfork.Shanghai ? 0 : step.blockDifficulty,
                gasLimit: step.blockGasLimit,
                number: step.blockNumber,
                timestamp: step.blockTime
            }
        },
        {
            common: common
        }
    );
}

/**
 * Helper class to run a set of TX and record info to allow debugging any of the TXs independently. This includes:
 *
 * 1. The TX data for each
 * 2. The Block info for each TX
 * 3. The State of the world before each TX
 * 4. The result of the TX
 * 5. The set of contracts before each TX
 * 6. The set of keccak256 (result, preimage) pairs computed by the TX (useful for computing Solidity-level maps)
 */
export class TxRunner {
    private tracer: SupportTracer;
    private _txs: TypedTransaction[];
    private _txToBlock: Map<string, Block>;
    private _results: FoundryTxResult[];
    private _stateRootBeforeTx = new Map<string, StateManagerInterface>();
    private _stateRootAfterTx = new Map<string, StateManagerInterface>();
    private _contractsBeforeTx = new Map<string, Set<PrefixedHexString>>();
    private _keccakPreimagesBeforeTx = new Map<string, Map<bigint, Uint8Array>>();

    constructor(
        public readonly artifactManager: IArtifactManager,
        private _foundryCheatcodes: boolean = true
    ) {
        this.tracer = new SupportTracer(artifactManager, {
            strict: true,
            foundryCheatcodes: this._foundryCheatcodes
        });

        this._txs = [];
        this._results = [];
        this._txToBlock = new Map();
    }

    async runScenario(scenario: Scenario): Promise<void> {
        /**
         * Dummy VM used just to get a StateManager and a Common instance. The actual VM used for execution is created inside
         * SupportTracer. (@todo this is kinda ugly... oh well)
         */
        const dummyVM = await BaseSolTxTracer.createVm(undefined, this._foundryCheatcodes);

        let stateManager = dummyVM.stateManager.shallowCopy();
        const common = dummyVM.common.copy();

        BaseSolTxTracer.releaseVM(dummyVM);

        const contractsBefore = await this.setupInitialState(
            scenario.initialState,
            stateManager as MerkleStateManager
        );

        const keccakPreimages: KeccakPreimageMap = new Map();

        for (let i = 0; i < scenario.steps.length; i++) {
            const tx = txDescToTx(scenario.steps[i], common);

            const block = blockFromTxDesc(scenario.steps[i], common);

            const txHash = bytesToHex(tx.hash());

            // Store the sets before the TX
            this._txs.push(tx);
            this._stateRootBeforeTx.set(txHash, stateManager);
            this._txToBlock.set(txHash, block);
            this._contractsBeforeTx.set(txHash, new Set(contractsBefore));
            this._keccakPreimagesBeforeTx.set(txHash, new Map(keccakPreimages));

            const [trace, res, stateAfter] = await this.tracer.debugTx(tx, block, stateManager);

            await (stateManager as MerkleStateManager).flush();

            const [gen, kill] = getContractGenKillSet(trace, res);
            set_add(contractsBefore, gen);
            set_subtract(contractsBefore, kill);

            // Update the keccak map
            const txKeccakPreimages = getKeccakPreimages(trace);
            map_add(keccakPreimages, txKeccakPreimages);

            // Add results
            this._results.push(res);

            stateManager = stateAfter;
            this._stateRootAfterTx.set(txHash, stateAfter);
        }
    }

    private async setupInitialState(
        initialState: InitialState,
        state: MerkleStateManager
    ): Promise<Set<PrefixedHexString>> {
        const initialContracts = new Set<PrefixedHexString>();

        await state.checkpoint();

        for (const addressStr of Object.keys(initialState.accounts)) {
            const { nonce, balance, code, storage } =
                initialState.accounts[addressStr as HexString];

            const address = createAddressFromString(addressStr);
            const codeBuf = hexToBytes(code.slice(2));

            const acct = createAccount({
                nonce: BigInt(nonce),
                balance: BigInt(balance)
            });

            await state.putAccount(address, acct);

            for (const [key, val] of Object.entries(storage)) {
                const keyBuf = hexStrToBuf32(key.slice(2));
                const valBuf = hexStrToBuf32(val.slice(2));

                await state.putStorage(address, keyBuf, valBuf);
            }

            await state.putCode(address, codeBuf);

            if (codeBuf.length > 0) {
                initialContracts.add(address.toString());
            }
        }

        await state.commit();
        await state.flush();

        return initialContracts;
    }

    get txs(): TypedTransaction[] {
        return this._txs;
    }

    get results(): FoundryTxResult[] {
        return this._results;
    }

    getStateBeforeTx(tx: TypedTransaction): StateManagerInterface {
        const txHash = bytesToHex(tx.hash());
        const res = this._stateRootBeforeTx.get(txHash);

        assert(res !== undefined, `Unable to find state before tx ${txHash}`);

        return res;
    }

    getStateAfterTx(tx: TypedTransaction): StateManagerInterface {
        const txHash = bytesToHex(tx.hash());
        const res = this._stateRootAfterTx.get(txHash);

        assert(res !== undefined, `Unable to find state after tx ${txHash}`);

        return res;
    }

    getBlock(tx: TypedTransaction): Block {
        const txHash = bytesToHex(tx.hash());
        const res = this._txToBlock.get(txHash);

        assert(res !== undefined, `Unable to find block for tx ${txHash}`);

        return res;
    }

    getContractsBefore(tx: TypedTransaction): Set<PrefixedHexString> {
        const txHash = bytesToHex(tx.hash());
        const res = this._contractsBeforeTx.get(txHash);

        assert(res !== undefined, `Unable to find contracts for tx ${txHash}`);

        return res;
    }

    getKeccakPreimagesBefore(tx: TypedTransaction): KeccakPreimageMap {
        const txHash = bytesToHex(tx.hash());
        const res = this._keccakPreimagesBeforeTx.get(txHash);

        assert(res !== undefined, `Unable to find keccak preimages for tx ${txHash}`);

        return res;
    }

    async getDecodedContractStatesBeforeTx(
        tx: TypedTransaction,
        contracts?: Iterable<Address>
    ): Promise<ContractStates> {
        const state = this.getStateBeforeTx(tx);
        const preimages = this.getKeccakPreimagesBefore(tx);

        if (contracts === undefined) {
            contracts = [...this.getContractsBefore(tx)].map(createAddressFromString);
        }

        return await decodeContractStates(this.artifactManager, contracts, state, preimages);
    }

    async getDecodedContractStatesOnTxStep(
        tx: TypedTransaction,
        stepNum: number
    ): Promise<ContractStates | undefined> {
        const tracer = new StorageDecodeTracer(this.artifactManager, {
            strict: true,
            foundryCheatcodes: this._foundryCheatcodes
        });

        const liveContracts = new Set(this.getContractsBefore(tx));
        const preimages = new Map(this.getKeccakPreimagesBefore(tx));
        const [trace, ,] = await this.debug(tx, tracer, {
            liveContracts,
            preimages,
            targetSteps: new Set([stepNum])
        });

        if (trace.length < stepNum) {
            return undefined;
        }

        assert(trace[stepNum].decodedStorage !== undefined, ``);

        return trace[stepNum].decodedStorage;
    }

    async debug<StepT, CtxT>(
        tx: TypedTransaction,
        tracer: BaseSolTxTracer<StepT, CtxT>,
        ctx: CtxT
    ): Promise<[StepT[], FoundryTxResult, StateManagerInterface, CtxT]> {
        const block = this.getBlock(tx);
        const stateBefore = this.getStateBeforeTx(tx);

        return await tracer.debugTx(tx, block, stateBefore, ctx);
    }
}
