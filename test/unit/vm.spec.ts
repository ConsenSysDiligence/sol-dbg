import { TypedTransaction } from "@ethereumjs/tx";
import expect from "expect";
import fse from "fs-extra";
import {
    ArtifactManager,
    buildSolTrace,
    PartialSolcOutput,
    pp,
    Scenario,
    SolTxDebugger,
    StepState,
    TxRunner
} from "../../src";

function makeTest(artifact: PartialSolcOutput): Scenario {
    const bytecode = artifact.contracts["sample.sol"]["Test"].evm.bytecode.object;
    return {
        initialState: { accounts: {} },
        steps: [
            {
                address: "0x0000000000000000000000000000000000000000",
                gasLimit: "0xff0000",
                gasPrice: "0x1",
                input: `0x${bytecode}`,
                origin: "0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0",
                value: "0x0",
                blockCoinbase: "0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0",
                blockDifficulty: "0xc",
                blockGasLimit: "0xff0000",
                blockNumber: "0x1",
                blockTime: "0x1"
            },
            {
                address: "0xf084a83954ce917799e0b4dbd03934a2ea65888c",
                gasLimit: "0xff0000",
                gasPrice: "0x1",
                input: "0xdffeadd0",
                origin: "0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0",
                value: "0x0",
                blockCoinbase: "0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa0",
                blockDifficulty: "0xc",
                blockGasLimit: "0xff0000",
                blockNumber: "0x2",
                blockTime: "0x2"
            }
        ]
    };
}

describe("Local tests", () => {
    for (const sample of fse.readdirSync("test/samples/solvm")) {
        describe(`Sample ${sample}`, () => {
            let artifact: PartialSolcOutput;
            let artifactManager: ArtifactManager;
            let txRunner: TxRunner;
            let tx: TypedTransaction;
            let trace: StepState[];

            beforeAll(async () => {
                artifact = fse.readJsonSync(`test/samples/solvm/${sample}/artifact.json`);
                artifactManager = new ArtifactManager([artifact]);
                txRunner = new TxRunner(artifactManager);

                const test = makeTest(artifact);

                await txRunner.runScenario(test);

                tx = txRunner.txs[1];

                const block = txRunner.getBlock(tx);
                const stateBefore = txRunner.getStateBeforeTx(tx);

                const dbg = new SolTxDebugger(artifactManager);

                [trace] = await dbg.debugTx(tx, block, stateBefore);
            });

            it(`We get some traces...`, async () => {
                const [segments, finalIdx] = await buildSolTrace(trace, artifactManager, 0);

                for (const seg of segments) {
                    console.error(`${seg.start}-${seg.end}: \n${pp(seg.trace)}`);
                }
                expect(finalIdx).toEqual(trace.length);
            });
        });
    }
});
