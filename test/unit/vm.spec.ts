import { TypedTransaction } from "@ethereumjs/tx";
import { AbiCoder } from "@ethersproject/abi";
import { bytesToHex } from "ethereum-cryptography/utils";
import expect from "expect";
import fse from "fs-extra";
import { ABIEncoderVersion, ContractDefinition, InferType } from "solc-typed-ast";
import {
    ArtifactManager,
    buildSolTrace,
    ContractInfo,
    PartialSolcOutput,
    pp,
    Scenario,
    SolTxDebugger,
    stackTop,
    StepState,
    TxRunner
} from "../../src";
import { ReturnStep } from "../../src/solvm/trace";

function makeTest(artifact: PartialSolcOutput, fileName: string): Scenario {
    const bytecode = artifact.contracts[fileName]["__IRTest__"].evm.bytecode.object;
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
    const coder = new AbiCoder();
    for (const sample of ["cfg"] /*fse.readdirSync("test/samples/solvm")*/) {
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

                const test = makeTest(artifact, `${sample}.sol`);

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
                    const evmStep = trace[seg.end];
                    const solStep = seg.trace[seg.trace.length - 1];

                    const info = stackTop(evmStep.stack).info as ContractInfo;
                    expect(info).toBeDefined();
                    const ast = (info as ContractInfo).ast as ContractDefinition;
                    expect(ast).toBeDefined();
                    const mainFun = ast.vFunctions.filter((f) => f.name === "main")[0];
                    const infer = new InferType(info.artifact.compilerVersion);
                    const mainT = infer.funDefToType(mainFun);
                    const abiRets = mainT.returns.map((t) =>
                        infer.toABIEncodedType(t, ABIEncoderVersion.V2).pp()
                    );

                    if (solStep instanceof ReturnStep) {
                        const encodedSolidityRet = coder.encode(abiRets, solStep.values);
                        expect(evmStep.retInfo).toBeDefined();
                        expect(
                            "0x" + bytesToHex(evmStep.retInfo?.rawReturnData as Uint8Array)
                        ).toEqual(encodedSolidityRet);
                    } else {
                        throw new Error(`NYI alignment point ${pp(solStep)}`);
                    }
                }
                expect(finalIdx).toEqual(trace.length);
            });
        });
    }
});
