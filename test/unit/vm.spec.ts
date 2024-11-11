import { TypedTransaction } from "@ethereumjs/tx";
import { bytesToHex } from "ethereum-cryptography/utils";
import expect from "expect";
import fse from "fs-extra";
import {
    ABIEncoderVersion,
    assert,
    ContractDefinition,
    FunctionDefinition,
    InferType
} from "solc-typed-ast";
import {
    ArtifactManager,
    buildSolTrace,
    ContractInfo,
    OPCODES,
    PartialSolcOutput,
    pp,
    Scenario,
    SolTxDebugger,
    stackTop,
    StepState,
    TxRunner
} from "../../src";
import {
    DefaultConstructorStep,
    ExternalCallStep,
    InternalReturnStep
} from "../../src/solvm/trace";
import { encodeReturns } from "../../src/solvm/utils";

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
    for (const sample of ["WhileV04"] /*fse.readdirSync("test/samples/solvm")*/) {
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

                const dbg = new SolTxDebugger(artifactManager, { strict: false });

                [trace] = await dbg.debugTx(tx, block, stateBefore);
            });

            it(`We get some traces...`, async () => {
                const [segments, finalIdx] = await buildSolTrace(trace, artifactManager, 0);

                for (const seg of segments) {
                    console.error(`${seg.start}-${seg.end}: \n${pp(seg.trace)}`);
                    const evmStep = trace[seg.end];
                    const solStep = seg.trace[seg.trace.length - 1];

                    const frame = stackTop(evmStep.stack);
                    const info = frame.info as ContractInfo;
                    expect(info).toBeDefined();
                    const ast = (info as ContractInfo).ast as ContractDefinition;
                    expect(ast).toBeDefined();
                    const infer = new InferType(info.artifact.compilerVersion);

                    if (solStep instanceof InternalReturnStep) {
                        assert(
                            frame.callee instanceof FunctionDefinition,
                            `NYI callee {0}`,
                            frame.callee
                        );

                        const encodedSolidityRet = encodeReturns(
                            frame.callee,
                            solStep.values,
                            infer,
                            ABIEncoderVersion.V2
                        );
                        expect(evmStep.retInfo).toBeDefined();
                        expect(bytesToHex(evmStep.retInfo?.rawReturnData as Uint8Array)).toEqual(
                            bytesToHex(encodedSolidityRet)
                        );
                    } else if (solStep instanceof ExternalCallStep) {
                        expect(solStep.opcode === evmStep.op.opcode);
                        expect(seg.end + 1 < trace.length);
                        const newFrame = stackTop(trace[seg.end + 1].stack);

                        expect(newFrame.address.toString()).toEqual(solStep.msg.to.toString());
                        expect(bytesToHex(newFrame.msgData)).toEqual(bytesToHex(solStep.msg.data));
                    } else if (solStep instanceof DefaultConstructorStep) {
                        expect(evmStep.op.opcode === OPCODES.RETURN);
                    } else {
                        throw new Error(`NYI alignment point ${solStep.constructor.name}`);
                    }
                }
                expect(finalIdx).toEqual(trace.length);
            });
        });
    }
});
