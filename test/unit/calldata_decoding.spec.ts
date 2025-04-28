import { bytesToHex, hexToBytes } from "ethereum-cryptography/utils";
import fse from "fs-extra";
import { assert, forAny } from "solc-typed-ast";
import {
    ArtifactManager,
    buildCalldataViews,
    liftABIValue,
    PartialSolcOutput,
    ppValue
} from "../../src";
import { BaseCalldataView } from "../../src/debug/decoding/calldata/view";
import { lsJson } from "../utils";
import { TestCase } from "../utils/test_case";

describe("Calldata decoding", () => {
    for (const sample of fse.readdirSync("test/samples/local")) {
        describe(`Sample ${sample}`, () => {
            for (const txFile of lsJson(`test/samples/local/${sample}/txs`)) {
                const testJSON: TestCase = fse.readJsonSync(txFile);

                if (forAny(testJSON.steps, (step) => step.decodedCalldata !== undefined)) {
                    let artifacts: PartialSolcOutput[] = [];
                    let artifactManager: ArtifactManager;

                    beforeAll(() => {
                        artifacts = lsJson(`test/samples/local/${sample}/artifacts`).map((name) =>
                            fse.readJsonSync(name)
                        );

                        artifactManager = new ArtifactManager(artifacts);
                    });

                    it(`Scenario ${txFile}`, () => {
                        for (const step of testJSON.steps) {
                            const calldata = hexToBytes(step.input);
                            const selector = calldata.slice(0, 4);

                            if (step.decodedCalldata === undefined) {
                                continue;
                            }

                            const methodInfo = artifactManager.findMethod(selector);

                            assert(
                                methodInfo !== undefined,
                                `Missing method with selector ${bytesToHex(selector)}`
                            );

                            const [contractInfo, method] = methodInfo;
                            console.error(`Method: ${method.name}`);
                            const infer = artifactManager.infer(
                                contractInfo.artifact.compilerVersion
                            );

                            const views = buildCalldataViews(method, calldata, infer);

                            const callStr = `${method.name}(${views
                                .map(([, v, origType]) => {
                                    if (v === undefined) {
                                        return `<failed decoding>`;
                                    }

                                    const val = (v as BaseCalldataView).decode(calldata);
                                    return ppValue(
                                        origType,
                                        liftABIValue(val, origType, infer),
                                        v.infer
                                    );
                                })
                                .join(",")})`;

                            expect(callStr).toEqual(step.decodedCalldata);
                        }
                    });
                }
            }
        });
    }
});
