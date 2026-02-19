import expect from "expect";
import fse from "fs-extra";
import { Scenario, SolTxDebugger, TxRunner } from "../../src";
import { loadSamples } from "../utils/misc";

describe("Sol2Maruir Tests", () => {
    for (const sample of fse.readdirSync("test/samples/sol2maruir").filter((name) => name.endsWith(".config.json"))) {
        it(`Sample ${sample}`, async () => {
            const scenario: Scenario = fse.readJsonSync(`test/samples/sol2maruir/${sample}`);
            const [artifactManager]= await loadSamples(
                    [sample.slice(0, -4) + "sol"],
                    "test/samples/sol2maruir"
                );

            const runner = new TxRunner(artifactManager);
            const dbg = new SolTxDebugger(artifactManager, {strict: false});

            await runner.runScenario(scenario);
            for (let tx of runner.txs) {
                const block = runner.getBlock(tx);
                const stateBefore = runner.getStateAfterTx(tx);
                const [trace, ] = await dbg.debugTx(tx, block, stateBefore)
                expect(trace.length).toBeGreaterThan(0);
            }
        });
    }
})
