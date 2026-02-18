import expect from "expect";
import fse from "fs-extra";
import { Scenario } from "../../src";

describe("Sol2Maruir Tests", () => {
    for (const sample of fse.readdirSync("test/samples/sol2maruir").filter((name) => name.endsWith(".config.json"))) {
        describe(`Sample ${sample}`, () => {
            const scenario = fse.readJsonSync(`test/samples/sol2maruir/${sample}`) as Scenario;
            const [artifactManager] = await loadSamples(
                [sample.slice(0, -4) + "sol"],
                "test/samples/sol2maruir"
            );
            const 
            let artifacts: PartialSolcOutput[] = [];
            let artifactManager: ArtifactManager;

            const sources = new Map<string, string>();

            beforeAll(() => {
                artifacts = lsJson(`test/samples/local/${sample}/artifacts`).map((name) =>
                    fse.readJsonSync(name)
                );

                artifactManager = new ArtifactManager(artifacts);
            });
