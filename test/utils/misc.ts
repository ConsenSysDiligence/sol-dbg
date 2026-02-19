import * as sol from "solc-typed-ast"
import { BaseRuntimeType } from "../../src/debug/runtime_types";
import { ArtifactManager, PartialSolcOutput } from "../../src";
import { bytesToUtf8 } from "@ethereumjs/util";
const fse = require("fs-extra")

export type TypeGenerator = (unit: sol.SourceUnit) => BaseRuntimeType;

export function ppType(t: BaseRuntimeType | TypeGenerator): string {
    if (t instanceof BaseRuntimeType) {
        return t.pp();
    }

    return "<type-generator>";
}


function getVersion(source: string): string {
    const version = source.match(/pragma solidity ([0-9.]*);/);
    sol.assert(version !== null, `No pragma found`);
    return version[1];
}

export interface SampleInfo {
    version: string;
    units: sol.SourceUnit[];
}

export type SampleMap = Map<string, SampleInfo>;

/**
 * Temporary hack
 * @todo remove after https://github.com/d1m0/sol-interp/issues/14 is fixed
 */
export function addSources(
    compilerOutput: PartialSolcOutput,
    fileMap: sol.FileMap
): PartialSolcOutput {
    for (const fileName in compilerOutput.sources) {
        const fileContents = fileMap.get(fileName);
        if (fileContents === undefined) {
            continue;
        }

        compilerOutput.sources[fileName].contents = bytesToUtf8(fileContents);
    }

    return compilerOutput;
}

export async function loadSamples(
    samples: Array<string | [string, any]>,
    basePath = `test/samples`
): Promise<[ArtifactManager, SampleMap]> {
    const res: SampleMap = new Map();
    const compileResults: Array<[PartialSolcOutput, string]> = [];
    const names: string[] = [];

    for (const sample of samples) {
        let fileName;
        let settings;

        if (sample instanceof Array) {
            [fileName, settings] = sample;
        } else {
            fileName = sample;
            settings = undefined;
        }

        const file = fse.readFileSync(`${basePath}/${fileName}`, {
            encoding: "utf-8"
        });
        const version = getVersion(file);

        names.push(fileName);

        const compileResult = await sol.compileSol(
            `${basePath}/${fileName}`,
            version,
            undefined,
            [sol.CompilationOutput.ALL],
            settings
        );
        compileResults.push([addSources(compileResult.data, compileResult.files), version]);
    }

    const artifactManager = new ArtifactManager(compileResults);
    const artifacts = artifactManager.artifacts();

    for (let i = 0; i < names.length; i++) {
        const artifact = artifacts[i];
        res.set(names[i], { version: artifact.compilerVersion, units: artifact.units });
    }

    return [artifactManager, res];
}
