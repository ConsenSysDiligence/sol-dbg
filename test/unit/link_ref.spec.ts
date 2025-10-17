import expect from "expect";
import { ArtifactManager, BytecodeReference, LinkMap, ZERO_ADDRESS } from "../../src";
import * as fse from "fs-extra"
import { createAddressFromString } from "@ethereumjs/util";

describe(`Link references test`, () => {
    it("Can load an artifact with link references", () => {
        expect(() => {
            new ArtifactManager([fse.readJSONSync("test/samples/static/link_ref/artifacts/main.json")]);
        }).not.toThrow();
    })

    it(`Can link library only with all addresses`, () => {
        const artifactManager: ArtifactManager = new ArtifactManager([fse.readJSONSync("test/samples/static/link_ref/artifacts/main.json")]);
        const lib = artifactManager.contracts().filter((info) => info.contractName === "Lib")[0];
        const linkRefs = lib.bytecode.linkReferences;
        const libId = "test/samples/static/link_ref/contracts/libraries.sol:Lib1";
        const addr = createAddressFromString("0xAaaaAaAAaaaAAaAAaAaaaaAAAAAaAaaaAaAaaAA0");

        const goodLinkMap: LinkMap = new Map([
            [libId, addr]
        ])

        const linkedBytecode = artifactManager.link(lib.bytecode, goodLinkMap);

        // All the link ranges are set correctly
        for (const range of linkRefs.get(libId) as BytecodeReference[]) {
            expect(linkedBytecode.slice(range.start, range.start + range.length)).toEqual(addr.bytes)
        }
        // If we zero them out we get the original bytecode
        for (const range of linkRefs.get(libId) as BytecodeReference[]) {
            linkedBytecode.set(ZERO_ADDRESS.bytes, range.start)
        }

        expect(linkedBytecode).toEqual(lib.bytecode.bytecode)

        expect(() => {
            artifactManager.link(lib.bytecode, new Map())
        }).toThrow()
    });
});
