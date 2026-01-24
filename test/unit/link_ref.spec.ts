import expect from "expect";
import { ArtifactManager, BytecodeReference, LinkMap, ZERO_ADDRESS } from "../../src";
import * as fse from "fs-extra";
import { createAddressFromString } from "@ethereumjs/util";
import { decodeLinkMap } from "../../src/debug/decoding/utils";
import * as sol from "solc-typed-ast";

describe(`Link references test`, () => {
    it("Can load an artifact with link references", () => {
        expect(() => {
            new ArtifactManager([
                fse.readJSONSync("test/samples/static/link_ref/artifacts/main.json")
            ]);
        }).not.toThrow();
    });

    it(`Can link library only with all addresses`, () => {
        const artifactManager: ArtifactManager = new ArtifactManager([
            fse.readJSONSync("test/samples/static/link_ref/artifacts/main.json")
        ]);
        const lib = artifactManager.contracts().filter((info) => info.contractName === "Lib")[0];
        const libBytecode = lib.bytecode;
        sol.assert(libBytecode !== undefined, ``);
        const linkRefs = libBytecode.linkReferences;
        const libId = "test/samples/static/link_ref/contracts/libraries.sol:Lib1";
        const addr = createAddressFromString("0xAaaaAaAAaaaAAaAAaAaaaaAAAAAaAaaaAaAaaAA0");

        const linkMap: LinkMap = new Map([[libId, addr]]);

        const linkedBytecode = artifactManager.link(libBytecode, linkMap);

        // All the link ranges are set correctly
        for (const range of linkRefs.get(libId) as BytecodeReference[]) {
            expect(linkedBytecode.slice(range.start, range.start + range.length)).toEqual(
                addr.bytes
            );
        }

        // Decoding works
        const decodedLinkMap = decodeLinkMap(libBytecode, linkedBytecode);
        expect(decodedLinkMap).toEqual(linkMap);

        // If we zero them out we get the original bytecode
        for (const range of linkRefs.get(libId) as BytecodeReference[]) {
            linkedBytecode.set(ZERO_ADDRESS.bytes, range.start);
        }

        expect(linkedBytecode).toEqual(libBytecode.bytecode);

        expect(() => {
            artifactManager.link(libBytecode, new Map());
        }).toThrow();
    });
});
