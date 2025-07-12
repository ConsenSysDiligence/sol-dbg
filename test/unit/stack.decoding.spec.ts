import expect from "expect";
import { DataLocation, InferType, TypeNode } from "solc-typed-ast";
import { hasPoison, Value } from "../../src/debug/decoding/value";
import { Stack } from "../../src";
import {
    DecodingFailure,
    FixedBytesStackView,
    makeStackView,
    simplifyType,
    SingleByteStackView
} from "../../src/debug/decoding/";
import { hexToBytes } from "ethereum-cryptography/utils";
import { address, bool, bytes21, int128, uint16, uint8 } from "../utils";
import { createAddressFromString } from "@ethereumjs/util";

const infer = new InferType("0.8.29");
const stack = [
    hexToBytes("0000000000000000000000000000000000000000000000000000000000000000"),
    hexToBytes("0000000000000000000000000000000000000000000000000000000000000001"),
    hexToBytes("0000000000000000000000000000000000000000000000000000000000010000"),
    hexToBytes("ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"),
    hexToBytes("000000000000000000000000cD6a42782d230D7c13A74ddec5dD140e55499Df9"),
    hexToBytes("cD6a42782d230D7c13A74ddec5dD140e55499Df9000000000000000000000000")
].reverse();

const samples: Array<[Stack, number, TypeNode, Value]> = [
    [stack, 0, bool, false],
    [stack, 1, bool, true],
    [stack, 1, uint8, 1n],
    [stack, 2, uint16, 65536n],
    [stack, 3, int128, -1n],
    [stack, 4, address, createAddressFromString("0xcD6a42782d230D7c13A74ddec5dD140e55499Df9")],
    [stack, 5, bytes21, hexToBytes("cD6a42782d230D7c13A74ddec5dD140e55499Df900")]
];

describe(`Stack Decoding Tests`, () => {
    for (const [stack, offFromTop, type, expectedValue] of samples) {
        it(`Sample ${type.pp()}`, () => {
            const simpleType = simplifyType(type, infer, DataLocation.Memory);
            const view = makeStackView(simpleType, offFromTop);
            const value = view.decode(stack);
            expect(hasPoison(value)).toBeFalsy();
            expect(value).toEqual(expectedValue);
        });
    }

    it("Fixed bytes indexing", () => {
        const fbView = makeStackView(bytes21, 5) as FixedBytesStackView;
        const bView = fbView.indexView(1n) as SingleByteStackView;
        expect(bView).not.toBeInstanceOf(DecodingFailure);
        expect(bView.decode(stack)).toEqual(0x6a);
        bView.encode(0x01, stack);
        expect(fbView.decode(stack)).toEqual(
            hexToBytes("cD0142782d230D7c13A74ddec5dD140e55499Df900")
        );
    });
});
