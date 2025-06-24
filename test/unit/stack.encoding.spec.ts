import expect from "expect";
import { DataLocation, InferType, TypeNode } from "solc-typed-ast";
import { hasPoison, Value } from "../../src/debug/decoding/value";
import { Stack } from "../../src";
import { makeStackView, simplifyType } from "../../src/debug/decoding";
import { bytesToHex, hexToBytes } from "ethereum-cryptography/utils";
import { address, bool, bytes21, int128, uint16, uint8 } from "../utils";
import { createAddressFromString } from "@ethereumjs/util";

const infer = new InferType("0.8.29");
const stack = [
    hexToBytes("0000000000000000000000000000000000000000000000000000000000000000"),
    hexToBytes("0000000000000000000000000000000000000000000000000000000000000001"),
    hexToBytes("0000000000000000000000000000000000000000000000000000000000010000"),
    hexToBytes("00000000000000000000000000000000ffffffffffffffffffffffffffffffff"),
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

describe(`Stack Encoding Tests`, () => {
    for (const [stack, offFromTop, type, value] of samples) {
        const buf = new Uint8Array(32);

        it(`Sample ${type.pp()}`, () => {
            const simpleType = simplifyType(type, infer, DataLocation.Memory);
            const encView = makeStackView(simpleType, 0);
            encView.encode(value, [buf]);
            expect(bytesToHex(buf)).toEqual(bytesToHex(stack[stack.length - offFromTop - 1]));
        });
    }
});

const rttSamples: [TypeNode, Value][] = [
    [bool, false],
    [bool, true],
    [uint8, 1n],
    [uint16, 65536n],
    [int128, -1n],
    [address, createAddressFromString("0xcD6a42782d230D7c13A74ddec5dD140e55499Df9")],
    [bytes21, hexToBytes("cD6a42782d230D7c13A74ddec5dD140e55499Df900")]
]

describe(`Stack Encoding/Decoding RTT Tests`, () => {
    for (const [type, value] of rttSamples) {
        const buf = new Uint8Array(32);

        it(`Sample ${type.pp()}`, () => {
            const simpleType = simplifyType(type, infer, DataLocation.Memory);
            const view = makeStackView(simpleType, 0);
            view.encode(value, [buf]);
            const newVal = view.decode([buf]);
            expect(hasPoison(newVal)).toBeFalsy();
            expect(newVal).toEqual(value);
        });
    }
});
