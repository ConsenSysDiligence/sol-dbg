import { IntMemView } from "../../src/debug/decoding/memory/view";
import { encodeBigintInBigEndianBuf } from "../../src/utils";
import { int256, uint256 } from "../utils/rtt_types";
const samples = [
    0n,
    -1n,
    1n,
    127n,
    128n,
    -127n,
    -128n,
    -129n,
    12345678910n,
    -12345678910n,
    115792089237316195423570985008687907853269984665640564039457584007913129639935n,
    -57896044618658097711785492504343953926634992332820282019728792003956564819968n,
    57896044618658097711785492504343953926634992332820282019728792003956564819968n
];

describe(`Twos complement tests`, () => {
    const buf = new Uint8Array(32);
    const intView = new IntMemView(int256, 0n);
    const uintView = new IntMemView(uint256, 0n);

    it("Roundtrip decoding", () => {
        for (const n of samples) {
            encodeBigintInBigEndianBuf(n, buf, 32);
            const x = (n < 0 ? intView : uintView).decode(buf);
            expect(x).toEqual(n);
        }
    });

    it("Unsigned overflow", () => {
        encodeBigintInBigEndianBuf(
            115792089237316195423570985008687907853269984665640564039457584007913129639936n,
            buf,
            32
        );
        expect(uintView.decode(buf)).toEqual(0n);
    });

    it("Signed overflow", () => {
        encodeBigintInBigEndianBuf(
            57896044618658097711785492504343953926634992332820282019728792003956564819968n,
            buf,
            32
        );
        expect(intView.decode(buf)).toEqual(
            -57896044618658097711785492504343953926634992332820282019728792003956564819968n
        );
    });

    it("Signed underflow", () => {
        encodeBigintInBigEndianBuf(
            -57896044618658097711785492504343953926634992332820282019728792003956564819969n,
            buf,
            32
        );
        expect(intView.decode(buf)).toEqual(
            57896044618658097711785492504343953926634992332820282019728792003956564819967n
        );
    });

    it("Signed same at positive values", () => {
        encodeBigintInBigEndianBuf(
            57896044618658097711785492504343953926634992332820282019728792003956564819967n,
            buf,
            32
        );
        expect(intView.decode(buf)).toEqual(
            57896044618658097711785492504343953926634992332820282019728792003956564819967n
        );
    });
});
