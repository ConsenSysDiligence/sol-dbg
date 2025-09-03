import expect from "expect";
import { assert, DataLocation } from "solc-typed-ast";
import { hasPoison, Value } from "../../src/debug/decoding/value";
import { hexToBytes } from "ethereum-cryptography/utils";
import { uint256 } from "../../src";
import { address, bool, bytes2, bytes3, bytes32, int128, int8, uint16 } from "../utils/rtt_types";
import {
    ArrayMemView,
    BaseMemoryView,
    BytesMemView,
    DecodingFailure,
    FixedBytesMemView,
    makeMemoryView,
    PointerMemView,
    SingleByteMemView,
    StructMemView
} from "../../src/debug/decoding/";
import {
    ArrayType,
    BaseRuntimeType,
    BytesType,
    PointerType,
    StructType
} from "../../src/debug/runtime_types";

const samples: Array<[string, number, BaseRuntimeType, Value[] | Uint8Array]> = [
    [
        "0102000000000000000000000000000000000000000000000000000000000000",
        0,
        bytes2,
        hexToBytes("0102")
    ],
    [
        "0000000000000000000000000000000000000000000000000000000000abcdef",
        0,
        bytes32,
        hexToBytes("0000000000000000000000000000000000000000000000000000000000abcdef")
    ],
    [
        "000000000000000000000000000000000000000000000000000000000000000a0102030405060708090a00000000000000000000000000000000000000000000",
        0,
        new BytesType(),
        hexToBytes("0102030405060708090a")
    ],

    [
        "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000c000000000000000000000000000000000000000000000000000000000000001400000000000000000000000000000000000000000000000000000000000000003000000000000000000000000000000000000000000000000000000000000000c000000000000000000000000000000000000000000000000000000000000000d000000000000000000000000000000000000000000000000000000000000000efffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffdfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc0000000000000000000000000000000000000000000000000000000000000080",
        128,
        new PointerType(new ArrayType(uint16), DataLocation.Memory),
        [12n, 13n, 14n]
    ],
    [
        "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000c000000000000000000000000000000000000000000000000000000000000001400000000000000000000000000000000000000000000000000000000000000003000000000000000000000000000000000000000000000000000000000000000c000000000000000000000000000000000000000000000000000000000000000d000000000000000000000000000000000000000000000000000000000000000efffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffdfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc0000000000000000000000000000000000000000000000000000000000000080",
        160,
        new PointerType(new ArrayType(int128, 4n), DataLocation.Memory),
        [-1n, -2n, -3n, -4n]
    ]
];

describe(`Memory Indexing Tests`, () => {
    for (const [memoryStr, offset, type, expectedValue] of samples) {
        const memory = hexToBytes(memoryStr);

        it(`Sample ${type.pp()}`, () => {
            let view: BaseMemoryView<Value, BaseRuntimeType> | DecodingFailure = makeMemoryView(
                type,
                BigInt(offset)
            );

            if (view instanceof PointerMemView) {
                view = view.toView(memory);
                assert(!(view instanceof DecodingFailure), ``);
            }

            assert(
                view instanceof ArrayMemView ||
                    view instanceof BytesMemView ||
                    view instanceof FixedBytesMemView,
                `Expected indexable view`
            );

            const value = view.decode(memory);
            expect(hasPoison(value)).toBeFalsy();
            expect(value).toEqual(expectedValue);

            for (let i = 0; i < expectedValue.length; i++) {
                const idxView = view.indexView(BigInt(i), memory);
                expect(idxView).not.toBeInstanceOf(DecodingFailure);
                let expectedIdxVal = expectedValue[i];
                expectedIdxVal =
                    typeof expectedIdxVal === "number" ? BigInt(expectedIdxVal) : expectedIdxVal;

                expect((idxView as BaseMemoryView<Value, BaseRuntimeType>).decode(memory)).toEqual(
                    expectedIdxVal
                );
            }
        });
    }

    it("SignleByteMemView encoding test", () => {
        const mem = hexToBytes("0000000000000000000000000000000000000000000000000000000000abcdef");
        const view = new FixedBytesMemView(bytes32, 0n);
        const elView = view.indexView(31n); // ef
        expect(elView).not.toBeInstanceOf(DecodingFailure);
        (elView as SingleByteMemView).encode(1n, mem);
        expect(view.decode(mem)).toEqual(
            hexToBytes("0000000000000000000000000000000000000000000000000000000000abcd01")
        );
    });

    it("Struct field test", () => {
        const mem = hexToBytes(
            "000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000180000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000e0000000000000000000000000000000000000000000000000000000000000010007080900000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000b26e0000000000000000000000000000000000000000000000000000000000000001000000000000000000000000cd6a42782d230d7c13a74ddec5dd140e55499df9"
        );
        const view = makeMemoryView(
            new StructType("", [
                ["t", new PointerType(new ArrayType(uint16), DataLocation.Memory)],
                [
                    "s",
                    new PointerType(
                        new StructType("", [
                            ["x", int8],
                            ["y", uint256],
                            ["b", bool],
                            ["addrs", address]
                        ]),
                        DataLocation.Memory
                    )
                ],
                ["b", bytes3]
            ]),
            128n
        ) as StructMemView;

        // t
        let fView = view.fieldView("t") as BaseMemoryView<Value, BaseRuntimeType>;
        expect(fView.decode(mem)).toEqual([]);
        fView = view.fieldView("b") as BaseMemoryView<Value, BaseRuntimeType>;
        expect(fView.decode(mem)).toEqual(hexToBytes("0x070809"));
        const sView = (view.fieldView("s") as PointerMemView).toView(mem) as StructMemView;
        fView = sView.fieldView("y") as BaseMemoryView<Value, BaseRuntimeType>;
        expect(fView.decode(mem)).toEqual(45678n);
    });
});
