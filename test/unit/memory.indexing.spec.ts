import expect from "expect";
import {
    ArrayType,
    assert,
    ASTReader,
    BytesType,
    compileSourceString,
    DataLocation,
    InferType,
    PointerType,
    SourceUnit,
    TypeNode,
} from "solc-typed-ast";
import { hasPoison, Value } from "../../src/debug/decoding/value";
import { hexToBytes } from "ethereum-cryptography/utils";
import { single, uint256 } from "../../src";
import { address, bool, bytes2, bytes3, bytes32, int128, int8, uint16 } from "../utils";
import {
    ArrayMemView,
    BaseMemoryView,
    BytesMemView,
    DecodingFailure,
    ExpStructType,
    FixedBytesMemView,
    makeMemoryView,
    PointerMemView,
    simplifyType,
    SingleByteMemView,
    StructMemView
} from "../../src/debug/decoding/";
import fse from "fs-extra";

const infer = new InferType("0.8.29");
type TypeGenerator = (unit: SourceUnit) => TypeNode;
const samples: Array<[string, number, TypeNode | TypeGenerator, Value[] | Uint8Array]> = [
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

let unit: SourceUnit;

beforeAll(async () => {
    const file = fse.readFileSync("test/samples/decoding/memory_views_test.sol", {
        encoding: "utf-8"
    });
    const compileResult = await compileSourceString("memory_views_test.sol", file, "0.8.21");
    const reader = new ASTReader();
    unit = single(reader.read(compileResult.data));
});

function ppType(t: TypeNode | TypeGenerator): string {
    if (t instanceof TypeNode) {
        return t.pp();
    }

    return "<type-generator>";
}

describe(`Memory Indexing Tests`, () => {
    for (const [memoryStr, offset, typeDesc, expectedValue] of samples) {
        const memory = hexToBytes(memoryStr);

        it(`Sample ${ppType(typeDesc)}`, () => {
            const type = simplifyType(
                typeDesc instanceof TypeNode ? typeDesc : typeDesc(unit),
                infer,
                DataLocation.Memory
            );
            let view: BaseMemoryView<Value, TypeNode> | DecodingFailure = makeMemoryView(
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
                expect((idxView as BaseMemoryView<Value, TypeNode>).decode(memory)).toEqual(
                    expectedValue[i]
                );
            }
        });
    }

    it("SignleByteMemView encoding test", () => {
        const mem = hexToBytes("0000000000000000000000000000000000000000000000000000000000abcdef");
        const view = new FixedBytesMemView(bytes32, 0n);
        const elView = view.indexView(31n); // ef
        expect(elView).not.toBeInstanceOf(DecodingFailure);
        (elView as SingleByteMemView).encode(1, mem);
        expect(view.decode(mem)).toEqual(
            hexToBytes("0000000000000000000000000000000000000000000000000000000000abcd01")
        );
    });

    it("Struct field test", () => {
        const mem = hexToBytes("000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000180000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000e0000000000000000000000000000000000000000000000000000000000000010007080900000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000b26e0000000000000000000000000000000000000000000000000000000000000001000000000000000000000000cd6a42782d230d7c13a74ddec5dd140e55499df9")
        const view = makeMemoryView(new ExpStructType("", [
            ["t", new PointerType(new ArrayType(uint16), DataLocation.Memory)],
            ["s", new PointerType(new ExpStructType("", [
                ["x", int8],
                ["y", uint256],
                ["b", bool],
                ["addrs", address]
            ]), DataLocation.Memory)],
            ["b", bytes3]
        ]), 128n) as StructMemView
        
        // t
        let fView = view.fieldView("t") as BaseMemoryView<Value, TypeNode>
        expect(fView.decode(mem)).toEqual([]);
        fView = view.fieldView("b") as BaseMemoryView<Value, TypeNode>
        expect(fView.decode(mem)).toEqual(hexToBytes("0x070809"));
        let sView = (view.fieldView("s") as PointerMemView).toView(mem) as StructMemView;
        fView = sView.fieldView("y") as BaseMemoryView<Value, TypeNode>
        expect(fView.decode(mem)).toEqual(45678n);
    })
});
