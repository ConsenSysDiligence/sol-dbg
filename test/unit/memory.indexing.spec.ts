import expect from "expect";
import {
    ArrayType,
    assert,
    ASTReader,
    compileSourceString,
    DataLocation,
    InferType,
    PointerType,
    SourceUnit,
    TypeNode,
} from "solc-typed-ast";
import { hasPoison,  Value } from "../../src/debug/decoding/value";
import { hexToBytes } from "ethereum-cryptography/utils";
import { single } from "../../src";
import { int128, uint16 } from "../utils";
import { ArrayMemView, BaseMemoryView, BytesMemView, DecodingFailure, makeMemoryView, PointerMemView, simplifyType } from "../../src/debug/decoding/";
import fse from "fs-extra";

const infer = new InferType("0.8.29");
type TypeGenerator = (unit: SourceUnit) => TypeNode;
const samples: Array<[string, number, TypeNode | TypeGenerator, Value[]]> = [
    /*
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
    */
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
    ],
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
            let view: BaseMemoryView<Value, TypeNode> | DecodingFailure = makeMemoryView(type, BigInt(offset));
            if (view instanceof PointerMemView) {
                view = view.toView(memory)
                assert(!(view instanceof DecodingFailure), ``);
            }

            assert(view  instanceof ArrayMemView || view instanceof BytesMemView, `Expected indexable view`);

            const value = view.decode(memory);
            expect(hasPoison(value)).toBeFalsy();
            expect(value).toEqual(expectedValue);
            for (let i = 0; i < expectedValue.length; i++) {
                const idxView = view.indexView(BigInt(i), memory)
                expect(idxView).not.toBeInstanceOf(DecodingFailure);
                expect((idxView as BaseMemoryView<Value, TypeNode>).decode(memory)).toEqual(expectedValue[i])
            }
        });
    }
});
