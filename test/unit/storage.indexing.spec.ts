import expect from "expect";
import {
    ArrayType,
    assert,
    BytesType,
    DataLocation,
    IntType,
    MappingType,
    PointerType,
    StringType,
    TypeNode,
    types
} from "solc-typed-ast";
import { DecodingFailure, hasPoison, Value } from "../../src/debug/decoding/value";
import { hexToBytes } from "ethereum-cryptography/utils";
import {
    ArrayStorageView,
    BaseStorageView,
    bigEndianBufToBigint,
    BytesStorageView,
    ExpStructType,
    FixedBytesStorageView,
    ImmMap,
    makeStorageView,
    MapStorageView,
    PointerStorageView,
    SingleByteStorageView,
    Storage,
    StructStorageView,
    uint256
} from "../../src";
import { bool, bytes21, bytes5, int32, int8, uint16, uint8 } from "../utils";
import { setLengthLeft } from "@ethereumjs/util";

type StorageDesc = { [key: string]: string };

function toStorage(s: StorageDesc): Storage {
    return ImmMap.fromEntries(
        Object.entries(s).map(([k, v]) => [
            bigEndianBufToBigint(hexToBytes(k.slice(2))),
            setLengthLeft(hexToBytes(v.slice(2)), 32)
        ])
    );
}

const simpleStorDesc = {
    "0x290decd9548b62a8d60345a988386fc84ba6bc95484008f6362f93160ef3e563":
        "0xcd6a42782d230d7c13a74ddec5dd140e55499df90180000000000001e240",
    "0x405787fa12a823e0f2b7631cc41b3ba8828b3321ca811111fa75cd3aa3bb5ace": "0x4342",
    "0xb10e2d527612073b26eecdfd717e6a320cf44b4afac2b0732d9fcbe2b7fa0cf6": "0x0405060708",
    "0xc2575a0e9e593c00f959f8c92f12db2869c3395a3b0502d05e2516446f71f85b": "0x44"
};
const arrStorDesc = {
    "0x036b6384b5eca791c62761152d0c79bb0604c104a5fb6f4eb0703f3154bb3db0": "0x69",
    "0x290decd9548b62a8d60345a988386fc84ba6bc95484008f6362f93160ef3e563": "0x05",
    "0x405787fa12a823e0f2b7631cc41b3ba8828b3321ca811111fa75cd3aa3bb5ace": "0x66",
    "0x510e4e770828ddbf7f7b00ab00a9f6adaf81c0dc9cc85f1f8249c256942d61d9":
        "0x07000006000005000004000003",
    "0x8a35acfbc15ff81a39ae7d344fd709f28e8600b4aa8c65c6b64bfe7fe36bd19b": "0x68",
    "0xb10e2d527612073b26eecdfd717e6a320cf44b4afac2b0732d9fcbe2b7fa0cf6": "0x65",
    "0xc2575a0e9e593c00f959f8c92f12db2869c3395a3b0502d05e2516446f71f85b": "0x67"
};
const bytesStorDesc = {
    "0x1ab0c6948a275349ae45a06aad66a8bd65ac18074615d53676c09b67809099e0":
        "0x0102030405060708090a0b0c0d0e0f000102030405060708090a0b0c0d0e0f",
    "0x290decd9548b62a8d60345a988386fc84ba6bc95484008f6362f93160ef3e563":
        "0x0102030405060708090a0b0c0d0e0f000102030405060708090a0b0c0d0e3e",
    "0x2f2149d90beac0570c7f26368e4bc897ca24bba51b1a0f4960d358f764f11f31":
        "0x0102030405060708090a0b0c0d0e0f00000000000000000000000000000000",
    "0x405787fa12a823e0f2b7631cc41b3ba8828b3321ca811111fa75cd3aa3bb5ace": "0x61",
    "0x8a35acfbc15ff81a39ae7d344fd709f28e8600b4aa8c65c6b64bfe7fe36bd19b": "0x41",
    "0xb10e2d527612073b26eecdfd717e6a320cf44b4afac2b0732d9fcbe2b7fa0cf6": "0x41",
    "0xb5d9d894133a730aa651ef62d26b0ffa846233c74177a591a4a896adfda97d22":
        "0x0102030405060708090a0b0c0d0e0f000102030405060708090a0b0c0d0e0f",
    "0xc167b0e3c82238f4f2d1a50a8b3a44f96311d77b148c30dc0ef863e1a060dcb6":
        "0x6161616161616161616161616161616161616161616161616161616161616161",
    "0xc2575a0e9e593c00f959f8c92f12db2869c3395a3b0502d05e2516446f71f85b":
        "0x616161616161616161616161616161616161616161616161616161616161613e"
};
const mapWithComplexKeysStorDesc = {
    "0x134266f27b803cd76c24f211c4457379daae4fac618eaef4ab979796508e0c3e": "0x03",
    "0x6b561827e89dd864e82f1287442a6f56c5408b1313c5133c7dacd8fed2fbd375": "0x04",
    "0xcb85c6f413feb024aaf9fe6ef133f21422160ad679b1000921a475400fde1ef5": "0x02",
    "0xe2fe0e2425d2aed896ad86c3e2c0ea7d679d08e1a849442d22f76adef98bbd97": "0x01"
};
const CStorDesc = {
    "0x028b9bace4e6c7d3310bd31a6a3810bcc4ffceb5c4e4242dbb19dacc27a59f72": "0x02",
    "0x11c44e4875b74d31ff9fd779bf2566af7bd15b87fc985d01f5094b89e3669e4f": "0x04",
    "0x37fa166cbdbfbb1561ccd9ea985ec0218b5e68502e230525f544285b2bdf3d7e": "0x010000000d",
    "0x4a2cc91ee622da3bc833a54c37ffcb6f3ec23b7793efc5eaf5e71b7b406c5c06": "0x2d0022",
    "0x6ff97a59c90d62cc7236ba3a37cd85351bf564556780cf8c1157a220f31f0cbb": "0x0a090807060504030201",
    "0x82a75bdeeae8604d839476ae9efd8b0e15aa447e21bfd7f41283bb54e22c9a82": "0x35",
    "0xa813484aef6fb598f9f753daf162068ff39ccea4075cb95e1a30f86995b5b7ee": "0x2a81",
    "0xbeced09521047d05b8960b7e7bcc1d1292cf3e4b2a6b63f48335cbde5f7545d2": "0x162e",
    "0xc54045fa7c6ec765e825df7f9e9bf9dec12c5cef146f93a5eee56772ee647fbc":
        "0x230000000000000000000000000000000000000000",
    "0xc9b370bcd3a6b8dd1220b7a7faea196be095b68db0e96af1b734f26b58075de4": "0x04030201",
    "0xde857217eaef9a2f6b2dade6c3e435fdb07f23d3e6a6109ea1626de7e649c81a": "0x0100000001"
};
const S = new ExpStructType("S", [
    ["x", int32],
    ["y", bool]
]);
const CLayoutType = new ExpStructType(
    "C",
    [
        ["a", uint256],
        ["e", new PointerType(new ArrayType(uint8), DataLocation.Storage)],
        [
            "f",
            new PointerType(
                new MappingType(uint256, new PointerType(S, DataLocation.Storage)),
                DataLocation.Storage
            )
        ],
        ["g", uint16],
        ["h", uint16],
        ["s", new PointerType(S, DataLocation.Storage)],
        ["k", int8],
        ["l", bytes21],
        ["m", new PointerType(new ArrayType(uint8, 12n), DataLocation.Storage)],
        ["n", new PointerType(new ArrayType(bytes5, 8n), DataLocation.Storage)],
        ["o", bytes5]
    ],
    undefined
);

const uint8x2 = new PointerType(new ArrayType(uint8, 2n), DataLocation.Storage);

const samples: Array<[StorageDesc, bigint, number, TypeNode, Value[] | Uint8Array]> = [
    [simpleStorDesc, 1n, 32, bytes5, hexToBytes("0405060708")],
    [simpleStorDesc, 2n, 32, uint8x2, [0x42n, 0x43n]],
    [
        arrStorDesc,
        0n,
        32,
        new PointerType(new ArrayType(new IntType(24, false)), DataLocation.Storage),
        [3n, 4n, 5n, 6n, 7n]
    ],
    [
        arrStorDesc,
        1n,
        32,
        new PointerType(new ArrayType(types.uint160, 5n), DataLocation.Storage),
        [101n, 102n, 103n, 104n, 105n]
    ],
    [
        bytesStorDesc,
        0n,
        32,
        new PointerType(new BytesType(), DataLocation.Storage),
        hexToBytes("000102030405060708090a0b0c0d0e0f000102030405060708090a0b0c0d0e")
    ],
    [
        bytesStorDesc,
        1n,
        32,
        new PointerType(new BytesType(), DataLocation.Storage),
        hexToBytes("000102030405060708090a0b0c0d0e0f000102030405060708090a0b0c0d0e0f")
    ],
    [
        bytesStorDesc,
        2n,
        32,
        new PointerType(new BytesType(), DataLocation.Storage),
        hexToBytes(
            "000102030405060708090a0b0c0d0e0f000102030405060708090a0b0c0d0e0f000102030405060708090a0b0c0d0e0f"
        )
    ]
];

describe(`Storage Indexing Tests`, () => {
    for (const [storageDesc, key, offset, type, expectedValue] of samples) {
        const storage = toStorage(storageDesc);

        it(`Sample ${type.pp()}`, () => {
            const view = makeStorageView(type, [BigInt(key), offset]);
            const value = view.decode(storage);

            assert(
                view instanceof FixedBytesStorageView ||
                    (view instanceof PointerStorageView &&
                        (view.toView() instanceof ArrayStorageView ||
                            view.toView() instanceof BytesStorageView)),
                `Expected indexable view`
            );

            expect(hasPoison(value)).toBeFalsy();
            expect(value).toEqual(expectedValue);

            for (let i = 0; i < expectedValue.length; i++) {
                const idxView = (
                    (view instanceof PointerStorageView ? view.toView() : view) as any
                ).indexView(BigInt(i), storage);
                expect(idxView).not.toBeInstanceOf(DecodingFailure);
                let expectedIdxVal = expectedValue[i];
                expectedIdxVal =
                    typeof expectedIdxVal === "number" ? BigInt(expectedIdxVal) : expectedIdxVal;
                expect((idxView as BaseStorageView<Value, TypeNode>).decode(storage)).toEqual(
                    expectedIdxVal
                );
            }
        });
    }

    it("Map with complex keys", () => {
        const m1View = makeStorageView(
            new PointerType(
                new MappingType(
                    new PointerType(new BytesType(), DataLocation.Memory),
                    types.uint256
                ),
                DataLocation.Storage
            ),
            [0n, 32]
        ) as PointerStorageView;
        const m2View = makeStorageView(
            new PointerType(
                new MappingType(
                    new PointerType(new StringType(), DataLocation.Memory),
                    types.uint256
                ),
                DataLocation.Storage
            ),
            [1n, 32]
        ) as PointerStorageView;
        const storage = toStorage(mapWithComplexKeysStorDesc);

        let v = (m1View.toView() as MapStorageView).indexView(hexToBytes("010203"));
        expect(v).not.toBeInstanceOf(DecodingFailure);
        expect((v as BaseStorageView<Value, TypeNode>).decode(storage)).toEqual(1n);
        v = (m1View.toView() as MapStorageView).indexView(hexToBytes("010205"));
        expect(v).not.toBeInstanceOf(DecodingFailure);
        expect((v as BaseStorageView<Value, TypeNode>).decode(storage)).toEqual(0n);

        v = (m2View.toView() as MapStorageView).indexView("abc");
        expect(v).not.toBeInstanceOf(DecodingFailure);
        expect((v as BaseStorageView<Value, TypeNode>).decode(storage)).toEqual(3n);

        v = (m2View.toView() as MapStorageView).indexView("xxx");
        expect(v).not.toBeInstanceOf(DecodingFailure);
        expect((v as BaseStorageView<Value, TypeNode>).decode(storage)).toEqual(0n);
    });

    it("SignleByteStorageView encoding test", () => {
        let storage = toStorage(simpleStorDesc);
        const view = new FixedBytesStorageView(bytes5, [1n, 32]);
        const elView = view.indexView(4n); // ef
        expect(elView).not.toBeInstanceOf(DecodingFailure);
        storage = (elView as SingleByteStorageView).encode(1n, storage);
        expect(view.decode(storage)).toEqual(hexToBytes("0405060701"));
    });

    it("Struct field test", () => {
        const storage = toStorage(CStorDesc);
        const view = makeStorageView(CLayoutType, [42n, 32]) as StructStorageView;

        // a
        let fView = view.fieldView("a");
        expect(fView).not.toBeInstanceOf(DecodingFailure);
        expect((fView as BaseStorageView<Value, TypeNode>).decode(storage)).toEqual(5678n);
        // e
        fView = view.fieldView("e");
        expect(fView).not.toBeInstanceOf(DecodingFailure);
        expect((fView as BaseStorageView<Value, TypeNode>).decode(storage)).toEqual([
            1n,
            2n,
            3n,
            4n
        ]);

        fView = (
            (view.fieldView("s") as PointerStorageView).toView() as StructStorageView
        ).fieldView("x");
        expect(fView).not.toBeInstanceOf(DecodingFailure);
        expect((fView as BaseStorageView<Value, TypeNode>).decode(storage)).toEqual(13n);
    });
});
