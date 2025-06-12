import expect from "expect";
import {
    ArrayType,
    ASTReader,
    compileSourceString,
    DataLocation,
    InferType,
    MappingType,
    PointerType,
    SourceUnit,
    TypeNode
} from "solc-typed-ast";
import { Struct, Value } from "../../src/debug/decoding/value";
import { hexToBytes } from "ethereum-cryptography/utils";
import {
    bigEndianBufToBigint,
    ExpStructType,
    ImmMap,
    makeStorageView,
    MapKeys,
    single,
    Storage,
    uint256
} from "../../src";
import { address, bool, bytes21, bytes5, int32, int8, uint16, uint64, uint8 } from "../utils";
import fse from "fs-extra";
import { ppType, TypeGenerator } from "../utils/misc";
import { Address, setLengthLeft } from "@ethereumjs/util";

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

const uint8x2 = new PointerType(new ArrayType(uint8, 2n), DataLocation.Storage);
const S = new ExpStructType("S", [
    ["x", int32],
    ["y", bool]
]);
const CLayoutType = new ExpStructType(
    "C_layout",
    [
        ["a", uint256],
        ["e", new PointerType(new ArrayType(uint8), DataLocation.Storage)],
        ["f", new MappingType(uint256, S)],
        ["g", uint16],
        ["h", uint16],
        ["s", new PointerType(S, DataLocation.Storage)],
        ["k", int8],
        ["l", bytes21],
        ["m", new PointerType(new ArrayType(uint8, 10n), DataLocation.Storage)],
        ["n", new PointerType(new ArrayType(bytes5, 8n), DataLocation.Storage)],
        ["o", bytes5]
    ],
    undefined
);

const samples: Array<[StorageDesc, number, number, TypeNode | TypeGenerator, Value]> = [
    [simpleStorDesc, 0, 32, uint64, 123456n],
    [simpleStorDesc, 0, 24, int8, -128n],
    [simpleStorDesc, 0, 23, bool, true],
    [
        simpleStorDesc,
        0,
        22,
        address,
        Address.fromString("0xcD6a42782d230D7c13A74ddec5dD140e55499Df9")
    ],
    [simpleStorDesc, 0, 2, bool, false],
    [simpleStorDesc, 1, 32, bytes5, hexToBytes("0405060708")],
    [simpleStorDesc, 2, 32, uint8x2, [0x42n, 0x43n]],
    [simpleStorDesc, 3, 32, uint8, 0x44n],
    [CStorDesc, 42, 32, uint256, 5678n],
    [
        CStorDesc,
        42,
        32,
        CLayoutType,
        new Struct([
            ["a", 5678n],
            ["e", [1n, 2n, 3n, 4n]],
            ["f", new Map()],
            ["g", 34n],
            ["h", 45n],
            [
                "s",
                new Struct([
                    ["x", 13n],
                    ["y", true]
                ])
            ],
            ["k", -127n],
            ["l", hexToBytes("00000000000000000000000000000000000000002a")],
            ["m", [1n, 2n, 3n, 4n, 5n, 6n, 7n, 8n, 9n, 10n]],
            [
                "n",
                [
                    hexToBytes("0000000000"),
                    hexToBytes("0000000000"),
                    hexToBytes("0000000000"),
                    hexToBytes("0000000000"),
                    hexToBytes("0000000023"),
                    hexToBytes("0000000000"),
                    hexToBytes("0000000000"),
                    hexToBytes("0000000000")
                ]
            ],
            ["o", hexToBytes("0000000035")]
        ])
    ]
];

let unit: SourceUnit;

beforeAll(async () => {
    const file = fse.readFileSync("test/samples/decoding/storage_views_test.sol", {
        encoding: "utf-8"
    });
    const compileResult = await compileSourceString("storage_views_test.sol", file, "0.8.29");
    const reader = new ASTReader();
    unit = single(reader.read(compileResult.data));
});

describe(`Storage Decoding Tests`, () => {
    const infer = new InferType("0.8.21");
    for (const [storageDesc, key, offset, typeDesc, expectedValue] of samples) {
        const storage = toStorage(storageDesc);

        it(`Sample ${ppType(typeDesc)}`, () => {
            const type = typeDesc instanceof TypeNode ? typeDesc : typeDesc(unit);
            const view = makeStorageView(type, infer, [BigInt(key), offset]);
            const value = view.decode(storage);
            expect(value).toEqual(expectedValue);
        });
    }

    it(`Map decoding with keys`, () => {
        const mapKeys: MapKeys = new Map([
            [44n, [
                [hexToBytes("0000000000000000000000000000000000000000000000000000000000000000"), 0x4bd6275b77b7e49eb0792b10b407951644bbdba590ba83c6764fe2c9da0b9befn],
                [hexToBytes("0000000000000000000000000000000000000000000000000000000000000001"), 0xa1f88ee5f5d946e3956f6291445d84cd8aea2bf6c57f4f4ac349f7a338882643n]
            ]]
        ])

        const expected = new Map([
            [0n, new Struct([["x", 1n], ["y", true]])],
            [1n, new Struct([["x", 2n], ["y", false]])]
        ]);

        const view = makeStorageView(CLayoutType, infer, [42n, 32], mapKeys);
        const value = view.decode(toStorage(CStorDesc)) as Struct;
        expect(value.field("f")).toEqual(expected);
    })
});
