import expect from "expect";
import {
    ArrayType,
    ASTReader,
    BytesType,
    compileSourceString,
    DataLocation,
    InferType,
    MappingType,
    PointerType,
    SourceUnit,
    StringType,
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
import {
    address,
    bool,
    bytes2,
    bytes21,
    bytes3,
    bytes32,
    bytes5,
    int128,
    int16,
    int32,
    int8,
    uint16,
    uint64,
    uint8
} from "../utils";
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
const moreStructsStorDesc = { '0x0175b7a638427703f0dbe7bb9bbf987a2551717b34e79f33b5b1008d1fa01db9': '0x02', '0x02c1acb1a5666d7e1b67652ace0fcd5a29681aed81ffbc4cf16cfeaa3c83369a': '0x02', '0x036b6384b5eca791c62761152d0c79bb0604c104a5fb6f4eb0703f3154bb3db0': '0xff', '0x057c384a7d1c54f3a1b2e5e67b2617b8224fdfd1ea7234eea573a6ff665ff63e': '0x02', '0x07d411a2b75625732d79f376a36908a89c9e8d258d2d147aceaa648a31375331': '0x01', '0x0e4562a10381dec21b205ed72637e6b1b523bdd0e4d4d50af5cd23dd4500a211': '0x80', '0x1b6847dc741a1b0cd08d278845f9d819d87b734759afb55fe2de5cb82a9ae672': '0x040506', '0x290decd9548b62a8d60345a988386fc84ba6bc95484008f6362f93160ef3e563': '0xffffff', '0x2eb6f567f599b88c7d439ce330a3bcb4f83de9bfccc843b6db6f3b2d11bdd8f2': '0xff', '0x3ad8aa4f87544323a9d1e5dd902f40c356527a7955687113db5f9a85ad579dc1': '0x020304', '0x405787fa12a823e0f2b7631cc41b3ba8828b3321ca811111fa75cd3aa3bb5ace': '0x01025b38da6a701c568545dcfcb03fcb875f56beddc401', '0x50bb669a95c7b50b7e8a6f09454034b2b14cf2b85c730dca9a539ca82cb6e350': '0x03', '0x55f448fdea98c4d29eb340757ef0a66cd03dbb9538908a6a81d96026b71ec475': '0x070809', '0x66de8ffda797e3de9c05e8fc57b3bf0ec28a930d40b0d285d93c06501cf6a090': '0xb26e', '0x6d4407e7be21f808e6509aa9fa9143369579dd7d760fe20a2c09680fc146134f': '0x02', '0x6e1540171b6c0c960b71a7020d9f60077f6af931a8bbf590da0223dacf75c7af': '0xb26e', '0x744fd2b04a866f3f5dcdb35fccc45b3d9cdec1c501e077f496ff358350592bcf': '0xcd6a42782d230d7c13a74ddec5dd140e55499df9', '0x75fcf6b6672bc14555e26639c35fcbb1aecdb68284a287ed21d7fc00252d71e9': '0xb26e', '0x76efdf26cb3a0d248906a912be95456494193d31e0497fb65b9a0bac08e803a5': '0xcd6a42782d230d7c13a74ddec5dd140e55499df9', '0x7bdb4cff72c5c3c62e5a52b046dbbafca87d8f0fbf7f0798db0c7d39182e65b7': '0xb26f', '0x8a35acfbc15ff81a39ae7d344fd709f28e8600b4aa8c65c6b64bfe7fe36bd19b': '0x01', '0x8d1108e10bcb7c27dddfc02ed9d693a074039d026cf4ea4240b40f7d581ac802': '0xcd6a42782d230d7c13a74ddec5dd140e55499df901', '0x8d7741e02537e0cbef65263699dc0b2909234e0e4f7d77f08fa28cd36d8ac6a8': '0x0e000d000c', '0x944998273e477b495144fb8794c914197f3ccb46be2900f4698fd0ef743c9695': '0x01', '0x9b787f7d3ff23785e0f06d6a3590539691b65820914f14b4c2050eca4b3f92fe': '0xfe', '0xa03837a25210ee280c2113ff4b77ca23440b19d4866cca721c801278fd08d807': '0xfffffffffffffffffffffffffffffffeffffffffffffffffffffffffffffffff', '0xa66cc928b5edb82af9bd49922954155ab7b0942694bea4ce44661d9a8736c688': '0xcd6a42782d230d7c13a74ddec5dd140e55499df901', '0xb10e2d527612073b26eecdfd717e6a320cf44b4afac2b0732d9fcbe2b7fa0cf6': '0x01e240', '0xb13d2d76d1f4b7be834882e410b3e3a8afaf69f83600ae24db354391d2378d2e': '0xb26e', '0xbb7b4a454dc3493923482f07822329ed19e8244eff582cc204f8554c3620c3fd': '0xb26e', '0xbb8a6a4669ba250d26cd7a459eca9d215f8307e33aebe50379bc5a3617ec3444': '0xff', '0xc2575a0e9e593c00f959f8c92f12db2869c3395a3b0502d05e2516446f71f85b': '0xabcdef', '0xc624b66cc0138b8fabc209247f72d758e1cf3343756d543badbf24212bed8c15': '0xff', '0xc65a7bb8d6351c1cf70c95a316cc6a92839c986682d98bc35f958f4883f9d2a8': '0x01', '0xc97bfaf2f8ee708c303a06d134f5ecd8389ae0432af62dc132a24118292866bb': '0xfffffffffffffffffffffffffffffffcfffffffffffffffffffffffffffffffd', '0xce6d7b5282bd9a3661ae061feed1dbda4e52ab073b1f9285be6e155d9c38d4ec': '0xcd6a42782d230d7c13a74ddec5dd140e55499df901', '0xd7b6990105719101dabeb77144f2a3385c8033acd3af97e9423a695e81ad1eb5': '0xff', '0xd833147d7dc355ba459fc788f669e58cfaf9dc25ddcd0702e87d69c7b5124289': '0xfb2e', '0xde9647372d0c1a3bf7d388adb42a76542657d064a5909aaa3ee76e8d628c8ac5': '0xcd6a42782d230d7c13a74ddec5dd140e55499df9', '0xdf6966c971051c3d54ec59162606531493a51404a002842f56009d7e5cf4a8c7': '0xffff', '0xf3f7a9fe364faab93b216da50a3214154f22a0a2b415b23a84c8169e8b636ee3': '0xff', '0xf652222313e28459528d920b65115c16c04f3efc82aaedc97be59f3f377c0d3f': '0xb26e' }
const bytesStorDesc = { '0x2584db4a68aa8b172f70bc04e2e74541617c003374de6eb4b295e823e5beab01': '0x6161616161616161616161616161616161616161616161616161616161616161', '0x290decd9548b62a8d60345a988386fc84ba6bc95484008f6362f93160ef3e563': '0x0102030405060708090a0b0c0d0e0f000102030405060708090a0b0c0d0e3e', '0x405787fa12a823e0f2b7631cc41b3ba8828b3321ca811111fa75cd3aa3bb5ace': '0x616161616161616161616161616161616161616161616161616161616161613e', '0xb10e2d527612073b26eecdfd717e6a320cf44b4afac2b0732d9fcbe2b7fa0cf6': '0x41', '0xb5d9d894133a730aa651ef62d26b0ffa846233c74177a591a4a896adfda97d22': '0x0102030405060708090a0b0c0d0e0f000102030405060708090a0b0c0d0e0f', '0xc2575a0e9e593c00f959f8c92f12db2869c3395a3b0502d05e2516446f71f85b': '0x41' }

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

const SimpleTypes = new ExpStructType("SimpleTypes", [
    ["a", int8],
    ["b", uint16],
    ["c", uint256],
    ["d", bool],
    ["e", address],
    ["b1", bytes2],
    ["b2", bytes32],
    ["en", uint8]
]);
const S_static = new ExpStructType("S_static", [
    ["x", int8],
    ["y", uint256],
    ["b", bool],
    ["addrs", address]
]);
const S1 = new ExpStructType("S1", [
    ["x", int8],
    ["y", uint256],
    ["b", bool],
    ["addrs", new PointerType(new ArrayType(address), DataLocation.Storage)]
]);
const S_nested_static_static = new ExpStructType("S_nested_static_static", [
    ["t", int16],
    ["s", new PointerType(S_static, DataLocation.Storage)],
    ["b", bytes3]
]);
const S_nested_dynamic_static = new ExpStructType("S_nested_dynamic_static", [
    ["t", new PointerType(new ArrayType(int16), DataLocation.Storage)],
    ["s", new PointerType(S_static, DataLocation.Storage)],
    ["b", bytes3]
]);
const S_nested_static_dynamic = new ExpStructType("S_nested_static_dynamic", [
    ["t", int16],
    ["s", new PointerType(S1, DataLocation.Storage)],
    ["b", bytes3]
]);
const S_struct_arr = new ExpStructType("S_struct_arr", [
    ["x", int8],
    ["sArr", new PointerType(new ArrayType(S1), DataLocation.Storage)]
]);
const ArrTypes = new ExpStructType("ArrTypes", [
    ["a1", new PointerType(new ArrayType(int16), DataLocation.Storage)],
    ["a2", new PointerType(new ArrayType(int128, 4n), DataLocation.Storage)]
]);

const MoreStructsLayoutType = new ExpStructType("MoreStructs", [
    ["st", new PointerType(SimpleTypes, DataLocation.Storage)],
    ["s_static", new PointerType(S_static, DataLocation.Storage)],
    ["s1", new PointerType(S1, DataLocation.Storage)],
    ["s_nested_static_static", new PointerType(S_nested_static_static, DataLocation.Storage)],
    ["s_nested_dynamic_static", new PointerType(S_nested_dynamic_static, DataLocation.Storage)],
    ["s_nested_static_dynamic", new PointerType(S_nested_static_dynamic, DataLocation.Storage)],
    ["s_struct_arr", new PointerType(S_struct_arr, DataLocation.Storage)],
    ["at", new PointerType(ArrTypes, DataLocation.Storage)]
]);
const bytesLayoutType = new ExpStructType("Bytes", [
    ["smallB", new PointerType(new BytesType(), DataLocation.Storage)],
    ["bigB", new PointerType(new BytesType(), DataLocation.Storage)],
    ["smallB", new PointerType(new StringType(), DataLocation.Storage)],
    ["bigB", new PointerType(new StringType(), DataLocation.Storage)],
])

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
    ],
    [moreStructsStorDesc, 0, 32, MoreStructsLayoutType, new Struct([
        ["st", new Struct([
            ["a", -1n],
            ["b", 65535n],
            ["c", 123456n],
            ["d", true],
            ["e", Address.fromString("0x5B38Da6a701c568545dCfcB03FcB875f56beddC4")],
            ["b1", hexToBytes("0102")],
            ["b2", hexToBytes("0000000000000000000000000000000000000000000000000000000000abcdef")],
            ["en", 1n],
        ])],
        ["s_static", new Struct([
            ["x", -1n],
            ["y", 45678n],
            ["b", true],
            ["addrs", Address.fromString("0xcD6a42782d230D7c13A74ddec5dD140e55499Df9")],
        ])],
        ["s1", new Struct([
            ["x", -1n],
            ["y", 45678n],
            ["b", true],
            ["addrs", [Address.fromString("0x0000000000000000000000000000000000000000"), Address.fromString("0xcD6a42782d230D7c13A74ddec5dD140e55499Df9")]],

        ])],
        ["s_nested_static_static", new Struct([
            ["t", -1n],
            ["s", new Struct([
                ["x", -1n],
                ["y", 45678n],
                ["b", true],
                ["addrs", Address.fromString("0xcD6a42782d230D7c13A74ddec5dD140e55499Df9")],
            ])],
            ["b", hexToBytes("040506")],
        ])],
        ["s_nested_dynamic_static", new Struct([
            ["t", []],
            ["s", new Struct([
                ["x", -1n],
                ["y", 45678n],
                ["b", true],
                ["addrs", Address.fromString("0xcD6a42782d230D7c13A74ddec5dD140e55499Df9")],
            ])],
            ["b", hexToBytes("070809")],

        ])],
        ["s_nested_static_dynamic", new Struct([
            ["t", -1234n],
            ["s", new Struct([
                ["x", -1n],
                ["y", 45678n],
                ["b", true],
                ["addrs", [Address.fromString("0x0000000000000000000000000000000000000000"), Address.fromString("0xcD6a42782d230D7c13A74ddec5dD140e55499Df9")]],

            ])],
            ["b", hexToBytes("020304")],
        ])],
        ["s_struct_arr", new Struct([
            ["x", -128n],
            ["sArr", [
                new Struct([
                    ["x", -1n],
                    ["y", 45678n],
                    ["b", true],
                    ["addrs", [Address.fromString("0x0000000000000000000000000000000000000000"), Address.fromString("0xcD6a42782d230D7c13A74ddec5dD140e55499Df9")]],
                ]),
                new Struct([
                    ["x", -2n],
                    ["y", 45679n],
                    ["b", false],
                    ["addrs", []],
                ]),
            ]]
        ])],
        ["at", new Struct([
            ["a1", [12n, 13n, 14n]],
            ["a2", [-1n, -2n, -3n, -4n]]
        ])],
    ])],
    [bytesStorDesc, 0, 32, bytesLayoutType, new Struct([])]
];

let unit: SourceUnit;

beforeAll(async () => {
    const file = fse.readFileSync("test/samples/decoding/storage_views_test.sol", {
        encoding: "utf-8"
    });
    const compileResult = await compileSourceString(
        "storage_views_test.sol",
        file,
        "0.8.29",
        undefined,
        undefined,
        { viaIR: true }
    );
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
            console.error(value, expectedValue);
            expect(value).toEqual(expectedValue);
        });
    }

    it(`Map decoding with keys`, () => {
        const mapKeys: MapKeys = new Map([
            [
                44n,
                [
                    [
                        hexToBytes(
                            "0000000000000000000000000000000000000000000000000000000000000000"
                        ),
                        0x4bd6275b77b7e49eb0792b10b407951644bbdba590ba83c6764fe2c9da0b9befn
                    ],
                    [
                        hexToBytes(
                            "0000000000000000000000000000000000000000000000000000000000000001"
                        ),
                        0xa1f88ee5f5d946e3956f6291445d84cd8aea2bf6c57f4f4ac349f7a338882643n
                    ]
                ]
            ]
        ]);

        const expected = new Map([
            [
                0n,
                new Struct([
                    ["x", 1n],
                    ["y", true]
                ])
            ],
            [
                1n,
                new Struct([
                    ["x", 2n],
                    ["y", false]
                ])
            ]
        ]);

        const view = makeStorageView(CLayoutType, infer, [42n, 32], mapKeys);
        const value = view.decode(toStorage(CStorDesc)) as Struct;
        expect(value.field("f")).toEqual(expected);
    });
});
