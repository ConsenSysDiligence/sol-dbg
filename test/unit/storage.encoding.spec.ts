import expect from "expect";
import {
    ArrayType,
    BytesType,
    DataLocation,
    MappingType,
    PointerType,
    StringType,
    TypeNode
} from "solc-typed-ast";
import { hasPoison, Struct, Value } from "../../src/debug/decoding/value";
import { bytesToHex, hexToBytes } from "ethereum-cryptography/utils";
import {
    bigEndianBufToBigint,
    ExpStructType,
    ImmMap,
    makeStorageView,
    MapKeys,
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
    int120,
    int128,
    int136,
    int16,
    int32,
    int8,
    uint112,
    uint120,
    uint136,
    uint144,
    uint16,
    uint248,
    uint8
} from "../utils";
import { createAddressFromString, setLengthLeft } from "@ethereumjs/util";

type StorageDesc = { [key: string]: string };

function toStorage(s: StorageDesc): Storage {
    return ImmMap.fromEntries(
        Object.entries(s).map(([k, v]) => [
            bigEndianBufToBigint(hexToBytes(k.slice(2))),
            setLengthLeft(hexToBytes(v.slice(2)), 32)
        ])
    );
}

export const simpleStorDesc = {
    "0x290decd9548b62a8d60345a988386fc84ba6bc95484008f6362f93160ef3e563":
        "0xcd6a42782d230d7c13a74ddec5dd140e55499df90180000000000001e240",
    "0x405787fa12a823e0f2b7631cc41b3ba8828b3321ca811111fa75cd3aa3bb5ace": "0x4342",
    "0xb10e2d527612073b26eecdfd717e6a320cf44b4afac2b0732d9fcbe2b7fa0cf6": "0x0405060708",
    "0xc2575a0e9e593c00f959f8c92f12db2869c3395a3b0502d05e2516446f71f85b": "0x44"
};
export const CStorDesc = {
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
export const moreStructsStorDesc = {
    "0x0175b7a638427703f0dbe7bb9bbf987a2551717b34e79f33b5b1008d1fa01db9": "0x02",
    "0x02c1acb1a5666d7e1b67652ace0fcd5a29681aed81ffbc4cf16cfeaa3c83369a": "0x02",
    "0x036b6384b5eca791c62761152d0c79bb0604c104a5fb6f4eb0703f3154bb3db0": "0xff",
    "0x057c384a7d1c54f3a1b2e5e67b2617b8224fdfd1ea7234eea573a6ff665ff63e": "0x02",
    "0x07d411a2b75625732d79f376a36908a89c9e8d258d2d147aceaa648a31375331": "0x01",
    "0x0e4562a10381dec21b205ed72637e6b1b523bdd0e4d4d50af5cd23dd4500a211": "0x80",
    "0x1b6847dc741a1b0cd08d278845f9d819d87b734759afb55fe2de5cb82a9ae672": "0x040506",
    "0x290decd9548b62a8d60345a988386fc84ba6bc95484008f6362f93160ef3e563": "0xffffff",
    "0x2eb6f567f599b88c7d439ce330a3bcb4f83de9bfccc843b6db6f3b2d11bdd8f2": "0xff",
    "0x3ad8aa4f87544323a9d1e5dd902f40c356527a7955687113db5f9a85ad579dc1": "0x020304",
    "0x405787fa12a823e0f2b7631cc41b3ba8828b3321ca811111fa75cd3aa3bb5ace":
        "0x01025b38da6a701c568545dcfcb03fcb875f56beddc401",
    "0x50bb669a95c7b50b7e8a6f09454034b2b14cf2b85c730dca9a539ca82cb6e350": "0x03",
    "0x55f448fdea98c4d29eb340757ef0a66cd03dbb9538908a6a81d96026b71ec475": "0x070809",
    "0x66de8ffda797e3de9c05e8fc57b3bf0ec28a930d40b0d285d93c06501cf6a090": "0xb26e",
    "0x6d4407e7be21f808e6509aa9fa9143369579dd7d760fe20a2c09680fc146134f": "0x02",
    "0x6e1540171b6c0c960b71a7020d9f60077f6af931a8bbf590da0223dacf75c7af": "0xb26e",
    "0x744fd2b04a866f3f5dcdb35fccc45b3d9cdec1c501e077f496ff358350592bcf":
        "0xcd6a42782d230d7c13a74ddec5dd140e55499df9",
    "0x75fcf6b6672bc14555e26639c35fcbb1aecdb68284a287ed21d7fc00252d71e9": "0xb26e",
    "0x76efdf26cb3a0d248906a912be95456494193d31e0497fb65b9a0bac08e803a5":
        "0xcd6a42782d230d7c13a74ddec5dd140e55499df9",
    "0x7bdb4cff72c5c3c62e5a52b046dbbafca87d8f0fbf7f0798db0c7d39182e65b7": "0xb26f",
    "0x8a35acfbc15ff81a39ae7d344fd709f28e8600b4aa8c65c6b64bfe7fe36bd19b": "0x01",
    "0x8d1108e10bcb7c27dddfc02ed9d693a074039d026cf4ea4240b40f7d581ac802":
        "0xcd6a42782d230d7c13a74ddec5dd140e55499df901",
    "0x8d7741e02537e0cbef65263699dc0b2909234e0e4f7d77f08fa28cd36d8ac6a8": "0x0e000d000c",
    "0x944998273e477b495144fb8794c914197f3ccb46be2900f4698fd0ef743c9695": "0x01",
    "0x9b787f7d3ff23785e0f06d6a3590539691b65820914f14b4c2050eca4b3f92fe": "0xfe",
    "0xa03837a25210ee280c2113ff4b77ca23440b19d4866cca721c801278fd08d807":
        "0xfffffffffffffffffffffffffffffffeffffffffffffffffffffffffffffffff",
    "0xa66cc928b5edb82af9bd49922954155ab7b0942694bea4ce44661d9a8736c688":
        "0xcd6a42782d230d7c13a74ddec5dd140e55499df901",
    "0xb10e2d527612073b26eecdfd717e6a320cf44b4afac2b0732d9fcbe2b7fa0cf6": "0x01e240",
    "0xb13d2d76d1f4b7be834882e410b3e3a8afaf69f83600ae24db354391d2378d2e": "0xb26e",
    "0xbb7b4a454dc3493923482f07822329ed19e8244eff582cc204f8554c3620c3fd": "0xb26e",
    "0xbb8a6a4669ba250d26cd7a459eca9d215f8307e33aebe50379bc5a3617ec3444": "0xff",
    "0xc2575a0e9e593c00f959f8c92f12db2869c3395a3b0502d05e2516446f71f85b": "0xabcdef",
    "0xc624b66cc0138b8fabc209247f72d758e1cf3343756d543badbf24212bed8c15": "0xff",
    "0xc65a7bb8d6351c1cf70c95a316cc6a92839c986682d98bc35f958f4883f9d2a8": "0x01",
    "0xc97bfaf2f8ee708c303a06d134f5ecd8389ae0432af62dc132a24118292866bb":
        "0xfffffffffffffffffffffffffffffffcfffffffffffffffffffffffffffffffd",
    "0xce6d7b5282bd9a3661ae061feed1dbda4e52ab073b1f9285be6e155d9c38d4ec":
        "0xcd6a42782d230d7c13a74ddec5dd140e55499df901",
    "0xd7b6990105719101dabeb77144f2a3385c8033acd3af97e9423a695e81ad1eb5": "0xff",
    "0xd833147d7dc355ba459fc788f669e58cfaf9dc25ddcd0702e87d69c7b5124289": "0xfb2e",
    "0xde9647372d0c1a3bf7d388adb42a76542657d064a5909aaa3ee76e8d628c8ac5":
        "0xcd6a42782d230d7c13a74ddec5dd140e55499df9",
    "0xdf6966c971051c3d54ec59162606531493a51404a002842f56009d7e5cf4a8c7": "0xffff",
    "0xf3f7a9fe364faab93b216da50a3214154f22a0a2b415b23a84c8169e8b636ee3": "0xff",
    "0xf652222313e28459528d920b65115c16c04f3efc82aaedc97be59f3f377c0d3f": "0xb26e"
};
export const bytesStorDesc = {
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

export const miscStorDesc = {
    "0x0175b7a638427703f0dbe7bb9bbf987a2551717b34e79f33b5b1008d1fa01db9":
        "0x04000000000000000000000000000003",
    "0x036b6384b5eca791c62761152d0c79bb0604c104a5fb6f4eb0703f3154bb3db0": "0x02",
    "0x057c384a7d1c54f3a1b2e5e67b2617b8224fdfd1ea7234eea573a6ff665ff63e": "0x05",
    "0x0e4562a10381dec21b205ed72637e6b1b523bdd0e4d4d50af5cd23dd4500a211": "0x07",
    "0x1b6847dc741a1b0cd08d278845f9d819d87b734759afb55fe2de5cb82a9ae672": "0x01",
    "0x290decd9548b62a8d60345a988386fc84ba6bc95484008f6362f93160ef3e563":
        "0x02000000000000000000000000000001",
    "0x31ecc21a745e3968a04e9570e4425bc18fa8019c68028196b546d1669c200c68": "0x02",
    "0x3ad8aa4f87544323a9d1e5dd902f40c356527a7955687113db5f9a85ad579dc1": "0x06",
    "0x405787fa12a823e0f2b7631cc41b3ba8828b3321ca811111fa75cd3aa3bb5ace":
        "0x0200000000000000000000000000000001",
    "0x50bb669a95c7b50b7e8a6f09454034b2b14cf2b85c730dca9a539ca82cb6e350": "0x65",
    "0x55f448fdea98c4d29eb340757ef0a66cd03dbb9538908a6a81d96026b71ec475": "0x06",
    "0x66de8ffda797e3de9c05e8fc57b3bf0ec28a930d40b0d285d93c06501cf6a090": "0x04",
    "0x6d4407e7be21f808e6509aa9fa9143369579dd7d760fe20a2c09680fc146134f": "0x08",
    "0x6e1540171b6c0c960b71a7020d9f60077f6af931a8bbf590da0223dacf75c7af":
        "0x04000000000000000000000000000003",
    "0x8a35acfbc15ff81a39ae7d344fd709f28e8600b4aa8c65c6b64bfe7fe36bd19b": "0x01",
    "0x8d1108e10bcb7c27dddfc02ed9d693a074039d026cf4ea4240b40f7d581ac802": "0x04",
    "0x944998273e477b495144fb8794c914197f3ccb46be2900f4698fd0ef743c9695": "0x04",
    "0xa66cc928b5edb82af9bd49922954155ab7b0942694bea4ce44661d9a8736c688": "0x04",
    "0xb10e2d527612073b26eecdfd717e6a320cf44b4afac2b0732d9fcbe2b7fa0cf6":
        "0x04000000000000000000000000000003",
    "0xb13d2d76d1f4b7be834882e410b3e3a8afaf69f83600ae24db354391d2378d2e": "0x03",
    "0xbb7b4a454dc3493923482f07822329ed19e8244eff582cc204f8554c3620c3fd": "0x03",
    "0xbb8a6a4669ba250d26cd7a459eca9d215f8307e33aebe50379bc5a3617ec3444": "0x03",
    "0xc2575a0e9e593c00f959f8c92f12db2869c3395a3b0502d05e2516446f71f85b":
        "0x0400000000000000000000000000000003",
    "0xc624b66cc0138b8fabc209247f72d758e1cf3343756d543badbf24212bed8c15": "0x02",
    "0xc65a7bb8d6351c1cf70c95a316cc6a92839c986682d98bc35f958f4883f9d2a8":
        "0x02000000000000000000000000000001",
    "0xce6d7b5282bd9a3661ae061feed1dbda4e52ab073b1f9285be6e155d9c38d4ec": "0x05",
    "0xd7b6990105719101dabeb77144f2a3385c8033acd3af97e9423a695e81ad1eb5": "0x02",
    "0xd833147d7dc355ba459fc788f669e58cfaf9dc25ddcd0702e87d69c7b5124289": "0x01",
    "0xdf6966c971051c3d54ec59162606531493a51404a002842f56009d7e5cf4a8c7": "0x01",
    "0xf3f7a9fe364faab93b216da50a3214154f22a0a2b415b23a84c8169e8b636ee3":
        "0x02000000000000000000000000000001",
    "0xf652222313e28459528d920b65115c16c04f3efc82aaedc97be59f3f377c0d3f": "0x03"
};
export const mapWithComplexKeysStorDesc = {
    "0x134266f27b803cd76c24f211c4457379daae4fac618eaef4ab979796508e0c3e": "0x03",
    "0x6b561827e89dd864e82f1287442a6f56c5408b1313c5133c7dacd8fed2fbd375": "0x04",
    "0xcb85c6f413feb024aaf9fe6ef133f21422160ad679b1000921a475400fde1ef5": "0x02",
    "0xe2fe0e2425d2aed896ad86c3e2c0ea7d679d08e1a849442d22f76adef98bbd97": "0x01"
};

export const uint8x2 = new PointerType(new ArrayType(uint8, 2n), DataLocation.Storage);
export const S = new ExpStructType("S", [
    ["x", int32],
    ["y", bool]
]);
export const CLayoutType = new ExpStructType(
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

export const SimpleTypes = new ExpStructType("MoreStructs.SimpleTypes", [
    ["a", int8],
    ["b", uint16],
    ["c", uint256],
    ["d", bool],
    ["e", address],
    ["b1", bytes2],
    ["b2", bytes32],
    ["en", uint8]
]);
export const S_static = new ExpStructType("MoreStructs.S_static", [
    ["x", int8],
    ["y", uint256],
    ["b", bool],
    ["addrs", address]
]);
export const S1 = new ExpStructType("MoreStructs.S1", [
    ["x", int8],
    ["y", uint256],
    ["b", bool],
    ["addrs", new PointerType(new ArrayType(address), DataLocation.Storage)]
]);
export const S_nested_static_static = new ExpStructType("MoreStructs.S_nested_static_static", [
    ["t", int16],
    ["s", new PointerType(S_static, DataLocation.Storage)],
    ["b", bytes3]
]);
export const S_nested_dynamic_static = new ExpStructType("MoreStructs.S_nested_dynamic_static", [
    ["t", new PointerType(new ArrayType(int16), DataLocation.Storage)],
    ["s", new PointerType(S_static, DataLocation.Storage)],
    ["b", bytes3]
]);
export const S_nested_static_dynamic = new ExpStructType("MoreStructs.S_nested_static_dynamic", [
    ["t", int16],
    ["s", new PointerType(S1, DataLocation.Storage)],
    ["b", bytes3]
]);
export const S_struct_arr = new ExpStructType("MoreStructs.S_struct_arr", [
    ["x", int8],
    [
        "sArr",
        new PointerType(
            new ArrayType(new PointerType(S1, DataLocation.Storage)),
            DataLocation.Storage
        )
    ]
]);
export const ArrTypes = new ExpStructType("MoreStructs.ArrTypes", [
    ["a1", new PointerType(new ArrayType(uint16), DataLocation.Storage)],
    ["a2", new PointerType(new ArrayType(int128, 4n), DataLocation.Storage)]
]);

export const MoreStructsLayoutType = new ExpStructType("MoreStructs", [
    ["st", new PointerType(SimpleTypes, DataLocation.Storage)],
    ["s_static", new PointerType(S_static, DataLocation.Storage)],
    ["s1", new PointerType(S1, DataLocation.Storage)],
    ["s_nested_static_static", new PointerType(S_nested_static_static, DataLocation.Storage)],
    ["s_nested_dynamic_static", new PointerType(S_nested_dynamic_static, DataLocation.Storage)],
    ["s_nested_static_dynamic", new PointerType(S_nested_static_dynamic, DataLocation.Storage)],
    ["s_struct_arr", new PointerType(S_struct_arr, DataLocation.Storage)],
    ["at", new PointerType(ArrTypes, DataLocation.Storage)]
]);
export const bytesLayoutType = new ExpStructType("Bytes", [
    ["smallB", new PointerType(new BytesType(), DataLocation.Storage)],
    ["bigB", new PointerType(new BytesType(), DataLocation.Storage)],
    ["biggerB", new PointerType(new BytesType(), DataLocation.Storage)],
    ["smallS", new PointerType(new StringType(), DataLocation.Storage)],
    ["bigS", new PointerType(new StringType(), DataLocation.Storage)]
]);
export const SmallerThanWordType = new ExpStructType("Misc.SmallerThanWord", [
    ["a", uint120],
    ["b", uint112]
]);
export const OneWordType = new ExpStructType("Misc.OneWord", [
    ["a", uint120],
    ["b", uint136]
]);
export const MoreThanOneWordType = new ExpStructType("Misc.MoreThanOneWord", [
    ["a", uint120],
    ["b", uint144]
]);
export const ThreeWordsType = new ExpStructType("Misc.ThreeWords", [
    ["a", uint120],
    ["c", uint248],
    ["b", uint144]
]);
export const FourWordsType = new ExpStructType("Misc.FourWords", [
    ["a", uint120],
    ["c", uint256],
    ["d", uint248],
    ["b", uint144]
]);

export const miscLayoutType = new ExpStructType("Misc", [
    [
        "x",
        new PointerType(
            new ArrayType(new PointerType(new ArrayType(int120, 2n), DataLocation.Storage), 2n),
            DataLocation.Storage
        )
    ],
    [
        "y",
        new PointerType(
            new ArrayType(new PointerType(new ArrayType(int128, 2n), DataLocation.Storage), 2n),
            DataLocation.Storage
        )
    ],
    [
        "z",
        new PointerType(
            new ArrayType(new PointerType(new ArrayType(int136, 2n), DataLocation.Storage), 2n),
            DataLocation.Storage
        )
    ],
    [
        "p",
        new PointerType(
            new ArrayType(new PointerType(SmallerThanWordType, DataLocation.Storage), 2n),
            DataLocation.Storage
        )
    ],
    [
        "q",
        new PointerType(
            new ArrayType(new PointerType(OneWordType, DataLocation.Storage), 2n),
            DataLocation.Storage
        )
    ],
    [
        "r",
        new PointerType(
            new ArrayType(new PointerType(MoreThanOneWordType, DataLocation.Storage), 2n),
            DataLocation.Storage
        )
    ],
    [
        "s",
        new PointerType(
            new ArrayType(new PointerType(ThreeWordsType, DataLocation.Storage), 2n),
            DataLocation.Storage
        )
    ],
    [
        "t",
        new PointerType(
            new ArrayType(new PointerType(FourWordsType, DataLocation.Storage), 2n),
            DataLocation.Storage
        )
    ],
    ["v", uint256]
]);

const samples: Array<[StorageDesc, number, number, ExpStructType, Value]> = [
    [
        moreStructsStorDesc,
        0,
        32,
        MoreStructsLayoutType,
        new Struct([
            [
                "st",
                new Struct([
                    ["a", -1n],
                    ["b", 65535n],
                    ["c", 123456n],
                    ["d", true],
                    ["e", createAddressFromString("0x5B38Da6a701c568545dCfcB03FcB875f56beddC4")],
                    ["b1", hexToBytes("0102")],
                    [
                        "b2",
                        hexToBytes(
                            "0000000000000000000000000000000000000000000000000000000000abcdef"
                        )
                    ],
                    ["en", 1n]
                ])
            ],
            [
                "s_static",
                new Struct([
                    ["x", -1n],
                    ["y", 45678n],
                    ["b", true],
                    ["addrs", createAddressFromString("0xcD6a42782d230D7c13A74ddec5dD140e55499Df9")]
                ])
            ],
            [
                "s1",
                new Struct([
                    ["x", -1n],
                    ["y", 45678n],
                    ["b", true],
                    [
                        "addrs",
                        [
                            createAddressFromString("0x0000000000000000000000000000000000000000"),
                            createAddressFromString("0xcD6a42782d230D7c13A74ddec5dD140e55499Df9")
                        ]
                    ]
                ])
            ],
            [
                "s_nested_static_static",
                new Struct([
                    ["t", -1n],
                    [
                        "s",
                        new Struct([
                            ["x", -1n],
                            ["y", 45678n],
                            ["b", true],
                            [
                                "addrs",
                                createAddressFromString(
                                    "0xcD6a42782d230D7c13A74ddec5dD140e55499Df9"
                                )
                            ]
                        ])
                    ],
                    ["b", hexToBytes("040506")]
                ])
            ],
            [
                "s_nested_dynamic_static",
                new Struct([
                    ["t", []],
                    [
                        "s",
                        new Struct([
                            ["x", -1n],
                            ["y", 45678n],
                            ["b", true],
                            [
                                "addrs",
                                createAddressFromString(
                                    "0xcD6a42782d230D7c13A74ddec5dD140e55499Df9"
                                )
                            ]
                        ])
                    ],
                    ["b", hexToBytes("070809")]
                ])
            ],
            [
                "s_nested_static_dynamic",
                new Struct([
                    ["t", -1234n],
                    [
                        "s",
                        new Struct([
                            ["x", -1n],
                            ["y", 45678n],
                            ["b", true],
                            [
                                "addrs",
                                [
                                    createAddressFromString(
                                        "0x0000000000000000000000000000000000000000"
                                    ),
                                    createAddressFromString(
                                        "0xcD6a42782d230D7c13A74ddec5dD140e55499Df9"
                                    )
                                ]
                            ]
                        ])
                    ],
                    ["b", hexToBytes("020304")]
                ])
            ],
            [
                "s_struct_arr",
                new Struct([
                    ["x", -128n],
                    [
                        "sArr",
                        [
                            new Struct([
                                ["x", -1n],
                                ["y", 45678n],
                                ["b", true],
                                [
                                    "addrs",
                                    [
                                        createAddressFromString(
                                            "0x0000000000000000000000000000000000000000"
                                        ),
                                        createAddressFromString(
                                            "0xcD6a42782d230D7c13A74ddec5dD140e55499Df9"
                                        )
                                    ]
                                ]
                            ]),
                            new Struct([
                                ["x", -2n],
                                ["y", 45679n],
                                ["b", false],
                                ["addrs", []]
                            ])
                        ]
                    ]
                ])
            ],
            [
                "at",
                new Struct([
                    ["a1", [12n, 13n, 14n]],
                    ["a2", [-1n, -2n, -3n, -4n]]
                ])
            ]
        ])
    ],
    [
        bytesStorDesc,
        0,
        32,
        bytesLayoutType,
        new Struct([
            [
                "smallB",
                hexToBytes("000102030405060708090a0b0c0d0e0f000102030405060708090a0b0c0d0e")
            ],
            [
                "bigB",
                hexToBytes("000102030405060708090a0b0c0d0e0f000102030405060708090a0b0c0d0e0f")
            ],
            [
                "biggerB",
                hexToBytes(
                    "000102030405060708090a0b0c0d0e0f000102030405060708090a0b0c0d0e0f000102030405060708090a0b0c0d0e0f"
                )
            ],
            ["smallS", "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"],
            ["bigS", "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"]
        ])
    ],
    [
        miscStorDesc,
        0,
        32,
        miscLayoutType,
        new Struct([
            [
                "x",
                [
                    [1n, 2n],
                    [3n, 4n]
                ]
            ],
            [
                "y",
                [
                    [1n, 2n],
                    [3n, 4n]
                ]
            ],
            [
                "z",
                [
                    [1n, 2n],
                    [3n, 4n]
                ]
            ],
            [
                "p",
                [
                    new Struct([
                        ["a", 1n],
                        ["b", 2n]
                    ]),
                    new Struct([
                        ["a", 3n],
                        ["b", 4n]
                    ])
                ]
            ],
            [
                "q",
                [
                    new Struct([
                        ["a", 1n],
                        ["b", 2n]
                    ]),
                    new Struct([
                        ["a", 3n],
                        ["b", 4n]
                    ])
                ]
            ],
            [
                "r",
                [
                    new Struct([
                        ["a", 1n],
                        ["b", 2n]
                    ]),
                    new Struct([
                        ["a", 3n],
                        ["b", 4n]
                    ])
                ]
            ],
            [
                "s",
                [
                    new Struct([
                        ["a", 1n],
                        ["c", 2n],
                        ["b", 3n]
                    ]),
                    new Struct([
                        ["a", 4n],
                        ["c", 5n],
                        ["b", 6n]
                    ])
                ]
            ],
            [
                "t",
                [
                    new Struct([
                        ["a", 1n],
                        ["c", 2n],
                        ["d", 3n],
                        ["b", 4n]
                    ]),
                    new Struct([
                        ["a", 5n],
                        ["c", 6n],
                        ["d", 7n],
                        ["b", 8n]
                    ])
                ]
            ],
            ["v", 101n]
        ])
    ],
    [
        CStorDesc,
        42,
        32,
        CLayoutType,
        new Struct([
            ["a", 5678n],
            ["e", [1n, 2n, 3n, 4n]],
            [
                "f",
                new Map([
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
                ])
            ],
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
            ["m", [1n, 2n, 3n, 4n, 5n, 6n, 7n, 8n, 9n, 10n, 0n, 0n]],
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

function ppStorage(s: Storage): string {
    const lines: string[] = [];

    for (const [k, v] of s.entries()) {
        lines.push(`${k}: ${bytesToHex(v)}`);
    }

    lines.sort();
    return `{\n${lines.join(",\n")}\n}`;
}

describe(`Storage Encoding Tests`, () => {
    for (const [expectedStorageDesc, key, offset, type, value] of samples) {
        const expStorage = toStorage(expectedStorageDesc);

        it(`Sample ${type.name} `, () => {
            const view = makeStorageView(type, [BigInt(key), offset]);
            const initialStorage = ImmMap.fromEntries<bigint, Uint8Array>([]);
            const actualStorage = view.encode(value, initialStorage);
            expect(ppStorage(actualStorage)).toEqual(ppStorage(expStorage));
        });
    }
});

const rttSamples: Array<[TypeNode, Value, bigint, MapKeys | undefined]> = [
    [
        MoreStructsLayoutType,
        new Struct([
            [
                "st",
                new Struct([
                    ["a", -1n],
                    ["b", 65535n],
                    ["c", 123456n],
                    ["d", true],
                    ["e", createAddressFromString("0x5B38Da6a701c568545dCfcB03FcB875f56beddC4")],
                    ["b1", hexToBytes("0102")],
                    [
                        "b2",
                        hexToBytes(
                            "0000000000000000000000000000000000000000000000000000000000abcdef"
                        )
                    ],
                    ["en", 1n]
                ])
            ],
            [
                "s_static",
                new Struct([
                    ["x", -1n],
                    ["y", 45678n],
                    ["b", true],
                    ["addrs", createAddressFromString("0xcD6a42782d230D7c13A74ddec5dD140e55499Df9")]
                ])
            ],
            [
                "s1",
                new Struct([
                    ["x", -1n],
                    ["y", 45678n],
                    ["b", true],
                    [
                        "addrs",
                        [
                            createAddressFromString("0x0000000000000000000000000000000000000000"),
                            createAddressFromString("0xcD6a42782d230D7c13A74ddec5dD140e55499Df9")
                        ]
                    ]
                ])
            ],
            [
                "s_nested_static_static",
                new Struct([
                    ["t", -1n],
                    [
                        "s",
                        new Struct([
                            ["x", -1n],
                            ["y", 45678n],
                            ["b", true],
                            [
                                "addrs",
                                createAddressFromString(
                                    "0xcD6a42782d230D7c13A74ddec5dD140e55499Df9"
                                )
                            ]
                        ])
                    ],
                    ["b", hexToBytes("040506")]
                ])
            ],
            [
                "s_nested_dynamic_static",
                new Struct([
                    ["t", []],
                    [
                        "s",
                        new Struct([
                            ["x", -1n],
                            ["y", 45678n],
                            ["b", true],
                            [
                                "addrs",
                                createAddressFromString(
                                    "0xcD6a42782d230D7c13A74ddec5dD140e55499Df9"
                                )
                            ]
                        ])
                    ],
                    ["b", hexToBytes("070809")]
                ])
            ],
            [
                "s_nested_static_dynamic",
                new Struct([
                    ["t", -1234n],
                    [
                        "s",
                        new Struct([
                            ["x", -1n],
                            ["y", 45678n],
                            ["b", true],
                            [
                                "addrs",
                                [
                                    createAddressFromString(
                                        "0x0000000000000000000000000000000000000000"
                                    ),
                                    createAddressFromString(
                                        "0xcD6a42782d230D7c13A74ddec5dD140e55499Df9"
                                    )
                                ]
                            ]
                        ])
                    ],
                    ["b", hexToBytes("020304")]
                ])
            ],
            [
                "s_struct_arr",
                new Struct([
                    ["x", -128n],
                    [
                        "sArr",
                        [
                            new Struct([
                                ["x", -1n],
                                ["y", 45678n],
                                ["b", true],
                                [
                                    "addrs",
                                    [
                                        createAddressFromString(
                                            "0x0000000000000000000000000000000000000000"
                                        ),
                                        createAddressFromString(
                                            "0xcD6a42782d230D7c13A74ddec5dD140e55499Df9"
                                        )
                                    ]
                                ]
                            ]),
                            new Struct([
                                ["x", -2n],
                                ["y", 45679n],
                                ["b", false],
                                ["addrs", []]
                            ])
                        ]
                    ]
                ])
            ],
            [
                "at",
                new Struct([
                    ["a1", [12n, 13n, 14n]],
                    ["a2", [-1n, -2n, -3n, -4n]]
                ])
            ]
        ]),
        0n,
        undefined
    ],
    [
        bytesLayoutType,
        new Struct([
            [
                "smallB",
                hexToBytes("000102030405060708090a0b0c0d0e0f000102030405060708090a0b0c0d0e")
            ],
            [
                "bigB",
                hexToBytes("000102030405060708090a0b0c0d0e0f000102030405060708090a0b0c0d0e0f")
            ],
            [
                "biggerB",
                hexToBytes(
                    "000102030405060708090a0b0c0d0e0f000102030405060708090a0b0c0d0e0f000102030405060708090a0b0c0d0e0f"
                )
            ],
            ["smallS", "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"],
            ["bigS", "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"]
        ]),
        0n,
        undefined
    ],
    [
        miscLayoutType,
        new Struct([
            [
                "x",
                [
                    [1n, 2n],
                    [3n, 4n]
                ]
            ],
            [
                "y",
                [
                    [1n, 2n],
                    [3n, 4n]
                ]
            ],
            [
                "z",
                [
                    [1n, 2n],
                    [3n, 4n]
                ]
            ],
            [
                "p",
                [
                    new Struct([
                        ["a", 1n],
                        ["b", 2n]
                    ]),
                    new Struct([
                        ["a", 3n],
                        ["b", 4n]
                    ])
                ]
            ],
            [
                "q",
                [
                    new Struct([
                        ["a", 1n],
                        ["b", 2n]
                    ]),
                    new Struct([
                        ["a", 3n],
                        ["b", 4n]
                    ])
                ]
            ],
            [
                "r",
                [
                    new Struct([
                        ["a", 1n],
                        ["b", 2n]
                    ]),
                    new Struct([
                        ["a", 3n],
                        ["b", 4n]
                    ])
                ]
            ],
            [
                "s",
                [
                    new Struct([
                        ["a", 1n],
                        ["c", 2n],
                        ["b", 3n]
                    ]),
                    new Struct([
                        ["a", 4n],
                        ["c", 5n],
                        ["b", 6n]
                    ])
                ]
            ],
            [
                "t",
                [
                    new Struct([
                        ["a", 1n],
                        ["c", 2n],
                        ["d", 3n],
                        ["b", 4n]
                    ]),
                    new Struct([
                        ["a", 5n],
                        ["c", 6n],
                        ["d", 7n],
                        ["b", 8n]
                    ])
                ]
            ],
            ["v", 101n]
        ]),
        0n,
        undefined
    ],
    [
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
            ["m", [1n, 2n, 3n, 4n, 5n, 6n, 7n, 8n, 9n, 10n, 0n, 0n]],
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
        ]),
        42n,
        undefined
    ],
    [
        CLayoutType,
        new Struct([
            ["a", 5678n],
            ["e", [1n, 2n, 3n, 4n]],
            [
                "f",
                new Map([
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
                ])
            ],
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
            ["m", [1n, 2n, 3n, 4n, 5n, 6n, 7n, 8n, 9n, 10n, 0n, 0n]],
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
        ]),
        42n,
        new Map([
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
        ])
    ]
];

describe(`Storage Eecoding RTT Tests`, () => {
    for (const [type, value, baseOff, m] of rttSamples) {
        it(`Sample ${type instanceof ExpStructType ? type.name : type.pp()} `, () => {
            const storage: Storage = ImmMap.fromEntries([]);
            const view = makeStorageView(type, [baseOff, 32]);
            const newStore = view.encode(value, storage);

            const decVal = view.decode(newStore, m);
            expect(hasPoison(decVal)).toBeFalsy();
            expect(decVal).toEqual(value);
        });
    }

    it(`Map encoding with complex keys`, () => {
        const mapKeys: MapKeys = new Map([
            [
                0n,
                [
                    [
                        hexToBytes("010203"),
                        0xce4edd0c850af0ce44d5c79dd9354de666aa901a00d2111f49bd97f94cb8f6bbn
                    ],
                    [
                        hexToBytes("010204"),
                        0x8a03f525df54a7cbda74e29f15a3ba86a222d05788f5db224bb729a2a10080f4n
                    ]
                ]
            ],
            [
                1n,
                [
                    [
                        hexToBytes("616263"),
                        0xac85c8cc1ac92e94a731b8df588044cbfd366c5ee08805d198cb1b094f3cacacn
                    ],
                    [
                        hexToBytes("646566"),
                        0x007190f0fed5dcd60b9b3a23e83c99c86ff19bc3d9b603c39d3faf6b4ed8c5dcn
                    ]
                ]
            ]
        ]);

        const value = new Struct([
            [
                "m1",
                new Map([
                    ["010203", 1n],
                    ["010204", 2n]
                ])
            ],
            [
                "m2",
                new Map([
                    ["abc", 3n],
                    ["def", 4n]
                ])
            ],
            ["mNoKeys", new Map([])]
        ]);

        const layout = new ExpStructType("MapWithComplexKeys", [
            ["m1", new MappingType(new PointerType(new BytesType(), DataLocation.Memory), uint256)],
            [
                "m2",
                new MappingType(new PointerType(new StringType(), DataLocation.Memory), uint256)
            ],
            [
                "mNoKeys",
                new MappingType(new PointerType(new StringType(), DataLocation.Memory), uint256)
            ]
        ]);

        const view = makeStorageView(layout, [0n, 32]);
        const storage: Storage = ImmMap.fromEntries([]);
        const newStore = view.encode(value, storage);
        const decValue = view.decode(newStore, mapKeys) as Struct;
        expect(hasPoison(decValue)).toBeFalsy();
        expect(decValue).toEqual(decValue);
    });
});
