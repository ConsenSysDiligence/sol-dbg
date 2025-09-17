import expect from "expect";
import {
    ArrayType,
    ASTReader,
    compileSourceString,
    DataLocation,
    InferType,
    PointerType,
    SourceUnit,
    TupleType,
    TypeNode,
    UserDefinedType,
    XPath
} from "solc-typed-ast";
import { hasPoison, Struct, Value } from "../../src/debug/decoding/value";
import { hexToBytes } from "ethereum-cryptography/utils";
import { MAX_ARR_DECODE_LIMIT, Memory, single } from "../../src";
import {
    address,
    bool,
    bytes1,
    bytes22DArr,
    bytes3,
    bytes4,
    bytesCalldata,
    int16,
    int16Arr,
    int256,
    int48,
    int8,
    int8x4,
    int8x4x2,
    int8x4xN,
    int8xNx2,
    stringCalldata,
    uint16,
    uint24,
    uint256,
    uint8
} from "../utils/sol_types";
import { createAddressFromString } from "@ethereumjs/util";
import {
    BaseCalldataView,
    DecodingFailure,
    isArrayLikeCalldataView,
    makeCalldataViews,
    PointerCalldataView,
    StructCalldataView
} from "../../src/debug/decoding/";
import fse from "fs-extra";
import { astToRuntimeType, BaseRuntimeType } from "../../src/debug/runtime_types";

const tupleS1 = new TupleType([
    int8,
    uint256,
    bool,
    new PointerType(new ArrayType(address), DataLocation.CallData)
]);

const tupleS_static = new TupleType([int8, uint256, bool, address]);

const tupleS_nested_static_static = new TupleType([int16, tupleS_static, bytes3]);

const tupleS_nested_dynamic_static = new TupleType([
    new PointerType(new ArrayType(int16), DataLocation.CallData),
    tupleS_static,
    bytes3
]);

const tupleS_nested_static_dynamic = new TupleType([int16, tupleS1, bytes3]);

const tupleS_struct_arr = new TupleType([
    int8,
    new PointerType(new ArrayType(tupleS1), DataLocation.CallData)
]);

type TypeGenerator = (unit: SourceUnit) => TypeNode;
const samples: Array<[string, Array<TypeNode | TypeGenerator>, Value[]]> = [
    [
        "0x83a25b8a000000000000000000000000000000000000000000000000000000000000007b",
        [uint256],
        [123n]
    ],
    [
        "0x1f0ea1f4ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff85",
        [int256],
        [-123n]
    ],
    [
        "0xf3aeb86a000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff800000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffffff800000000000",
        [uint16, int8, uint24, int48],
        [65535n, -128n, 16777215n, -140737488355328n]
    ],
    ["0x723d94da0000000000000000000000000000000000000000000000000000000000000001", [bool], [true]],
    [
        "0x0848ace50000000000000000000000004838b106fce9647bdf1e7877bf73ce8b0bad5f97",
        [address],
        [createAddressFromString("0x4838B106FCe9647Bdf1E7877BF73cE8B0BAD5f97")]
    ],
    [
        "0x20a6db030012340000000000000000000000000000000000000000000000000000000000",
        [bytes4],
        [hexToBytes("00123400")]
    ],
    [
        "0x9b297c66000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ff0000000000000000000000004838b106fce9647bdf1e7877bf73ce8b0bad5f97fa00000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
        [bool, uint8, address, bytes1, int16],
        [
            false,
            255n,
            createAddressFromString("0x4838B106FCe9647Bdf1E7877BF73cE8B0BAD5f97"),
            hexToBytes("0xfa"),
            -129n
        ]
    ],
    [
        "0xff11557600000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000000008abcdef0102030405000000000000000000000000000000000000000000000000",
        [bytesCalldata],
        [hexToBytes("abcdef0102030405")]
    ],
    [
        "0x435f7bac0000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000000a68656c6c6f776f726c6400000000000000000000000000000000000000000000",
        [stringCalldata],
        ["helloworld"]
    ],
    [
        "0x435f7bac0000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000000a68656c6c6f776f726c6400000000000000000000000000000000000000000000",
        [stringCalldata],
        ["helloworld"]
    ],
    [
        "0x18c7338a00000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000000006ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000a0000000000000000000000000000000000000000000000000000000000007fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff80000000000000000000000000000000000000000000000000000000000000000000fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff4",
        [int16Arr],
        [[-1n, 10n, 32767n, -32768n, 0n, -12n]]
    ],
    [
        "0x550d3ccd00000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000a0000000000000000000000000000000000000000000000000000000000000000201010000000000000000000000000000000000000000000000000000000000000102000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000202010000000000000000000000000000000000000000000000000000000000000202000000000000000000000000000000000000000000000000000000000000",
        [bytes22DArr],
        [[["0101", "0102"].map(hexToBytes), ["0201", "0202"].map(hexToBytes)]]
    ],
    [
        "0x34c70a6b0000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000000030000000000000000000000000000000000000000000000000000000000000004",
        [int8x4],
        [[1n, 2n, 3n, 4n]]
    ],
    [
        "0x6708542a00000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000300000000000000000000000000000000000000000000000000000000000000040000000000000000000000000000000000000000000000000000000000000005000000000000000000000000000000000000000000000000000000000000000600000000000000000000000000000000000000000000000000000000000000070000000000000000000000000000000000000000000000000000000000000008",
        [int8x4x2],
        [
            [
                [1n, 2n, 3n, 4n],
                [5n, 6n, 7n, 8n]
            ]
        ]
    ],
    [
        "0x43f924850000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000c00000000000000000000000000000000000000000000000000000000000000003000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000003000000000000000000000000000000000000000000000000000000000000000500000000000000000000000000000000000000000000000000000000000000040000000000000000000000000000000000000000000000000000000000000005000000000000000000000000000000000000000000000000000000000000000600000000000000000000000000000000000000000000000000000000000000070000000000000000000000000000000000000000000000000000000000000008",
        [int8xNx2],
        [
            [
                [1n, 2n, 3n],
                [4n, 5n, 6n, 7n, 8n]
            ]
        ]
    ],
    [
        "0x2453082400000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000000003000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000003000000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000000000000000000050000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000700000000000000000000000000000000000000000000000000000000000000080000000000000000000000000000000000000000000000000000000000000009000000000000000000000000000000000000000000000000000000000000000a000000000000000000000000000000000000000000000000000000000000000b000000000000000000000000000000000000000000000000000000000000000c",
        [int8x4xN],
        [
            [
                [1n, 2n, 3n, 4n],
                [5n, 6n, 7n, 8n],
                [9n, 10n, 11n, 12n]
            ]
        ]
    ],
    [
        "0xdaca483c0000000000000000000000000000000000000000000000000000000000000020ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff8000000000000000000000000000000000000000000000000000000000000000c8000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000800000000000000000000000000000000000000000000000000000000000000002000000000000000000000000f8e81d47203a594245e36c48e151709f0c19fbe80000000000000000000000005b38da6a701c568545dcfcb03fcb875f56beddc4",
        [tupleS1],
        [
            [
                -128n,
                200n,
                true,
                [
                    "0xf8e81D47203A594245E36C48e151709F0C19fBe8",
                    "0x5B38Da6a701c568545dCfcB03FcB875f56beddC4"
                ].map(createAddressFromString)
            ]
        ]
    ],
    [
        "0x6aa9556cffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff8000000000000000000000000000000000000000000000000000000000000000e700000000000000000000000000000000000000000000000000000000000000000000000000000000000000005b38da6a701c568545dcfcb03fcb875f56beddc4",
        [tupleS_static],
        [
            [
                -128n,
                231n,
                false,
                createAddressFromString("0x5B38Da6a701c568545dCfcB03FcB875f56beddC4")
            ]
        ]
    ],
    [
        "0x86bd3303ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000d000000000000000000000000000000000000000000000000000000000000002a00000000000000000000000000000000000000000000000000000000000000000000000000000000000000005b38da6a701c568545dcfcb03fcb875f56beddc40102030000000000000000000000000000000000000000000000000000000000",
        [tupleS_nested_static_static],
        [
            [
                -1n,
                [
                    13n,
                    42n,
                    false,
                    createAddressFromString("0x5B38Da6a701c568545dCfcB03FcB875f56beddC4")
                ],
                hexToBytes("010203")
            ]
        ]
    ],
    [
        "0x4c451e44000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000c0ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff80000000000000000000000000000000000000000000000000000000000000002500000000000000000000000000000000000000000000000000000000000000010000000000000000000000005b38da6a701c568545dcfcb03fcb875f56beddc404050600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000003fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffd",
        [tupleS_nested_dynamic_static],
        [
            [
                [-1n, -2n, -3n],
                [
                    -128n,
                    37n,
                    true,
                    createAddressFromString("0x5B38Da6a701c568545dCfcB03FcB875f56beddC4")
                ],
                hexToBytes("040506")
            ]
        ]
    ],
    [
        "0xb28be7570000000000000000000000000000000000000000000000000000000000000020ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff00000000000000000000000000000000000000000000000000000000000000600708090000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff800000000000000000000000000000000000000000000000000000000000000065000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000800000000000000000000000000000000000000000000000000000000000000000",
        [tupleS_nested_static_dynamic],
        [[-1n, [-128n, 101n, true, []], hexToBytes("070809")]]
    ],
    [
        "0x0910fae300000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000000000",
        [new PointerType(new ArrayType(tupleS_static), DataLocation.CallData)],
        [[]]
    ],
    [
        "0x0910fae300000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000000002ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff00000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000001000000000000000000000000ae036c65c649172b43ef7156b009c6221b596b8bfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe000000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000000010000000000000000000000005b38da6a701c568545dcfcb03fcb875f56beddc4",
        [new PointerType(new ArrayType(tupleS_static), DataLocation.CallData)],
        [
            [
                [
                    -1n,
                    1n,
                    true,
                    createAddressFromString("0xaE036c65C649172b43ef7156b009c6221B596B8b")
                ],
                [
                    -2n,
                    2n,
                    true,
                    createAddressFromString("0x5B38Da6a701c568545dCfcB03FcB875f56beddC4")
                ]
            ]
        ]
    ],
    [
        "0x47c4e6460000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000000000000000000100ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000800000000000000000000000000000000000000000000000000000000000000001000000000000000000000000ae036c65c649172b43ef7156b009c6221b596b8bfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe0000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000800000000000000000000000000000000000000000000000000000000000000000",
        [new PointerType(new ArrayType(tupleS1), DataLocation.CallData)],
        [
            [
                [
                    -1n,
                    1n,
                    true,
                    [createAddressFromString("0xaE036c65C649172b43ef7156b009c6221B596B8b")]
                ],
                [-2n, 2n, false, []]
            ]
        ]
    ],
    [
        "0x998697b7000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000020ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000040000000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000000000000000000100ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff00000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000008000000000000000000000000000000000000000000000000000000000000000010000000000000000000000005b38da6a701c568545dcfcb03fcb875f56beddc4fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe0000000000000000000000000000000000000000000000000000000000000003000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000800000000000000000000000000000000000000000000000000000000000000001000000000000000000000000ae036c65c649172b43ef7156b009c6221b596b8b",
        [new PointerType(new ArrayType(tupleS_struct_arr), DataLocation.CallData)],
        [
            [
                [
                    -1n,
                    [
                        [
                            -1n,
                            2n,
                            true,
                            [createAddressFromString("0x5B38Da6a701c568545dCfcB03FcB875f56beddC4")]
                        ],
                        [
                            -2n,
                            3n,
                            false,
                            [createAddressFromString("0xaE036c65C649172b43ef7156b009c6221B596B8b")]
                        ]
                    ]
                ]
            ]
        ]
    ],
    [
        "0x6aa9556cffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff8000000000000000000000000000000000000000000000000000000000000000e700000000000000000000000000000000000000000000000000000000000000000000000000000000000000005b38da6a701c568545dcfcb03fcb875f56beddc4",
        [
            (unit) => {
                const decl = new XPath(unit).query(
                    "//ContractDefinition/StructDefinition[@name='S_static']"
                )[0];

                const t = new PointerType(
                    new UserDefinedType("S_static", decl),
                    DataLocation.CallData
                );
                return t;
            }
        ],
        [
            new Struct([
                ["x", -128n],
                ["y", 231n],
                ["b", false],
                ["addrs", createAddressFromString("0x5B38Da6a701c568545dCfcB03FcB875f56beddC4")]
            ])
        ]
    ],
    [
        "0x4c451e44000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000c0ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff80000000000000000000000000000000000000000000000000000000000000002500000000000000000000000000000000000000000000000000000000000000010000000000000000000000005b38da6a701c568545dcfcb03fcb875f56beddc404050600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000003fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffd",
        [
            (unit) => {
                const decl = new XPath(unit).query(
                    "//ContractDefinition/StructDefinition[@name='S_nested_dynamic_static']"
                )[0];

                const t = new PointerType(
                    new UserDefinedType("S_nested_dynamic_static", decl),
                    DataLocation.CallData
                );
                return t;
            }
        ],
        [
            new Struct([
                ["t", [-1n, -2n, -3n]],
                [
                    "s",
                    new Struct([
                        ["x", -128n],
                        ["y", 37n],
                        ["b", true],
                        [
                            "addrs",
                            createAddressFromString("0x5B38Da6a701c568545dCfcB03FcB875f56beddC4")
                        ]
                    ])
                ],
                ["b", hexToBytes("040506")]
            ])
        ]
    ],
    [
        "0x2d045a170000000000000000000000005fd6eb55d12e759a21c09ef703fe0cba1dc9d88d",
        [
            (unit) => {
                const decl = new XPath(unit).query("//ContractDefinition[@name='Foo']")[0];

                const t = new UserDefinedType("Foo", decl);
                return t;
            }
        ],
        [createAddressFromString("0x5FD6eB55D12E759a21C09eF703fe0CBa1DC9d88D")]
    ],
    [
        "0x37048b0e0000000000000000000000000000000000000000000000000000000000000002",
        [
            (unit) => {
                const decl = new XPath(unit).query(
                    "//ContractDefinition/EnumDefinition[@name='E']"
                )[0];

                const t = new UserDefinedType("E", decl);
                return t;
            }
        ],
        [2n]
    ],
    [
        "0x7fe6db4b000000000000000000000000000000000000000000000000000000000000007b00000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000000000000000000060000000000000000000000000000000000000000000000000000000000000012000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000d0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008000000000000000000000000000000000000000000000000000000000000000010000000000000000000000005b38da6a701c568545dcfcb03fcb875f56beddc40000000000000000000000000000000000000000000000000000000000000003000000000000000000000000000000000000000000000000000000000000000e0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008000000000000000000000000000000000000000000000000000000000000000020000000000000000000000005b38da6a701c568545dcfcb03fcb875f56beddc40000000000000000000000005b38da6a701c568545dcfcb03fcb875f56beddc40000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000d000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000800000000000000000000000000000000000000000000000000000000000000000",
        [
            uint256,
            (unit) => {
                const decl = new XPath(unit).query(
                    "//ContractDefinition/StructDefinition[@name='S1']"
                )[0];

                const t = new PointerType(
                    new ArrayType(
                        new PointerType(new UserDefinedType("S1", decl), DataLocation.CallData),
                        3n
                    ),
                    DataLocation.CallData
                );
                return t;
            }
        ],
        [
            123n,
            [
                new Struct([
                    ["x", 2n],
                    ["y", 13n],
                    ["b", false],
                    [
                        "addrs",
                        [createAddressFromString("0x5B38Da6a701c568545dCfcB03FcB875f56beddC4")]
                    ]
                ]),
                new Struct([
                    ["x", 3n],
                    ["y", 14n],
                    ["b", false],
                    [
                        "addrs",
                        [
                            createAddressFromString("0x5B38Da6a701c568545dCfcB03FcB875f56beddC4"),
                            createAddressFromString("0x5B38Da6a701c568545dCfcB03FcB875f56beddC4")
                        ]
                    ]
                ]),
                new Struct([
                    ["x", 2n],
                    ["y", 13n],
                    ["b", false],
                    ["addrs", []]
                ])
            ]
        ]
    ]
];

let unit: SourceUnit;

beforeAll(async () => {
    const file = fse.readFileSync("test/samples/decoding/calldata_views_test.sol", {
        encoding: "utf-8"
    });
    const compileResult = await compileSourceString("calldata_views_test.sol", file, "0.8.21");
    const reader = new ASTReader();
    unit = single(reader.read(compileResult.data));
});

function ppType(t: TypeNode | TypeGenerator): string {
    if (t instanceof TypeNode) {
        return t.pp();
    }

    return "<type-generator>";
}

describe(`Calldata Decoding Tests`, () => {
    const infer = new InferType("0.8.21");
    for (const [calldataStr, typeDesc, expectedValues] of samples) {
        const calldata = hexToBytes(calldataStr.slice(2));

        it(`Sample [${typeDesc.map(ppType).join(", ")}]`, () => {
            const types = typeDesc.map((t) =>
                astToRuntimeType(t instanceof TypeNode ? t : t(unit), infer, DataLocation.CallData)
            );
            const views = makeCalldataViews(types, 4n);
            const value = views.map((v) => v.decode(calldata));

            console.error(value);
            expect(hasPoison(value)).toBeFalsy();
            expect(value).toEqual(expectedValues);
            for (let i = 0; i < views.length; i++) {
                expect(recCheckViewDecodesTo(views[i], expectedValues[i], calldata));
            }
        });
    }
});

function recCheckViewDecodesTo(
    v: BaseCalldataView<Value, BaseRuntimeType>,
    value: Value,
    state: Memory
): boolean {
    if (v instanceof PointerCalldataView) {
        return recCheckViewDecodesTo(
            v.toView(state) as BaseCalldataView<Value, TypeNode>,
            value,
            state
        );
    }

    // Check indexing
    if (isArrayLikeCalldataView(v)) {
        if (!(value instanceof Array || value instanceof Uint8Array)) {
            console.error(`Expected indexable of type ${v.type.pp()} not ${value}`);
            return false;
        }

        const size = v.size(state);

        if (size instanceof DecodingFailure || size > MAX_ARR_DECODE_LIMIT) {
            console.error(`Couldn't get size of ${v.type.pp()}`);
            return false;
        }

        for (let i = 0; i < Number(size); i++) {
            const idxView = v.indexView(BigInt(i), state);

            if (idxView instanceof DecodingFailure) {
                console.error(`Couldnt make index ${i} of indexable of type ${v.type.pp()}`);
                return false;
            }

            const el = value[i];
            if (!recCheckViewDecodesTo(idxView, typeof el === "number" ? BigInt(el) : el, state)) {
                return false;
            }
        }
    }

    // Check field views
    if (v instanceof StructCalldataView) {
        if (!(value instanceof Struct)) {
            console.error(`Expected object of type ${v.type.pp()} not ${value}`);
            return false;
        }

        for (const [name] of value.entries) {
            const fieldView = v.fieldView(name);

            if (fieldView instanceof DecodingFailure) {
                console.error(`Couldnt make field ${name} of struct of type ${v.type.pp()}`);
                return false;
            }

            if (!recCheckViewDecodesTo(fieldView, value.field(name), state)) {
                return false;
            }
        }
    }

    // Simple case
    const got = String(v.decode(state));
    const expected = String(value);

    if (got !== expected) {
        console.error(`Got: ${got} expected ${expected}`);
    }
    return got === expected;
}
