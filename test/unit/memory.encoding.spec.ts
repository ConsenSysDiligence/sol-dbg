import expect from "expect";
import {
    ArrayType,
    ASTReader,
    compileSourceString,
    DataLocation,
    InferType,
    PointerType,
    SourceUnit,
    TypeNode,
    UserDefinedType,
    XPath,
    types,
    IntType,
    FixedBytesType
} from "solc-typed-ast";
import { DecodingFailure, Struct, Value } from "../../src/debug/decoding/value";
import { hexToBytes } from "ethereum-cryptography/utils";
import { single } from "../../src";
import { makeMemoryView } from "../../src/debug/decoding/";
import fse from "fs-extra";
import { bytesToHex, createAddressFromString } from "@ethereumjs/util";
import { DefaultAllocator } from "../../src/debug/decoding/memory/allocator";
import { PointerMemView } from "../../src/debug/decoding/memory/view";
import { astToRuntimeType } from "../../src/debug/runtime_types";

const infer = new InferType("0.8.29");
type TypeGenerator = (unit: SourceUnit) => TypeNode;

const int8 = new IntType(8, true);
const uint16 = new IntType(16, false);
const bytes2 = new FixedBytesType(2);
const int128 = new IntType(128, true);

const valueTypeSamples: Array<[string, number, TypeNode | TypeGenerator, Value]> = [
    ["ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff", 0, int8, -1n],
    ["000000000000000000000000000000000000000000000000000000000000ffff", 0, uint16, 65535n],
    ["000000000000000000000000000000000000000000000000000000000001e240", 0, uint16, 123456n],
    ["0000000000000000000000000000000000000000000000000000000000000001", 0, types.bool, true],
    [
        "0000000000000000000000005b38da6a701c568545dcfcb03fcb875f56beddc4",
        0,
        types.address,
        createAddressFromString("0x5B38Da6a701c568545dCfcB03FcB875f56beddC4")
    ],
    [
        "0102000000000000000000000000000000000000000000000000000000000000",
        0,
        bytes2,
        hexToBytes("0102")
    ],
    [
        "0000000000000000000000000000000000000000000000000000000000abcdef",
        0,
        types.bytes32,
        hexToBytes("0000000000000000000000000000000000000000000000000000000000abcdef")
    ]
];

const refTypeSamples: Array<[string, number, TypeNode | TypeGenerator, Value]> = [
    [
        "000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000120000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000a00000000000000000000000000000000000000000000000000000000000000003000000000000000000000000000000000000000000000000000000000000000c000000000000000000000000000000000000000000000000000000000000000d000000000000000000000000000000000000000000000000000000000000000e",
        128,
        new PointerType(new ArrayType(uint16), DataLocation.Memory),
        [12n, 13n, 14n]
    ],
    [
        "000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000120000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000a0fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffdfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc",
        160,
        new PointerType(new ArrayType(int128, 4n), DataLocation.Memory),
        [-1n, -2n, -3n, -4n]
    ],
    [
        "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001a0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000a0ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000ffff000000000000000000000000000000000000000000000000000000000001e24000000000000000000000000000000000000000000000000000000000000000010000000000000000000000005b38da6a701c568545dcfcb03fcb875f56beddc401020000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000abcdef0000000000000000000000000000000000000000000000000000000000000001",
        128,
        (unit) => {
            const decl = new XPath(unit).query(
                "//ContractDefinition/StructDefinition[@name='SimpleTypes']"
            )[0];

            return new PointerType(new UserDefinedType("SimpleTypes", decl), DataLocation.Memory);
        },
        new Struct([
            ["a", -1n],
            ["b", 65535n],
            ["c", 123456n],
            ["d", true],
            ["e", createAddressFromString("0x5B38Da6a701c568545dCfcB03FcB875f56beddC4")],
            ["b1", hexToBytes("0102")],
            ["b2", hexToBytes("0000000000000000000000000000000000000000000000000000000000abcdef")],
            ["en", 1n]
        ])
    ],
    [
        "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001e0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000a000000000000000000000000000000000000000000000000000000000000000e000000000000000000000000000000000000000000000000000000000000001600000000000000000000000000000000000000000000000000000000000000003000000000000000000000000000000000000000000000000000000000000000c000000000000000000000000000000000000000000000000000000000000000d000000000000000000000000000000000000000000000000000000000000000efffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffdfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc",
        128,
        (unit) => {
            const decl = new XPath(unit).query(
                "//ContractDefinition/StructDefinition[@name='ArrTypes']"
            )[0];

            const t = new UserDefinedType("ArrTypes", decl);
            return new PointerType(t, DataLocation.Memory);
        },
        new Struct([
            ["a1", [12n, 13n, 14n]],
            ["a2", [-1n, -2n, -3n, -4n]]
        ])
    ],
    [
        "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001a0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000a00000000000000000000000000000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000012007080900000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000b26e0000000000000000000000000000000000000000000000000000000000000001000000000000000000000000cD6a42782d230D7c13A74ddec5dD140e55499Df9",
        128,
        (unit) => {
            const decl = new XPath(unit).query(
                "//ContractDefinition/StructDefinition[@name='S_nested_dynamic_static']"
            )[0];

            const t = new UserDefinedType("S_nested_dynamic_static", decl);
            return new PointerType(t, DataLocation.Memory);
        },
        new Struct([
            ["t", []],
            [
                "s",
                new Struct([
                    ["x", -1n],
                    ["y", 45678n],
                    ["b", true],
                    ["addrs", createAddressFromString("0xcD6a42782d230D7c13A74ddec5dD140e55499Df9")]
                ])
            ],
            ["b", hexToBytes("0x070809")]
        ])
    ],
    [
        "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001e0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000a0fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffb2e00000000000000000000000000000000000000000000000000000000000001000203040000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000b26e0000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000018000000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000000000000000000000000000000cd6a42782d230d7c13a74ddec5dd140e55499df9",
        128,
        (unit) => {
            const decl = new XPath(unit).query(
                "//ContractDefinition/StructDefinition[@name='S_nested_static_dynamic']"
            )[0];

            const t = new UserDefinedType("S_nested_static_dynamic", decl);
            return new PointerType(t, DataLocation.Memory);
        },
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
                            createAddressFromString("0x0000000000000000000000000000000000000000"),
                            createAddressFromString("0xcD6a42782d230D7c13A74ddec5dD140e55499Df9")
                        ]
                    ]
                ])
            ],
            ["b", hexToBytes("0x020304")]
        ])
    ],
    [
        "000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000180000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000a0ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff00000000000000000000000000000000000000000000000000000000000001000405060000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000b26e0000000000000000000000000000000000000000000000000000000000000001000000000000000000000000cd6a42782d230d7c13a74ddec5dd140e55499df9",
        128,
        (unit) => {
            const decl = new XPath(unit).query(
                "//ContractDefinition/StructDefinition[@name='S_nested_static_static']"
            )[0];

            const t = new UserDefinedType("S_nested_static_static", decl);
            return new PointerType(t, DataLocation.Memory);
        },
        new Struct([
            ["t", -1n],
            [
                "s",
                new Struct([
                    ["x", -1n],
                    ["y", 45678n],
                    ["b", true],
                    ["addrs", createAddressFromString("0xcD6a42782d230D7c13A74ddec5dD140e55499Df9")]
                ])
            ],
            ["b", hexToBytes("0x040506")]
        ])
    ],
    [
        "000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000120000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000a0ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000b26e0000000000000000000000000000000000000000000000000000000000000001000000000000000000000000cd6a42782d230d7c13a74ddec5dd140e55499df9",
        128,
        (unit) => {
            const decl = new XPath(unit).query(
                "//ContractDefinition/StructDefinition[@name='S_static']"
            )[0];

            const t = new UserDefinedType("S_static", decl);
            return new PointerType(t, DataLocation.Memory);
        },
        new Struct([
            ["x", -1n],
            ["y", 45678n],
            ["b", true],
            ["addrs", createAddressFromString("0xcD6a42782d230D7c13A74ddec5dD140e55499Df9")]
        ])
    ],
    [
        "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000a0ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff8000000000000000000000000000000000000000000000000000000000000000e0000000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000001400000000000000000000000000000000000000000000000000000000000000220ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000b26e000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000001c000000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000000000000000000000000000000cd6a42782d230d7c13a74ddec5dd140e55499df9fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe000000000000000000000000000000000000000000000000000000000000b26f000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002a00000000000000000000000000000000000000000000000000000000000000000",
        128,
        (unit) => {
            const decl = new XPath(unit).query(
                "//ContractDefinition/StructDefinition[@name='S_struct_arr']"
            )[0];

            const t = new UserDefinedType("S_struct_arr", decl);
            return new PointerType(t, DataLocation.Memory);
        },
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

describe(`Memory Value Types Encoding Tests`, () => {
    for (const [expectedMemoryStr, offset, typeDesc, value] of valueTypeSamples) {
        const newMem = new Uint8Array(32);
        const expectedMem = hexToBytes(expectedMemoryStr);

        it(`Sample ${ppType(typeDesc)}`, () => {
            const type = astToRuntimeType(
                typeDesc instanceof TypeNode ? typeDesc : typeDesc(unit),
                infer,
                DataLocation.Memory
            );
            const view = makeMemoryView(type, BigInt(offset));
            view.encode(value, newMem, undefined as any);
            expect(bytesToHex(newMem)).toEqual(bytesToHex(expectedMem));
        });
    }
});

describe(`Memory Reference Types Encoding Tests`, () => {
    for (const [expectedMemoryStr, , typeDesc, value] of refTypeSamples) {
        const expectedMem = hexToBytes(expectedMemoryStr);
        it(`Sample ${ppType(typeDesc)}`, () => {
            const alloc = new DefaultAllocator();
            const ptrOff = alloc.alloc(32);
            const type = astToRuntimeType(
                typeDesc instanceof TypeNode ? typeDesc : typeDesc(unit),
                infer,
                DataLocation.Memory
            );

            const view = makeMemoryView(type, BigInt(ptrOff));
            view.encode(value, alloc.memory, alloc);
            expect(bytesToHex(alloc.memory)).toEqual(bytesToHex(expectedMem));
        });
    }
});

const rttSamples: Array<[TypeNode | TypeGenerator, Value]> = [
    [int8, -1n],
    [uint16, 65535n],
    [uint16, 123456n],
    [types.bool, true],
    [types.address, createAddressFromString("0x5B38Da6a701c568545dCfcB03FcB875f56beddC4")],
    [bytes2, hexToBytes("0102")],
    [types.bytes32, hexToBytes("0000000000000000000000000000000000000000000000000000000000abcdef")],
    [new PointerType(new ArrayType(uint16), DataLocation.Memory), [12n, 13n, 14n]],
    [new PointerType(new ArrayType(int128, 4n), DataLocation.Memory), [-1n, -2n, -3n, -4n]],
    [
        (unit) => {
            const decl = new XPath(unit).query(
                "//ContractDefinition/StructDefinition[@name='SimpleTypes']"
            )[0];

            const t = new UserDefinedType("SimpleTypes", decl);
            return new PointerType(t, DataLocation.Memory);
        },
        new Struct([
            ["a", -1n],
            ["b", 65535n],
            ["c", 123456n],
            ["d", true],
            ["e", createAddressFromString("0x5B38Da6a701c568545dCfcB03FcB875f56beddC4")],
            ["b1", hexToBytes("0102")],
            ["b2", hexToBytes("0000000000000000000000000000000000000000000000000000000000abcdef")],
            ["en", 1n]
        ])
    ],
    [
        (unit) => {
            const decl = new XPath(unit).query(
                "//ContractDefinition/StructDefinition[@name='ArrTypes']"
            )[0];

            const t = new UserDefinedType("ArrTypes", decl);
            return new PointerType(t, DataLocation.Memory);
        },
        new Struct([
            ["a1", [12n, 13n, 14n]],
            ["a2", [-1n, -2n, -3n, -4n]]
        ])
    ],
    [
        (unit) => {
            const decl = new XPath(unit).query(
                "//ContractDefinition/StructDefinition[@name='S_nested_dynamic_static']"
            )[0];

            const t = new UserDefinedType("S_nested_dynamic_static", decl);
            return new PointerType(t, DataLocation.Memory);
        },
        new Struct([
            ["t", []],
            [
                "s",
                new Struct([
                    ["x", -1n],
                    ["y", 45678n],
                    ["b", true],
                    ["addrs", createAddressFromString("0xcD6a42782d230D7c13A74ddec5dD140e55499Df9")]
                ])
            ],
            ["b", hexToBytes("0x070809")]
        ])
    ],
    [
        (unit) => {
            const decl = new XPath(unit).query(
                "//ContractDefinition/StructDefinition[@name='S_nested_static_dynamic']"
            )[0];

            const t = new UserDefinedType("S_nested_static_dynamic", decl);
            return new PointerType(t, DataLocation.Memory);
        },
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
                            createAddressFromString("0x0000000000000000000000000000000000000000"),
                            createAddressFromString("0xcD6a42782d230D7c13A74ddec5dD140e55499Df9")
                        ]
                    ]
                ])
            ],
            ["b", hexToBytes("0x020304")]
        ])
    ],
    [
        (unit) => {
            const decl = new XPath(unit).query(
                "//ContractDefinition/StructDefinition[@name='S_nested_static_static']"
            )[0];

            const t = new UserDefinedType("S_nested_static_static", decl);
            return new PointerType(t, DataLocation.Memory);
        },
        new Struct([
            ["t", -1n],
            [
                "s",
                new Struct([
                    ["x", -1n],
                    ["y", 45678n],
                    ["b", true],
                    ["addrs", createAddressFromString("0xcD6a42782d230D7c13A74ddec5dD140e55499Df9")]
                ])
            ],
            ["b", hexToBytes("0x040506")]
        ])
    ],
    [
        (unit) => {
            const decl = new XPath(unit).query(
                "//ContractDefinition/StructDefinition[@name='S_static']"
            )[0];

            const t = new UserDefinedType("S_static", decl);
            return new PointerType(t, DataLocation.Memory);
        },
        new Struct([
            ["x", -1n],
            ["y", 45678n],
            ["b", true],
            ["addrs", createAddressFromString("0xcD6a42782d230D7c13A74ddec5dD140e55499Df9")]
        ])
    ],
    [
        (unit) => {
            const decl = new XPath(unit).query(
                "//ContractDefinition/StructDefinition[@name='S_struct_arr']"
            )[0];

            const t = new UserDefinedType("S_struct_arr", decl);
            return new PointerType(t, DataLocation.Memory);
        },
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
        new PointerType(
            new ArrayType(new PointerType(new ArrayType(uint16), DataLocation.Memory)),
            DataLocation.Memory
        ),
        [
            [1n, 2n, 3n],
            [4n, 5n]
        ]
    ],
    [
        new PointerType(
            new ArrayType(new PointerType(new ArrayType(uint16), DataLocation.Memory), 2n),
            DataLocation.Memory
        ),
        [
            [1n, 2n, 3n],
            [4n, 5n]
        ]
    ],
    [
        new PointerType(
            new ArrayType(new PointerType(new ArrayType(uint16, 3n), DataLocation.Memory)),
            DataLocation.Memory
        ),
        [
            [1n, 2n, 3n],
            [4n, 5n, 6n]
        ]
    ],
    [
        new PointerType(
            new ArrayType(new PointerType(new ArrayType(uint16, 3n), DataLocation.Memory), 2n),
            DataLocation.Memory
        ),
        [
            [1n, 2n, 3n],
            [4n, 5n, 6n]
        ]
    ]
];

describe(`Memory encoding/decoding RTT tests`, () => {
    for (const [typeDesc, value] of rttSamples) {
        it(`Sample ${ppType(typeDesc)}`, () => {
            const alloc = new DefaultAllocator();
            const type = astToRuntimeType(
                typeDesc instanceof TypeNode ? typeDesc : typeDesc(unit),
                infer,
                DataLocation.Memory
            );
            const ptrOff = alloc.alloc(PointerMemView.allocSize(value, type));
            const view = makeMemoryView(type, BigInt(ptrOff));
            view.encode(value, alloc.memory, alloc);
            const v1 = view.decode(alloc.memory);
            expect(v1 instanceof DecodingFailure).toBeFalsy();
            expect(v1).toEqual(value);
        });
    }
});
