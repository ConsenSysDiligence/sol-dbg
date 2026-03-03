import expect from "expect";
import { ImmMap } from "../../src";

type TestStep = ["a" | "am" | "d" | "g" | "col" | "clp", any];

function testStep(step: TestStep, immMaps: Array<ImmMap<any, any>>): void {
    const immMap = immMaps[immMaps.length - 1];
    switch (step[0]) {
        case "a":
            immMaps.push(immMap.set(step[1][0], step[1][1]));
            break;
        case "am":
            immMaps.push(immMap.setMany(step[1]));
            break;
        case "am":
            immMaps.push(immMap.set(step[1][0], step[1][1]));
            break;
        case "d":
            immMaps.push(immMap.delete(step[1]));
            break;
        case "g":
            const v = immMap.get(step[1][0]);
            expect(v).toEqual(step[1][1]);
            immMaps.push(immMap);
            break;
        case "col":
            const cs = immMap.collectMap();
            expect(cs).toEqual(step[1]);
            immMaps.push(immMap);
            break;
        case "clp":
            const parent = immMaps[(step[1] + immMaps.length) % immMaps.length]; // support negative indexing from end
            immMaps.push(immMap.collapseUntil(parent));
            break;
    }
}

const tests: TestStep[][] = [
    [
        ["a", [1, 1]],
        ["a", [2, 2]],
        [
            "col",
            new Map([
                [1, 1],
                [2, 2]
            ])
        ]
    ],
    [
        ["a", [1, 1]],
        ["a", [2, 2]],
        ["d", 1],
        ["col", new Map([[2, 2]])]
    ],
    [
        ["a", [1, 1]],
        ["a", [2, 2]],
        ["d", 2],
        ["col", new Map([[1, 1]])]
    ],
    [
        ["a", [1, 1]],
        ["a", [2, 2]],
        ["d", 2],
        ["a", [3, 3]],
        [
            "col",
            new Map([
                [1, 1],
                [3, 3]
            ])
        ]
    ],
    [
        ["a", [1, 1]],
        ["a", [2, 2]],
        ["d", 2],
        ["a", [2, 5]],
        [
            "col",
            new Map([
                [1, 1],
                [2, 5]
            ])
        ]
    ],
    [
        ["a", [1, 1]],
        ["a", [2, 2]],
        ["d", 2],
        ["g", [2, undefined]]
    ],
    [
        ["a", [1, 1]],
        ["a", [2, 2]],
        ["d", 2],
        ["g", [1, 1]]
    ],
    [
        ["a", [1, 1]],
        ["a", [2, 2]],
        ["d", 2],
        ["g", [1, 1]]
    ],
    [
        ["a", [1, 1]],
        ["a", [1, 2]],
        ["g", [1, 2]]
    ],
    [
        ["a", [1, 1]],
        ["a", [1, 2]],
        ["col", new Map([[1, 2]])]
    ],
    [["col", new Map([])]],
    [["g", [2, undefined]]],
    [
        [
            "am",
            [
                [1, 1],
                [2, 2]
            ]
        ],
        [
            "col",
            new Map([
                [1, 1],
                [2, 2]
            ])
        ]
    ],
    [
        [
            "am",
            [
                [1, 1],
                [1, 2]
            ]
        ],
        ["col", new Map([[1, 2]])]
    ],
    [
        [
            "am",
            [
                [1, 1],
                [1, 2]
            ]
        ],
        ["d", 1],
        ["col", new Map()]
    ],
    [
        [
            "am",
            [
                [1, 1],
                [1, 2]
            ]
        ],
        ["d", 1],
        ["clp", 0],
        ["col", new Map()]
    ],
    [
        [
            "am",
            [
                [1, 1],
                [1, 2]
            ]
        ],
        ["d", 1],
        ["clp", 1],
        ["col", new Map([[1, 2]])]
    ],
    [
        [
            "am",
            [
                [1, 1],
                [1, 2]
            ]
        ],
        ["d", 1],
        ["d", 1],
        ["d", 1],
        ["clp", 3],
        ["col", new Map()]
    ],
    [
        [
            "am",
            [
                [1, 1],
                [1, 2]
            ]
        ],
        ["d", 1],
        ["d", 1],
        ["g", [1, undefined]]
    ]
];

describe(`Calldata Indexing Tests`, () => {
    let i = 0;
    for (const testSeq of tests) {
        it(`Test ${i}`, () => {
            const immMaps: Array<ImmMap<any, any>> = [ImmMap.fromEntries([])];
            for (const step of testSeq) {
                testStep(step, immMaps);
            }
        });
        i++;
    }
});
