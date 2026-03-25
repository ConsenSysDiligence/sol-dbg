import { Block, BlockData } from "@ethereumjs/block";
import { Hardfork, Common, Mainnet } from "@ethereumjs/common";
import { bytesToBigInt } from "@ethereumjs/util";

const hardforkBlocks: Array<[bigint, Hardfork]> = [
    [0n, Hardfork.Chainstart],
    [1150000n, Hardfork.Homestead],
    [1920000n, Hardfork.Dao],
    [2463000n, Hardfork.TangerineWhistle],
    [2675000n, Hardfork.SpuriousDragon],
    [4370000n, Hardfork.Byzantium],
    [7280000n, Hardfork.Constantinople],
    [9069000n, Hardfork.Istanbul],
    [9200000n, Hardfork.MuirGlacier],
    [12244000n, Hardfork.Berlin],
    [12965000n, Hardfork.London],
    [13774000n, Hardfork.ArrowGlacier],
    [15050000n, Hardfork.GrayGlacier],
    [15537394n, Hardfork.Paris],
    [17034870n, Hardfork.Shanghai],
    [19426587n, Hardfork.Cancun],
    [22431084n, Hardfork.Prague],
    [23935694n, Hardfork.Osaka]
];

export function getCommon(hardfork: Hardfork): Common {
    return new Common({ chain: Mainnet, hardfork });
}

export function getCommonByBlockNum(blockNum: bigint | undefined = undefined): Common {
    let hardfork: Hardfork = hardforkBlocks[hardforkBlocks.length - 1][1];

    if (blockNum !== undefined) {
        for (let i = 0; i < hardforkBlocks.length - 1; i++) {
            if (blockNum >= hardforkBlocks[i][0] && blockNum < hardforkBlocks[i + 1][0]) {
                hardfork = hardforkBlocks[i][1];
                break;
            }
        }
    }

    return getCommon(hardfork);
}
export function getCommonForBlock(blockData: BlockData | Block): Common {
    if (blockData.header && blockData.header.number !== undefined) {
        const num =
            blockData.header.number instanceof Uint8Array
                ? bytesToBigInt(blockData.header.number)
                : BigInt(blockData.header.number);
        return getCommonByBlockNum(num);
    }

    // @todo fix this after we update underlying repos to latest compiler and sol-dbg to latest hardfork
    return getCommon(Hardfork.Cancun);
}
