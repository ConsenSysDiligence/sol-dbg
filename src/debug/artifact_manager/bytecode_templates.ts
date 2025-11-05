import { equalsBytes } from "@ethereumjs/util";
import { RangeList } from "../../artifacts";
import { BytecodeInfo } from ".";

export interface BytecodeTemplate {
    object: Uint8Array;
    skipRanges: Array<[number, number]>;
}

function makeSkipRanges(rawList: RangeList): Array<[number, number]> {
    return rawList.map((raw) => [raw.start, raw.start + raw.length]);
}

export function makeTemplate(artifact: BytecodeInfo): BytecodeTemplate {
    const skipRanges: Array<[number, number]> = [];

    for (const [, ranges] of artifact.linkReferences) {
        skipRanges.push(...makeSkipRanges(ranges));
    }

    for (const [, ranges] of artifact.immutableReferences) {
        skipRanges.push(...makeSkipRanges(ranges));
    }

    skipRanges.sort(([s1, e1], [s2, e2]) => s1 < s2 ? -1 : s1 == s2 ? 0 : 1);

    return {
        object: artifact.bytecode,
        skipRanges
    };
}

export function matchesTemplate(
    bytecode: Uint8Array,
    template: BytecodeTemplate,
    isCreation: boolean
): boolean {
    if (
        (isCreation && bytecode.length < template.object.length) ||
        (!isCreation && bytecode.length !== template.object.length)
    ) {
        return false;
    }

    let curIdx = 0;
    let rangeIdx = 0;

    while (curIdx < template.object.length) {
        let nextIdx: number;
        let compEnd: number;

        if (rangeIdx < template.skipRanges.length) {
            [compEnd, nextIdx] = template.skipRanges[rangeIdx];
        } else {
            compEnd = nextIdx = template.object.length;
        }

        if (!equalsBytes(bytecode.slice(curIdx, compEnd), template.object.slice(curIdx, compEnd))) {
            return false;
        }

        curIdx = nextIdx;
        rangeIdx++;
    }

    return true;
}
