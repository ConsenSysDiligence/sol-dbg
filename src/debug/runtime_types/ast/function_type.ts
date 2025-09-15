import { BaseRuntimeType } from "./base_type";

export class FunctionType extends BaseRuntimeType {
    constructor(
        public readonly argTs: BaseRuntimeType[],
        public readonly external: boolean,
        public readonly mutability: "pure" | "view" | "payable",
        public readonly retTs: BaseRuntimeType[]
    ) {
        super();
    }

    pp(): string {
        const argStr = this.argTs.map((argT) => argT.pp()).join(", ")
        const retStr = this.retTs.length > 0 ? `returns (${this.retTs.map((retT) => retT.pp()).join(", ")})` : ""
        return `function (${argStr})${this.external ? ' external ' : ' '}${this.mutability} ${retStr}`
    }
}
