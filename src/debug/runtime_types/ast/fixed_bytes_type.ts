import { BaseRuntimeType } from "./base_type";

export class FixedBytesType extends BaseRuntimeType {
    constructor(public readonly numBytes: number) {
        super();
    }

    pp(): string {
        return `bytes${this.numBytes}`;
    }
}
