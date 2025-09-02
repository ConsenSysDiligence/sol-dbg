import * as sol from "solc-typed-ast";
import { BaseRuntimeType } from "./base_type";

export class PointerType extends BaseRuntimeType {
    constructor(
        public readonly toType: BaseRuntimeType,
        public readonly location: sol.DataLocation
    ) {
        super();
        sol.assert(
            location !== sol.DataLocation.Default,
            `Unexpected pointer type with default location`
        );
    }

    pp(): string {
        return `${this.toType.pp()} ${this.location}`;
    }
}
