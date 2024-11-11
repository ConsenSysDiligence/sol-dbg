import { bytesToHex } from "ethereum-cryptography/utils";
import * as sol from "solc-typed-ast";
import { nyi } from "./exceptions";
import {
    DefaultConstructorStep,
    EvalStep,
    ExceptionStep,
    ExecStep,
    ExternalCallStep,
    InternalReturnStep,
    SolTrace,
    SolTraceStep
} from "./trace";

export function pp(t: SolTraceStep | SolTrace): string {
    if (t instanceof Array) {
        return t.map(pp).join("\n");
    }

    if (t instanceof ExternalCallStep) {
        return `External call to ${t.msg.to?.toString()} with data ${bytesToHex(t.msg.data)}`;
    }

    if (t instanceof ExceptionStep) {
        return `Exception thrown with data ${bytesToHex(t.exc.data)}`;
    }

    if (t instanceof DefaultConstructorStep) {
        return `Default constructor for ${t.contract.name} ran`;
    }

    if (t instanceof EvalStep) {
        return `Eval ${sol.pp(t.expr)} to ${t.value}`;
    }

    if (t instanceof InternalReturnStep) {
        return `Return ${t.values}`;
    }

    if (t instanceof ExecStep) {
        return `Exec ${sol.pp(t.stmt)}`;
    }

    nyi(`pp(${t.constructor.name})`);
}
