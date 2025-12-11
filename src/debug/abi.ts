import * as sol from "solc-typed-ast";
import { View } from "./decoding/view";
import { DecodedEventDesc, EventDefInfo, EventDesc, Memory } from "./types";
import { bytes4, split, uint256, zip } from "../utils";
import { BaseCalldataView, makeCalldataView, makeCalldataViews } from "./decoding/calldata/view";
import { DecodingFailure, Value } from "./decoding/value";
import { IArtifactManager } from "./artifact_manager";
import { astToRuntimeType, BaseRuntimeType, getterArgsAndReturn, PointerType, typeIdToRuntimeType } from "./runtime_types";

/**
 * Return true if the given callee requires a selector
 * @param callee
 * @returns
 */
export function hasSelector(callee: sol.FunctionDefinition | sol.VariableDeclaration): boolean {
    if (callee instanceof sol.VariableDeclaration) {
        return true;
    }

    if (
        callee.isConstructor ||
        callee.kind === sol.FunctionKind.Receive ||
        callee.kind === sol.FunctionKind.Fallback
    ) {
        return false;
    }

    return true;
}

function isTypeUnknownContract(t: sol.TypeName | undefined): boolean {
    return (
        t instanceof sol.UserDefinedTypeName &&
        t.referencedDeclaration < 0 &&
        (t.typeString.startsWith("contract ") ||
            t.typeString.startsWith("interface ") ||
            t.typeString.startsWith("library "))
    );
}

export function buildMsgViews(
    callee: sol.FunctionDefinition | sol.VariableDeclaration,
    base?: bigint
): Array<[string, View<Memory>]> {
    const res: Array<[string, View]> = [];
    const ctx = callee.requiredContext

    if (base === undefined) {
        base = 0n;

        if (hasSelector(callee)) {
            res.push(["<selector>", makeCalldataView(bytes4, 0n, base)]);
            base = 4n;
        }
    }

    const formals: Array<[string, sol.TypeIdentifier]> =
        callee instanceof sol.FunctionDefinition
            ? callee.vParameters.vParameters.map((argDef: sol.VariableDeclaration) => [
                  argDef.name,
                  isTypeUnknownContract(argDef.vType)
                      ? new sol.AddressTypeId(false)
                      : sol.typeOf(argDef)
              ])
            : getterArgsAndReturn(callee)[0].map((typ: sol.TypeIdentifier, i: number) => [`ARG_${i}`, typ]);

    const views = makeCalldataViews(
        formals.map((x) => {
            const rtt = typeIdToRuntimeType(x[1], ctx, sol.DataLocation.CallData);
            return rtt instanceof PointerType && rtt.location === sol.DataLocation.Storage
                ? uint256
                : rtt;
        }),
        base
    );
    res.push(
        ...zip(
            formals.map((x) => x[0]),
            views
        )
    );

    return res;
}

abstract class BaseEventView<V extends Value, L, T extends BaseRuntimeType> extends View<
    EventDesc,
    V,
    L,
    T
> {}

class EventPayloadView<V extends Value, T extends BaseRuntimeType> extends BaseEventView<
    V,
    BaseCalldataView<V, T>,
    T
> {
    decode(state: EventDesc): V | DecodingFailure {
        return this.loc.decode(state.payload);
    }

    pp(): string {
        return `<${this.type.pp()}@${this.loc} in event payload>`;
    }
}

class TopicPayloadView<T extends BaseRuntimeType> extends BaseEventView<Value, number, T> {
    decode(state: EventDesc): Value | DecodingFailure {
        if (this.type instanceof PointerType) {
            return new DecodingFailure(`Cannot decode indexed complex type ${this.type.pp()}`);
        }

        const inner = makeCalldataView(this.type, 0n, 0n);
        return inner.decode(state.topics[this.loc]);
    }

    pp(): string {
        return `<${this.type.pp()} in topic ${this.loc}>`;
    }
}

type GenEventView = BaseEventView<Value, any, BaseRuntimeType>;
export function buildEventViews(
    evtDef: EventDefInfo,
    infer: sol.InferType
): Array<[string, GenEventView]> {
    const [indexedArgs, nonIndexedArgs] = split(
        evtDef.args.map<[number, [string, sol.TypeNode, boolean]]>((x, i) => [i, x]),
        ([, [, , indexed]]) => indexed
    );

    let topicIdx = evtDef.definition.anonymous ? 0 : 1;
    const indexedViews: GenEventView[] = indexedArgs.map(
        ([, [, type]]) =>
            new TopicPayloadView(
                astToRuntimeType(type, infer, sol.DataLocation.CallData),
                topicIdx++
            )
    );
    const nonIndexedViews: GenEventView[] = makeCalldataViews(
        nonIndexedArgs.map(([, [, type]]) =>
            astToRuntimeType(type, infer, sol.DataLocation.CallData)
        ),
        0n
    ).map((v) => new EventPayloadView(v.type, v));

    const allArgDesc: Array<[number, string, GenEventView]> = [
        ...indexedViews.map<[number, string, GenEventView]>((view, i) => [
            indexedArgs[i][0],
            indexedArgs[i][1][0],
            view
        ]),
        ...nonIndexedViews.map<[number, string, GenEventView]>((view, i) => [
            nonIndexedArgs[i][0],
            nonIndexedArgs[i][1][0],
            view
        ])
    ];

    allArgDesc.sort();

    return allArgDesc.map(([, name, view]) => [name, view]);
}

/**
 * Decode a raw event. Currently only supports non-anonmyous events.
 */
export function decodeEvent(
    artifactManager: IArtifactManager,
    evt: EventDesc
): DecodedEventDesc | undefined {
    if (evt.topics.length === 0) {
        return undefined;
    }

    const defInfo = artifactManager.getEventDefInfo(evt.topics[0]);

    if (!defInfo) {
        return undefined;
    }

    const infer = artifactManager.infer(defInfo.artifact.compilerVersion);
    const dataViews = buildEventViews(defInfo, infer);
    const argVals: Array<[string, any]> = dataViews.map(([name, view]) => [name, view.decode(evt)]);

    return {
        def: defInfo,
        args: argVals
    };
}
