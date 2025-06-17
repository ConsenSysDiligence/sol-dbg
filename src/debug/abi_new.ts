import {
    FunctionDefinition,
    VariableDeclaration,
    InferType,
    FunctionKind,
    TypeNode,
    types,
    TypeName,
    UserDefinedTypeName,
    DataLocation,
    PointerType
} from "solc-typed-ast";
import { View } from "./decoding/view";
import { DecodedEventDesc, EventDefInfo, EventDesc, Memory } from "./types";
import { bytes4, split, zip } from "../utils";
import { BaseCalldataView, makeCalldataView, makeCalldataViews } from "./decoding/calldata/view";
import { DecodingFailure, Value } from "./decoding/value";
import { simplifyType } from "./decoding";
import { IArtifactManager } from "./artifact_manager";

/**
 * Return true if the given callee requires a selector
 * @param callee
 * @returns
 */
function hasSelector(callee: FunctionDefinition | VariableDeclaration): boolean {
    if (callee instanceof VariableDeclaration) {
        return true;
    }

    if (
        callee.isConstructor ||
        callee.kind === FunctionKind.Receive ||
        callee.kind === FunctionKind.Fallback
    ) {
        return false;
    }

    return true;
}

function isTypeUnknownContract(t: TypeName | undefined): boolean {
    return (
        t instanceof UserDefinedTypeName &&
        t.referencedDeclaration < 0 &&
        (t.typeString.startsWith("contract ") ||
            t.typeString.startsWith("interface ") ||
            t.typeString.startsWith("library "))
    );
}

export function buildMsgViews(
    callee: FunctionDefinition | VariableDeclaration,
    infer: InferType
): Array<[string, View<Memory>]> {
    const res: Array<[string, View]> = [];
    let base: bigint = 0n;

    if (hasSelector(callee)) {
        res.push(["<selector>", makeCalldataView(bytes4, 0n, base)]);
        base = 4n;
    }

    const formals: Array<[string, TypeNode]> =
        callee instanceof FunctionDefinition
            ? callee.vParameters.vParameters.map((argDef: VariableDeclaration) => [
                argDef.name,
                isTypeUnknownContract(argDef.vType)
                    ? types.address
                    : infer.variableDeclarationToTypeNode(argDef)
            ])
            : infer
                .getterArgsAndReturn(callee)[0]
                .map((typ: TypeNode, i: number) => [`ARG_${i}`, typ]);

    const views = makeCalldataViews(
        formals.map((x) => x[1]),
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

abstract class BaseEventView<V extends Value, L, T extends TypeNode> extends View<
    EventDesc,
    V,
    L,
    T
> { }

class EventPayloadView<V extends Value, T extends TypeNode> extends BaseEventView<
    V,
    BaseCalldataView<V, T>,
    T
> {
    decode(state: EventDesc): V {
        return this.loc.decode(state.payload);
    }

    pp(): string {
        return `<${this.type.pp()}@${this.loc} in event payload>`;
    }
}

class TopicPayloadView<T extends TypeNode> extends BaseEventView<Value, number, T> {
    decode(state: EventDesc): Value {
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

type GenEventView = BaseEventView<Value, any, TypeNode>;
export function buildEventViews(
    evtDef: EventDefInfo,
    infer: InferType
): Array<[string, GenEventView]> {
    const [indexedArgs, nonIndexedArgs] = split(
        evtDef.args.map<[number, [string, TypeNode, boolean]]>((x, i) => [i, x]),
        ([, [, , indexed]]) => indexed
    );

    let topicIdx = evtDef.definition.anonymous ? 0 : 1;
    const indexedViews: GenEventView[] = indexedArgs.map(
        ([, [, type]]) =>
            new TopicPayloadView(simplifyType(type, infer, DataLocation.CallData), topicIdx++)
    );
    const nonIndexedViews: GenEventView[] = makeCalldataViews(
        nonIndexedArgs.map(([, [, type]]) => simplifyType(type, infer, DataLocation.CallData)),
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
