import { assert } from "solc-typed-ast";

export class ImmMap<KeyT, ValT> {
    private innerM: Map<KeyT, ValT>;
    private deletedKeys: Set<KeyT>;
    private _next: this | undefined;

    static fromEntries<K, V>(arg: Iterable<[K, V]>): ImmMap<K, V> {
        const res = new ImmMap<K, V>(undefined);

        for (const [k, v] of arg) {
            res.innerM.set(k, v);
        }

        return res;
    }

    private constructor(next: any = undefined) {
        this.innerM = new Map();
        this._next = next;
        this.deletedKeys = new Set();
    }

    get(key: KeyT): ValT | undefined {
        if (this.deletedKeys.has(key)) {
            return undefined;
        }

        if (this.innerM.has(key)) {
            return this.innerM.get(key);
        }

        if (this._next === undefined) {
            return undefined;
        }

        const resInNext = this._next.get(key);

        if (resInNext !== undefined) {
            this.innerM.set(key, resInNext);
        }

        return resInNext;
    }

    set(key: KeyT, val: ValT): this {
        const newMap = new ImmMap<KeyT, ValT>(this);

        newMap.innerM.set(key, val);

        return newMap as this;
    }

    delete(key: KeyT): this {
        const res = new ImmMap<KeyT, ValT>(this);
        res.deletedKeys.add(key);
        return res as this;
    }

    setMany(entries: Iterable<[KeyT, ValT]>): this {
        const newMap = new ImmMap<KeyT, ValT>(this);

        for (const [key, val] of entries) {
            newMap.innerM.set(key, val);
        }

        return newMap as this;
    }

    public collectMap(untilParent: ImmMap<KeyT, ValT> | undefined = undefined): Map<KeyT, ValT> {
        let res: Map<KeyT, ValT>;

        if (this._next === untilParent) {
            res = new Map();
        } else {
            assert(
                this._next !== undefined,
                `Error in collectMap chain tracking. Did you mix up your chains?`
            );
            res = this._next.collectMap(untilParent);
        }

        for (const [key, val] of this.innerM) {
            res.set(key, val);
        }

        for (const delKey of this.deletedKeys) {
            res.delete(delKey);
        }

        return res;
    }

    collapseUntil(parent: ImmMap<KeyT, ValT>): this {
        const rawMap = this.collectMap(parent);
        const res = new ImmMap<KeyT, ValT>(parent);
        res.innerM = rawMap;
        return res as this;
    }

    entries(): Iterable<[KeyT, ValT]> {
        return this.collectMap();
    }
}
