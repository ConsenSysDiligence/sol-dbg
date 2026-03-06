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
        let _this: this | undefined = this;

        // Note: we convert the recursion to a loop here to avoid
        // JS stack overflow
        while (_this !== undefined) {
            if (_this.deletedKeys.has(key)) {
                return undefined;
            }

            if (_this.innerM.has(key)) {
                return _this.innerM.get(key);
            }

            _this = _this._next;
        }

        return undefined;
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
        const stack: this[] = [];
        const res: Map<KeyT, ValT> = new Map();

        if (untilParent === this) {
            return res;
        }

        let _this: this | undefined = this;

        while (_this !== untilParent && _this !== undefined) {
            stack.unshift(_this);
            _this = _this._next;
        }

        for (const _this of stack) {
            for (const [key, val] of _this.innerM) {
                res.set(key, val);
            }

            for (const delKey of _this.deletedKeys) {
                res.delete(delKey);
            }
        }

        return res;
    }

    collapseUntil(parent: ImmMap<KeyT, ValT>): this {
        if (parent === this) {
            return this;
        }

        const rawMap = this.collectMap(parent);
        const res = new ImmMap<KeyT, ValT>(parent);
        res.innerM = rawMap;
        return res as this;
    }

    entries(): Iterable<[KeyT, ValT]> {
        return this.collectMap();
    }
}
