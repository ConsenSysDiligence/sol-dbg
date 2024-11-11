pragma solidity >=0.4.18<0.5.0;

contract UntypedVarsStorageInference {
    uint internal a;
    uint[] internal b;
    uint[][] internal c;

    function main() public {
        var x = a;
        x = 1;
        assert(a == 0);
        var y = b;
        y.push(2);
        assert((b[0] == 2) && (b.length == 1));
        c.push([uint(1)]);
        var z = c[0];
        z[0] = 3;
        assert(((c.length == 1) && (c[0].length == 1)) && (c[0][0] == 3));
        var u = c;
        u.push([uint(42)]);
        assert(((c.length == 2) && (c[1].length == 1)) && (c[1][0] == 42));
    }
}

contract __IRTest__ {
    function main() public {
        UntypedVarsStorageInference __this__ = new UntypedVarsStorageInference();
        __testCase140__(__this__);
    }

    function __testCase140__(UntypedVarsStorageInference __this__) internal {
        __this__.main();
    }
}