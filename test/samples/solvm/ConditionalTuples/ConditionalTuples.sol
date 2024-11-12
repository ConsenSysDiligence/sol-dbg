pragma solidity 0.7.6;

contract ConditionalTuples {
    struct Some {
        uint a;
        uint b;
    }

    mapping(uint => Some) public m;
    uint internal SV;

    function a() public returns (uint, uint) {
        return (1, 2);
    }

    function setSV(uint x) internal returns (uint) {
        SV = x;
        return x;
    }

    function main() public {
        m[0] = Some(10, 15);
        m[1] = Some(20, 25);
        (uint a, uint b) = true ? (1, 2) : (3, 4);
        assert(a == 1);
        assert(b == 2);
        (uint c, uint d) = false ? (1, 2) : (3, 4);
        assert(c == 3);
        assert(d == 4);
        (uint e, uint f) = true ? this.m(0) : this.m(1);
        assert(e == 10);
        assert(f == 15);
        (uint g, uint h) = false ? this.m(0) : this.m(1);
        assert(g == 20);
        assert(h == 25);
        (uint i, uint j) = true ? this.m(0) : (3, 4);
        assert(i == 10);
        assert(j == 15);
        (uint k, ) = (42, uint);
        assert(k == 42);
        (uint l, , bool n) = true ? (10, uint, true) : (20, uint, false);
        assert((l == 10) && n);
        (uint p, , bool q) = true ? (10, (uint, 1, uint), false) : (20, (uint, 2, uint), true);
        assert((p == 10) && (!q));
        SV = 1;
        (uint r, , bool s) = true ? (10, (uint, setSV(43), uint), true) : (20, (uint, setSV(44), uint), false);
        assert((r == 10) && s);
        assert(SV == 43);
    }
}

contract __IRTest__ {
    function main() public {
        ConditionalTuples __this__ = new ConditionalTuples();
        __testCase324__(__this__);
    }

    function __testCase324__(ConditionalTuples __this__) internal {
        __this__.main();
    }
}