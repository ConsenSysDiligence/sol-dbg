pragma solidity ^0.4.24;

contract A {
    int internal a;

    constructor(int _a) public {
        a = _a;
    }
}

contract B is A {
    int internal b;

    constructor(int _a, int _b) public A(_a) {
        b = _b;
    }
}

contract UnknownVar is B {
    int internal c;

    constructor(int _a, int _b, int _c) public B(_a + _b,_b) {
        c = _c;
    }

    function getA() public returns (int) {
        return a;
    }
}

contract __IRTest__ {
    function main() public {
        UnknownVar __this__ = new UnknownVar(int256(3), int256(4), int256(7));
        __testCase90__(__this__);
    }

    function __testCase90__(UnknownVar __this__) internal {
        int256 ret_90_0 = __this__.getA();
        assert(ret_90_0 == int256(7));
    }
}