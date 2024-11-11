pragma solidity 0.7.6;

contract A {
    int internal a;

    constructor(int v) {
        a = v;
    }
}

contract B is A(20) {
    uint internal b = 40;
}

contract C is B {
    int internal c;

    constructor(int v) {
        c = v;
    }
}

contract D is C(30) {}

contract T is D {
    function test() public view {
        assert(a == 20);
        assert(b == 40);
        assert(c == 30);
    }
}

contract __IRTest__ {
    function main() public {
        T __this__ = new T();
        __testCase79__(__this__);
    }

    function __testCase79__(T __this__) internal {
        __this__.test();
    }
}