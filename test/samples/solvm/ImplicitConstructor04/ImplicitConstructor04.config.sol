pragma solidity ^0.4.13;

contract A {
    int internal a;

    function A(int v) public {
        a = v;
    }
}

contract B is A(20) {
    uint internal b = 40;
}

contract C is B {
    int internal c;

    function C(int v) public {
        c = v;
    }
}

contract D is C(30) {}

contract T is D {
    function test() public {
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