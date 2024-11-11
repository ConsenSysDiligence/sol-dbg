pragma solidity ^0.4.24;

contract A {
    uint internal z = 1;

    function E1(uint a, uint b) public returns (uint) {
        return (a + b) + z;
    }
}

contract B is A {
    uint internal w = 2;

    function E1(uint c) public returns (uint) {
        return c + w;
    }

    function E2(uint d) public returns (uint) {
        return d + 1;
    }
}

contract C is B {
    uint internal d;

    function E3() public returns (uint) {
        return 0;
    }

    function main() public {
        uint t1 = E1(1, 2);
        assert(t1 == 4);
        uint t2 = E1(3);
        assert(t2 == 5);
        uint t3 = E2(5);
        assert(t3 == 6);
        uint t4 = E3();
        assert(t4 == 0);
    }
}

contract __IRTest__ {
    function main() public {
        C __this__ = new C();
        __testCase130__(__this__);
    }

    function __testCase130__(C __this__) internal {
        __this__.main();
    }
}