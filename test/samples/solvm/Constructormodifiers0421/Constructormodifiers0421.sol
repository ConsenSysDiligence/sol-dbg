pragma solidity 0.4.21;

contract A {
    uint8[] public vals;

    modifier beforeA() {
        vals.push(1);
        _;
    }

    modifier afterA() {
        _;
        vals.push(2);
    }

    function A(uint8 x) public beforeA() afterA() {
        vals.push(x);
    }
}

contract B is A(10) {}

contract C is A {
    function C() public A(15) {}
}

contract D is B, C {
    function D() public {}
}

contract E is C, B {}

contract F is A(5) {
    function F() public A(8) {}
}

contract G is B {
    function G() public A(20) {}
}

contract Validator {
    function validateD() public {
        D d = new D();
        uint8[3] memory expected = [1, 15, 2];
        for (uint i = 0; i < expected.length; i++) {
            assert(d.vals(i) == expected[i]);
        }
    }

    function validateE() public {
        E e = new E();
        uint8[3] memory expected = [1, 10, 2];
        for (uint i = 0; i < expected.length; i++) {
            assert(e.vals(i) == expected[i]);
        }
    }

    function validateF() public {
        F f = new F();
        uint8[3] memory expected = [1, 8, 2];
        for (uint i = 0; i < expected.length; i++) {
            assert(f.vals(i) == expected[i]);
        }
    }

    function validateG() public {
        G g = new G();
        uint8[3] memory expected = [1, 20, 2];
        for (uint i = 0; i < expected.length; i++) {
            assert(g.vals(i) == expected[i]);
        }
    }
}

contract __IRTest__ {
    function main() public {
        Validator __this__ = new Validator();
        __testCase281__(__this__);
        __testCase295__(__this__);
        __testCase309__(__this__);
        __testCase323__(__this__);
    }

    function __testCase281__(Validator __this__) internal {
        __this__.validateD();
    }

    function __testCase295__(Validator __this__) internal {
        __this__.validateE();
    }

    function __testCase309__(Validator __this__) internal {
        __this__.validateF();
    }

    function __testCase323__(Validator __this__) internal {
        __this__.validateG();
    }
}