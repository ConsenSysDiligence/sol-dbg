pragma solidity ^0.4.24;

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

    constructor(uint8 x) public beforeA() afterA() {
        vals.push(x);
    }
}

contract B is A(3) {
    modifier beforeB() {
        vals.push(4);
        _;
    }

    modifier afterB() {
        _;
        vals.push(5);
    }

    constructor(uint8 x) public beforeB() afterB() {
        vals.push(x);
    }
}

contract C is B {
    modifier beforeC() {
        vals.push(6);
        _;
    }

    modifier afterC() {
        _;
        vals.push(7);
    }

    constructor(uint8 x) public beforeC() afterC() B(8) {
        vals.push(x);
    }
}

contract D is B {
    modifier beforeD() {
        vals.push(6);
        _;
    }

    modifier afterD() {
        _;
        vals.push(7);
    }

    constructor(uint8 x) public B(8) beforeD() afterD() {
        vals.push(x);
    }
}

contract E is B {
    modifier beforeE() {
        vals.push(6);
        _;
    }

    modifier afterE() {
        _;
        vals.push(7);
    }

    constructor(uint8 x) public beforeE() B(8) afterE() {
        vals.push(x);
    }
}

contract F is B(6) {
    modifier beforeF() {
        vals.push(7);
        _;
    }

    modifier afterF() {
        _;
        vals.push(8);
    }

    constructor() public beforeF() afterF() {
        vals.push(9);
    }
}

contract G is C(0) {}

contract Validator {
    function validateA() public {
        A a = new A(uint8(0));
        uint8[3] memory expected = [1, 0, 2];
        for (uint i = 0; i < expected.length; i++) {
            assert(a.vals(i) == expected[i]);
        }
    }

    function validateB() public {
        B b = new B(uint8(0));
        uint8[6] memory expected = [1, 3, 2, 4, 0, 5];
        for (uint i = 0; i < expected.length; i++) {
            assert(b.vals(i) == expected[i]);
        }
    }

    function validateCDEG() public {
        C c = new C(uint8(0));
        D d = new D(uint8(0));
        E e = new E(uint8(0));
        G g = new G();
        uint8[9] memory expected = [1, 3, 2, 4, 8, 5, 6, 0, 7];
        for (uint i = 0; i < expected.length; i++) {
            assert(c.vals(i) == expected[i]);
            assert(d.vals(i) == expected[i]);
            assert(e.vals(i) == expected[i]);
            assert(g.vals(i) == expected[i]);
        }
    }

    function validateF() public {
        F f = new F();
        uint8[9] memory expected = [1, 3, 2, 4, 6, 5, 7, 9, 8];
        for (uint i = 0; i < expected.length; i++) {
            assert(f.vals(i) == expected[i]);
        }
    }
}

contract __IRTest__ {
    function main() public {
        Validator __this__ = new Validator();
        __testCase521__(__this__);
        __testCase535__(__this__);
        __testCase549__(__this__);
        __testCase563__(__this__);
    }

    function __testCase521__(Validator __this__) internal {
        __this__.validateA();
    }

    function __testCase535__(Validator __this__) internal {
        __this__.validateB();
    }

    function __testCase549__(Validator __this__) internal {
        __this__.validateCDEG();
    }

    function __testCase563__(Validator __this__) internal {
        __this__.validateF();
    }
}