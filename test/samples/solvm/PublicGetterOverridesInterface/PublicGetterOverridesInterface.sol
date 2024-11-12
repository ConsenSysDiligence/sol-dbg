pragma solidity 0.6.3;

abstract contract Base {
    function x() virtual external returns (uint);

    function y(uint a) virtual external returns (uint);

    function z(uint a, uint b) virtual external returns (int);

    function a(int8 i) virtual external returns (address);
}

contract A is Base {
    function x() virtual override external returns (uint) {
        return 10;
    }

    function y(uint a) virtual override external returns (uint) {
        return 15;
    }

    function z(uint a, uint b) virtual override external returns (int) {
        return -5;
    }

    function a(int8 i) virtual override external returns (address) {
        return 0xC2321f2fa1a28f0553D4f400E92C49159376FAa6;
    }
}

contract B is A {
    uint public override x;
    uint[] public override y;
    int[][] public override z;
    mapping(int8 => address) public override a;

    constructor() public {
        x = 42;
        y.push(45);
        z.push([int(-25)]);
        a[-9] = 0x7eA9b52e9f8673f3E22b4eec2c4c7A7e2d1b6636;
    }
}

contract Main {
    function callX(Base b) public returns (uint) {
        return b.x();
    }

    function callY(Base b) public returns (uint) {
        return b.y(0);
    }

    function callZ(Base b) public returns (int) {
        return b.z(0, 0);
    }

    function callA(Base b) public returns (address) {
        return b.a(-9);
    }

    function main() public {
        A a = new A();
        B b = new B();
        assert(callX(a) == 10);
        assert(callX(b) == 42);
        assert(callY(a) == 15);
        assert(callY(b) == 45);
        assert(callZ(a) == (-5));
        assert(callZ(b) == (-25));
        assert(callA(a) == 0xC2321f2fa1a28f0553D4f400E92C49159376FAa6);
        assert(callA(b) == 0x7eA9b52e9f8673f3E22b4eec2c4c7A7e2d1b6636);
    }
}

contract __IRTest__ {
    function main() public {
        Main __this__ = new Main();
        __testCase280__(__this__);
    }

    function __testCase280__(Main __this__) internal {
        __this__.main();
    }
}