pragma solidity ^0.5.12;

contract A {
    int256 public a;
    uint256 internal b;
    bytes1 private c;

    function asetA(int256 x) public {
        a = x;
    }

    function agetA() public returns (int256) {
        return a;
    }
}

contract B {
    int256 public a;
    uint256 internal b;
    bytes1 private c;

    function bsetA(int256 x) public {
        a = x;
    }

    function bgetA() public returns (int256) {
        return a;
    }
}

contract C is A, B {
    function csetA(int256 x) public {
        a = x;
    }
}

contract Main {
    function main() public {
        C c = new C();
        (A(c)).asetA(10);
        (B(c)).bsetA(20);
        assert(((c.a() == 20) && (A(c).agetA() == 10)) && (B(c).bgetA() == 20));
        c.csetA(30);
        assert(((c.a() == 30) && (A(c).agetA() == 10)) && (B(c).bgetA() == 30));
    }
}

contract __IRTest__ {
    function main() public {
        Main __this__ = new Main();
        __testCase161__(__this__);
    }

    function __testCase161__(Main __this__) internal {
        __this__.main();
    }
}