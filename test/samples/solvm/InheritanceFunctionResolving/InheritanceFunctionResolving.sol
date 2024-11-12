pragma solidity ^0.5.0;

contract A {
    function test() public pure returns (uint) {
        return 1;
    }
}

contract B is A {
    function test() public pure returns (uint) {
        return super.test() + 1;
    }
}

contract C is B {
    function test() public pure returns (uint) {
        return super.test() + 5;
    }
}

contract Test {
    function main() public {
        assert((new A()).test() == 1);
        assert((new B()).test() == 2);
        assert((new C()).test() == 7);
    }
}

contract __IRTest__ {
    function main() public {
        Test __this__ = new Test();
        __testCase92__(__this__);
    }

    function __testCase92__(Test __this__) internal {
        __this__.main();
    }
}