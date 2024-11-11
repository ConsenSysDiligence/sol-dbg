pragma solidity 0.7.6;

contract Base {
    function foo() virtual internal returns (uint) {
        return 1;
    }

    function moo() public returns (uint) {
        return foo();
    }
}

contract Child is Base {
    function foo() override internal returns (uint) {
        return 2;
    }
}

contract Test {
    function main() public returns (uint, uint, uint) {
        Base b = new Base();
        Child c = new Child();
        Base b1 = new Child();
        return (b.moo(), c.moo(), b1.moo());
    }
}

contract __IRTest__ {
    function main() public {
        Test __this__ = new Test();
        __testCase85__(__this__);
    }

    function __testCase85__(Test __this__) internal {
        (uint256 ret_85_0, uint256 ret_85_1, uint256 ret_85_2) = __this__.main();
        assert(ret_85_0 == uint256(1));
        assert(ret_85_1 == uint256(2));
        assert(ret_85_2 == uint256(2));
    }
}