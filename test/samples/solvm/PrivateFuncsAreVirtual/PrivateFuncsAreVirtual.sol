pragma solidity ^0.4.24;

contract Base {
    function boo() public returns (uint) {
        return foo();
    }

    function foo() private returns (uint) {
        return 1;
    }
}

contract Child is Base {
    function foo() private returns (uint) {
        return 2;
    }
}

contract PrivateFuncsAreVirtual {
    function main() public {
        Base b = new Child();
        Base b1 = new Base();
        assert(b.boo() == 2);
        assert(b1.boo() == 1);
    }
}

contract __IRTest__ {
    function main() public {
        PrivateFuncsAreVirtual __this__ = new PrivateFuncsAreVirtual();
        __testCase77__(__this__);
    }

    function __testCase77__(PrivateFuncsAreVirtual __this__) internal {
        __this__.main();
    }
}