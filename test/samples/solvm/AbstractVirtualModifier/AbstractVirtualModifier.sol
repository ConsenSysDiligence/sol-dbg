pragma solidity ^0.6.8;

abstract contract Base {
    uint internal x = 0;

    modifier Moo() virtual;

    function foo() public Moo() returns (uint) {
        return x;
    }
}

contract Child is Base {
    modifier Moo() override {
        x = 42;
        _;
    }
}

contract Test {
    function main() public {
        Base b = new Child();
        assert(b.foo() == 42);
    }
}

contract __IRTest__ {
    function main() public {
        Test __this__ = new Test();
        __testCase62__(__this__);
    }

    function __testCase62__(Test __this__) internal {
        __this__.main();
    }
}