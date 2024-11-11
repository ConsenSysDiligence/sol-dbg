pragma solidity 0.7.6;

contract Base {
    uint internal x;

    modifier A() virtual {
        x = 0;
        _;
    }

    function foo() public A() returns (uint) {
        return x;
    }
}

contract Child is Base {
    modifier A() virtual override {
        x = 1;
        _;
    }

    function goo() public returns (uint) {
        return this.foo();
    }
}

contract Test {
    function main() public {
        Child c = new Child();
        assert(c.goo() == 1);
    }
}

contract __IRTest__ {
    function main() public {
        Test __this__ = new Test();
        __testCase77__(__this__);
    }

    function __testCase77__(Test __this__) internal {
        __this__.main();
    }
}