pragma solidity 0.7.6;

contract Base {
    uint internal x = 0;

    modifier A() virtual {
        x = 42;
        _;
    }

    function foo() virtual public A() returns (uint) {
        return x;
    }
}

contract Child1 is Base {
    function foo() virtual override public returns (uint) {
        return x;
    }
}

contract Child2 is Child1 {
    modifier A() override {
        x = 2;
        _;
    }
}

contract Test {
    function main() public {
        Base b = new Base();
        Child1 c1 = new Child1();
        Child2 c2 = new Child2();
        assert(c2.foo() == 0);
        assert(c1.foo() == 0);
        assert(b.foo() == 42);
    }
}

contract __IRTest__ {
    function main() public {
        Test __this__ = new Test();
        __testCase108__(__this__);
    }

    function __testCase108__(Test __this__) internal {
        __this__.main();
    }
}