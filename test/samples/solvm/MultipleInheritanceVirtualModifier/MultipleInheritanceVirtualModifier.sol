pragma solidity 0.7.6;

contract Base1 {
    int internal a;

    modifier A() virtual {
        a = 1;
        _;
    }

    function foo1() public A() returns (int) {
        return a;
    }
}

contract Base2 {
    int internal b;

    modifier A() virtual {
        b = 2;
        _;
    }

    function foo2() public A() returns (int) {
        return b;
    }
}

contract Foo is Base1, Base2 {
    modifier A() override(Base1, Base2) {
        a = 4;
        b = 4;
        _;
    }
}

contract Test {
    function main() public {
        Base1 b1 = new Base1();
        Base1 bf1 = new Foo();
        Base2 b2 = new Base2();
        Base2 bf2 = new Foo();
        Foo f = new Foo();
        assert(b1.foo1() == 1);
        assert(bf1.foo1() == 4);
        assert(b2.foo2() == 2);
        assert(bf2.foo2() == 4);
        assert(f.foo1() == 4);
        assert(f.foo2() == 4);
    }
}

contract __IRTest__ {
    function main() public {
        Test __this__ = new Test();
        __testCase160__(__this__);
    }

    function __testCase160__(Test __this__) internal {
        __this__.main();
    }
}