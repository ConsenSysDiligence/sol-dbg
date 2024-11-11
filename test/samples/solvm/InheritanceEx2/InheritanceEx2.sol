pragma solidity ^0.4.24;

contract Base {
    uint internal s = 42;

    function foo(uint n) public returns (uint) {
        return s + n;
    }

    function bar(uint n) public returns (uint) {
        return this.foo(n);
    }

    function boo(uint n) public returns (uint) {
        return foo(n);
    }
}

contract Child is Base {
    uint internal s1 = 142;

    function foo(uint n) public returns (uint) {
        return s1 + n;
    }
}

contract DynamicDispatch {
    function main() public {
        Base b = new Base();
        Base cb = new Child();
        Child c = new Child();
        Base bc = Base(c);
        assert(b.foo(1) == 43);
        assert(cb.foo(1) == 143);
        assert(bc.foo(1) == 143);
        assert(b.bar(1) == 43);
        assert(cb.bar(1) == 143);
        assert(bc.bar(1) == 143);
        assert(b.boo(1) == 43);
        assert(cb.boo(1) == 143);
        assert(bc.boo(1) == 143);
    }
}

contract __IRTest__ {
    function main() public {
        DynamicDispatch __this__ = new DynamicDispatch();
        __testCase184__(__this__);
    }

    function __testCase184__(DynamicDispatch __this__) internal {
        __this__.main();
    }
}