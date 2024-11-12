pragma solidity ^0.4.24;

contract Base {
    function foo() public returns (uint) {
        return 1;
    }

    function boo() public;
}

contract Child1 is Base {
    function foo() public returns (uint) {
        return 2;
    }

    function boo() public {
        assert(super.foo() == 1);
    }
}

contract Child2 is Child1 {
    function foo() public returns (uint) {
        return 3;
    }
}

contract SuperKeyword {
    function main() public {
        Base b1 = new Child1();
        Base b2 = new Child2();
        b1.boo();
        b2.boo();
    }
}

contract __IRTest__ {
    function main() public {
        SuperKeyword __this__ = new SuperKeyword();
        __testCase88__(__this__);
    }

    function __testCase88__(SuperKeyword __this__) internal {
        __this__.main();
    }
}