pragma solidity ^0.4.20;

interface IOther {
    function id() external pure returns (uint);
}

contract Base is IOther {
    function id() public pure returns (uint) {
        return 1;
    }
}

contract Child is Base {
    function id() public pure returns (uint) {
        return 2;
    }
}

contract Other {
    function id() public pure returns (uint) {
        return 3;
    }
}

contract Test {
    function verify() public {
        Base b = new Base();
        Child c = new Child();
        Other o = new Other();
        assert(Base(b).id() == 1);
        assert(Child(b).id() == 1);
        assert(Other(b).id() == 1);
        assert(Base(c).id() == 2);
        assert(Child(c).id() == 2);
        assert(Other(c).id() == 2);
        assert(Base(o).id() == 3);
        assert(Child(o).id() == 3);
        assert(Other(o).id() == 3);
    }
}

contract __IRTest__ {
    function main() public {
        Test __this__ = new Test();
        __testCase165__(__this__);
    }

    function __testCase165__(Test __this__) internal {
        __this__.verify();
    }
}