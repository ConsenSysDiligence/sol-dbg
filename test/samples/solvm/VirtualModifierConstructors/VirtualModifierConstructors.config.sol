pragma solidity 0.7.6;

contract Base {
    uint public x = 0;

    modifier M() virtual {
        x += 1;
        _;
    }

    constructor() M() {
        x *= 5;
    }
}

contract Child is Base {
    modifier M() override {
        x += 2;
        _;
    }

    constructor() {
        x *= 3;
    }
}

contract Test {
    function main() public {
        Base b = new Base();
        Base bc = new Child();
        Child c = new Child();
        assert(b.x() == 5);
        assert(bc.x() == 30);
        assert(c.x() == 30);
    }
}

contract __IRTest__ {
    function main() public {
        Test __this__ = new Test();
        __testCase104__(__this__);
    }

    function __testCase104__(Test __this__) internal {
        __this__.main();
    }
}