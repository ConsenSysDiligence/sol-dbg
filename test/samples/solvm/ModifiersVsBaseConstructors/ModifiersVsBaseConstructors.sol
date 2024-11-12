pragma solidity 0.7.6;

contract Base {
    uint public x = 0;

    modifier M() {
        x *= 2;
        _;
    }

    constructor() {
        x = 1;
    }
}

contract Child is Base {
    constructor() M() {}
}

contract Test {
    function main() public {
        Base b = new Child();
        assert(b.x() == 2);
    }
}

contract __IRTest__ {
    function main() public {
        Test __this__ = new Test();
        __testCase63__(__this__);
    }

    function __testCase63__(Test __this__) internal {
        __this__.main();
    }
}