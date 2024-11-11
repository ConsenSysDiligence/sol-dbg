pragma solidity 0.8.17;

contract A {
    uint internal x;

    constructor(uint a) {
        x = a;
    }

    function inc() public returns (uint) {
        x = x + 1;
        return x;
    }
}

contract Main {
    function main() public returns (uint) {
        A a = new A(42);
        return a.inc();
    }
}

contract __IRTest__ {
    function main() public {
        Main __this__ = new Main();
        __testCase62__(__this__);
    }

    function __testCase62__(Main __this__) internal {
        uint256 ret_62_0 = __this__.main();
        assert(ret_62_0 == uint256(43));
    }
}