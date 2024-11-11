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

contract __IRTest__ {
    function main() public {
        A __this__ = new A(uint256(1));
        __testCase45__(__this__);
    }

    function __testCase45__(A __this__) internal {
        uint256 ret_45_0 = __this__.inc();
        assert(ret_45_0 == uint256(2));
    }
}