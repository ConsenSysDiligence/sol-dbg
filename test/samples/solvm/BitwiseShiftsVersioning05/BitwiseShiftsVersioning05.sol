pragma solidity 0.5.5;

/// Returns `-2` for 0.4.x and `-3` since 0.5.x
contract Test {
    function main() public returns (int256) {
        int256 a = -5;
        return a >> 1;
    }
}

contract __IRTest__ {
    function main() public {
        Test __this__ = new Test();
        __testCase31__(__this__);
    }

    function __testCase31__(Test __this__) internal {
        int256 ret_31_0 = __this__.main();
        assert(ret_31_0 == int256(-3));
    }
}