pragma solidity 0.7.6;

contract Test {
    function returnsTuple() internal pure returns (uint a, uint b) {
        return (1, 2);
    }

    function callsReturnsTuple() public pure returns (uint a, uint b) {
        return (((((returnsTuple())))));
    }

    function verify() public {
        (uint a1, uint b1) = returnsTuple();
        assert(a1 == 1);
        assert(b1 == 2);
        (uint a2, uint b2) = callsReturnsTuple();
        assert(a2 == 1);
        assert(b2 == 2);
    }
}

contract __IRTest__ {
    function main() public {
        Test __this__ = new Test();
        __testCase86__(__this__);
    }

    function __testCase86__(Test __this__) internal {
        __this__.verify();
    }
}