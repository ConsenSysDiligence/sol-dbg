pragma solidity 0.7.6;

library Some {
    function add(uint a, uint b) internal returns (uint) {
        return a + b;
    }

    function add(uint a, uint b, uint c) internal returns (uint) {
        return (a + b) + c;
    }
}

contract Test {
    function verify() public {
        assert(Some.add(1, 2) == 3);
        assert(Some.add(1, 2, 3) == 6);
    }
}

contract __IRTest__ {
    function main() public {
        Test __this__ = new Test();
        __testCase74__(__this__);
    }

    function __testCase74__(Test __this__) internal {
        __this__.verify();
    }
}