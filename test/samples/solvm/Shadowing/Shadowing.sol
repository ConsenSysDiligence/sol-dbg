pragma solidity ^0.4.24;

import "./contract_v04.sol";

contract Shadowing {
    uint private x = 1;
    uint internal y = 1;

    function dummy(OwnedToken o) private {}

    function shadow(uint msg) public returns (uint) {
        uint x = msg;
        uint OwnedToken = 1337;
        if (msg > 5) {
            x = 2;
            if ((x + msg) > 6) {
                return x + msg;
            }
        }
        return x;
    }

    function shadowReturn1() public returns (uint a) {
        uint x = 1;
    }

    function shadowReturn2() public returns (uint x) {
        x = 2;
    }

    function shadowReturn2Harness() public {
        shadowReturn2();
        assert(x == 1);
    }

    function shadowReturn2Harness2() public {
        assert(2 == shadowReturn2());
        assert(x == 1);
    }
}

contract __IRTest__ {
    function main() public {
        Shadowing __this__ = new Shadowing();
        __testCase237__(__this__);
        __testCase262__(__this__);
        __testCase287__(__this__);
        __testCase301__(__this__);
    }

    function __testCase237__(Shadowing __this__) internal {
        uint256 ret_237_0 = __this__.shadow(uint256(6));
        assert(ret_237_0 == uint256(8));
    }

    function __testCase262__(Shadowing __this__) internal {
        uint256 ret_262_0 = __this__.shadowReturn1();
        assert(ret_262_0 == uint256(0));
    }

    function __testCase287__(Shadowing __this__) internal {
        __this__.shadowReturn2Harness();
    }

    function __testCase301__(Shadowing __this__) internal {
        __this__.shadowReturn2Harness2();
    }
}