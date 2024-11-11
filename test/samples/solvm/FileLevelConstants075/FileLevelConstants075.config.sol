pragma solidity ^0.7.5;
import "./FileLevelConstants075_lib.sol";
import "./FileLevelConstants075_lib.sol" as Lib;

uint constant SOME_CONST = 100;
uint constant SOME_OTHER = 15;
uint constant SOME_ELSE = SOME_CONST + SOME_OTHER;
uint constant C2 = SOME_ELSE + ANOTHER_CONST;
uint constant C3 = SOME_ELSE + Lib.ANOTHER_CONST;
string constant FOO = "abcd";
bytes constant BOO = bytes("abcd");
bytes1 constant MOO = BOO[0];

contract Test {
    uint internal constant STATE_CONST = 20;

    function verifyNumConsts() public {
        assert(SOME_CONST == 100);
        assert(SOME_OTHER == 15);
        assert(SOME_ELSE == 115);
        assert(STATE_CONST == 20);
        uint a = ((SOME_CONST + SOME_OTHER) + SOME_ELSE) + STATE_CONST;
        assert(a == 250);
        assert(MOO == 0x61);
        assert(C2 == 157);
        assert(C3 == 157);
    }

    function verifyByteConsts() public {
        assert(keccak256(bytes("abcd")) == keccak256(bytes(FOO)));
        assert(keccak256(BOO) == keccak256(bytes(FOO)));
    }

    function verify() public {
        verifyNumConsts();
        verifyByteConsts();
    }
}

contract __IRTest__ {
    function main() public {
        Test __this__ = new Test();
        __testCase126__(__this__);
    }

    function __testCase126__(Test __this__) internal {
        __this__.verify();
    }
}
