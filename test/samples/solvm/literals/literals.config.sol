pragma solidity ^0.5.0;

contract Literals {
    function literals() public {
        uint[3] memory array = [uint(1), 2, 3];
        assert(array[0] == 1);
        assert(array[1] == 2);
        assert(array[2] == 3);
        string memory someText = "test";
        bytes memory b = bytes(someText);
        assert(b[0] == byte("t")[0]);
        assert(b[1] == byte("e")[0]);
        assert(b[2] == byte("s")[0]);
        assert(b[3] == byte("t")[0]);
        uint x = 1;
        assert(x == 1);
        address lol = 0xCf5609B003B2776699eEA1233F7C82D5695cC9AA;
        assert(lol == 0xCf5609B003B2776699eEA1233F7C82D5695cC9AA);
        bytes4 y = 0x01020F0C;
        assert(y[0] == 0x01);
        assert(y[1] == 0x02);
        assert(y[2] == 0x0F);
        assert(y[3] == 0x0C);
    }

    function literalsWithSeparators() public {
        uint256 a = 2_000_000;
        assert(a == 2000000);
        int128 b = -1_000_000;
        assert(b == (-1000000));
        uint256 c = 0x00_ff_cc;
        assert(c == 0x00ffcc);
        uint256 d = 1_2e1_2;
        assert(d == 12e12);
        address e = 0x74_4E_60_db_92_7F_62_bd_98_53_Fb_bA_61_02_9f_77_0C_17_9E_56;
        assert(e == 0x744E60db927F62bd9853FbbA61029f770C179E56);
    }
}

contract __IRTest__ {
    function main() public {
        Literals __this__ = new Literals();
        __testCase226__(__this__);
        __testCase240__(__this__);
    }

    function __testCase226__(Literals __this__) internal {
        __this__.literals();
    }

    function __testCase240__(Literals __this__) internal {
        __this__.literalsWithSeparators();
    }
}