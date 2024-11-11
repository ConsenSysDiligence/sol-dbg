pragma solidity 0.7.6;

contract Calldata {
    function stringArgCopy(string calldata s) external returns (string memory) {
        string memory mS = s;
        return mS;
    }

    function byteArg(byte[] calldata s) external returns (byte) {
        return s[0];
    }

    function byteArgCopy(byte[] calldata s) external returns (byte) {
        byte[] memory mS = s;
        mS[0] = 0x42;
        return mS[0];
    }
}

contract __IRTest__ {
    function main() public {
        Calldata __this__ = new Calldata();
        __testCase70__(__this__);
        __testCase146__(__this__);
        __testCase216__(__this__);
    }

    function __testCase70__(Calldata __this__) internal {
        string memory ret_70_0 = __this__.stringArgCopy("abcd");
        assert(keccak256(abi.encodePacked(ret_70_0)) == keccak256(abi.encodePacked("abcd")));
    }

    function __testCase146__(Calldata __this__) internal {
        bytes1[] memory arr_lit_0;
        arr_lit_0 = new bytes1[](3);
        arr_lit_0[0] = bytes1(uint8(0x2b));
        arr_lit_0[1] = bytes1(uint8(0x2));
        arr_lit_0[2] = bytes1(uint8(0x3));
        bytes1 ret_146_0 = __this__.byteArg(arr_lit_0);
        assert(ret_146_0 == bytes1(uint8(0x2b)));
    }

    function __testCase216__(Calldata __this__) internal {
        bytes1[] memory arr_lit_1;
        arr_lit_1 = new bytes1[](3);
        arr_lit_1[0] = bytes1(uint8(0x2b));
        arr_lit_1[1] = bytes1(uint8(0x2));
        arr_lit_1[2] = bytes1(uint8(0x3));
        bytes1 ret_216_0 = __this__.byteArgCopy(arr_lit_1);
        assert(ret_216_0 == bytes1(uint8(0x42)));
    }
}