pragma solidity ^0.4.25;

contract Casts {
    function castToChar(byte b) public pure returns (byte c) {
        if (b < 10) return byte(uint8(b) + 0x30); else return byte(uint8(b) + 0x57);
    }

    function castToString(address a) public pure returns (string memory) {
        bytes memory str = new bytes(40);
        for (uint i = 0; i < 20; i++) {
            byte strb = byte(uint8(uint(a) / (2 ** (8 * (19 - i)))));
            byte hi = byte(uint8(strb) / 16);
            byte lo = byte(uint8(strb) - (16 * uint8(hi)));
            str[2 * i] = castToChar(hi);
            str[(2 * i) + 1] = castToChar(lo);
        }
        return string(str);
    }

    function castToUpper(string memory str) public pure returns (string memory) {
        bytes memory bStr = bytes(str);
        bytes memory bUpper = new bytes(bStr.length);
        for (uint i = 0; i < bStr.length; i++) {
            if ((bStr[i] >= 97) && (bStr[i] <= 122)) {
                bUpper[i] = byte(int(bStr[i]) - 32);
            } else {
                bUpper[i] = bStr[i];
            }
        }
        return string(bUpper);
    }

    function castToUint(string memory self) public view returns (uint result) {
        bytes memory b = bytes(self);
        uint i;
        result = 0;
        for (i = 0; i < b.length; i++) {
            uint c = uint(b[i]);
            if ((c >= 48) && (c <= 57)) {
                result = (result * 10) + (c - 48);
            }
        }
    }
}

contract __IRTest__ {
    function main() public {
        Casts __this__ = new Casts();
        __testCase286__(__this__);
        __testCase318__(__this__);
        __testCase348__(__this__);
        __testCase382__(__this__);
        __testCase416__(__this__);
    }

    function __testCase286__(Casts __this__) internal {
        bytes1 ret_286_0 = __this__.castToChar(bytes1(uint8(0x25)));
        assert(ret_286_0 == bytes1(uint8(0x7c)));
    }

    function __testCase318__(Casts __this__) internal {
        bytes1 ret_318_0 = __this__.castToChar(bytes1(uint8(0x5)));
        assert(ret_318_0 == bytes1(uint8(0x35)));
    }

    function __testCase348__(Casts __this__) internal {
        string memory ret_348_0 = __this__.castToString(address(0x14723a09acff6d2a60dcdf7aa4aff308fddc160c));
        assert(keccak256(abi.encodePacked(ret_348_0)) == keccak256(abi.encodePacked("14723a09acff6d2a60dcdf7aa4aff308fddc160c")));
    }

    function __testCase382__(Casts __this__) internal {
        string memory ret_382_0 = __this__.castToUpper("ab1c2y3xyz");
        assert(keccak256(abi.encodePacked(ret_382_0)) == keccak256(abi.encodePacked("AB1C2Y3XYZ")));
    }

    function __testCase416__(Casts __this__) internal {
        uint256 ret_416_0 = __this__.castToUint("te1st123xy3z");
        assert(ret_416_0 == uint256(11233));
    }
}