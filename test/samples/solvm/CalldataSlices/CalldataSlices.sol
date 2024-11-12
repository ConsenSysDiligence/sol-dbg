pragma solidity ^0.6.0;

contract CalldataSlices {
    function first(uint x, uint y) public returns (uint) {
        return abi.decode(msg.data[4:36], (uint));
    }

    function second(uint x, uint y) public returns (uint) {
        return abi.decode(msg.data[36:], (uint));
    }
}

contract __IRTest__ {
    function main() public {
        CalldataSlices __this__ = new CalldataSlices();
        __testCase65__(__this__);
        __testCase96__(__this__);
    }

    function __testCase65__(CalldataSlices __this__) internal {
        uint256 ret_65_0 = __this__.first(uint256(42), uint256(13));
        assert(ret_65_0 == uint256(42));
    }

    function __testCase96__(CalldataSlices __this__) internal {
        uint256 ret_96_0 = __this__.second(uint256(42), uint256(13));
        assert(ret_96_0 == uint256(13));
    }
}