pragma solidity ^0.4.24;

import "./library_v04.sol";

contract LibraryUsage {
    using SafeMath for uint256;

    function libraryUsing(uint a) public returns (uint) {
        uint x = a.mul(2);
        return (x.div(2));
    }

    function libraryCall(uint a) public returns (uint) {
        uint x = SafeMath.mul(a, 2);
        return (SafeMath.div(x, 2));
    }
}

contract __IRTest__ {
    function main() public {
        LibraryUsage __this__ = new LibraryUsage();
        __testCase195__(__this__);
        __testCase223__(__this__);
    }

    function __testCase195__(LibraryUsage __this__) internal {
        uint256 ret_195_0 = __this__.libraryUsing(uint256(10));
        assert(ret_195_0 == uint256(10));
    }

    function __testCase223__(LibraryUsage __this__) internal {
        uint256 ret_223_0 = __this__.libraryCall(uint256(10));
        assert(ret_223_0 == uint256(10));
    }
}