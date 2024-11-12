pragma solidity ^0.6.9;
pragma experimental ABIEncoderV2;
pragma experimental SMTChecker;

contract Test {
    /// Stored values
    uint[] public values;

    function addValues(uint[] calldata row) public returns (uint[] calldata) {
        uint256[] calldata local = row;
        return pushToValues(local);
    }

    function pushToValues(uint[] calldata row) private returns (uint[] calldata) {
        for (uint i = 0; i < row.length; i++) {
            values.push(row[i]);
        }
        return row;
    }
}

contract __IRTest__ {
    function main() public {
        Test __this__ = new Test();
        __testCase113__(__this__);
        __testCase187__(__this__);
        __testCase215__(__this__);
        __testCase243__(__this__);
    }

    function __testCase113__(Test __this__) internal {
        uint256[] memory arr_lit_2;
        arr_lit_2 = new uint256[](3);
        arr_lit_2[0] = uint256(1);
        arr_lit_2[1] = uint256(2);
        arr_lit_2[2] = uint256(3);
        uint256[] memory arr_lit_3;
        arr_lit_3 = new uint256[](3);
        arr_lit_3[0] = uint256(1);
        arr_lit_3[1] = uint256(2);
        arr_lit_3[2] = uint256(3);
        uint256[] memory ret_113_0 = __this__.addValues(arr_lit_2);
        assert(keccak256(abi.encodePacked(ret_113_0)) == keccak256(abi.encodePacked(arr_lit_3)));
    }

    function __testCase187__(Test __this__) internal {
        uint256 ret_187_0 = __this__.values(uint256(0));
        assert(ret_187_0 == uint256(1));
    }

    function __testCase215__(Test __this__) internal {
        uint256 ret_215_0 = __this__.values(uint256(1));
        assert(ret_215_0 == uint256(2));
    }

    function __testCase243__(Test __this__) internal {
        uint256 ret_243_0 = __this__.values(uint256(2));
        assert(ret_243_0 == uint256(3));
    }
}