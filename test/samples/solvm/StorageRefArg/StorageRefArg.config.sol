pragma solidity 0.7.6;

contract StorageRefArg {
    uint[] internal a;
    uint[] internal b;

    function setFirst(uint[] storage t, uint v) internal {
        t = b;
        t[0] = v;
    }

    function main() public returns (uint[] memory, uint[] memory) {
        a = [1, 2, 3];
        b = [4, 5, 6];
        setFirst(a, 42);
        assert(a[0] == 1);
        assert(b[0] == 42);
        return (a, b);
    }
}

contract __IRTest__ {
    function main() public {
        StorageRefArg __this__ = new StorageRefArg();
        __testCase90__(__this__);
    }

    function __testCase90__(StorageRefArg __this__) internal {
        uint256[] memory arr_lit_4;
        arr_lit_4 = new uint256[](3);
        arr_lit_4[0] = uint256(1);
        arr_lit_4[1] = uint256(2);
        arr_lit_4[2] = uint256(3);
        uint256[] memory arr_lit_5;
        arr_lit_5 = new uint256[](3);
        arr_lit_5[0] = uint256(42);
        arr_lit_5[1] = uint256(5);
        arr_lit_5[2] = uint256(6);
        (uint256[] memory ret_90_0, uint256[] memory ret_90_1) = __this__.main();
        assert(keccak256(abi.encodePacked(ret_90_0)) == keccak256(abi.encodePacked(arr_lit_4)));
        assert(keccak256(abi.encodePacked(ret_90_1)) == keccak256(abi.encodePacked(arr_lit_5)));
    }
}