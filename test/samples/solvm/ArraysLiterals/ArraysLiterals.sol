pragma solidity 0.8.19;

contract ArrayLiterals {
    uint[] internal arr;
    uint8[] internal arr1;

    function foo() public returns (uint) {
        uint[3] memory t = [uint(1), 2, 3];
        uint x = [4, 5, 6][1];
        arr = [7, 8, 20000];
        arr1 = [9, 2, 11];
        return ((t[0] + x) + arr[2]) + arr1[1];
    }
}

contract __IRTest__ {
    function main() public {
        ArrayLiterals __this__ = new ArrayLiterals();
        __testCase79__(__this__);
    }

    function __testCase79__(ArrayLiterals __this__) internal {
        uint256 ret_79_0 = __this__.foo();
        assert(ret_79_0 == uint256(20008));
    }
}