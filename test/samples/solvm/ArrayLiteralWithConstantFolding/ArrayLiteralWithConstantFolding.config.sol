pragma solidity 0.7.6;

contract Test {
    uint[3] public arr;

    function useCase1() public {
        arr = [2 ** uint256(2), 2 ** uint256(4), 2 ** uint256(8)];
        assert(arr[0] == 4);
        assert(arr[1] == 16);
        assert(arr[2] == 256);
    }

    function useCase2() public {
        uint32[3] memory a = [1, 2, 3 + uint32(0)];
        assert(a[0] == 1);
        assert(a[1] == 2);
        assert(a[2] == 3);
    }

    function useCase3() public {
        uint8[2] memory a = [uint8(0 - 1), uint8(255 + 1)];
        assert(a[0] == 255);
        assert(a[1] == 0);
    }

    function useCase4() public {
        assert(uint8(uint256(257)) == 1);
        assert(uint(uint8(257) + 1) == 2);
        assert(uint(~uint8(1)) == 254);
    }

    function verify() public {
        useCase1();
        useCase2();
        useCase3();
        useCase4();
    }
}

contract __IRTest__ {
    function main() public {
        Test __this__ = new Test();
        __testCase213__(__this__);
    }

    function __testCase213__(Test __this__) internal {
        __this__.verify();
    }
}