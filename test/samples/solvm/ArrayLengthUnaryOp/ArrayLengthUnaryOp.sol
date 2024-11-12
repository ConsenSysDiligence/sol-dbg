pragma solidity ^0.4.24;

contract ArrayLengthUnaryOp {
    uint[] internal arr1;
    uint[] internal arr2;
    uint[] internal arr3;
    uint[] internal arr4 = [3, 4, 5, 6, 7];
    uint[] internal arr5 = [3, 4, 5, 6, 7];
    uint[] internal arr6 = [3, 4, 5, 6, 7];

    function main() public {
        uint x = ((++arr1.length) + (++arr1.length)) + (++arr1.length);
        assert(x == 6);
        assert(arr1.length == 3);
        uint y = ((arr2.length++) + (arr2.length++)) + (arr2.length++);
        assert(y == 3);
        assert(arr2.length == 3);
        uint z = ((++arr3.length) + (++arr3.length)) + (arr3.length++);
        assert(z == 5);
        assert(arr3.length == 3);
        uint w = ((--arr4.length) + (--arr4.length)) + (--arr4.length);
        assert(w == 9);
        assert(arr4.length == 2);
        uint v = ((arr5.length--) + (arr5.length--)) + (arr5.length--);
        assert(v == 12);
        assert(arr5.length == 2);
        uint u = ((--arr6.length) + (--arr6.length)) + (arr6.length--);
        assert(u == 10);
        assert(arr6.length == 2);
    }
}

contract __IRTest__ {
    function main() public {
        ArrayLengthUnaryOp __this__ = new ArrayLengthUnaryOp();
        __testCase218__(__this__);
    }

    function __testCase218__(ArrayLengthUnaryOp __this__) internal {
        __this__.main();
    }
}