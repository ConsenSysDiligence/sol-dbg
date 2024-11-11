pragma solidity ^0.5.4;

contract LengthTest {
    int[] internal i = [1, 2, 3];

    function lengthOnFixedBytes() public {
        require(byte(0x00).length == 1);
        require(bytes1(0x01).length == 1);
        require(bytes4("test").length == 4);
        int x = 1256;
        require(bytes32(x).length == 32);
        bytes32 y;
        require(y.length == 32);
    }

    function lengthOnRefArrays() public {
        require(i.length == 3);
        i.push(1);
        require(i.length == 4);
        i.pop();
        require(i.length == 3);
        uint8[6] memory u = [uint8(1), 2, 3, 3, 2, 1];
        require(u.length == 6);
        int[5] memory t;
        require(t.length == 5);
    }
}

contract __IRTest__ {
    function main() public {
        LengthTest __this__ = new LengthTest();
        __testCase148__(__this__);
        __testCase162__(__this__);
    }

    function __testCase148__(LengthTest __this__) internal {
        __this__.lengthOnFixedBytes();
    }

    function __testCase162__(LengthTest __this__) internal {
        __this__.lengthOnRefArrays();
    }
}