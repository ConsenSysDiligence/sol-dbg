pragma solidity ^0.4.13;

contract IntByteCasts {
    function main() public {
        bytes16 a = 0;
        bytes32 b = 10;
        uint8 c = uint8(a);
        int16 d = int16(b);
    }
}

contract __IRTest__ {
    function main() public {
        IntByteCasts __this__ = new IntByteCasts();
        __testCase40__(__this__);
    }

    function __testCase40__(IntByteCasts __this__) internal {
        __this__.main();
    }
}