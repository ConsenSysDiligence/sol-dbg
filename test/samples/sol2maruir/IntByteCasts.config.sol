pragma solidity 0.4.13;

contract IntByteCasts {
    function main() public payable {
        bytes16 a = 0x0102;
        bytes32 b = 0x01020304;
        uint8 c = uint8(a);
        assert(c == 0x02);
        int16 d = int16(b);
        assert(d == 0x0304);
        uint32 e = uint32(a);
        assert(e == 0x0102);
        int32 f = int32(a);
        assert(f == 0x0102);

        bytes16 g = 0xffff;
        int16 h = int16(g);
        assert(h == -1);

        int32 i = 0x010203ff;
        bytes1 j = bytes1(i);
        assert(j == 0xff);

        int8 k = -1;
        bytes2 l = bytes2(k);
        assert(l == 0xffff);

        uint16 m = uint16(g);
        assert(m == 0xffff);
    }
}

contract __IRTest__ {
    function main() public payable {
        IntByteCasts __this__ = new IntByteCasts();
        __testCase40__(__this__);
    }

    function __testCase40__(IntByteCasts __this__) internal {
        __this__.main();
    }
}
