pragma solidity ^0.6.0;

contract HexLiterals {
    bytes4 public constant signatureOfSmt = hex"7532eaac";
    bytes4 public signatureOther = hex"e9afa7a1";
    bytes2 internal twoBytes;

    constructor() public {
        twoBytes = hex"ff00";
    }

    function some(bytes memory x) public pure returns (bytes memory) {
        return x;
    }

    function main() public {
        assert(signatureOfSmt == 0x7532eaac);
        assert(this.signatureOfSmt() == 0x7532eaac);
        assert(signatureOther == 0xe9afa7a1);
        assert(this.signatureOther() == 0xe9afa7a1);
        assert(twoBytes == 0xff00);
        assert((bytes1(0x01) | "\u0002") == 0x03);
        assert((~bytes1("\u0001")) == 0xfe);
        bytes2 a = hex"aabb";
        assert(a == 0xaabb);
        bytes memory b = hex"11ff33bb";
        assert(b[0] == 0x11);
        assert(b[1] == 0xff);
        assert(b[2] == 0x33);
        assert(b[3] == 0xbb);
        string memory c = "\u0001\u0002\u0003";
        assert(bytes(c)[0] == 0x01);
        assert(bytes(c)[1] == 0x02);
        assert(bytes(c)[2] == 0x03);
        bytes memory d = some("\u0004\u0005");
        assert(d[0] == 0x04);
        assert(d[1] == 0x05);
    }
}

contract __IRTest__ {
    function main() public {
        HexLiterals __this__ = new HexLiterals();
        __testCase206__(__this__);
    }

    function __testCase206__(HexLiterals __this__) internal {
        __this__.main();
    }
}