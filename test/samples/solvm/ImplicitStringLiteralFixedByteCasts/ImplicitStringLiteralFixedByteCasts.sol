pragma solidity 0.7.6;

contract ImplicitStringLiteralFixedByteCasts {
    function main() public {
        bytes memory b = new bytes(1);
        b[0] = bytes1(0x61);
        bytes2 b1 = hex"6162";
        bytes3 b2 = hex"610000";
        bytes3 b3 = hex"616200";
        assert(b[0] == "a");
        assert(b1 == "ab");
        assert(b2 == "a");
        assert(b3 == "ab");
    }
}

contract __IRTest__ {
    function main() public {
        ImplicitStringLiteralFixedByteCasts __this__ = new ImplicitStringLiteralFixedByteCasts();
        __testCase74__(__this__);
    }

    function __testCase74__(ImplicitStringLiteralFixedByteCasts __this__) internal {
        __this__.main();
    }
}