pragma solidity 0.7.6;

contract ImplicitMemoryStorageCasts {
    string internal s = "abc";
    bytes internal b = hex"2a2b";
    uint256[] internal a = [1, 2, 3];

    function main() public {
        string memory mS = s;
        assert(keccak256(abi.encode(mS)) == keccak256(abi.encode("abc")));
        bytes memory mB = b;
        assert(mB.length == 2);
        assert(mB[0] == bytes1(0x2a));
        assert(mB[1] == bytes1(0x2b));
        uint256[] memory mA;
        assert(mA.length == 0);
        mA = a;
        assert(mA.length == 3);
        assert(mA[0] == 1);
        assert(mA[1] == 2);
        assert(mA[2] == 3);
        mB[0] = bytes1(0x2c);
        mA[0] = 5;
        s = mS;
        b = mB;
        a = mA;
        assert(keccak256(abi.encode(s)) == keccak256(abi.encode("abc")));
        assert(b.length == 2);
        assert(b[0] == bytes1(0x2c));
        assert(b[1] == bytes1(0x2b));
        assert(a.length == 3);
        assert(a[0] == 5);
        assert(a[1] == 2);
        assert(a[2] == 3);
        b[0] = bytes1(0x2d);
        assert(mB[0] == bytes1(0x2c));
    }
}

contract __IRTest__ {
    function main() public {
        ImplicitMemoryStorageCasts __this__ = new ImplicitMemoryStorageCasts();
        __testCase257__(__this__);
    }

    function __testCase257__(ImplicitMemoryStorageCasts __this__) internal {
        __this__.main();
    }
}