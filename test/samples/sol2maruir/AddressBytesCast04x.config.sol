pragma solidity 0.4.13;

contract AddressBytesCast04x {
    function main() public payable {
        bytes21 a = 0x01000000000000000000000000000000000000000f;
        address b = address(a);
        bytes21 c = bytes21(b);
        assert(b == address(0xf));
        assert(c == 0xf);
        a = 1461501637330902918203684832716283019655932542991;
        b = address(a);
        c = bytes21(b);
        assert(b == address(0xf));
        assert(c == 0xf);
    }
}

contract __IRTest__ {
    function main() public payable {
        AddressBytesCast04x __this__ = new AddressBytesCast04x();
        __testCase80__(__this__);
    }

    function __testCase80__(AddressBytesCast04x __this__) internal {
        __this__.main();
    }
}
