pragma solidity 0.4.24;

contract HashingTest {
    enum SomeEnum { A, B, C }

    function usesKeccak256() public {
        bytes memory empty;
        assert(keccak256() == 0xc5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470);
        assert(keccak256(empty) == 0xc5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470);
        assert(keccak256(uint8(1)) == 0x5fe7f977e71dba2ea1a68e21057beebb9be2ac30c6410aa38d4f3fbe41dcffd2);
        assert(keccak256(1) == 0x5fe7f977e71dba2ea1a68e21057beebb9be2ac30c6410aa38d4f3fbe41dcffd2);
        assert(keccak256(int8(-1)) == 0x8b1a944cf13a9a1c08facb2c9e98623ef3254d2ddb48113885c3e8e97fec8db9);
        assert(keccak256(-1) == 0x8b1a944cf13a9a1c08facb2c9e98623ef3254d2ddb48113885c3e8e97fec8db9);
        assert(keccak256("1") == 0xc89efdaa54c0f20c7adf612882df0950f5a951637e0307cdcb4c672f298b8bc6);
        assert(keccak256("-1") == 0x798272c22de7de1bbb41d9d76b5240e67bb83e9ece1afeb940834536b3646693);
        assert(keccak256("Te$t123\nNewLine&*!") == 0x926fb17bc7d694f60ff0849cb95cb383ae82e18283d88724dc6068337e07374c);
        assert(keccak256(address(0xCA35b7d915458EF540aDe6068dFe2F44E8fa733c)) == 0xb1591967aed668a4b27645ff40c444892d91bf5951b382995d4d4f6ee3a2ce03);
        assert(keccak256([int8(1), 2, 3]) == 0x6e0c627900b24bd432fe7b1f713f1b0744091a646a9fe4a65a18dfed21f2949c);
        assert(keccak256(int8(-1), "xyz", 333) == 0x9518c311dfcf36cc7f4043498b08b8112edb130c117be14dff7f772a78a25232);
        assert(keccak256(SomeEnum.A) == 0xbc36789e7a1e281436464229828f817d6612f7b477d66591ff96a9e064bcc98a);
    }

    function usesSha3() public {
        bytes memory empty;
        assert(sha3() == 0xc5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470);
        assert(sha3(empty) == 0xc5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470);
        assert(sha3(uint8(1)) == 0x5fe7f977e71dba2ea1a68e21057beebb9be2ac30c6410aa38d4f3fbe41dcffd2);
        assert(sha3(1) == 0x5fe7f977e71dba2ea1a68e21057beebb9be2ac30c6410aa38d4f3fbe41dcffd2);
        assert(sha3(int8(-1)) == 0x8b1a944cf13a9a1c08facb2c9e98623ef3254d2ddb48113885c3e8e97fec8db9);
        assert(sha3(-1) == 0x8b1a944cf13a9a1c08facb2c9e98623ef3254d2ddb48113885c3e8e97fec8db9);
        assert(sha3("1") == 0xc89efdaa54c0f20c7adf612882df0950f5a951637e0307cdcb4c672f298b8bc6);
        assert(sha3("-1") == 0x798272c22de7de1bbb41d9d76b5240e67bb83e9ece1afeb940834536b3646693);
        assert(sha3("Te$t123\nNewLine&*!") == 0x926fb17bc7d694f60ff0849cb95cb383ae82e18283d88724dc6068337e07374c);
        assert(sha3(address(0xCA35b7d915458EF540aDe6068dFe2F44E8fa733c)) == 0xb1591967aed668a4b27645ff40c444892d91bf5951b382995d4d4f6ee3a2ce03);
        assert(sha3([int8(1), 2, 3]) == 0x6e0c627900b24bd432fe7b1f713f1b0744091a646a9fe4a65a18dfed21f2949c);
        assert(sha3(int8(-1), "xyz", 333) == 0x9518c311dfcf36cc7f4043498b08b8112edb130c117be14dff7f772a78a25232);
        assert(sha3(SomeEnum.A) == 0xbc36789e7a1e281436464229828f817d6612f7b477d66591ff96a9e064bcc98a);
    }
}

contract __IRTest__ {
    function main() public {
        HashingTest __this__ = new HashingTest();
        __testCase238__(__this__);
        __testCase252__(__this__);
    }

    function __testCase238__(HashingTest __this__) internal {
        __this__.usesKeccak256();
    }

    function __testCase252__(HashingTest __this__) internal {
        __this__.usesSha3();
    }
}