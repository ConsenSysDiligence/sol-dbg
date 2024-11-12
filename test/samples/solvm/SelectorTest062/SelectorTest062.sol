pragma solidity ^0.6.2;

library TestLibrary {
    function some(uint arg) external pure returns (uint ret) {
        return arg + 100;
    }
}

interface TestInterface {
    function someOther(string calldata arg) external pure returns (bytes memory);
}

contract SelectorTest {
    function verify() public {
        assert(TestLibrary.some.selector == 0x206e0cd6);
        assert(TestInterface.someOther.selector == 0xdda16b9e);
        assert(this.verify.selector == 0xfc735e99);
    }
}

contract __IRTest__ {
    function main() public {
        SelectorTest __this__ = new SelectorTest();
        __testCase65__(__this__);
    }

    function __testCase65__(SelectorTest __this__) internal {
        __this__.verify();
    }
}