pragma solidity ^0.4.24;

contract ConstantFolding {
    function main() public {
        uint8 a = 258 - 10;
        assert(a == 248);
        int8 b = (10 * 2) - 22;
        assert(b == (-2));
        uint8 u8_2 = 231584178474632390847141970017375815706539969331281128078915168015826259279915231584178474632390847141970017375815706539969331281128078915168015826259279915 - 231584178474632390847141970017375815706539969331281128078915168015826259279915231584178474632390847141970017375815706539969331281128078915168015826259279873;
        assert(u8_2 == 42);
    }
}

contract __IRTest__ {
    function main() public {
        ConstantFolding __this__ = new ConstantFolding();
        __testCase59__(__this__);
    }

    function __testCase59__(ConstantFolding __this__) internal {
        __this__.main();
    }
}