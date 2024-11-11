pragma solidity ^0.8.0;

contract Solidity08BuiltinsTest {
    function addrCodeAndCodeHash() public view returns (uint) {
        bytes memory code = address(this).code;
        bytes32 codeHash = address(this).codehash;
        bytes32 customHash = keccak256(code);
        assert(codeHash == customHash);
        return (code.length);
    }

    function blockChainId() public view returns (uint) {
        return block.chainid;
    }
}

contract __IRTest__ {
    function main() public {
        Solidity08BuiltinsTest __this__ = new Solidity08BuiltinsTest();
        __testCase69__(__this__);
    }

    function __testCase69__(Solidity08BuiltinsTest __this__) internal {
        (uint256 ret_69_0) = __this__.addrCodeAndCodeHash();
        assert(ret_69_0 == uint256(402));
    }
}
