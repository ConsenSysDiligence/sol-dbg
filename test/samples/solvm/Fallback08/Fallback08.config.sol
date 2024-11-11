pragma solidity ^0.8.0;

contract Fallback08 {
    fallback(bytes calldata input) external returns (bytes memory) {
        assert(keccak256(input) == keccak256(msg.data));
        return input;
    }

    function main() public {
        bytes memory data = hex"000102030405";
        (bool success, bytes memory ret) = address(this).call(data);
        assert(success);
        assert(keccak256(ret) == keccak256(data));
    }
}

contract __IRTest__ {
    function main() public {
        Fallback08 __this__ = new Fallback08();
        __testCase71__(__this__);
    }

    function __testCase71__(Fallback08 __this__) internal {
        __this__.main();
    }
}