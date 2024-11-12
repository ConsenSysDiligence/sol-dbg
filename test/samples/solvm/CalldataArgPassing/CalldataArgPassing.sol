pragma solidity 0.7.6;

contract CalldataArgPassing {
    function callee(string calldata s) external {
        assert(keccak256(abi.encodePacked(s)) == keccak256(abi.encodePacked("abcd")));
    }

    function main() public {
        string memory arg = "abcd";
        this.callee(arg);
    }
}

contract __IRTest__ {
    function main() public {
        CalldataArgPassing __this__ = new CalldataArgPassing();
        __testCase52__(__this__);
    }

    function __testCase52__(CalldataArgPassing __this__) internal {
        __this__.main();
    }
}