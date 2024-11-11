pragma solidity 0.8.0;

contract CodeSize {
    function f() public returns (uint) {
        return address(this).code.length;
    }
}

contract __IRTest__ {
    function main() public {
        CodeSize __this__ = new CodeSize();
        __testCase29__(__this__);
    }

    function __testCase29__(CodeSize __this__) internal {
        uint256 ret_29_0 = __this__.f();
        assert(ret_29_0 == uint256(4));
    }
}