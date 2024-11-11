pragma solidity 0.8.4;

contract Misc08 {
    function expAssoc(uint a, uint b, uint c) public returns (uint) {
        return a ** (b ** c);
    }
}

contract __IRTest__ {
    function main() public {
        Misc08 __this__ = new Misc08();
        __testCase43__(__this__);
    }

    function __testCase43__(Misc08 __this__) internal {
        uint256 ret_43_0 = __this__.expAssoc(uint256(2), uint256(3), uint256(2));
        assert(ret_43_0 == uint256(512));
    }
}