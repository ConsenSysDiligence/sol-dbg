pragma solidity 0.5.17;

contract EmptyArgNames {
    function main(uint, uint) public returns (uint) {
        return 42;
    }
}

contract __IRTest__ {
    function main() public payable {
        EmptyArgNames __this__ = new EmptyArgNames();
        __testCase34__(__this__);
    }

    function __testCase34__(EmptyArgNames __this__) internal {
        uint256 ret_34_0 = __this__.main(uint256(1), uint256(2));
        assert(ret_34_0 == uint256(42));
    }
}
