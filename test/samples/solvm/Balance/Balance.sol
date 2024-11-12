pragma solidity 0.4.26;

contract BalanceFunc {
    function balance() public returns (uint) {
        return 42;
    }

    function getBalance() public returns (uint) {
        return this.balance();
    }
}

contract Balance {
    constructor() public payable {}

    function getBalance() public returns (uint) {
        return this.balance;
    }
}

contract __IRTest__ {
    function main() public {
        Balance __this__ = (new Balance).value(43)();
        __testCase52__(__this__);
        BalanceFunc __this1__ = new BalanceFunc();
        __testCase82__(__this__, __this1__);
    }

    function __testCase52__(Balance __this__) internal {
        uint256 ret_52_0 = __this__.getBalance();
        assert(ret_52_0 == uint256(43));
    }

    function __testCase82__(Balance __this__, BalanceFunc __this1__) internal {
        uint256 ret_82_0 = __this1__.getBalance();
        assert(ret_82_0 == uint256(42));
    }
}