pragma solidity 0.8.4;

contract CantReceiveOnCreate {}

contract CanReceiveOnCreate {
    constructor() payable {}
}

contract ReceiveThroughMethod {
    function m() external payable {}
}

contract Test {
    constructor() payable {}

    function main() public {
        uint myBal = address(this).balance;
        CanReceiveOnCreate crc = (new CanReceiveOnCreate){value: 1}();
        assert((address(crc).balance == 1) && ((myBal - 1) == address(this).balance));
        myBal = address(this).balance;
        ReceiveThroughMethod rtm = new ReceiveThroughMethod();
        assert((address(rtm).balance == 0) && (myBal == address(this).balance));
        myBal = address(this).balance;
        rtm.m{value: 1}();
        assert((address(rtm).balance == 1) && ((myBal - 1) == address(this).balance));
    }
}

contract __IRTest__ {
    function main() public {
        Test __this__ = new Test{value: 10}();
        __testCase145__(__this__);
    }

    function __testCase145__(Test __this__) internal {
        __this__.main();
    }
}