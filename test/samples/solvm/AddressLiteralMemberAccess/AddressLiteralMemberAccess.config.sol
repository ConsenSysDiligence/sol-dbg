pragma solidity 0.7.6;

contract AddressLiteralMemberAccess {
    constructor() payable {}

    function noAddrBalance() public {
        uint b = 0xDeaDbeefdEAdbeefdEadbEEFdeadbeEFdEaDbeeF.balance;
        assert(b == 0);
    }

    function noAddrSend() public {
        bool sendSuccess = 0xDeaDbeefdEAdbeefdEadbEEFdeadbeEFdEaDbeeF.send(1 wei);
        assert(sendSuccess);
    }

    function noAddrCall() public {
        (bool callSuccess, bytes memory callResult) = 0xDeaDbeefdEAdbeefdEadbEEFdeadbeEFdEaDbeeF.call("");
        assert(callSuccess);
    }

    function noAddrStaticCall() public {
        (bool sCallSuccess, bytes memory sCallResult) = 0xDeaDbeefdEAdbeefdEadbEEFdeadbeEFdEaDbeeF.staticcall("");
        assert(sCallSuccess);
    }

    function noAddrTransfer() public {
        0xDeaDbeefdEAdbeefdEadbEEFdeadbeEFdEaDbeeF.transfer(1 wei);
    }

    function verify() public {
        noAddrBalance();
        noAddrSend();
        noAddrCall();
        noAddrStaticCall();
        noAddrTransfer();
    }
}

contract __IRTest__ {
    function main() public {
        AddressLiteralMemberAccess __this__ = new AddressLiteralMemberAccess();
        __testCase112__(__this__);
    }

    function __testCase112__(AddressLiteralMemberAccess __this__) internal {
        __this__.verify();
    }
}