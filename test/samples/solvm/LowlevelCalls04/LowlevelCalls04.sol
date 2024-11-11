pragma solidity 0.4.26;

contract NoFB {}

contract FBNoArgs {
    int8 public lastCall = 0;

    function () external payable {
        lastCall = 1;
    }

    function id(uint x) public returns (uint) {
        lastCall = 2;
        return x;
    }
}

contract AcceptFalback {
    function () external payable {}
}

contract RejectFallback {
    function () external payable {
        assert(false);
    }
}

contract RejectNoFuns {}

contract AcceptFun {
    function id(uint x) public payable returns (uint) {
        return x;
    }
}

contract RejectFun {
    function id(uint x) public payable returns (uint) {
        assert(false);
    }
}

contract Throws {
    function throws(bool shouldFail) public {
        if (shouldFail) {
            revert("foo");
        }
    }

    function double(uint x) public returns (uint) {
        return x + x;
    }
}

contract Test {
    uint public x;

    function inc() external returns (uint) {
        x++;
        return x;
    }

    function incAndThrow() external {
        x++;
        revert("foo");
    }

    constructor() public payable {}

    function getIdData(uint x) internal returns (bytes memory) {
        return abi.encodeWithSignature("id(uint256)", x);
    }

    function transfer(address a) public {
        a.transfer(1);
    }

    function noContractTests() public returns (bool) {
        address a = address(0x0000000000000000000000111111111111111111);
        bool res0 = a.call.gas(23000)("");
        assert(res0);
        bool res1 = a.call.gas(23000)(getIdData(1));
        assert(res1);
    }

    function fallbackTests() public returns (bool, int8) {
        FBNoArgs c = new FBNoArgs();
        address a = address(c);
        bool res0 = a.call.gas(23000)(getIdData(42));
        assert(res0 && (c.lastCall() == 2));
        bool res1 = a.call.gas(23000)("");
        assert(res1 && (c.lastCall() == 1));
        bool res2 = a.call.gas(23000)("\n\u000b");
        assert(res2 && (c.lastCall() == 1));
        bool res3 = a.call.gas(23000)("\n\u000b\f\r\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000");
        assert(res3 && (c.lastCall() == 1));
    }

    function noFallbackTest() public {
        NoFB c = new NoFB();
        address a = address(c);
        bool res0 = a.call.gas(2300)(getIdData(42));
        assert(!res0);
        bool res1 = a.call.gas(2300)("");
        assert(!res1);
        bool res2 = a.call.gas(2300)("\n\u000b");
        assert(!res2);
    }

    function callTests(Test other) public {
        address a = address(other);
        bytes memory msgData = abi.encodeWithSignature("inc()");
        uint oldX = x;
        uint otherOldX = other.x();
        bool res = a.call(msgData);
        assert(res);
        assert(x == oldX);
        assert((otherOldX + 1) == other.x());
        AcceptFun af = new AcceptFun();
        address afAddr = address(af);
        bytes memory msgData2 = abi.encodeWithSignature("id(uint256)", 42);
        uint oldBal = address(this).balance;
        uint oldAfBal = afAddr.balance;
        bool res2 = afAddr.call.value(1)(msgData2);
        assert(res2);
        assert(oldBal == (address(this).balance + 1));
        assert(oldAfBal == (afAddr.balance - 1));
        RejectFun rf = new RejectFun();
        address rfAddr = (address(rf));
        oldBal = address(this).balance;
        uint oldRfBal = rfAddr.balance;
        bool res3 = rfAddr.call.value(1)(msgData2);
        assert(!res3);
        assert(oldBal == address(this).balance);
        assert(oldRfBal == rfAddr.balance);
    }

    function exceptionBytesTests() public {
        Throws c = new Throws();
        address a = address(c);
        bytes memory successMsgData = abi.encodeWithSignature("throws(bool)", false);
        bool res0 = a.call.gas(2300)(successMsgData);
        assert(res0);
        bytes memory failMsgData = abi.encodeWithSignature("throws(bool)", true);
        bool res1 = a.call.gas(2300)(failMsgData);
        assert(!res1);
    }

    function sendTests() public {
        AcceptFalback af = new AcceptFalback();
        address afAddr = (address(af));
        uint oldBalance = afAddr.balance;
        assert(afAddr.send(1));
        assert((oldBalance + 1) == afAddr.balance);
        RejectFallback rf = new RejectFallback();
        address rfAddr = (address(rf));
        oldBalance = rfAddr.balance;
        assert(!rfAddr.send(1));
        assert(oldBalance == rfAddr.balance);
        RejectNoFuns rnf = new RejectNoFuns();
        address rnfAddr = (address(rnf));
        oldBalance = rnfAddr.balance;
        assert(!rnfAddr.send(1));
        assert(oldBalance == rnfAddr.balance);
    }

    function transferTests() public returns (uint res) {
        AcceptFalback af = new AcceptFalback();
        address afAddr = (address(af));
        uint oldBalance = afAddr.balance;
        afAddr.transfer(1);
        assert((oldBalance + 1) == afAddr.balance);
    }

    function main() public {
        noContractTests();
        noFallbackTest();
        fallbackTests();
        exceptionBytesTests();
        sendTests();
        transferTests();
    }
}

contract __IRTest__ {
    function main() public {
        Test __this__ = (new Test).value(20)();
        Test __this1__ = (new Test).value(20)();
        RejectFallback __rf__ = new RejectFallback();
        RejectNoFuns __rnf__ = new RejectNoFuns();
        __testCase796__(__this__, __this1__, __rf__, __rnf__);
        __testCase822__(__this__, __this1__, __rf__, __rnf__);
        __testCase848__(__this__, __this1__, __rf__, __rnf__);
    }

    function __testCase796__(Test __this__, Test __this1__, RejectFallback __rf__, RejectNoFuns __rnf__) internal {
        __this__.main();
    }

    function __testCase822__(Test __this__, Test __this1__, RejectFallback __rf__, RejectNoFuns __rnf__) internal {
        __this__.transfer(address(0x0000000000000000000000000000000000000101));
    }

    function __testCase848__(Test __this__, Test __this1__, RejectFallback __rf__, RejectNoFuns __rnf__) internal {
        __this__.transfer(address(0x0000000000000000000000000000000000000102));
    }
}