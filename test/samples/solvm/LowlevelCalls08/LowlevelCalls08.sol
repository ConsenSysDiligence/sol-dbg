pragma solidity 0.8.4;

contract RecvOverFB {
    receive() external payable {}

    fallback() external {
        assert(false);
    }
}

contract NoFB {}

contract FBNoArgs {
    fallback() external payable {}

    function id(uint x) public returns (uint) {
        return x;
    }
}

contract FBArgs {
    fallback(bytes calldata msg) external payable returns (bytes memory) {
        return msg;
    }

    function id(uint x) public returns (uint) {
        return x;
    }
}

contract AcceptRecv {
    receive() external payable {}

    fallback() external payable {
        assert(false);
    }
}

contract RejectRecv {
    receive() external payable {
        assert(false);
    }

    fallback() external payable {}
}

contract AcceptFalback {
    fallback() external payable {}
}

contract RejectFallback {
    fallback() external payable {
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

    constructor() payable {}

    function getIdData(uint x) internal returns (bytes memory) {
        return abi.encodeWithSignature("id(uint256)", x);
    }

    function transfer(address payable a) public {
        a.transfer(1);
    }

    function noContractTests() public {
        address payable a = payable(0x0000000100030004000100000000000000000001);
        (bool res0, bytes memory resBytes0) = a.call{gas: 2300}("");
        assert(res0 && (resBytes0.length == 0));
        (bool res1, bytes memory resBytes1) = a.call{gas: 2300}(getIdData(1));
        assert(res1 && (resBytes1.length == 0));
        (bool res2, bytes memory resBytes2) = a.staticcall{gas: 2300}(getIdData(1));
        assert(res2 && (resBytes2.length == 0));
        bool res4 = a.send(0);
        assert(res4);
        try this.transfer(a) {} catch Error(string memory s) {
            assert(false);
        } catch Panic(uint x) {
            assert(false);
        } catch {
            assert(false);
        }
    }

    function fallbackTests() public {
        FBNoArgs c = new FBNoArgs();
        address a = address(c);
        (bool res0, bytes memory resBytes0) = a.call{gas: 2300}(getIdData(42));
        assert(res0);
        uint retV = abi.decode(resBytes0, (uint));
        assert(retV == 42);
        (bool res1, bytes memory resBytes1) = a.call{gas: 2300}(hex"");
        assert(res1 && (resBytes1.length == 0));
        (bool res2, bytes memory resBytes2) = a.call{gas: 2300}(hex"0a0b");
        assert(res2 && (resBytes2.length == 0));
        (bool res3, bytes memory resBytes3) = a.call{gas: 2300}(hex"0a0b0c0d000000000000000000000000000000000000000000000000000000000000000000");
        assert(res3 && (resBytes3.length == 0));
        (bool res4, bytes memory resBytes4) = a.call{gas: 2300}(hex"7d3c40c8");
        assert((!res4) && (resBytes4.length == 0));
        (bool res5, bytes memory resBytes5) = a.call{gas: 2300}(hex"7d3c40c8deadbeef");
        assert((!res5) && (resBytes5.length == 0));
        FBArgs c1 = new FBArgs();
        address a1 = address(c1);
        (bool res6, bytes memory resBytes6) = a1.call{gas: 2300}(hex"");
        assert(res6 && (resBytes6.length == 0));
        (bool res7, bytes memory resBytes7) = a1.call{gas: 2300}(hex"0a0b");
        assert(res7 && (keccak256(resBytes7) == keccak256(hex"0a0b")));
    }

    function noFallbackTest() public {
        NoFB c = new NoFB();
        address a = address(c);
        (bool res0, bytes memory resBytes0) = a.call{gas: 2300}(getIdData(42));
        assert((!res0) && (resBytes0.length == 0));
        (bool res1, bytes memory resBytes1) = a.call{gas: 2300}(hex"");
        assert((!res1) && (resBytes1.length == 0));
        (bool res2, bytes memory resBytes2) = a.call{gas: 2300}(hex"0a0b");
        assert((!res2) && (resBytes2.length == 0));
    }

    function receiveTests() public {
        RecvOverFB c = new RecvOverFB();
        address a = address(c);
        address payable ap = payable(a);
        (bool res0, bytes memory resBytes0) = ap.call{gas: 2300}(getIdData(42));
        assert((!res0) && (resBytes0.length == 36));
        uint oldBalance = address(this).balance;
        (bool res1, bytes memory resBytes1) = ap.call{gas: 2300, value: 1}(hex"");
        uint newBalance = address(this).balance;
        assert(res1 && (resBytes1.length == 0));
        assert(newBalance == (oldBalance - 1));
    }

    function callTests(Test other) public {
        assert(address(other) != address(this));
        address a = address(other);
        bytes memory msgData = abi.encodeWithSignature("inc()");
        uint oldX = x;
        uint otherOldX = other.x();
        (bool res, bytes memory ret) = a.call(msgData);
        assert(res && (ret.length > 0));
        uint retX = abi.decode(ret, (uint));
        assert(retX == (otherOldX + 1));
        assert(x == oldX);
        assert((otherOldX + 1) == other.x());
        AcceptFun af = new AcceptFun();
        address payable afAddr = payable(address(af));
        bytes memory msgData2 = abi.encodeWithSignature("id(uint256)", 42);
        uint oldBal = address(this).balance;
        uint oldAfBal = afAddr.balance;
        (bool res2, bytes memory ret2) = afAddr.call{value: 1}(msgData2);
        assert(res2 && (ret2.length > 0));
        assert(42 == abi.decode(ret2, (uint)));
        assert(oldBal == (address(this).balance + 1));
        assert(oldAfBal == (afAddr.balance - 1));
        RejectFun rf = new RejectFun();
        address payable rfAddr = payable(address(rf));
        oldBal = address(this).balance;
        uint oldRfBal = rfAddr.balance;
        (bool res3, bytes memory ret3) = rfAddr.call{value: 1}(msgData2);
        assert((!res3) && (ret3.length > 0));
        assert(oldBal == address(this).balance);
        assert(oldRfBal == rfAddr.balance);
    }

    function exceptionBytesTests() public {
        Throws c = new Throws();
        address a = address(c);
        bytes memory successMsgData = abi.encodeWithSignature("throws(bool)", false);
        (bool res0, bytes memory resBytes0) = a.call{gas: 2300}(successMsgData);
        assert(res0 && (resBytes0.length == 0));
        bytes memory failMsgData = abi.encodeWithSignature("throws(bool)", true);
        (bool res1, bytes memory resBytes1) = a.call{gas: 2300}(failMsgData);
        assert((!res1) && (keccak256(resBytes1) == keccak256(abi.encodeWithSignature("Error(string)", "foo"))));
        bytes memory failMsgData2 = abi.encodeWithSignature("double(uint256)", (2 ** 255) + 1);
        (bool res2, bytes memory resBytes2) = a.call{gas: 2300}(failMsgData2);
        assert((!res2) && (keccak256(resBytes2) == keccak256(abi.encodeWithSignature("Panic(uint256)", 0x11))));
    }

    function sendTests() public {
        AcceptRecv ar = new AcceptRecv();
        address payable arAddr = payable(address(ar));
        uint oldBalance = arAddr.balance;
        assert(arAddr.send(1));
        assert((oldBalance + 1) == arAddr.balance);
        RejectRecv rr = new RejectRecv();
        address payable rrAddr = payable(address(rr));
        oldBalance = rrAddr.balance;
        assert(!rrAddr.send(1));
        assert(oldBalance == rrAddr.balance);
        AcceptFalback af = new AcceptFalback();
        address payable afAddr = payable(address(af));
        oldBalance = afAddr.balance;
        assert(afAddr.send(1));
        assert((oldBalance + 1) == afAddr.balance);
        RejectFallback rf = new RejectFallback();
        address payable rfAddr = payable(address(rf));
        oldBalance = rfAddr.balance;
        assert(!rfAddr.send(1));
        assert(oldBalance == rfAddr.balance);
        RejectNoFuns rnf = new RejectNoFuns();
        address payable rnfAddr = payable(address(rnf));
        oldBalance = rnfAddr.balance;
        assert(!rnfAddr.send(1));
        assert(oldBalance == rnfAddr.balance);
    }

    function transferTests() public {
        AcceptRecv ar = new AcceptRecv();
        address payable arAddr = payable(address(ar));
        uint oldBalance = arAddr.balance;
        arAddr.transfer(1);
        assert((oldBalance + 1) == arAddr.balance);
        RejectRecv rr = new RejectRecv();
        address payable rrAddr = payable(address(rr));
        oldBalance = rrAddr.balance;
        try this.transfer(rrAddr) {
            assert(false);
        } catch Error(string memory s) {
            assert(false);
        } catch Panic(uint code) {
            assert(oldBalance == rrAddr.balance);
            assert(code == 1);
        } catch {
            assert(false);
        }
        AcceptFalback af = new AcceptFalback();
        address payable afAddr = payable(address(af));
        oldBalance = afAddr.balance;
        afAddr.transfer(1);
        assert((oldBalance + 1) == afAddr.balance);
        RejectFallback rf = new RejectFallback();
        address payable rfAddr = payable(address(rf));
        oldBalance = rfAddr.balance;
        try this.transfer(rfAddr) {
            assert(false);
        } catch Error(string memory s) {
            assert(false);
        } catch Panic(uint code) {
            assert(oldBalance == rfAddr.balance);
            assert(code == 1);
        } catch {
            assert(false);
        }
        RejectNoFuns rnf = new RejectNoFuns();
        address payable rnfAddr = payable(address(rnf));
        oldBalance = rnfAddr.balance;
        try this.transfer(rnfAddr) {
            assert(false);
        } catch Error(string memory s) {
            assert(false);
        } catch Panic(uint code) {
            assert(false);
        } catch {
            assert(oldBalance == rnfAddr.balance);
        }
    }

    function main() public {
        noContractTests();
        noFallbackTest();
        fallbackTests();
        receiveTests();
        exceptionBytesTests();
        sendTests();
        transferTests();
    }
}

contract __IRTest__ {
    function main() public {
        Test __this__ = new Test{value: 20}();
        Test __this1__ = new Test();
        __testCase1600__(__this__, __this1__);
        __testCase1618__(__this__, __this1__);
    }

    function __testCase1600__(Test __this__, Test __this1__) internal {
        __this__.main();
    }

    function __testCase1618__(Test __this__, Test __this1__) internal {
        __this__.callTests(__this1__);
    }
}