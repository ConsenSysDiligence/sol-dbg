pragma solidity ^0.4.24;

contract Returns {
    uint[3] internal a1;
    uint internal y = 1;

    function mixedReturn1(uint x) public returns (uint, uint a) {
        a = 10;
        return (1, 2);
    }

    function mixedReturn2(uint x) public returns (uint, uint a) {
        a = 10;
    }

    function returnImplicitCopy() public returns (uint[3] memory) {
        return a1;
    }

    function addOne(uint a) public returns (uint) {
        return a + 1;
    }

    function addOneTwice(uint a, uint b) public returns (uint, uint) {
        return (addOne(a), addOne(b));
    }

    function paramReturnAssignments(uint a) public returns (uint b, uint) {
        a = a + 1;
        a = 1;
        b = 2;
        return (a, b);
    }

    function paramReturnSwap() public returns (uint a, uint b) {
        a = 1;
        b = 2;
        return (b, a);
    }

    function noArgReturn() public returns (uint a, uint b) {
        a = 1;
        b = 2;
        return;
    }

    function noArgReturnDefaults() public returns (uint a, int16 b) {
        return;
    }

    function paramReturnSwap2() public returns (uint a, uint b, uint c) {
        a = 1;
        b = 2;
        return (b, a, 2);
    }

    function deadCodeAfterReturn(uint x) public returns (uint) {
        return x;
        y = 2;
    }
}

contract __IRTest__ {
    function main() public {
        Returns __this__ = new Returns();
        __testCase211__(__this__);
        __testCase250__(__this__);
        __testCase286__(__this__);
        __testCase335__(__this__);
        __testCase366__(__this__);
        __testCase405__(__this__);
        __testCase430__(__this__);
        __testCase466__(__this__);
        __testCase502__(__this__);
        __testCase538__(__this__);
        __testCase588__(__this__);
    }

    function __testCase211__(Returns __this__) internal {
        (uint256 ret_211_0, uint256 ret_211_1) = __this__.mixedReturn1(uint256(777));
        assert(ret_211_0 == uint256(1));
        assert(ret_211_1 == uint256(2));
    }

    function __testCase250__(Returns __this__) internal {
        (uint256 ret_250_0, uint256 ret_250_1) = __this__.mixedReturn2(uint256(777));
        assert(ret_250_0 == uint256(0));
        assert(ret_250_1 == uint256(10));
    }

    function __testCase286__(Returns __this__) internal {
        uint256[3] memory ret_286_0 = __this__.returnImplicitCopy();
        assert(keccak256(abi.encodePacked(ret_286_0)) == keccak256(abi.encodePacked([uint256(0), uint256(0), uint256(0)])));
    }

    function __testCase335__(Returns __this__) internal {
        uint256 ret_335_0 = __this__.addOne(uint256(8));
        assert(ret_335_0 == uint256(9));
    }

    function __testCase366__(Returns __this__) internal {
        (uint256 ret_366_0, uint256 ret_366_1) = __this__.addOneTwice(uint256(5), uint256(9));
        assert(ret_366_0 == uint256(6));
        assert(ret_366_1 == uint256(10));
    }

    function __testCase405__(Returns __this__) internal {
        uint256 ret_405_0 = __this__.deadCodeAfterReturn(uint256(100));
        assert(ret_405_0 == uint256(100));
    }

    function __testCase430__(Returns __this__) internal {
        (uint256 ret_430_0, uint256 ret_430_1) = __this__.paramReturnSwap();
        assert(ret_430_0 == uint256(2));
        assert(ret_430_1 == uint256(1));
    }

    function __testCase466__(Returns __this__) internal {
        (uint256 ret_466_0, uint256 ret_466_1) = __this__.noArgReturn();
        assert(ret_466_0 == uint256(1));
        assert(ret_466_1 == uint256(2));
    }

    function __testCase502__(Returns __this__) internal {
        (uint256 ret_502_0, int16 ret_502_1) = __this__.noArgReturnDefaults();
        assert(ret_502_0 == uint256(0));
        assert(ret_502_1 == int16(0));
    }

    function __testCase538__(Returns __this__) internal {
        (uint256 ret_538_0, uint256 ret_538_1, uint256 ret_538_2) = __this__.paramReturnSwap2();
        assert(ret_538_0 == uint256(2));
        assert(ret_538_1 == uint256(1));
        assert(ret_538_2 == uint256(2));
    }

    function __testCase588__(Returns __this__) internal {
        (uint256 ret_588_0, uint256 ret_588_1) = __this__.paramReturnAssignments(uint256(100));
        assert(ret_588_0 == uint256(1));
        assert(ret_588_1 == uint256(2));
    }
}