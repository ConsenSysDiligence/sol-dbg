pragma solidity ^0.4.24;

contract Modifiers {
    uint256 internal b;
    uint public sum = 0;

    modifier checkBefore(uint x, uint y) {
        require(x != y);
        _;
    }

    modifier checkAfter(uint x, uint y) {
        _;
        require(x > y);
    }

    modifier increaseSum() {
        _;
        require(sum > 0);
        sum += 1;
    }

    modifier greaterThanStateVar(uint c) {
        uint a;
        a = c;
        require(a > b);
        _;
    }

    modifier alterMemoryBefore(uint[3] memory x) {
        x[0] = 1;
        _;
    }

    modifier alterMemoryAfter(uint[3] memory x) {
        _;
        x[0] = 1;
    }

    function modifierRepeated(uint x, uint y) public greaterThanStateVar(x) greaterThanStateVar(y) returns (uint) {
        return x + y;
    }

    function modifierBefore(uint x, uint y) public checkBefore(x,y) returns (uint) {
        return x + y;
    }

    function modifierReturn(uint x, uint y) public increaseSum() returns (uint) {
        sum = 1;
        return sum;
    }

    function modifierAfter(uint x, uint y) public checkAfter(x,y) returns (uint) {
        x += 1;
        return x + y;
    }

    function modifierTwo(uint x, uint y) public checkAfter(x,y) checkBefore(x,y) returns (uint) {
        return x + y;
    }

    function modifierChangeMemoryArrBefore(uint[3] memory a) public alterMemoryBefore(a) returns (uint[3] memory) {
        return a;
    }

    function modifierChangeMemoryArrAfter1(uint[3] memory a) public alterMemoryAfter(a) returns (uint[3] memory) {
        return a;
    }

    function modifierChangeMemoryArrAfter2(uint[3] memory a) public alterMemoryAfter(a) returns (uint) {
        uint res = a[0];
        return res;
    }
}

contract __IRTest__ {
    function main() public {
        Modifiers __this__ = new Modifiers();
        __testCase271__(__this__);
        __testCase320__(__this__);
        __testCase351__(__this__);
        __testCase400__(__this__);
        __testCase431__(__this__);
        __testCase462__(__this__);
        __testCase511__(__this__);
        __testCase560__(__this__);
        __testCase591__(__this__);
        __testCase640__(__this__);
        __testCase675__(__this__);
        __testCase731__(__this__);
        __testCase787__(__this__);
    }

    function __testCase271__(Modifiers __this__) internal {
        bool res;
        bytes memory retData;
        bytes memory data;
        data = abi.encodeWithSignature("modifierRepeated(uint256,uint256)", uint256(1), uint256(0));
        res = address(__this__).call(data);
        assert(!res);
    }

    function __testCase320__(Modifiers __this__) internal {
        uint256 ret_320_0 = __this__.modifierRepeated(uint256(1), uint256(2));
        assert(ret_320_0 == uint256(3));
    }

    function __testCase351__(Modifiers __this__) internal {
        bool res;
        bytes memory retData;
        bytes memory data;
        data = abi.encodeWithSignature("modifierBefore(uint256,uint256)", uint256(2), uint256(2));
        res = address(__this__).call(data);
        assert(!res);
    }

    function __testCase400__(Modifiers __this__) internal {
        uint256 ret_400_0 = __this__.modifierBefore(uint256(2), uint256(3));
        assert(ret_400_0 == uint256(5));
    }

    function __testCase431__(Modifiers __this__) internal {
        uint256 ret_431_0 = __this__.modifierReturn(uint256(1), uint256(2));
        assert(ret_431_0 == uint256(1));
    }

    function __testCase462__(Modifiers __this__) internal {
        bool res;
        bytes memory retData;
        bytes memory data;
        data = abi.encodeWithSignature("modifierAfter(uint256,uint256)", uint256(0), uint256(2));
        res = address(__this__).call(data);
        assert(!res);
    }

    function __testCase511__(Modifiers __this__) internal {
        bool res;
        bytes memory retData;
        bytes memory data;
        data = abi.encodeWithSignature("modifierAfter(uint256,uint256)", uint256(1), uint256(1));
        res = address(__this__).call(data);
        assert(!res);
    }

    function __testCase560__(Modifiers __this__) internal {
        uint256 ret_560_0 = __this__.modifierAfter(uint256(2), uint256(1));
        assert(ret_560_0 == uint256(4));
    }

    function __testCase591__(Modifiers __this__) internal {
        bool res;
        bytes memory retData;
        bytes memory data;
        data = abi.encodeWithSignature("modifierTwo(uint256,uint256)", uint256(0), uint256(2));
        res = address(__this__).call(data);
        assert(!res);
    }

    function __testCase640__(Modifiers __this__) internal {
        uint256 ret_640_0 = __this__.modifierTwo(uint256(2), uint256(1));
        assert(ret_640_0 == uint256(3));
    }

    function __testCase675__(Modifiers __this__) internal {
        uint256[3] memory ret_675_0 = __this__.modifierChangeMemoryArrBefore([uint256(9), uint256(8), uint256(7)]);
        assert(keccak256(abi.encodePacked(ret_675_0)) == keccak256(abi.encodePacked([uint256(1), uint256(8), uint256(7)])));
    }

    function __testCase731__(Modifiers __this__) internal {
        uint256[3] memory ret_731_0 = __this__.modifierChangeMemoryArrAfter1([uint256(5), uint256(5), uint256(5)]);
        assert(keccak256(abi.encodePacked(ret_731_0)) == keccak256(abi.encodePacked([uint256(1), uint256(5), uint256(5)])));
    }

    function __testCase787__(Modifiers __this__) internal {
        uint256 ret_787_0 = __this__.modifierChangeMemoryArrAfter2([uint256(5), uint256(5), uint256(5)]);
        assert(ret_787_0 == uint256(5));
    }
}