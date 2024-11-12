pragma solidity ^0.4.24;

contract Ternary {
    uint private x = 1;
    uint internal y = 1;

    function sqrt(int32 x) public pure returns (int32 y) {
        int32 z = (x + 1) / 2;
        y = x;
        while (z < y) {
            y = z;
            z = ((x / z) + z) / 2;
        }
    }

    function ternaryInExpressionStatement(uint a) public returns (uint) {
        require(a > 0);
        (a == 1) ? a += 1 : a += 2;
        return a;
    }

    function ternaryNested(uint a) public returns (uint) {
        if ((a == 1) ? ((a <= 1) ? true : false) : false) {
            a += 1;
        } else if ((a <= 1) ? true : ((false == (a <= 2)) ? true : false)) {
            a += 1;
        }
        return a;
    }

    function ternaryNestedFunctionCallArgument(bool b) public returns (int64) {
        int16 x = 1337;
        return sqrt((b ? x = 2 : x = 8));
    }

    function ternaryReturn(uint a) public returns (uint) {
        require(a > 0);
        return ((a == 1) ? a += 1 : a += 2);
    }

    function ternaryReturnMultiple(bool b) public returns (uint, uint) {
        return ((b ? x = 1 : x = 2), (b ? y = 1 : y = 2));
    }

    function ternaryCommonType(bool b) public returns (address) {
        address a = 0xdeadbeef;
        return (b ? a : 0);
    }
}

contract __IRTest__ {
    function main() public {
        Ternary __this__ = new Ternary();
        __testCase238__(__this__);
        __testCase266__(__this__);
        __testCase294__(__this__);
        __testCase322__(__this__);
        __testCase350__(__this__);
        __testCase376__(__this__);
        __testCase402__(__this__);
        __testCase428__(__this__);
        __testCase465__(__this__);
        __testCase504__(__this__);
        __testCase532__(__this__);
        __testCase558__(__this__);
    }

    function __testCase238__(Ternary __this__) internal {
        uint256 ret_238_0 = __this__.ternaryInExpressionStatement(uint256(1));
        assert(ret_238_0 == uint256(2));
    }

    function __testCase266__(Ternary __this__) internal {
        uint256 ret_266_0 = __this__.ternaryInExpressionStatement(uint256(3));
        assert(ret_266_0 == uint256(5));
    }

    function __testCase294__(Ternary __this__) internal {
        uint256 ret_294_0 = __this__.ternaryNested(uint256(0));
        assert(ret_294_0 == uint256(1));
    }

    function __testCase322__(Ternary __this__) internal {
        uint256 ret_322_0 = __this__.ternaryNested(uint256(1));
        assert(ret_322_0 == uint256(2));
    }

    function __testCase350__(Ternary __this__) internal {
        uint256 ret_350_0 = __this__.ternaryNested(uint256(2));
        assert(ret_350_0 == uint256(2));
    }

    function __testCase376__(Ternary __this__) internal {
        int64 ret_376_0 = __this__.ternaryNestedFunctionCallArgument(true);
        assert(ret_376_0 == int64(1));
    }

    function __testCase402__(Ternary __this__) internal {
        int64 ret_402_0 = __this__.ternaryNestedFunctionCallArgument(false);
        assert(ret_402_0 == int64(2));
    }

    function __testCase428__(Ternary __this__) internal {
        (uint256 ret_428_0, uint256 ret_428_1) = __this__.ternaryReturnMultiple(true);
        assert(ret_428_0 == uint256(1));
        assert(ret_428_1 == uint256(1));
    }

    function __testCase465__(Ternary __this__) internal {
        (uint256 ret_465_0, uint256 ret_465_1) = __this__.ternaryReturnMultiple(false);
        assert(ret_465_0 == uint256(2));
        assert(ret_465_1 == uint256(2));
    }

    function __testCase504__(Ternary __this__) internal {
        uint256 ret_504_0 = __this__.ternaryReturn(uint256(1));
        assert(ret_504_0 == uint256(2));
    }

    function __testCase532__(Ternary __this__) internal {
        uint256 ret_532_0 = __this__.ternaryReturn(uint256(5));
        assert(ret_532_0 == uint256(7));
    }

    function __testCase558__(Ternary __this__) internal {
        address ret_558_0 = __this__.ternaryCommonType(false);
        assert(ret_558_0 == address(0x0));
    }
}