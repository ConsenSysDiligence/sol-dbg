pragma solidity ^0.4.24;

import "./contract_v04.sol";

contract Calls {
    event Operand(uint256 value);

    event Sum(uint256 value);

    uint[3] internal a1;
    uint[3] internal a2;

    function sqrt(int32 x) public returns (int32 y) {
        int32 z = (x + 1) / 2;
        y = x;
        while (z < y) {
            y = z;
            z = ((x / z) + z) / 2;
        }
        assert((y * y) <= x);
        assert(((y + 1) * (y + 1)) >= x);
    }

    function complexExpressionsNesting() public {
        int8 a = 1;
        int8 b = 5;
        int16 c = 2;
        int32 d = 2 + 9;
        assert(d == 11);
        int h = 144;
        int x = (((3 + (a * b)) * (c + d)) * d) / (a + h);
        assert(x == 7);
    }

    function functionCallInExpression() public {
        int32 a = 5;
        int32 b = 8;
        int32 c = 15 + (sqrt((a * a) + (b * b)) / 2);
        assert(c == 19);
    }

    function functionCallPublicGetter(OwnedToken o) public returns (address) {
        return o.owner();
    }

    function requireCall(uint x, uint y) public returns (uint) {
        require((x + y) > x);
        uint z = x + y;
        require((z > 0) && (x > 0), "z and x should be greater than 0");
        z += 1;
        return z;
    }

    function assertCall(uint x, uint y) public returns (uint) {
        assert((x + y) > x);
        uint z = x + y;
        assert((z > 0) && (x > 0));
        z += 1;
        return z;
    }

    function revertCall(uint x, uint y) public returns (uint) {
        if (!((x + y) > x)) {
            revert();
        }
        uint z = x + y;
        if ((z > 0) && (x > 0)) {
            z += 1;
        } else {
            revert("z and x should be greater than 0");
        }
        return z;
    }

    function multipleReturn() public returns (uint) {
        uint8 a = 1;
        uint8 b = 0;
        if ((a + 1) >= 2) {
            return a;
        } else {
            if (b > 0) {
                return b;
            } else {
                b++;
            }
        }
    }

    function sort2(uint x, uint y) public returns (uint, uint) {
        if (x > y) {
            return (y, x);
        } else {
            return (x, y);
        }
    }

    function returnNoExplicitReturn() public returns (uint x) {
        x = 1;
    }

    function returnMixedNamedUnamed(bool b) public returns (uint, uint a) {
        if (b) {
            a = 10;
            return (2, 3);
        } else {
            a = 11;
        }
    }

    function returnOverwrite() public returns (uint x) {
        uint y = 1;
        return y;
    }

    function returnAssignBeforeBreak1() public returns (uint) {
        uint x = 0;
        do {
            break;
        } while((x = 1) > 0);
        return x;
    }

    function returnAssignBeforeBreak2() public returns (uint, uint) {
        uint x = 0;
        uint y = 0;
        for (uint i = 5; (x = i) >= 0; (y = i++)) {
            break;
        }
        return (x, y);
    }

    function returnBreakBeforeAssign() public returns (uint) {
        uint x = 0;
        while ((x = 1) > 0) {
            break;
        }
        return x;
    }

    function returnTuplesFromFunction() public {
        uint[3] memory x1;
        uint[3] memory x2;
        (x1, x2) = arrayStorageToStorage();
        (x1, ) = arrayStorageToStorage();
        (, x2) = arrayStorageToStorage();
    }

    function returnTuplesFromFunctionCall() public returns (uint[3], uint[3]) {
        return arrayStorageToStorage();
    }

    function emitFunction() public {
        uint x = 1;
        uint y = 2;
        uint sum = x + y;
    }

    function arrayStorageToStorage() public returns (uint[3], uint[3]) {
        a1 = [1, 2, 3];
        assert(((a1[0] == 1) && (a1[1] == 2)) && (a1[2] == 3));
        a2 = a1;
        assert(((a1[0] == 1) && (a1[1] == 2)) && (a1[2] == 3));
        assert(((a2[0] == 1) && (a2[1] == 2)) && (a2[2] == 3));
        a2[0] = 4;
        assert(((a1[0] == 1) && (a1[1] == 2)) && (a1[2] == 3));
        assert(((a2[0] == 4) && (a2[1] == 2)) && (a2[2] == 3));
        return (a1, a2);
    }
}

contract __IRTest__ {
    function main() public {
        Calls __this__ = new Calls();
        __testCase843__(__this__);
        __testCase871__(__this__);
        __testCase896__(__this__);
        __testCase910__(__this__);
    }

    function __testCase843__(Calls __this__) internal {
        int32 ret_843_0 = __this__.sqrt(int32(25));
        assert(ret_843_0 == int32(5));
    }

    function __testCase871__(Calls __this__) internal {
        int32 ret_871_0 = __this__.sqrt(int32(35));
        assert(ret_871_0 == int32(5));
    }

    function __testCase896__(Calls __this__) internal {
        __this__.complexExpressionsNesting();
    }

    function __testCase910__(Calls __this__) internal {
        __this__.functionCallInExpression();
    }
}