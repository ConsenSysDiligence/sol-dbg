pragma solidity ^0.4.24;

contract Assignments {
    function simpleAssignment() public {
        int8 test = 1;
        assert(test == 1);
    }

    function multipleAssignmentsLong() public {
        uint8 test = 2;
        uint8 newTest = 1;
        assert(test == 2);
        assert(newTest == 1);
        test = test + 2;
        assert(test == 4);
        test = test - 2;
        assert(test == 2);
        test = test * 2;
        assert(test == 4);
        test = test / 2;
        assert(test == 2);
        test = test ** 2;
        assert(test == 4);
        test = test % 2;
        assert(test == 0);
        test = test | 1;
        assert(test == 1);
        test = test << 1;
        assert(test == 2);
        test = test >> 1;
        assert(test == 1);
        test = test & 0;
        assert(test == 0);
        test = test ^ 1;
        assert(test == 1);
        test = ~test;
        assert(test == 254);
        test = -test;
        assert(test == 2);
        newTest = test++;
        assert(test == 3);
        assert(newTest == 2);
        newTest = ++test;
        assert(test == 4);
        assert(newTest == 4);
        newTest = test--;
        assert(test == 3);
        assert(newTest == 4);
        newTest = --test;
        assert(test == 2);
        assert(newTest == 2);
    }

    function multipleAssignmentsShort() public {
        int16 test = 2;
        test++;
        assert(test == 3);
        ++test;
        assert(test == 4);
        test--;
        assert(test == 3);
        --test;
        assert(test == 2);
        test += 2;
        assert(test == 4);
        test -= 2;
        assert(test == 2);
        test *= 2;
        assert(test == 4);
        test /= 2;
        assert(test == 2);
        test %= 2;
        assert(test == 0);
        test <<= 1;
        assert(test == 0);
        test >>= 1;
        assert(test == 0);
        test |= 1;
        assert(test == 1);
        test &= 1;
        assert(test == 1);
        test ^= 1;
        assert(test == 0);
    }
}

contract __IRTest__ {
    function main() public {
        Assignments __this__ = new Assignments();
        __testCase420__(__this__);
        __testCase434__(__this__);
        __testCase448__(__this__);
    }

    function __testCase420__(Assignments __this__) internal {
        __this__.simpleAssignment();
    }

    function __testCase434__(Assignments __this__) internal {
        __this__.multipleAssignmentsLong();
    }

    function __testCase448__(Assignments __this__) internal {
        __this__.multipleAssignmentsShort();
    }
}