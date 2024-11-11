pragma solidity 0.5.17;

contract ConditionalTuples2 {
    uint[] internal x;
    uint[] internal y;

    function someFunc() public returns (uint, uint, uint) {
        return (10, 11, 12);
    }

    function verifyNestedTuples() public {
        uint a;
        uint b;
        uint c;
        uint d;
        (a, (b, c), d) = true ? (1, (2, 3), 4) : (5, (6, 7), 8);
        assert((((a == 1) && (b == 2)) && (c == 3)) && (d == 4));
        (a, (b, c), d) = false ? (1, (2, 3), 4) : (5, (6, 7), 8);
        assert((((a == 5) && (b == 6)) && (c == 7)) && (d == 8));
    }

    function verifyTupleOrFunctionCall() public {
        uint a;
        uint b;
        uint c;
        (a, b, c) = true ? (1, 2, 3) : someFunc();
        assert(((a == 1) && (b == 2)) && (c == 3));
        (a, b, c) = false ? (1, 2, 3) : someFunc();
        assert(((a == 10) && (b == 11)) && (c == 12));
    }

    function verifyNestedConditionalTuples() public {
        uint a;
        uint b;
        uint c;
        (a, b, c) = true ? (true ? (1, 2, 3) : (4, 5, 6)) : (false ? (7, 8, 9) : (10, 11, 12));
        assert(((a == 1) && (b == 2)) && (c == 3));
        (a, b, c) = true ? (false ? (1, 2, 3) : (4, 5, 6)) : (false ? (7, 8, 9) : (10, 11, 12));
        assert(((a == 4) && (b == 5)) && (c == 6));
        (a, b, c) = false ? (false ? (1, 2, 3) : (4, 5, 6)) : (true ? (7, 8, 9) : (10, 11, 12));
        assert(((a == 7) && (b == 8)) && (c == 9));
        (a, b, c) = false ? (false ? (1, 2, 3) : (4, 5, 6)) : (false ? (7, 8, 9) : (10, 11, 12));
        assert(((a == 10) && (b == 11)) && (c == 12));
    }

    function verifyAssignmentsToArrayLengths() public {
        uint z;
        (x.length, y.length, z) = true ? (1, 2, 3) : (4, 5, 6);
        assert(((x.length == 1) && (y.length == 2)) && (z == 3));
        (x.length, y.length, z) = false ? (1, 2, 3) : (4, 5, 6);
        assert(((x.length == 4) && (y.length == 5)) && (z == 6));
    }

    function main() public {
        verifyNestedTuples();
        verifyTupleOrFunctionCall();
        verifyNestedConditionalTuples();
        verifyAssignmentsToArrayLengths();
    }
}

contract __IRTest__ {
    function main() public {
        ConditionalTuples2 __this__ = new ConditionalTuples2();
        __testCase476__(__this__);
    }

    function __testCase476__(ConditionalTuples2 __this__) internal {
        __this__.main();
    }
}