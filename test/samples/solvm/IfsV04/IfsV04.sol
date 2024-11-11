pragma solidity ^0.4.24;

contract Ifs {
    function ifElseStatementNested() public {
        uint8 a = 1;
        uint8 b = 42;
        if ((a + 1) >= 2) {
            b = a / 4;
            if (b >= 2) {
                assert(false);
                b = 2;
            } else {
                a = a * 2;
            }
        } else {
            assert(false);
            b = a * 2;
        }
        b += 15;
        assert(b == 15);
        assert(a == 2);
    }

    function ifElseStatement() public {
        uint8 a = 1;
        uint8 b = 42;
        if ((a + 1) < 2) {
            assert(false);
            b = a / 2;
        } else {
            b = a * 2;
        }
        assert(b == 2);
        b += 15;
        assert(b == 17);
    }

    function ifStatement() public {
        uint8 a = 1;
        uint8 b = 42;
        if ((a + 1) >= 2) {
            b = a / 2;
        }
        assert(b == 0);
        b += 15;
        assert(b == 15);
    }

    function ifElseStatementWithExpressions() public {
        uint8 a = 1;
        uint8 b = 42;
        if ((a + 1) >= 2) b = a / 2; else b = a * 2;
        assert(b == 0);
        b += 15;
        assert(b == 15);
    }

    function ifStatementWithExpression() public {
        uint8 a = 1;
        uint8 b = 42;
        if ((a + 1) >= 2) b = a / 2;
        assert(b == 0);
        b += 15;
        assert(b == 15);
    }

    function ifStatementWithReturn() public {
        uint8 a = 1;
        uint8 b = 0;
        if ((a + 1) >= 2) return;
        assert(false);
        b += 15;
    }

    function ifStatementWithThrow() public {
        uint8 a = 1;
        uint8 b = 42;
        if ((a + 1) >= 2) {
            b = a * 2;
        } else {
            revert();
        }
        assert(b == 2);
        b += 15;
        assert(b == 17);
    }
}

contract __IRTest__ {
    function main() public {
        Ifs __this__ = new Ifs();
        __testCase339__(__this__);
        __testCase353__(__this__);
        __testCase367__(__this__);
        __testCase381__(__this__);
        __testCase395__(__this__);
        __testCase409__(__this__);
        __testCase423__(__this__);
    }

    function __testCase339__(Ifs __this__) internal {
        __this__.ifElseStatementNested();
    }

    function __testCase353__(Ifs __this__) internal {
        __this__.ifElseStatement();
    }

    function __testCase367__(Ifs __this__) internal {
        __this__.ifStatement();
    }

    function __testCase381__(Ifs __this__) internal {
        __this__.ifElseStatementWithExpressions();
    }

    function __testCase395__(Ifs __this__) internal {
        __this__.ifStatementWithExpression();
    }

    function __testCase409__(Ifs __this__) internal {
        __this__.ifStatementWithReturn();
    }

    function __testCase423__(Ifs __this__) internal {
        __this__.ifStatementWithThrow();
    }
}