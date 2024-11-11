pragma solidity ^0.4.24;

contract ForLoops {
    function forStatementCompleteWithExpression() public {
        uint16 x = 0;
        for (uint8 a = 0; a < 10; a++) x += a;
        assert(x == 45);
        x *= 2;
        assert(x == 90);
    }

    function forStatementCompleteWithBlock() public {
        uint16 x = 0;
        int16 y = 0;
        for (uint8 a = 0; a < 10; a++) {
            x += a;
            y -= a;
        }
        assert(x == 45);
        assert(y == (-45));
        x *= 2;
        y /= 2;
        assert(x == 90);
        assert(y == (-22));
    }

    function forStatementInitializationWithNoDeclaration() public {
        uint16 x = 0;
        int16 y = 0;
        uint8 a;
        for (a = 0; a < 10; a++) {
            x += a;
            y -= a;
        }
        assert(a == 10);
        assert(x == 45);
        assert(y == (-45));
        x *= 2;
        y /= 2;
        assert(x == 90);
        assert(y == (-22));
    }

    function forStatementNoInitialization() public {
        uint16 x = 0;
        uint8 y = 0;
        for (; y < 10; y++) {
            x += y;
        }
        assert(y == 10);
        assert(x == 45);
        x *= 2;
        assert(x == 90);
    }

    function forStatementNoLoopExpression() public {
        uint16 x = 0;
        for (uint8 a = 0; a < 10; ) x += ++a;
        assert(x == 55);
        x *= 2;
        assert(x == 110);
    }

    function forStatementNoLoopCondition() public {
        uint16 x = 0;
        for (uint8 a = 0; ; a++) {
            if (a > 10) {
                break;
            }
            x += a;
        }
        assert(x == 55);
        x *= 2;
    }

    function forStatementLoopExpressionOnly() public {
        uint16 x = 0;
        uint8 a = 0;
        for (; ; a++) {
            if (a > 10) {
                break;
            }
            x += a;
        }
        assert(a == 11);
        assert(x == 55);
        x *= 2;
    }

    function forStatementLoopConditionOnly() public {
        uint16 x = 0;
        uint8 a = 0;
        for (; a < 10; ) x += a++;
        assert(a == 10);
        assert(x == 45);
        x *= 2;
    }

    function forStatementLoopInitializationOnly() public {
        uint16 x = 0;
        for (uint8 a = 0; ; ) {
            if (a > 10) {
                break;
            }
            x += a++;
        }
        assert(x == 55);
        x *= 2;
    }

    function forStatementEmpty() public {
        uint16 x = 0;
        uint8 a = 0;
        for (; ; ) {
            if (a > 10) {
                break;
            }
            x += a++;
        }
        assert(a == 11);
        assert(x == 55);
        x *= 2;
    }

    function forStatementWithLoopControlStatements() public {
        uint16 x = 0;
        uint8 a = 0;
        for (a = 1; a < 15; a++) {
            if (a > 10) {
                return;
            }
        }
        assert(false);
        x *= 2;
    }

    function forStatementwithTernaryInHeader() public {
        uint16 x = 0;
        uint16 y = 0;
        uint16 a = 0;
        for (a = (true ? 9 : 0); a < 10; a++) {
            x += a;
            y -= a;
        }
        assert(a == 10);
        assert(x == 9);
        x = 0;
        for (uint16 b = 1; b <= (true ? 2 : 1); b++) {
            x += b;
            y -= b;
        }
        assert(x == 3);
        x = 0;
        for (uint16 c = 1; c < 10; c += (false ? 0 : 1)) {
            x += c;
            y -= c;
        }
        assert(x == 45);
    }
}

contract __IRTest__ {
    function main() public {
        ForLoops __this__ = new ForLoops();
        __testCase631__(__this__);
        __testCase645__(__this__);
        __testCase659__(__this__);
        __testCase673__(__this__);
        __testCase687__(__this__);
        __testCase701__(__this__);
        __testCase715__(__this__);
        __testCase729__(__this__);
        __testCase743__(__this__);
        __testCase757__(__this__);
        __testCase771__(__this__);
        __testCase785__(__this__);
    }

    function __testCase631__(ForLoops __this__) internal {
        __this__.forStatementCompleteWithExpression();
    }

    function __testCase645__(ForLoops __this__) internal {
        __this__.forStatementCompleteWithBlock();
    }

    function __testCase659__(ForLoops __this__) internal {
        __this__.forStatementInitializationWithNoDeclaration();
    }

    function __testCase673__(ForLoops __this__) internal {
        __this__.forStatementNoInitialization();
    }

    function __testCase687__(ForLoops __this__) internal {
        __this__.forStatementNoLoopExpression();
    }

    function __testCase701__(ForLoops __this__) internal {
        __this__.forStatementNoLoopCondition();
    }

    function __testCase715__(ForLoops __this__) internal {
        __this__.forStatementLoopExpressionOnly();
    }

    function __testCase729__(ForLoops __this__) internal {
        __this__.forStatementLoopConditionOnly();
    }

    function __testCase743__(ForLoops __this__) internal {
        __this__.forStatementLoopInitializationOnly();
    }

    function __testCase757__(ForLoops __this__) internal {
        __this__.forStatementEmpty();
    }

    function __testCase771__(ForLoops __this__) internal {
        __this__.forStatementWithLoopControlStatements();
    }

    function __testCase785__(ForLoops __this__) internal {
        __this__.forStatementwithTernaryInHeader();
    }
}