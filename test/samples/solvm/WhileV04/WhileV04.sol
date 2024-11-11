pragma solidity 0.4.25;

contract While {
    function whileStatementWithBlock() public returns (uint) {
        uint8 x = 0;
        uint8 a = 100;
        uint16 b = 0;
        while ((++x) < a) {
            a -= 10;
            b += a + x;
        }
        assert(x == 10);
        assert(a == 10);
        assert(b == 495);
        return (uint(x) + uint(a) + uint(b));
    }

    function whileStatementWithExpression() public returns (uint8) {
        uint8 x = 0;
        while (x < 100) x++;
        assert(x == 100);
        return x;
    }

    function whileStatementWithLoopControlStatements() public returns (uint8) {
        uint8 x = 0;
        while (true) {
            if (x >= 100) {
                break;
            } else if (x < 10) {
                x += 5;
                continue;
            }
            assert((x >= 10) && (x <= 90));
            x++;
            if (x > 90) {
                return x;
            }
        }
        assert(false);
    }

    function doWhileStatementWithBlock() public returns (uint) {
        uint8 x = 0;
        uint8 a = 100;
        uint16 b = 0;
        do {
            a -= 10;
            b += a + x;
        } while ((++x) < a);
        assert(x == 10);
        assert(a == 0);
        assert(b == 495);

        return (x + a + b);
    }

    function doWhileStatementWithExpression() public returns (uint8) {
        uint8 x = 0;
        do x++; while (x < 100);

        return x;
    }

    function doWhileStatementWithLoopControlStatements() public returns (uint8) {
        uint8 x = 0;
        do {
            if (x >= 100) {
                break;
            } else if (x < 10) {
                x += 5;
                continue;
            }
            assert((x >= 10) && (x <= 90));
            x++;
            if (x > 90) {
                return x;
            }
        } while (true);
        assert(false);
    }
}

contract __IRTest__ {
    function main() public {
        While __this__ = new While();
        __testCase252__(__this__);
        __testCase266__(__this__);
        __testCase280__(__this__);
        __testCase294__(__this__);
        __testCase308__(__this__);
        __testCase322__(__this__);
    }

    function __testCase252__(While __this__) internal {
        __this__.whileStatementWithBlock();
    }

    function __testCase266__(While __this__) internal {
        __this__.whileStatementWithExpression();
    }

    function __testCase280__(While __this__) internal {
        __this__.whileStatementWithLoopControlStatements();
    }

    function __testCase294__(While __this__) internal {
        __this__.doWhileStatementWithBlock();
    }

    function __testCase308__(While __this__) internal {
        __this__.doWhileStatementWithExpression();
    }

    function __testCase322__(While __this__) internal {
        __this__.doWhileStatementWithLoopControlStatements();
    }
}
