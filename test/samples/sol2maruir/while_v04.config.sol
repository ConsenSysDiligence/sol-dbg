pragma solidity 0.4.25;

contract While {
    function whileStatementWithBlock() public {
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
    }

    function whileStatementWithExpression() public {
        uint8 x = 0;
        while (x < 100) x++;
        assert(x == 100);
    }

    function whileStatementWithLoopControlStatements() public {
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
                return;
            }
        }
        assert(false);
    }

    function doWhileStatementWithBlock() public {
        uint8 x = 0;
        uint8 a = 100;
        uint16 b = 0;
        do {
            a -= 10;
            b += a + x;
        } while((++x) < a);
        assert(x == 10);
        assert(a == 0);
        assert(b == 495);
    }

    function doWhileStatementWithExpression() public {
        uint8 x = 0;
        do x++; while(x < 100);
    }

    function doWhileStatementWithLoopControlStatements() public {
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
                return;
            }
        } while(true);
        assert(false);
    }
}

contract __IRTest__ {
    function main() public payable {
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
