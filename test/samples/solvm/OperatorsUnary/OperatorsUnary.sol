pragma solidity ^0.4.24;

contract OperatorsUnary {
    function testArithmeticOperators() public {
        int a;
        int b;
        a = 10;
        b = a++;
        assert(a == 11);
        assert(b == 10);
        a = 10;
        b = ++a;
        assert(a == 11);
        assert(b == 11);
        a = 10;
        b = a--;
        assert(a == 9);
        assert(b == 10);
        a = 10;
        b = --a;
        assert(a == 9);
        assert(b == 9);
        a = 5;
        b = -a;
        assert(a == 5);
        assert(b == (-5));
        a = -5;
        b = -a;
        assert(a == (-5));
        assert(b == 5);
    }

    function testBitwiseOperators() public {
        int a;
        int b;
        a = 8;
        b = ~a;
        assert(a == 8);
        assert(b == (-9));
        a = -9;
        b = ~a;
        assert(a == (-9));
        assert(b == 8);
        a = 0;
        b = ~a;
        assert(a == 0);
        assert(b == (-1));
        a = -1;
        b = ~a;
        assert(a == (-1));
        assert(b == 0);
    }

    function testLogicOperators() public {
        bool a;
        bool b;
        a = true;
        b = !a;
        assert(a == true);
        assert(b == false);
        a = false;
        b = !a;
        assert(a == false);
        assert(b == true);
    }

    function testDelete() public {
        int a = 1;
        delete a;
        assert(a == 0);
        uint b = 2;
        delete b;
        assert(b == 0);
        bool c = true;
        delete c;
        assert(c == false);
    }

    function testTupleArgs() public {
        uint[3] memory numbers = [uint(1), 2, 3];
        assert(numbers[0] == 1);
        delete (numbers[0]);
        assert(numbers[0] == 0);
        assert(numbers[1] == 2);
        assert((++(((numbers[1])))) == 3);
        assert(numbers[1] == 3);
        assert(numbers[2] == 3);
        assert((((numbers[2]))--) == 3);
        assert(numbers[2] == 2);
    }
}

contract __IRTest__ {
    function main() public {
        OperatorsUnary __this__ = new OperatorsUnary();
        __testCase443__(__this__);
        __testCase457__(__this__);
        __testCase471__(__this__);
        __testCase485__(__this__);
        __testCase499__(__this__);
    }

    function __testCase443__(OperatorsUnary __this__) internal {
        __this__.testArithmeticOperators();
    }

    function __testCase457__(OperatorsUnary __this__) internal {
        __this__.testBitwiseOperators();
    }

    function __testCase471__(OperatorsUnary __this__) internal {
        __this__.testLogicOperators();
    }

    function __testCase485__(OperatorsUnary __this__) internal {
        __this__.testDelete();
    }

    function __testCase499__(OperatorsUnary __this__) internal {
        __this__.testTupleArgs();
    }
}