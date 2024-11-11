pragma solidity ^0.4.24;

contract OperatorsBinary {
    bytes2 public constant x = bytes2(0x0100) | bytes2(0x0001);

    function testArithmeticOperators() public {
        int a;
        int b;
        int c;
        a = 10;
        b = 15;
        c = a + b;
        assert(c == 25);
        a = -10;
        b = -15;
        c = a + b;
        assert(c == (-25));
        a = 10;
        b = 15;
        c = a - b;
        assert(c == (-5));
        a = -10;
        b = -15;
        c = a - b;
        assert(c == 5);
        a = 10;
        b = 15;
        c = a * b;
        assert(c == 150);
        a = -10;
        b = -15;
        c = a * b;
        assert(c == 150);
        a = 25;
        b = 10;
        c = a / b;
        assert(c == 2);
        a = -25;
        b = -10;
        c = a / b;
        assert(c == 2);
        a = 25;
        b = 10;
        c = a % b;
        assert(c == 5);
        a = -25;
        b = -10;
        c = a % b;
        assert(c == (-5));
        uint x;
        uint y;
        uint z;
        x = 0;
        y = 0;
        z = x ** y;
        assert(z == 1);
        x = 5;
        y = 8;
        z = x ** y;
        assert(z == 390625);
        x = 2;
        y = 64;
        z = x ** y;
        assert(z == 18446744073709551616);
    }

    function testBitwiseOperators() public {
        int a;
        int b;
        int c;
        a = 2;
        b = 5;
        c = a << b;
        assert(c == 64);
        a = 2;
        b = 100;
        c = a << b;
        assert(c == 2535301200456458802993406410752);
        a = 126;
        b = 3;
        c = a >> b;
        assert(c == 15);
        a = 2535301200456458802993406410752;
        b = 100;
        c = a >> b;
        assert(c == 2);
        a = 11;
        b = 116;
        c = a | b;
        assert(c == 127);
        a = -11;
        b = -116;
        c = a | b;
        assert(c == (-3));
        a = 10;
        b = 3;
        c = a & b;
        assert(c == 2);
        a = -10;
        b = -3;
        c = a & b;
        assert(c == (-12));
        a = 8;
        b = 10;
        c = a ^ b;
        assert(c == 2);
        a = -8;
        b = -10;
        c = a ^ b;
        assert(c == 14);
    }

    function testLogicOperators() public {
        int a;
        int b;
        bool c;
        a = 2;
        b = 1;
        c = a < b;
        assert(c == false);
        a = 1;
        b = 2;
        c = a < b;
        assert(c == true);
        a = 2;
        b = 2;
        c = a < b;
        assert(c == false);
        a = 2;
        b = 1;
        c = a > b;
        assert(c == true);
        a = 1;
        b = 2;
        c = a > b;
        assert(c == false);
        a = 2;
        b = 2;
        c = a > b;
        assert(c == false);
        a = 2;
        b = 1;
        c = a >= b;
        assert(c == true);
        a = 1;
        b = 2;
        c = a >= b;
        assert(c == false);
        a = 2;
        b = 2;
        c = a >= b;
        assert(c == true);
        a = 2;
        b = 1;
        c = a <= b;
        assert(c == false);
        a = 1;
        b = 2;
        c = a <= b;
        assert(c == true);
        a = 2;
        b = 2;
        c = a <= b;
        assert(c == true);
        a = 2;
        b = 1;
        c = a == b;
        assert(c == false);
        a = 2;
        b = 2;
        c = a == b;
        assert(c == true);
        a = 2;
        b = 1;
        c = a != b;
        assert(c == true);
        a = 2;
        b = 2;
        c = a != b;
        assert(c == false);
        bool x;
        bool y;
        bool z;
        x = true;
        y = true;
        z = x && y;
        assert(z == true);
        x = true;
        y = false;
        z = x && y;
        assert(z == false);
        x = false;
        y = false;
        z = x && y;
        assert(z == false);
        x = true;
        y = true;
        z = x || y;
        assert(z == true);
        x = false;
        y = true;
        z = x || y;
        assert(z == true);
        x = false;
        y = false;
        z = x || y;
        assert(z == false);
    }

    function testOperatorsOnBitTypes() public {
        assert(uint16(bytes2(0x0001)) == 1);
        assert(bytes2(0xff00) == bytes2(0xff00));
        assert(bytes2(0xff00) == byte(0xff));
        assert(bytes2(0xff00) > bytes1(0xee));
        assert(bytes2(0xff00) > 0xeeff);
        assert(bytes2(0xcc00) < byte(0xee));
        assert(bytes2(0xffff) != bytes2(0xcccc));
        assert(bytes2(0xffff) != 0xcccc);
        assert(byte(0x30) < "A");
        assert(bytes2(0xFFFF) > "AZ");
        assert(bytes2(0xFFFF) > "A");
        assert(bytes3(0x414243) >= "ABC");
        assert(bytes3(0x414244) >= "ABC");
        assert(bytes3(0x414243) == "ABC");
        assert(bytes3(0x414243) <= "ABC");
        assert(bytes3(0x414242) <= "ABC");
        assert(bytes3(0x414245) != "ABC");
        assert((bytes4(0x01010101) | bytes4(0x10101010)) == 0x11111111);
        assert((bytes4(0x01111101) & bytes4(0x11111111)) == 0x01111101);
        assert((bytes4(0x00000001) << 16) == 0x00010000);
        assert((bytes4(0x10000000) >> 16) == 0x00001000);
        assert((bytes4(0x00011110) ^ bytes4(0x00101101)) == 0x00110011);
        assert((~bytes4(0xF0FF000F)) == 0x0f00fff0);
        assert((~bytes4(0xFFFFFFFF)) == 0);
        assert((~bytes4(0x00000000)) == 0xFFFFFFFF);
    }
}

contract __IRTest__ {
    function main() public {
        OperatorsBinary __this__ = new OperatorsBinary();
        __testCase1238__(__this__);
        __testCase1252__(__this__);
        __testCase1266__(__this__);
        __testCase1280__(__this__);
    }

    function __testCase1238__(OperatorsBinary __this__) internal {
        __this__.testArithmeticOperators();
    }

    function __testCase1252__(OperatorsBinary __this__) internal {
        __this__.testBitwiseOperators();
    }

    function __testCase1266__(OperatorsBinary __this__) internal {
        __this__.testLogicOperators();
    }

    function __testCase1280__(OperatorsBinary __this__) internal {
        __this__.testOperatorsOnBitTypes();
    }
}