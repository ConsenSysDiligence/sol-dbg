pragma solidity >=0.4.18<0.5.0;

contract UntypedVars {
    struct Foo {
        uint f1;
    }

    bytes internal sB;
    Foo internal sFoo;

    function scalar() private pure returns (string memory) {
        return "some string";
    }

    function test_simple() public returns (uint8) {
        var u8 = 123;
        assert((u8 - uint8(124)) == 255);
        var u8_1 = 255;
        assert((u8_1 + uint8(1)) == 0);
        var i8 = -1;
        assert((i8 - int8(128)) == 127);
        var i8_1 = -128;
        assert((i8_1 - int8(1)) == 127);
        var u16 = 256;
        assert((u16 - uint16(257)) == 65535);
        var u16_1 = 65535;
        assert((u16_1 + uint16(1)) == 0);
        var i16 = -129;
        assert((i16 - int16(32640)) == 32767);
        var i16_1 = -32768;
        assert((i16_1 - int16(1)) == 32767);
        var (b, c, d) = (0x00, "test", -15);
        assert((b - uint8(1)) == 255);
        assert(bytes(c)[0] == byte("t"));
        assert(bytes(c)[1] == byte("e"));
        assert(bytes(c)[2] == byte("s"));
        assert(bytes(c)[3] == byte("t"));
        assert((d - int8(114)) == 127);
        var u8_2 = 231584178474632390847141970017375815706539969331281128078915168015826259279915231584178474632390847141970017375815706539969331281128078915168015826259279915 - 231584178474632390847141970017375815706539969331281128078915168015826259279915231584178474632390847141970017375815706539969331281128078915168015826259279873;
        assert(u8_2 == 42);
        u8 = b;
        i8 = d;
        sB = bytes(c);
        sB[0] = 0x32;
        assert(bytes(c)[0] == 116);
        bytes memory mB = bytes(c);
        mB[0] = 0x32;
        assert(bytes(c)[0] == 0x32);
        var e = scalar();
        assert(bytes(e)[0] == byte("s"));
        assert(bytes(e)[1] == byte("o"));
        assert(bytes(e)[2] == byte("m"));
        assert(bytes(e)[3] == byte("e"));
        assert(bytes(e)[10] == byte("g"));
    }

    function test_complex() public {
        var u8_arr = [1, 2, 255];
        var i8_arr = [-127, 127];
        var strct = Foo(1);
        sFoo = strct;
        sFoo.f1 = 42;
        assert(strct.f1 == 1);
        Foo memory mFoo = strct;
        mFoo.f1 = 43;
        assert(strct.f1 == 43);
        var u8_2d = [[1, 2], [0, 255]];
        var i8_2d = [[-1, 0], [-1, 127]];
        var u8_3 = [1, 1, 1];
        u8_3[0] = 2;
        uint8 x = 10;
        var u16_4 = [x, 1, 256];
    }

    function main() public {
        test_simple();
        test_complex();
    }
}

contract __IRTest__ {
    function main() public {
        UntypedVars __this__ = new UntypedVars();
        __testCase456__(__this__);
    }

    function __testCase456__(UntypedVars __this__) internal {
        __this__.main();
    }
}