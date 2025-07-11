contract Foo {
    function bytesTest() public returns (uint) {
        bytes memory s = hex"0102030405060708090a";

        uint256 offset;

        assembly {
            offset := s
        }

        return offset;

    }

    struct SimpleTypes {
        int8 a;
        uint16 b;
        uint256 c;
        bool d;
        address e;
        bytes2 b1;
        bytes32 b2;
        E en;
    }

    struct ArrTypes {
        uint16[] a1;
        int128[4] a2;
    }

    struct S_static {
        int8 x;
        uint256 y;
        bool b;
        address addrs;
    }

    function fun_S_static() public returns (uint) {
        S_static memory s = S_static(
            -1,
            45678,
            true,
            0xcD6a42782d230D7c13A74ddec5dD140e55499Df9
        );

        uint256 offset;

        assembly {
            offset := s
        }

        return offset;
    }

    struct S1 {
        int8 x;
        uint256 y;
        bool b;
        address[] addrs;
    }

    function fun_S1() public returns (uint) {
        S1 memory s = S1(
            -1,
            45678,
            true,
            new address[](2)
        );

        s.addrs[1] = 0xcD6a42782d230D7c13A74ddec5dD140e55499Df9;

        uint256 offset;

        assembly {
            offset := s
        }

        return offset;
    }

    struct S_nested_static_static {
        int16 t;
        S_static s;
        bytes3 b;
    }

    function fun_S_nested_static_static() public returns (uint) {
        S_nested_static_static memory s = S_nested_static_static(
            -1,
            S_static(
                -1,
                45678,
                true,
                0xcD6a42782d230D7c13A74ddec5dD140e55499Df9
            ),
            0x040506
        );

        uint256 offset;

        assembly {
            offset := s
        }

        return offset;
    }

    struct S_nested_dynamic_static {
        int16[] t;
        S_static s;
        bytes3 b;
    }

    function fun_S_nested_dynamic_static() public returns (uint) {
        S_nested_dynamic_static memory s = S_nested_dynamic_static(
            new int16[](0),
            S_static(
                -1,
                45678,
                true,
                0xcD6a42782d230D7c13A74ddec5dD140e55499Df9
            ),
            0x070809
        );

        uint256 offset;

        assembly {
            offset := s
        }

        return offset;
    }

    struct S_nested_static_dynamic {
        int16 t;
        S1 s;
        bytes3 b;
    }

    function fun_S_nested_static_dynamic() public returns (uint) {
        S_nested_static_dynamic memory s = S_nested_static_dynamic(
            -1234,
            S1(
                -1,
                45678,
                true,
                new address[](2)
            ),
            0x020304
        );

        s.s.addrs[1] = 0xcD6a42782d230D7c13A74ddec5dD140e55499Df9;
        uint256 offset;

        assembly {
            offset := s
        }

        return offset;
    }

    struct S_struct_arr {
        int8 x;
        S1[] sArr;
    }

    function fun_S_struct_arr() public returns (uint) {
        S_struct_arr memory s = S_struct_arr(
            -128,
            new S1[](2)
        );

        s.sArr[0] = S1(
                    -1,
                    45678,
                    true,
                    new address[](2)
                );
        s.sArr[1] = S1(
                    -2,
                    45679,
                    false,
                    new address[](0)
                );
        s.sArr[0].addrs[1] = 0xcD6a42782d230D7c13A74ddec5dD140e55499Df9;
        uint256 offset;

        assembly {
            offset := s
        }

        return offset;
    }

    enum E {
        A,
        B,
        C
    }

    function arrTypes() public returns (uint) {
        ArrTypes memory at = ArrTypes(new uint16[](3), [int128(-1), -2, -3, -4]);

        at.a1[0] = 12;
        at.a1[1] = 13;
        at.a1[2] = 14;

        uint256 offset;

        assembly {
            offset := at
        }

        return offset;

    }

    function simpleTypes() public returns (uint) {
        SimpleTypes memory st = SimpleTypes(
            -1,
            65535,
            123456,
            true,
            0x5B38Da6a701c568545dCfcB03FcB875f56beddC4,
            0x0102,
            bytes32(uint256(0xabcdef)),
            E.B
        );

        uint256 offset;

        assembly {
            offset := st
        }

        return offset;
    }
}
