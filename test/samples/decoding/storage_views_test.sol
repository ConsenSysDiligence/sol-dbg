pragma solidity ^0.8.29;

contract Foo {
    uint64 x = 123456;
    int8 y = -128;
    bool b = true;
    address a = 0xcD6a42782d230D7c13A74ddec5dD140e55499Df9;
    bool b1 = false;
    bytes5 bts = 0x0405060708;

    function foo() public {}
}

struct S {
    int32 x;
    bool y;
}

contract A {
    uint a = 5678;
    uint128 transient b;
    uint constant c = 10;
    uint immutable d = 12;
}

contract B {
    uint8[] e = [1,2,3,4];
    mapping(uint => S) f;
    uint16 g = 34;
    uint16 h = 45;
    bytes16 transient i;
    S s;
    int8 k = -127;
}

contract C is A, B layout at 42 {
    bytes21 l;
    uint8[12] m = [1,2,3,4,5,6,7,8,9,10];
    bytes5[8] n;
    bytes5 o = 0x0102030405;

    constructor() {
        f[0] = S(1, true);
        f[1] = S(2, false);

        s.x = 13;
        s.y = true;

        l = bytes21(uint168(42));
        n[4] = bytes5(uint40(0x23));
        o = bytes5(uint40(0x35));
    }

    function foo() public {}
}

contract MoreStructs {
    enum E {
        A,
        B,
        C
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

    struct S1 {
        int8 x;
        uint256 y;
        bool b;
        address[] addrs;
    }

    struct S_nested_static_static {
        int16 t;
        S_static s;
        bytes3 b;
    }


    struct S_nested_dynamic_static {
        int16[] t;
        S_static s;
        bytes3 b;
    }

    struct S_nested_static_dynamic {
        int16 t;
        S1 s;
        bytes3 b;
    }

    struct S_struct_arr {
        int8 x;
        S1[] sArr;
    }

    SimpleTypes st;
    S_static s_static;
    S1 s1;
    S_nested_static_static s_nested_static_static;
    S_nested_dynamic_static s_nested_dynamic_static;
    S_nested_static_dynamic s_nested_static_dynamic;
    S_struct_arr s_struct_arr;
    ArrTypes at;

    constructor() {
        st = SimpleTypes(
            -1,
            65535,
            123456,
            true,
            0x5B38Da6a701c568545dCfcB03FcB875f56beddC4,
            0x0102,
            bytes32(uint256(0xabcdef)),
            E.B
        );

        s_static = S_static(
            -1,
            45678,
            true,
            0xcD6a42782d230D7c13A74ddec5dD140e55499Df9
        );

        s1 = S1(
            -1,
            45678,
            true,
            new address[](2)
        );

        s1.addrs[1] = 0xcD6a42782d230D7c13A74ddec5dD140e55499Df9;

        s_nested_static_static = S_nested_static_static(
            -1,
            S_static(
                -1,
                45678,
                true,
                0xcD6a42782d230D7c13A74ddec5dD140e55499Df9
            ),
            0x040506
        );

        s_nested_dynamic_static = S_nested_dynamic_static(
            new int16[](0),
            S_static(
                -1,
                45678,
                true,
                0xcD6a42782d230D7c13A74ddec5dD140e55499Df9
            ),
            0x070809
        );

        s_nested_static_dynamic = S_nested_static_dynamic(
            -1234,
            S1(
                -1,
                45678,
                true,
                new address[](2)
            ),
            0x020304
        );

        s_nested_static_dynamic.s.addrs[1] = 0xcD6a42782d230D7c13A74ddec5dD140e55499Df9;

        s_struct_arr= S_struct_arr(
            -128,
            new S1[](2)
        );

        s_struct_arr.sArr[0] = S1(
                    -1,
                    45678,
                    true,
                    new address[](2)
                );
        s_struct_arr.sArr[1] = S1(
                    -2,
                    45679,
                    false,
                    new address[](0)
                );
        s_struct_arr.sArr[0].addrs[1] = 0xcD6a42782d230D7c13A74ddec5dD140e55499Df9;
        at = ArrTypes(new uint16[](3), [int128(-1), -2, -3, -4]);

        at.a1[0] = 12;
        at.a1[1] = 13;
        at.a1[2] = 14;
    }

    function foo() public {}
}