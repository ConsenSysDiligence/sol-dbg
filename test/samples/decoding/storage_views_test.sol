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