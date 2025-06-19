pragma solidity 0.8.21;

struct Bar {
    int8 z;
}

struct Foo {
    int8 fa;
    int[] fd;
    Bar missing;
    int16 fb;
    string fc;
}

struct MapStruct {
    int8 msA;
    mapping(address => uint) b1;
}

contract Layout_map {
    uint8 a;
    Foo f;
    int8 b;

    constructor() public {
        a = 1;
        f = Foo(-1, new int[](1), Bar(-1), 1000, "dy");
        b = -1;
    }

    function ping() public {}
}
