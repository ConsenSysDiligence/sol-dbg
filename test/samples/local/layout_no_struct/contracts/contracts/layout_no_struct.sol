pragma solidity 0.8.21;

struct Foo {
    int8 fa;
    int16 fb;
    string fc;
    int[] fd;
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
        f = Foo(-1, 1000, "dy", new int[](1));
        b = -1;
    }

    function ping() public {}
}
