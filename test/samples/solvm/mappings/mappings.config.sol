pragma solidity ^0.4.24;

contract Mappings {
    enum TestEnum { A, B, C }

    struct TestStructA {
        string memberX;
        int memberY;
        TestEnum memberZ;
    }

    struct TestStructB {
        TestStructA memberA;
        string memberB;
        int8 memberC;
        address memberD;
    }

    mapping(uint => uint) internal uintMap;
    mapping(uint => uint[]) internal uintArrMap;
    mapping(uint => TestStructB) internal uintStructMap;

    function mappings() public {
        mapping(uint => uint) m = uintMap;
        uint a = uintMap[1];
        uintMap[1] = 10;
        uint b = m[1];
        assert(b == 10);
        mapping(uint => uint[]) m1 = uintArrMap;
        m1[0] = [1, 2, 3];
        assert(uintArrMap[0][2] == 3);
        mapping(uint => TestStructB) m3 = uintStructMap;
        m3[1] = TestStructB(TestStructA("sup", 42, TestEnum.C), "dawg", 127, address(0x43));
        assert(uintStructMap[1].memberA.memberY == 42);
        assert(uintStructMap[1].memberC == 127);
        assert(bytes(uintStructMap[1].memberA.memberX).length == 3);
    }
}

contract __IRTest__ {
    function main() public {
        Mappings __this__ = new Mappings();
        __testCase165__(__this__);
    }

    function __testCase165__(Mappings __this__) internal {
        __this__.mappings();
    }
}