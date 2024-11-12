pragma solidity ^0.4.24;

contract MemoryAliasing {
    struct Foo {
        uint x;
        uint[4] arr;
    }

    struct Bar {
        uint x;
    }

    struct Boo {
        uint y;
        Bar b;
    }

    struct MapStruct {
        mapping(uint => uint) m;
    }

    Foo internal sa;
    Foo internal sb;
    Foo internal sfoo;
    mapping(uint => uint) internal m;
    mapping(uint => uint) internal m1;
    MapStruct internal ms;

    function primitiveValuesDontAlias() public {
        uint x = 1;
        uint y = 2;
        x = y;
        x = 3;
        assert(y == 2);
    }

    function arrays() public {
        uint[4] memory a = [uint(1), 2, 3, 4];
        uint[4] memory b = a;
        b[0] = 42;
        assert(a[0] == b[0]);
        assert(a[0] == 42);
    }

    function nestedArrays() public {
        uint[3][2] memory a = [[uint(1), 2, 3], [uint(4), 5, 6]];
        a[0] = a[1];
        assert((a[0][0] == 4) && (a[1][0] == 4));
        a[0][0] = 42;
        assert((a[0][0] == 42) && (a[1][0] == 42));
    }

    function structs() public {
        Foo memory x1;
        Foo memory x2;
        assert((x1.x == 0) && (x2.x == 0));
        x1.x = 1;
        assert((x1.x == 1) && (x2.x == 0));
        Foo memory a;
        a.x = 43;
        a.arr = [uint(1), 2, 3, 4];
        Foo memory b = a;
        b.arr[0] = 42;
        assert(a.arr[0] == b.arr[0]);
        assert(a.arr[0] == 42);
        b.x = 42;
        assert(a.x == b.x);
        assert(a.x == 42);
    }

    function arraysInMemoryStructs() public returns (uint[4] memory, uint[4] memory) {
        Foo memory a;
        a.arr = [uint(1), 2, 3, 4];
        Foo memory b;
        b.arr = [uint(5), 5, 6, 8];
        assert(b.arr[0] == 5);
        b.arr = a.arr;
        assert((((b.arr[0] == 1) && (b.arr[1] == 2)) && (b.arr[2] == 3)) && (b.arr[3] == 4));
        b.arr[0] = 42;
        assert(b.arr[0] == a.arr[0]);
        assert(a.arr[0] == 42);
        a.arr[1] = 80;
        assert(b.arr[1] == a.arr[1]);
        assert(b.arr[1] == 80);
        return (a.arr, b.arr);
    }

    function structInMemoryStructs() public {
        Boo memory a;
        a.y = 1;
        a.b.x = 2;
        Boo memory b;
        b.y = 3;
        b.b.x = 4;
        assert(b.y == 3);
        assert(b.b.x == 4);
        b.b = a.b;
        assert(b.y == 3);
        assert(a.y == 1);
        assert(b.b.x == 2);
        b.b.x = 42;
        assert(a.b.x == b.b.x);
        assert(a.b.x == 42);
        a.b.x = 80;
        assert(a.b.x == b.b.x);
        assert(b.b.x == 80);
    }

    function structsInMemoryArrays() public {
        Boo[4] memory a;
        a[0].y = 1;
        a[0].b.x = 2;
        a[1].y = 3;
        a[1].b.x = 4;
        assert(a[0].y == 1);
        assert(a[0].b.x == 2);
        a[0] = a[1];
        assert(a[0].y == 3);
        assert(a[0].b.x == 4);
        a[0].y = 42;
        assert(a[0].y == a[1].y);
        assert(a[1].y == 42);
        assert(a[1].b.x == 4);
        assert(a[0].b.x == 4);
        a[1].b.x = 43;
        assert(a[1].b.x == a[0].b.x);
        assert(a[0].b.x == 43);
        Bar memory b;
        b.x = 123;
        a[0].b = b;
        assert(a[0].b.x == b.x);
        assert(a[0].b.x == 123);
        assert(a[1].b.x == 123);
        b.x = 112233;
        assert(a[0].b.x == b.x);
        assert(a[0].b.x == 112233);
        assert(a[1].b.x == 112233);
        a[2].y = 1;
        a[2].b.x = 2;
        a[3].y = 3;
        a[3].b.x = 4;
        a[2].b = a[3].b;
        assert(a[2].b.x == a[3].b.x);
        assert(a[2].y == 1);
        assert(a[3].y == 3);
        assert(a[2].b.x == 4);
        assert(a[3].b.x == 4);
        a[2].b.x = 42;
        assert(a[2].b.x == a[3].b.x);
        assert(a[3].b.x == 42);
    }

    function structReAssignment() public {
        Foo memory a;
        Foo memory b = a;
        a.x = 42;
        a.arr[0] = 43;
        assert((b.x == 42) && (b.arr[0] == 43));
        a = Foo({x: 1, arr: [uint(1), 2, 3, 4]});
        assert((a.x == 1) && (a.arr[0] == 1));
        assert((b.x == 42) && (b.arr[0] == 43));
    }

    function structReAssignmentFromStorage() public {
        Foo memory a;
        Foo memory b = a;
        a.x = 42;
        a.arr[0] = 43;
        assert((b.x == 42) && (b.arr[0] == 43));
        sfoo = Foo({x: 1, arr: [uint(1), 2, 3, 4]});
        a = sfoo;
        assert((a.x == 1) && (a.arr[0] == 1));
        assert((b.x == 42) && (b.arr[0] == 43));
        assert((sfoo.x == 1) && (sfoo.arr[0] == 1));
        a.x = 50;
        assert((a.x == 50) && (a.arr[0] == 1));
        assert((b.x == 42) && (b.arr[0] == 43));
        assert((sfoo.x == 1) && (sfoo.arr[0] == 1));
    }

    function copyMap() public {
        m[0] = 1;
        assert((m[0] == 1) && (ms.m[0] == 0));
    }
}

contract __IRTest__ {
    function main() public {
        MemoryAliasing __this__ = new MemoryAliasing();
        __testCase1229__(__this__);
        __testCase1243__(__this__);
        __testCase1257__(__this__);
        __testCase1271__(__this__);
        __testCase1285__(__this__);
        __testCase1369__(__this__);
        __testCase1383__(__this__);
        __testCase1397__(__this__);
        __testCase1411__(__this__);
    }

    function __testCase1229__(MemoryAliasing __this__) internal {
        __this__.primitiveValuesDontAlias();
    }

    function __testCase1243__(MemoryAliasing __this__) internal {
        __this__.arrays();
    }

    function __testCase1257__(MemoryAliasing __this__) internal {
        __this__.nestedArrays();
    }

    function __testCase1271__(MemoryAliasing __this__) internal {
        __this__.structs();
    }

    function __testCase1285__(MemoryAliasing __this__) internal {
        (uint256[4] memory ret_1285_0, uint256[4] memory ret_1285_1) = __this__.arraysInMemoryStructs();
        assert(keccak256(abi.encodePacked(ret_1285_0)) == keccak256(abi.encodePacked([uint256(42), uint256(80), uint256(3), uint256(4)])));
        assert(keccak256(abi.encodePacked(ret_1285_1)) == keccak256(abi.encodePacked([uint256(42), uint256(80), uint256(3), uint256(4)])));
    }

    function __testCase1369__(MemoryAliasing __this__) internal {
        __this__.structInMemoryStructs();
    }

    function __testCase1383__(MemoryAliasing __this__) internal {
        __this__.structsInMemoryArrays();
    }

    function __testCase1397__(MemoryAliasing __this__) internal {
        __this__.structReAssignment();
    }

    function __testCase1411__(MemoryAliasing __this__) internal {
        __this__.structReAssignmentFromStorage();
    }
}