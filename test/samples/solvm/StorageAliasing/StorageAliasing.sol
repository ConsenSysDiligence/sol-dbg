pragma solidity ^0.4.24;

contract StorageAliasing {
    struct ArrStruct {
        uint[4] arr;
    }

    struct Foo {
        uint x;
    }

    struct Inner {
        uint x;
    }

    struct Outer {
        uint y;
        Inner inner;
    }

    uint[4] internal a1;
    uint[4] internal a2;
    ArrStruct internal s1;
    ArrStruct internal s2;
    mapping(uint => Foo) internal m;
    Foo internal f;
    Outer internal os1;
    Outer internal os2;

    function arrays() public {
        uint[4] storage x = a1;
        uint[4] storage y = a2;
        assert((((x[0] == 0) && (x[1] == 0)) && (x[2] == 0)) && (x[3] == 0));
        assert((((y[0] == 0) && (y[1] == 0)) && (y[2] == 0)) && (y[3] == 0));
        x[0] = 42;
        assert((((x[0] == 42) && (y[0] == 0)) && (a1[0] == 42)) && (a2[0] == 0));
        x = y;
        assert(((x[0] == 0) && (y[0] == 0)) && (a1[0] == 42));
        x[0] = 43;
        assert(x[0] == y[0]);
        assert((((x[0] == 43) && (y[0] == 43)) && (a1[0] == 42)) && (a2[0] == 43));
    }

    function arraysInStructs() public {
        ArrStruct storage x = s1;
        ArrStruct storage y = s2;
        assert((((x.arr[0] == 0) && (x.arr[1] == 0)) && (x.arr[2] == 0)) && (x.arr[3] == 0));
        assert((((y.arr[0] == 0) && (y.arr[1] == 0)) && (y.arr[2] == 0)) && (y.arr[3] == 0));
        x.arr[0] = 42;
        assert((((x.arr[0] == 42) && (s1.arr[0] == 42)) && (y.arr[0] == 0)) && (s2.arr[0] == 0));
        x.arr = y.arr;
        assert((((x.arr[0] == 0) && (s1.arr[0] == 0)) && (y.arr[0] == 0)) && (s2.arr[0] == 0));
        x.arr[0] = 43;
        assert(x.arr[0] != y.arr[0]);
        assert((((x.arr[0] == 43) && (s1.arr[0] == 43)) && (y.arr[0] == 0)) && (s2.arr[0] == 0));
    }

    function maps() public {
        m[0] = Foo({x: 1});
        m[1] = Foo({x: 2});
        m[2] = m[0];
        assert((m[0].x == 1) && (m[2].x == 1));
        m[2].x = 3;
        assert(m[2].x != m[0].x);
        assert((m[2].x == 3) && (m[0].x == 1));
        Foo memory b;
        b.x = 43;
        m[0] = b;
        assert((m[0].x == 43) && (b.x == 43));
        m[0].x = 42;
        assert(m[0].x != b.x);
        assert((m[0].x == 42) && (b.x == 43));
        Foo storage b1 = f;
        b1.x = 44;
        assert((b1.x == 44) && (f.x == 44));
        m[0] = b1;
        assert(((m[0].x == 44) && (b1.x == 44)) && (f.x == 44));
        m[0].x = 45;
        assert(m[0].x != b1.x);
        assert(((m[0].x == 45) && (b1.x == 44)) && (f.x == 44));
    }

    function structInStructCopy() public {
        os1.y = 1;
        os1.inner.x = 42;
        os2.y = 2;
        os2.inner.x = 43;
        os1.inner = os2.inner;
        assert((os1.inner.x == 43) && (os2.inner.x == 43));
        os1.inner.x = 50;
        assert((os1.inner.x == 50) && (os2.inner.x == 43));
    }
}

contract __IRTest__ {
    function main() public {
        StorageAliasing __this__ = new StorageAliasing();
        __testCase716__(__this__);
        __testCase730__(__this__);
        __testCase744__(__this__);
        __testCase758__(__this__);
    }

    function __testCase716__(StorageAliasing __this__) internal {
        __this__.arrays();
    }

    function __testCase730__(StorageAliasing __this__) internal {
        __this__.arraysInStructs();
    }

    function __testCase744__(StorageAliasing __this__) internal {
        __this__.maps();
    }

    function __testCase758__(StorageAliasing __this__) internal {
        __this__.structInStructCopy();
    }
}