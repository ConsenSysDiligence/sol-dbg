pragma solidity ^0.4.24;

contract StateVarInitializers {
    struct Bar {
        int32 x;
    }

    struct Foo {
        uint a;
        uint[3] b;
        Bar c;
    }

    uint internal a;
    uint internal b = 1;
    uint internal c = 1 + 3;
    uint internal d = ((c < 0) ? 1 : 2);
    uint internal e = b;
    uint internal f = g;
    uint internal g = 1;
    uint[] internal arr;
    uint[3] internal fArr;
    uint[3] internal fArr2 = [a, b, c];
    Foo internal st;
    Foo internal st1 = Foo(1, fArr2, Bar(int32(g)));

    constructor() public {
        assert(a == 0);
        assert(b == 1);
        assert(c == 4);
        assert(d == 2);
        assert(e == 1);
        assert(f == 0);
        assert(g == 1);
        assert(arr.length == 0);
        assert(fArr.length == 3);
        assert(((fArr[0] == 0) && (fArr[1] == 0)) && (fArr[2] == 0));
        assert(fArr2.length == 3);
        assert(((fArr2[0] == 0) && (fArr2[1] == 1)) && (fArr2[2] == 4));
        assert(st.a == 0);
        assert(st.b.length == 3);
        assert(((st.b[0] == 0) && (st.b[1] == 0)) && (st.b[2] == 0));
        assert(st.c.x == 0);
        assert(st1.a == 1);
        assert(st1.b.length == 3);
        assert(((st1.b[0] == 0) && (st1.b[1] == 1)) && (st1.b[2] == 4));
        assert(st1.c.x == 1);
    }
}

contract __IRTest__ {
    function main() public {
        StateVarInitializers __this__ = new StateVarInitializers();
    }
}