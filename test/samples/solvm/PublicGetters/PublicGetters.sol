pragma solidity 0.5.10;

contract PublicGetters {
    struct Foo {
        int8 a;
        uint32 b;
        string c;
        int64 d;
    }

    int public a = -150;
    uint public b = 200;
    string public c = "test";
    byte public d = 0xFA;
    bytes4 public e = 0x010F01DD;
    Foo public f = Foo(-1, 2, "abc", 4);
    int16[] public g = [int16(5), 9, -17];
    int[][][] public h;

    function testElementaryUnchanged() public view {
        assert(a == (-150));
        assert(a == this.a());
        assert(b == 200);
        assert(b == this.b());
        bytes memory bC = bytes(this.c());
        assert(bC[0] == byte("t"));
        assert(bC[1] == byte("e"));
        assert(bC[2] == byte("s"));
        assert(bC[3] == byte("t"));
        assert(d == 0xFA);
        assert(d == this.d());
        assert(e == 0x010F01DD);
        assert(e == this.e());
    }

    function testElementaryChanged() public {
        a = -100;
        assert(this.a() == (-100));
        b = 150;
        assert(this.b() == 150);
        c = "hWnd";
        bytes memory bC = bytes(this.c());
        assert(bC[0] == byte("h"));
        assert(bC[1] == byte("W"));
        assert(bC[2] == byte("n"));
        assert(bC[3] == byte("d"));
        d = 0xAF;
        assert(this.d() == 0xAF);
        e = 0xAABBCCDD;
        assert(this.e() == 0xAABBCCDD);
    }

    function testStructUnchanged() public view {
        assert(f.a == (-1));
        assert(f.b == 2);
        bytes memory bFc = bytes(f.c);
        assert(bFc[0] == byte("a"));
        assert(bFc[1] == byte("b"));
        assert(bFc[2] == byte("c"));
        assert(f.d == 4);
        (int8 vFa, uint32 vFb, string memory vFc, int64 vFd) = this.f();
        bytes memory bVFc = bytes(vFc);
        assert(vFa == f.a);
        assert(vFb == f.b);
        assert(bFc[0] == bVFc[0]);
        assert(bFc[1] == bVFc[1]);
        assert(bFc[2] == bVFc[2]);
        assert(vFd == f.d);
    }

    function testStructChanged() public {
        f.a = -5;
        f.b = 5;
        f.c = "xyz";
        f.d = 25;
        (int8 vFa, uint32 vFb, string memory vFc, int64 vFd) = this.f();
        bytes memory bVFc = bytes(vFc);
        assert(vFa == (-5));
        assert(vFb == 5);
        assert(bVFc[0] == byte("x"));
        assert(bVFc[1] == byte("y"));
        assert(bVFc[2] == byte("z"));
        assert(vFd == 25);
    }

    function testArrayOneDim() public {
        assert(g[0] == 5);
        assert(g[1] == 9);
        assert(g[2] == (-17));
        assert(this.g(0) == 5);
        assert(this.g(1) == 9);
        assert(this.g(2) == (-17));
        g[0] = 7;
        assert(this.g(0) == 7);
        g[1] = -100;
        assert(this.g(1) == (-100));
        g[2] = 89;
        assert(this.g(2) == 89);
    }
}

contract __IRTest__ {
    function main() public {
        PublicGetters __this__ = new PublicGetters();
        __testCase612__(__this__);
        __testCase626__(__this__);
        __testCase640__(__this__);
        __testCase654__(__this__);
        __testCase668__(__this__);
    }

    function __testCase612__(PublicGetters __this__) internal {
        __this__.testElementaryUnchanged();
    }

    function __testCase626__(PublicGetters __this__) internal {
        __this__.testElementaryChanged();
    }

    function __testCase640__(PublicGetters __this__) internal {
        __this__.testStructUnchanged();
    }

    function __testCase654__(PublicGetters __this__) internal {
        __this__.testStructChanged();
    }

    function __testCase668__(PublicGetters __this__) internal {
        __this__.testArrayOneDim();
    }
}