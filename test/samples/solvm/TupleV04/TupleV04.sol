pragma solidity 0.4.25;

contract Tuples {
    function declarationTuples() public {
        (uint8 r, uint16 t, string memory x) = (1, 2, "abc");
        assert(r == 1);
        assert(t == 2);
        (uint8 a, , string memory c, ) = (1, 2, "abc", 4);
        assert(a == 1);
    }

    function nestedTuples() public {
        (uint8 r, uint16 t, string memory x) = (1, 2, "abc");
        (uint8 a, , string memory c, ) = (1, 2, "abc", 4);
        assert(r == 1);
        assert(t == 2);
        assert(a == 1);
    }

    function missingTupleFields() public {
        uint8 r;
        uint16 t;
        string memory x;
        address f;
        (r, t, (x, f)) = (3, 4, ("abc", address(0x0)));
        assert(r == 3);
        assert(t == 4);
        assert(f == address(0x0));
        (, t, (, f)) = (5, 6, ("def", address(0x1)));
        assert(t == 6);
        assert(f == address(0x1));
        (r, t, , f) = (7, 8, ("xyz", 0x42), address(0x0));
        assert(r == 7);
        assert(t == 8);
        assert(f == address(0x0));
        uint a = 0;
        (uint b, uint c, ) = (1, 2, a = 42);
        assert(b == 1);
        assert(c == 2);
        assert(a == 42);
    }
}

contract __IRTest__ {
    function main() public {
        Tuples __this__ = new Tuples();
        __testCase250__(__this__);
        __testCase264__(__this__);
        __testCase278__(__this__);
    }

    function __testCase250__(Tuples __this__) internal {
        __this__.declarationTuples();
    }

    function __testCase264__(Tuples __this__) internal {
        __this__.nestedTuples();
    }

    function __testCase278__(Tuples __this__) internal {
        __this__.missingTupleFields();
    }
}