pragma solidity ^0.4.26;

contract Foo {
    uint public a;

    constructor() public {
        a = 42;
    }
}

contract ArrayTypesDesugaring {
    function normalArrays() public {
        int[] memory a = new int[](1);
        assert(a.length == 1);
        assert(a[0] == 0);
        bytes[] memory b = new bytes[](1);
        assert(b.length == 1);
        assert(b[0].length == 0);
        string[] memory c = new string[](1);
        assert(c.length == 1);
        uint[6][] memory d = new uint[6][](1);
        assert((d.length == 1) && (d[0].length == 6));
    }

    function contractArrays() public {
        Foo[] memory x = new Foo[](1);
        assert(x.length == 1);
    }

    function main() public {
        normalArrays();
        contractArrays();
    }
}

contract __IRTest__ {
    function main() public {
        ArrayTypesDesugaring __this__ = new ArrayTypesDesugaring();
        __testCase158__(__this__);
    }

    function __testCase158__(ArrayTypesDesugaring __this__) internal {
        __this__.main();
    }
}