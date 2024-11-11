pragma solidity ^0.5.0;

contract MapWithStringKeys {
    mapping(string => int) internal x;
    mapping(bytes => uint) internal z;
    string internal y;
    bytes internal b;

    constructor() public {
        y = "test";
        b = bytes(y);
    }

    function useCase(string memory str) public view returns (int) {
        return x[str];
    }

    function main() public {
        x[y] = 1;
        string memory foo = "foo";
        x[foo] = 2;
        x["boo"] = 3;
        assert(x[y] == 1);
        assert(x[foo] == 2);
        assert(x["boo"] == 3);
        z[b] = 4;
        bytes memory bar = b;
        assert(z[bar] == 4);
    }
}

contract __IRTest__ {
    function main() public {
        MapWithStringKeys __this__ = new MapWithStringKeys();
        __testCase122__(__this__);
    }

    function __testCase122__(MapWithStringKeys __this__) internal {
        __this__.main();
    }
}