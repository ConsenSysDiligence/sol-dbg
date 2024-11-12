pragma solidity ^0.6.0;

struct Foo {
    bool b;
    uint x;
    int[] y;
}

contract Boo {
    constructor(uint a) public {}
}

contract NoArgPush {
    uint[] internal arr;
    string[] internal arr1;
    Foo[] internal arr2;
    Boo[] internal arr3;

    function main() public {
        arr.push();
        assert((arr.length == 1) && (arr[0] == 0));
        arr1.push();
        assert((arr1.length == 1) && (keccak256(abi.encodePacked(arr1[0])) == keccak256(abi.encodePacked(""))));
        arr2.push();
        assert(arr2.length == 1);
        assert(((arr2[0].b == false) && (arr2[0].x == 0)) && (arr2[0].y.length == 0));
        arr3.push();
        assert(arr3.length == 1);
    }
}

contract __IRTest__ {
    function main() public {
        NoArgPush __this__ = new NoArgPush();
        __testCase141__(__this__);
    }

    function __testCase141__(NoArgPush __this__) internal {
        __this__.main();
    }
}