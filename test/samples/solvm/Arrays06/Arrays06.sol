pragma solidity ^0.6.0;

contract Arrays {
    uint[] internal data;

    function addOne(uint a) public returns (uint) {
        return a + 1;
    }

    function addOneTwice(uint a, uint b) public returns (uint, uint) {
        return (addOne(a), addOne(b));
    }

    function arrays() public returns (uint[] memory) {
        uint[] storage a = data;
        uint b;
        a.push(1);
        a.push(2);
        uint v = a.push() = 3;
        assert((((a[0] == 1) && (a[1] == 2)) && (a[2] == 3)) && (v == 3));
        a.pop();
        assert(((a[0] == 1) && (a[1] == 2)) && (a.length == 2));
        b = a.push() + 1;
        assert((((a[0] == 1) && (a[1] == 2)) && (a[2] == 0)) && (a.length == 3));
        assert(b == 1);
        a.pop();
        assert(((a[0] == 1) && (a[1] == 2)) && (a.length == 2));
        uint c = a.length;
        return a;
    }

    function tupleInlineArrayAssignment() public {
        uint[3] memory a;
        uint[3] memory b;
        (a, b) = ([uint(1), 2, 3], [uint(4), 5, 6]);
    }
}

contract __IRTest__ {
    function main() public {
        Arrays __this__ = new Arrays();
        __testCase244__(__this__);
        __testCase307__(__this__);
    }

    function __testCase244__(Arrays __this__) internal {
        uint256[] memory arr_lit_7;
        arr_lit_7 = new uint256[](2);
        arr_lit_7[0] = uint256(1);
        arr_lit_7[1] = uint256(2);
        uint256[] memory ret_244_0 = __this__.arrays();
        assert(keccak256(abi.encodePacked(ret_244_0)) == keccak256(abi.encodePacked(arr_lit_7)));
    }

    function __testCase307__(Arrays __this__) internal {
        __this__.tupleInlineArrayAssignment();
    }
}