pragma solidity ^0.8.0;

contract Calldata08 {
    struct Point {
        uint x;
        uint y;
    }

    bytes internal x;
    uint[][] internal a = [[1, 2, 3], [4, 5, 6], [7, 8, 9]];

    function bytesSlice(bytes calldata v) external returns (bytes memory, bytes memory) {
        bytes memory m = v[0:10];
        x = v[0:10];
        return (x, m);
    }

    function swap(Point calldata p) external returns (uint, uint) {
        Point memory m = p;
        return (m.x, m.y);
    }

    function nestedArrays(uint[][] calldata arg) external returns (uint) {
        uint[][] memory arr = arg;
        uint sum = 0;
        for (uint i = 0; i < arr.length; i++) {
            for (uint j = 0; j < arr[i].length; j++) {
                sum += arr[i][j];
            }
        }
        return sum;
    }

    function main() public {
        bytes memory arg = hex"0001020304050607080910";
        (bytes memory b1, bytes memory b2) = this.bytesSlice(arg);
        assert(keccak256(b1) == keccak256(hex"00010203040506070809"));
        assert(keccak256(b2) == keccak256(hex"00010203040506070809"));
        Point memory p = Point(42, 43);
        (uint u1, uint u2) = this.swap(p);
        assert((u1 == 42) && (u2 == 43));
        uint sum = this.nestedArrays(a);
        assert(sum == 45);
    }
}

contract __IRTest__ {
    function main() public {
        Calldata08 __this__ = new Calldata08();
        __testCase227__(__this__);
    }

    function __testCase227__(Calldata08 __this__) internal {
        __this__.main();
    }
}