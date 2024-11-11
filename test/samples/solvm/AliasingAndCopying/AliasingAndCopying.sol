pragma solidity ^0.4.24;

contract AliasingAndCopying {
    struct A {
        uint a;
    }

    struct B {
        uint b;
        A a;
    }

    struct TestStructC {
        string memberX;
        int memberY;
        int[1] memberZ;
    }

    uint[3] internal a1;
    uint[3] internal a2;
    B internal b1 = B(1, A(2));
    B internal b2 = B(3, A(4));
    TestStructC internal e;
    uint[3][3] internal store_b;
    uint[3] internal store_c;

    function arrayStorageToMemory() public returns (uint[3], uint[3]) {
        a1 = [1, 2, 3];
        uint[3] memory b;
        b = a1;
        b[0] = 4;
        return (a1, b);
    }

    function arrayMemoryToStorage() public returns (uint[3], uint[3]) {
        uint256[3] memory b = [uint(1), 2, 3];
        a1 = b;
        a1[0] = 4;
        return (a1, b);
    }

    function arrayMemoryToMemory() public returns (uint[3], uint[3]) {
        uint256[3] memory b = [uint(1), 2, 3];
        uint256[3] memory c;
        c = b;
        c[0] = 4;
        return (b, c);
    }

    function arrayStorageToStorage() public returns (uint[3], uint[3]) {
        a1 = [1, 2, 3];
        a2 = a1;
        a2[0] = 4;
        return (a1, a2);
    }

    function twoDimArrayMemoryToMemory() public returns (uint[3] memory, uint[3] memory) {
        uint[3][3] memory b;
        b[0] = [uint(1), 1, 1];
        b[1] = [uint(2), 2, 2];
        b[2] = [uint(3), 3, 3];
        uint[3] memory c;
        c = [uint(4), 4, 4];
        b[0] = c;
        b[0][0] = 42;
        return (b[0], c);
    }

    function twoDimArrayStorageToStorage() public returns (uint[3] memory, uint[3] memory) {
        uint[3][3] storage b = store_b;
        b[0] = [uint(1), 1, 1];
        b[1] = [uint(2), 2, 2];
        b[2] = [uint(3), 3, 3];
        uint[3] storage c = store_c;
        c[0] = 4;
        c[1] = 4;
        c[2] = 4;
        b[0] = c;
        b[0][0] = 42;
        return (b[0], c);
    }

    function structStorageToStorage() public returns (uint, uint) {
        B storage b3 = b1;
        b3.b = 5;
        return (b3.b, b1.b);
    }

    function structStorageToMemory() public returns (uint, uint) {
        b1.b = 1;
        B memory b3 = b1;
        b3.b = 5;
        return (b3.b, b1.b);
    }

    function structMemoryToStorage() public returns (uint, uint) {
        B memory b3 = B(1, A(2));
        b1 = b3;
        b1.b = 5;
        return (b1.b, b3.b);
    }

    function structMemoryToMemory() public returns (uint, uint) {
        B memory b3 = B(1, A(2));
        B memory b4;
        b4 = b3;
        b4.b = 5;
        return (b4.b, b3.b);
    }

    function copyNestedStruct() public returns (uint, uint) {
        b1.a = b2.a;
        b1.a.a = 6;
        return (b1.a.a, b2.a.a);
    }

    function structOperations() public returns (int, int) {
        int[1] memory y;
        y[0] = 1;
        TestStructC memory z = TestStructC("x", 2, y);
        e = z;
        z.memberZ[0] = 2;
        return (e.memberZ[0], z.memberZ[0]);
    }
}

contract __IRTest__ {
    function main() public {
        AliasingAndCopying __this__ = new AliasingAndCopying();
        __testCase614__(__this__);
        __testCase692__(__this__);
        __testCase770__(__this__);
        __testCase848__(__this__);
        __testCase926__(__this__);
        __testCase1004__(__this__);
        __testCase1082__(__this__);
        __testCase1118__(__this__);
        __testCase1154__(__this__);
        __testCase1190__(__this__);
        __testCase1226__(__this__);
        __testCase1262__(__this__);
    }

    function __testCase614__(AliasingAndCopying __this__) internal {
        (uint256[3] memory ret_614_0, uint256[3] memory ret_614_1) = __this__.arrayStorageToMemory();
        assert(keccak256(abi.encodePacked(ret_614_0)) == keccak256(abi.encodePacked([uint256(1), uint256(2), uint256(3)])));
        assert(keccak256(abi.encodePacked(ret_614_1)) == keccak256(abi.encodePacked([uint256(4), uint256(2), uint256(3)])));
    }

    function __testCase692__(AliasingAndCopying __this__) internal {
        (uint256[3] memory ret_692_0, uint256[3] memory ret_692_1) = __this__.arrayMemoryToStorage();
        assert(keccak256(abi.encodePacked(ret_692_0)) == keccak256(abi.encodePacked([uint256(4), uint256(2), uint256(3)])));
        assert(keccak256(abi.encodePacked(ret_692_1)) == keccak256(abi.encodePacked([uint256(1), uint256(2), uint256(3)])));
    }

    function __testCase770__(AliasingAndCopying __this__) internal {
        (uint256[3] memory ret_770_0, uint256[3] memory ret_770_1) = __this__.arrayMemoryToMemory();
        assert(keccak256(abi.encodePacked(ret_770_0)) == keccak256(abi.encodePacked([uint256(4), uint256(2), uint256(3)])));
        assert(keccak256(abi.encodePacked(ret_770_1)) == keccak256(abi.encodePacked([uint256(4), uint256(2), uint256(3)])));
    }

    function __testCase848__(AliasingAndCopying __this__) internal {
        (uint256[3] memory ret_848_0, uint256[3] memory ret_848_1) = __this__.arrayStorageToStorage();
        assert(keccak256(abi.encodePacked(ret_848_0)) == keccak256(abi.encodePacked([uint256(1), uint256(2), uint256(3)])));
        assert(keccak256(abi.encodePacked(ret_848_1)) == keccak256(abi.encodePacked([uint256(4), uint256(2), uint256(3)])));
    }

    function __testCase926__(AliasingAndCopying __this__) internal {
        (uint256[3] memory ret_926_0, uint256[3] memory ret_926_1) = __this__.twoDimArrayMemoryToMemory();
        assert(keccak256(abi.encodePacked(ret_926_0)) == keccak256(abi.encodePacked([uint256(42), uint256(4), uint256(4)])));
        assert(keccak256(abi.encodePacked(ret_926_1)) == keccak256(abi.encodePacked([uint256(42), uint256(4), uint256(4)])));
    }

    function __testCase1004__(AliasingAndCopying __this__) internal {
        (uint256[3] memory ret_1004_0, uint256[3] memory ret_1004_1) = __this__.twoDimArrayStorageToStorage();
        assert(keccak256(abi.encodePacked(ret_1004_0)) == keccak256(abi.encodePacked([uint256(42), uint256(4), uint256(4)])));
        assert(keccak256(abi.encodePacked(ret_1004_1)) == keccak256(abi.encodePacked([uint256(4), uint256(4), uint256(4)])));
    }

    function __testCase1082__(AliasingAndCopying __this__) internal {
        (uint256 ret_1082_0, uint256 ret_1082_1) = __this__.structStorageToStorage();
        assert(ret_1082_0 == uint256(5));
        assert(ret_1082_1 == uint256(5));
    }

    function __testCase1118__(AliasingAndCopying __this__) internal {
        (uint256 ret_1118_0, uint256 ret_1118_1) = __this__.structStorageToMemory();
        assert(ret_1118_0 == uint256(5));
        assert(ret_1118_1 == uint256(1));
    }

    function __testCase1154__(AliasingAndCopying __this__) internal {
        (uint256 ret_1154_0, uint256 ret_1154_1) = __this__.structMemoryToStorage();
        assert(ret_1154_0 == uint256(5));
        assert(ret_1154_1 == uint256(1));
    }

    function __testCase1190__(AliasingAndCopying __this__) internal {
        (uint256 ret_1190_0, uint256 ret_1190_1) = __this__.structMemoryToMemory();
        assert(ret_1190_0 == uint256(5));
        assert(ret_1190_1 == uint256(5));
    }

    function __testCase1226__(AliasingAndCopying __this__) internal {
        (uint256 ret_1226_0, uint256 ret_1226_1) = __this__.copyNestedStruct();
        assert(ret_1226_0 == uint256(6));
        assert(ret_1226_1 == uint256(4));
    }

    function __testCase1262__(AliasingAndCopying __this__) internal {
        (int256 ret_1262_0, int256 ret_1262_1) = __this__.structOperations();
        assert(ret_1262_0 == int256(1));
        assert(ret_1262_1 == int256(2));
    }
}