pragma solidity ^0.5.14;

contract Test {
    uint public constant SOME_SIZE = 2;
    uint[SOME_SIZE] public some = [1, 2];
    uint[SOME_SIZE + 1] public other = [6, 7, 8];

    function verify() public {
        assert(some[0] == 1);
        assert(some[1] == 2);
        assert(other[0] == 6);
        assert(other[1] == 7);
        assert(other[2] == 8);
    }
}

contract __IRTest__ {
    function main() public {
        Test __this__ = new Test();
        __testCase80__(__this__);
    }

    function __testCase80__(Test __this__) internal {
        __this__.verify();
    }
}