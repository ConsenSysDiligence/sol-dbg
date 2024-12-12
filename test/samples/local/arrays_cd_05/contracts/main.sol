pragma solidity 0.5.17;

contract Arrays {
    constructor() public { }

    function allNonZero(uint[] memory arr) public {
        for (uint i = 0; i < arr.length; i++) {
            assert(arr[i] != 0);
        }
    }

    function allNonZeroExt(uint[] calldata arr) external {
        for (uint i = 0; i < arr.length; i++) {
            assert(arr[i] != 0);
        }
    }

}
