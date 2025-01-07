pragma solidity 0.8.26;


contract Test {
  constructor() public {}

  function indexCalldataArray(uint[] calldata a) public {
    assert(a[0] == 0);
  }
}
