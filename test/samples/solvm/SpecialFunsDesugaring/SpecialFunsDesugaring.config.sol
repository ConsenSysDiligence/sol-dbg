pragma solidity 0.8.4;

contract Test {
    fallback() external {}

    receive() external payable {}

    constructor() {}

    function receive() external payable {}

    function fallback() external {}
}

contract __IRTest__ {
    function main() public {}
}