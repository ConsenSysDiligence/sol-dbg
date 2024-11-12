pragma solidity 0.7.6;

contract Proxy {
    uint256 public val;

    constructor(uint256 v) {
        val = v;
    }
}

contract TypeBuiltin {
    function verify() public {
        string memory name = type(Proxy).name;
        bytes memory creationCode = type(Proxy).creationCode;
        bytes memory runtimeCode = type(Proxy).runtimeCode;
    }
}

contract __IRTest__ {
    function main() public {}
}