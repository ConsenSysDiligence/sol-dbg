pragma solidity 0.8.29;

contract __IRTest__ {
    function main() public payable {
        bytes32 hash = blockhash(42);
        assert(hash == 0x64df4a6bbd73d88237617446ade20dd4ef463678657f54f65ba9db77b2d59ed9);
    }
}