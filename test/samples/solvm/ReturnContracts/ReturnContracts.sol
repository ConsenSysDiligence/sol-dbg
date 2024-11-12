pragma solidity ^0.5.0;

contract ERC20 {}

contract ReturnContracts {
    struct Foo {
        uint256 x;
        ERC20 t;
        address a;
    }

    ERC20[] public tokens;
    ERC20 internal token;
    Foo internal f;
    Foo[] internal fs;

    function retArray() public view returns (ERC20[] memory) {
        return tokens;
    }

    function retArrayNamed() public view returns (ERC20[] memory x) {
        x = tokens;
    }

    function retContract() public view returns (ERC20) {
        return token;
    }

    function retContractNamed() public view returns (ERC20 x) {
        x = token;
    }

    function retStruct() internal view returns (Foo memory) {
        return f;
    }

    function retStructNamed() internal view returns (Foo memory x) {
        x = f;
    }

    function retStructs() internal view returns (Foo[] memory) {
        return fs;
    }

    function retStructsNamed() internal view returns (Foo[] memory x) {
        x = fs;
    }
}

contract __IRTest__ {
    function main() public {}
}