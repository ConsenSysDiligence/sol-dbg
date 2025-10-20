pragma solidity 0.8.29;

library Lib {
    function mustBeEmpty(uint[] storage p) external {
        assert(p.length == 0);
    }
}

contract Foo {
    uint[] a;

    function main() public {
        a.push(1);
        Lib.mustBeEmpty(a);
    }
}
