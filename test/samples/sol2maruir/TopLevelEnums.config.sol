pragma solidity 0.6.10;

enum A { A, B, C }

enum B { D, E, F }

contract Foo {
    enum A { X, Y, Z }

    function foo() public returns (A) {
        return A.X;
    }

    function boo() public returns (B) {
        return B.D;
    }
}

contract __IRTest__ {
    function main() public payable {
        Foo __this__ = new Foo();
    }
}
