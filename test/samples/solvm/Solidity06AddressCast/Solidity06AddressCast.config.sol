pragma solidity ^0.6.0;

contract Foo {
    function main() public returns (address) {
        address x;
        address payable b = payable(x);
        return b;
    }
}

contract __IRTest__ {
    function main() public {
        Foo __this__ = new Foo();
        __testCase34__(__this__);
    }

    function __testCase34__(Foo __this__) internal {
        address ret_34_0 = __this__.main();
        assert(ret_34_0 == address(0x0));
    }
}