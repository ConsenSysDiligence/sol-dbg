pragma solidity ^0.5.0;

contract Test {
    uint public a = 0;

    constructor() public Test() {
        a++;
    }

    function verify() public {
        assert(a == 1);
    }
}

contract __IRTest__ {
    function main() public {
        Test __this__ = new Test();
        __testCase38__(__this__);
    }

    function __testCase38__(Test __this__) internal {
        __this__.verify();
    }
}