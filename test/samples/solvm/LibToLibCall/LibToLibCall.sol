pragma solidity ^0.4.24;

library Test {
    function a() public pure returns (uint) {
        return 1;
    }

    function b() public pure returns (uint) {
        return 1 + a();
    }
}

contract LibToLibCall {
    function main() public {
        assert(Test.b() == 2);
    }
}

contract __IRTest__ {
    function main() public {
        LibToLibCall __this__ = new LibToLibCall();
        __testCase48__(__this__);
    }

    function __testCase48__(LibToLibCall __this__) internal {
        __this__.main();
    }
}