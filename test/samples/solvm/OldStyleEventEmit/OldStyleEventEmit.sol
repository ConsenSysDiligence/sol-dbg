pragma solidity ^0.4.25;

contract OldStyleEventEmit {
    event E(uint x);

    function main() public {
        E(10);
    }
}

contract __IRTest__ {
    function main() public {
        OldStyleEventEmit __this__ = new OldStyleEventEmit();
        __testCase28__(__this__);
    }

    function __testCase28__(OldStyleEventEmit __this__) internal {
        __this__.main();
    }
}