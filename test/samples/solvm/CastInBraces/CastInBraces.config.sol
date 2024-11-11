pragma solidity ^0.5.0;

contract CastInBraces {
    function main() public {
        int8 a = (int8)(128 + 129);
        assert(a == 1);
    }
}

contract __IRTest__ {
    function main() public {
        CastInBraces __this__ = new CastInBraces();
        __testCase35__(__this__);
    }

    function __testCase35__(CastInBraces __this__) internal {
        __this__.main();
    }
}