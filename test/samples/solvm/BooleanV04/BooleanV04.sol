pragma solidity ^0.4.24;

contract Boolean {
    function booleanLogic() public {
        int32 a = 4;
        int32 b = 3;
        bool c = a == 4;
        assert(c);
        c = c && (b != 4);
        assert(c);
        c = c || (!c);
        assert(c);
        c = a >= b;
        assert(c);
        c = a <= b;
        assert(!c);
        c = true;
        assert(c);
        c = false;
        assert(!c);
        c = true || false;
        assert(c);
    }
}

contract __IRTest__ {
    function main() public {
        Boolean __this__ = new Boolean();
        __testCase109__(__this__);
    }

    function __testCase109__(Boolean __this__) internal {
        __this__.booleanLogic();
    }
}