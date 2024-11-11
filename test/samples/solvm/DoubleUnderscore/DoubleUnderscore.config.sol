pragma solidity ^0.5.10;

contract DoubleUnderscore {
    bool internal locked;
    uint internal x;

    modifier branch(bool flag) {
        uint a;
        if (flag) {
            a = x + 1;
            x = a;
            _;
        } else {
            a = x + 2;
            x = a;
            _;
        }
    }

    modifier branch2(bool flag) {
        uint a;
        if (flag) {
            a = x * 3;
            x = a;
            _;
        } else {
            a = x * 5;
            x = a;
            _;
        }
    }

    function reset() public {
        x = 0;
    }

    function singleMod(bool flag) public branch(flag) returns (uint) {
        uint a = x;
        return a;
    }

    function doubleMod(bool flag) public branch(flag) branch(flag) returns (uint) {
        uint a = x;
        return a;
    }

    function twoMods(bool flag) public branch(flag) branch2(flag) returns (uint) {
        uint a = x;
        return a;
    }
}

contract __IRTest__ {
    function main() public {
        DoubleUnderscore __this__ = new DoubleUnderscore();
        __testCase154__(__this__);
        __testCase179__(__this__);
        __testCase194__(__this__);
        __testCase219__(__this__);
        __testCase234__(__this__);
        __testCase259__(__this__);
        __testCase274__(__this__);
        __testCase299__(__this__);
        __testCase314__(__this__);
    }

    function __testCase154__(DoubleUnderscore __this__) internal {
        uint256 ret_154_0 = __this__.singleMod(true);
        assert(ret_154_0 == uint256(1));
    }

    function __testCase179__(DoubleUnderscore __this__) internal {
        __this__.reset();
    }

    function __testCase194__(DoubleUnderscore __this__) internal {
        uint256 ret_194_0 = __this__.singleMod(false);
        assert(ret_194_0 == uint256(2));
    }

    function __testCase219__(DoubleUnderscore __this__) internal {
        __this__.reset();
    }

    function __testCase234__(DoubleUnderscore __this__) internal {
        uint256 ret_234_0 = __this__.doubleMod(true);
        assert(ret_234_0 == uint256(2));
    }

    function __testCase259__(DoubleUnderscore __this__) internal {
        __this__.reset();
    }

    function __testCase274__(DoubleUnderscore __this__) internal {
        uint256 ret_274_0 = __this__.doubleMod(false);
        assert(ret_274_0 == uint256(4));
    }

    function __testCase299__(DoubleUnderscore __this__) internal {
        __this__.reset();
    }

    function __testCase314__(DoubleUnderscore __this__) internal {
        uint256 ret_314_0 = __this__.twoMods(false);
        assert(ret_314_0 == uint256(10));
    }
}