pragma solidity ^0.6.0;

contract TryCatchShadowing {
    uint256 public x;

    function modifyAndMaybeFail(uint256 newX, bool fail) public returns (uint) {
        x = newX;
        if (fail) {
            require(false, "nooo");
        }
        return x;
    }

    function successArgShadow() public {
        uint t = 10;
        x = 1;
        try this.modifyAndMaybeFail(2, false) returns (uint t) {
            x = x + t;
        } catch {
            assert(false);
        }
        assert((x == 4) && (t == 10));
    }
}

contract __IRTest__ {
    function main() public {
        TryCatchShadowing __this__ = new TryCatchShadowing();
        __testCase87__(__this__);
    }

    function __testCase87__(TryCatchShadowing __this__) internal {
        __this__.successArgShadow();
    }
}