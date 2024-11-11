pragma solidity ^0.6.0;

contract VarHolder {
    uint256 public x;

    function modifyAndMaybeFail(uint256 newX, bool fail) public {
        x = newX;
        if (fail) {
            require(false, "nooo");
        }
    }
}

contract TryCatchState {
    uint256 public x;
    VarHolder internal v;

    constructor() public {
        v = new VarHolder();
    }

    function modifyAndMaybeFail(uint256 newX, bool fail) public {
        x = newX;
        if (fail) {
            require(false, "nooo");
        }
    }

    function successfulModifySelf() public {
        x = 1;
        try this.modifyAndMaybeFail(2, false) {
            x = x + 1;
        } catch {
            assert(false);
        }
        assert(x == 3);
    }

    function revertingModifySelf() public {
        x = 1;
        try this.modifyAndMaybeFail(2, true) {
            x = x + 1;
        } catch {
            x = x + 100;
        }
        assert(x == 101);
    }

    function successfulModifyOther() public {
        v.modifyAndMaybeFail(1, false);
        assert(v.x() == 1);
        x = 1;
        try v.modifyAndMaybeFail(2, false) {} catch {
            assert(false);
        }
        assert((v.x() == 2) && (x == 1));
    }

    function revertingModifyOther() public {
        v.modifyAndMaybeFail(1, false);
        assert(v.x() == 1);
        x = 1;
        try v.modifyAndMaybeFail(2, true) {
            x = x + 1;
        } catch {
            x = x + 100;
        }
        assert((v.x() == 1) && (x == 101));
    }
}

contract __IRTest__ {
    function main() public {
        TryCatchState __this__ = new TryCatchState();
        __testCase249__(__this__);
        __testCase263__(__this__);
        __testCase277__(__this__);
        __testCase291__(__this__);
    }

    function __testCase249__(TryCatchState __this__) internal {
        __this__.successfulModifySelf();
    }

    function __testCase263__(TryCatchState __this__) internal {
        __this__.revertingModifySelf();
    }

    function __testCase277__(TryCatchState __this__) internal {
        __this__.successfulModifyOther();
    }

    function __testCase291__(TryCatchState __this__) internal {
        __this__.revertingModifyOther();
    }
}