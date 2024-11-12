pragma solidity ^0.6.0;

contract Caller {
    uint256 public x;
    Callee internal c;

    constructor(Callee arg) public {
        c = arg;
    }

    function setX(uint256 newX) public {
        x = newX;
    }

    function callAndMaybeFail(uint256 newX, bool calleeFail, bool fail) public {
        x = newX;
        c.modifyAndMaybeFail(newX, calleeFail);
        if (fail) {
            require(false, "caller nooo");
        }
    }
}

contract Callee {
    uint256 public y;

    function setY(uint256 newY) public {
        y = newY;
    }

    function modifyAndMaybeFail(uint256 newY, bool fail) public {
        y = newY;
        if (fail) {
            require(false, "callee nooo");
        }
    }
}

contract TryCatchStateNested {
    Caller internal a;
    Callee internal b;
    uint256 internal z;

    constructor() public {
        b = new Callee();
        a = new Caller(b);
    }

    function callerFail() public {
        z = 10;
        a.setX(20);
        b.setY(30);
        try a.callAndMaybeFail(100, false, true) {
            z = 1000;
        } catch {
            z = z + 1;
        }
        assert(z == 11);
        assert(a.x() == 20);
        assert(b.y() == 30);
    }

    function calleeFail() public {
        z = 10;
        a.setX(20);
        b.setY(30);
        try a.callAndMaybeFail(100, true, false) {
            z = 1000;
        } catch {
            z = z + 1;
        }
        assert(z == 11);
        assert(a.x() == 20);
        assert(b.y() == 30);
    }
}

contract __IRTest__ {
    function main() public {
        TryCatchStateNested __this__ = new TryCatchStateNested();
        __testCase252__(__this__);
        __testCase266__(__this__);
    }

    function __testCase252__(TryCatchStateNested __this__) internal {
        __this__.callerFail();
    }

    function __testCase266__(TryCatchStateNested __this__) internal {
        __this__.calleeFail();
    }
}