pragma solidity ^0.4.24;

contract Base1 {
    uint internal x;

    constructor() public {
        x = x + 1;
    }
}

/// Case 1: One parent, no explicit call
contract Child1 is Base1 {
    constructor() public {}

    function main() public {
        assert(x == 1);
    }
}

/// Case 2: One parent, explicit call in constructor. Make sure its called exactly once.
contract Child2 is Base1 {
    constructor() public Base1() {}

    function main() public {
        assert(x == 1);
    }
}

/// Case 3: One parent, explicit call in inheritance list. Make sure its called exactly once.
contract Child3 is Base1 {
    constructor() public {}

    function main() public {
        assert(x == 1);
    }
}

/// Case 4: Two direct parents, no explicit call. Make sure all are called exactly once.
contract Base2 {
    uint internal y;

    constructor() public {
        y = y + 1;
    }
}

contract Child4 is Base1, Base2 {
    constructor() public {}

    function main() public {
        assert((x == 1) && (y == 1));
    }
}

/// Case 5: Two direct parents, one explicit call. Make sure all are called exactly once.
contract Child5 is Base1, Base2 {
    constructor() public Base1() {}

    function main() public {
        assert((x == 1) && (y == 1));
    }
}

/// Case 6: Two direct parents, two explicit call. Make sure all are called exactly once.
contract Child6 is Base1, Base2 {
    constructor() public Base1() Base2() {}

    function main() public {
        assert((x == 1) && (y == 1));
    }
}

/// Case 7: Indirect parent.
contract Base3 is Base1 {
    uint internal y;

    constructor() public {
        y = y + 1;
    }
}

contract Child7 is Base3 {
    constructor() public Base3() {}

    function main() public {
        assert((x == 1) && (y == 1));
    }
}

/// Case 8: Both direct and indirect parent.
contract Child8 is Base1, Base3 {
    constructor() public Base3() {}

    function main() public {
        assert((x == 1) && (y == 1));
    }
}

/// Case 9: Both direct and indirect parent. 2 explicit calls.
contract Child9 is Base1, Base3 {
    constructor() public Base3() Base1() {}

    function main() public {
        assert((x == 1) && (y == 1));
    }
}

/// Case 10: Explicit constructors in the inheritance list
contract Base4 {
    constructor(uint a) public {
        assert(a == 1);
    }
}

contract Child10 is Base4(1) {}

contract __IRTest__ {
    function main() public {
        Child1 __this__ = new Child1();
        __testCase276__(__this__);
        Child2 __this2__ = new Child2();
        __testCase295__(__this__, __this2__);
        Child3 __this3__ = new Child3();
        __testCase317__(__this__, __this2__, __this3__);
        Child4 __this4__ = new Child4();
        __testCase342__(__this__, __this2__, __this3__, __this4__);
        Child5 __this5__ = new Child5();
        __testCase370__(__this__, __this2__, __this3__, __this4__, __this5__);
        Child6 __this6__ = new Child6();
        __testCase401__(__this__, __this2__, __this3__, __this4__, __this5__, __this6__);
        Child7 __this7__ = new Child7();
        __testCase435__(__this__, __this2__, __this3__, __this4__, __this5__, __this6__, __this7__);
        Child8 __this8__ = new Child8();
        __testCase472__(__this__, __this2__, __this3__, __this4__, __this5__, __this6__, __this7__, __this8__);
        Child9 __this9__ = new Child9();
        __testCase512__(__this__, __this2__, __this3__, __this4__, __this5__, __this6__, __this7__, __this8__, __this9__);
        Child10 __this10__ = new Child10();
    }

    function __testCase276__(Child1 __this__) internal {
        __this__.main();
    }

    function __testCase295__(Child1 __this__, Child2 __this2__) internal {
        __this2__.main();
    }

    function __testCase317__(Child1 __this__, Child2 __this2__, Child3 __this3__) internal {
        __this3__.main();
    }

    function __testCase342__(Child1 __this__, Child2 __this2__, Child3 __this3__, Child4 __this4__) internal {
        __this4__.main();
    }

    function __testCase370__(Child1 __this__, Child2 __this2__, Child3 __this3__, Child4 __this4__, Child5 __this5__) internal {
        __this5__.main();
    }

    function __testCase401__(Child1 __this__, Child2 __this2__, Child3 __this3__, Child4 __this4__, Child5 __this5__, Child6 __this6__) internal {
        __this6__.main();
    }

    function __testCase435__(Child1 __this__, Child2 __this2__, Child3 __this3__, Child4 __this4__, Child5 __this5__, Child6 __this6__, Child7 __this7__) internal {
        __this7__.main();
    }

    function __testCase472__(Child1 __this__, Child2 __this2__, Child3 __this3__, Child4 __this4__, Child5 __this5__, Child6 __this6__, Child7 __this7__, Child8 __this8__) internal {
        __this8__.main();
    }

    function __testCase512__(Child1 __this__, Child2 __this2__, Child3 __this3__, Child4 __this4__, Child5 __this5__, Child6 __this6__, Child7 __this7__, Child8 __this8__, Child9 __this9__) internal {
        __this9__.main();
    }
}