pragma solidity ^0.4.24;

contract O {
    uint[] internal arr;

    constructor() public {
        arr.push(1);
    }

    function getArr(uint i) public returns (uint) {
        return arr[i];
    }

    function arrLen() public returns (uint) {
        return arr.length;
    }
}

contract A is O {
    constructor() public {
        arr.push(2);
    }
}

contract B is O {
    constructor() public {
        arr.push(3);
    }
}

contract C is O {
    constructor() public {
        arr.push(4);
    }
}

contract D is O {
    constructor() public {
        arr.push(5);
    }
}

contract E is O {
    constructor() public {
        arr.push(6);
    }
}

contract K1 is A, B, C {
    constructor() public {
        arr.push(7);
    }
}

contract K2 is D, B, E {
    constructor() public {
        arr.push(8);
    }
}

contract K3 is D, A {
    constructor() public {
        arr.push(9);
    }
}

contract Z is K1, K2, K3 {
    constructor() public {
        arr.push(10);
    }
}

contract ConstructorLinearization {
    function main() public returns (uint, uint, uint, uint, uint, uint, uint, uint, uint, uint) {
        Z z = new Z();
        assert(z.arrLen() == 10);
        assert(z.getArr(0) == 1);
        assert(z.getArr(1) == 5);
        assert(z.getArr(2) == 2);
        assert(z.getArr(3) == 3);
        assert(z.getArr(4) == 4);
        assert(z.getArr(5) == 7);
        assert(z.getArr(6) == 6);
        assert(z.getArr(7) == 8);
        assert(z.getArr(8) == 9);
        assert(z.getArr(9) == 10);
        return (z.getArr(0), z.getArr(1), z.getArr(2), z.getArr(3), z.getArr(4), z.getArr(5), z.getArr(6), z.getArr(7), z.getArr(8), z.getArr(9));
    }
}

contract __IRTest__ {
    function main() public {
        ConstructorLinearization __this__ = new ConstructorLinearization();
        __testCase352__(__this__);
    }

    function __testCase352__(ConstructorLinearization __this__) internal {
        (uint256 ret_352_0, uint256 ret_352_1, uint256 ret_352_2, uint256 ret_352_3, uint256 ret_352_4, uint256 ret_352_5, uint256 ret_352_6, uint256 ret_352_7, uint256 ret_352_8, uint256 ret_352_9) = __this__.main();
        assert(ret_352_0 == uint256(1));
        assert(ret_352_1 == uint256(5));
        assert(ret_352_2 == uint256(2));
        assert(ret_352_3 == uint256(3));
        assert(ret_352_4 == uint256(4));
        assert(ret_352_5 == uint256(7));
        assert(ret_352_6 == uint256(6));
        assert(ret_352_7 == uint256(8));
        assert(ret_352_8 == uint256(9));
        assert(ret_352_9 == uint256(10));
    }
}