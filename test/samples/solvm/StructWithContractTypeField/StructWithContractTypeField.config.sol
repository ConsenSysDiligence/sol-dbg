pragma solidity ^0.6.0;

interface Infc {
    function doSomething() virtual external returns (uint256);
}

contract A is Infc {
    uint256 internal val;

    constructor(uint256 v) public {
        val = v;
    }

    function doSomething() override external returns (uint256) {
        return val;
    }
}

contract Test {
    struct Some {
        Infc implementor;
        uint val;
    }

    mapping(bytes2 => Some) public map;

    function verify() public {
        map[0x0000] = Some(new A(uint256(10)), 10);
        map[0x0001] = Some(new A(uint256(15)), 20);
        map[0x0101] = Some(new A(uint256(20)), 30);
        (Infc x1, uint x2) = this.map(0x0000);
        assert(x1.doSomething() == 10);
        assert(x2 == 10);
        (Infc x3, uint x4) = this.map(0x0001);
        assert(x3.doSomething() == 15);
        assert(x4 == 20);
        (Infc x5, ) = this.map(0x0101);
        assert(x5.doSomething() == 20);
        (, uint x6) = this.map(0x0101);
        assert(x6 == 30);
    }
}

contract __IRTest__ {
    function main() public {
        Test __this__ = new Test();
        __testCase178__(__this__);
    }

    function __testCase178__(Test __this__) internal {
        __this__.verify();
    }
}