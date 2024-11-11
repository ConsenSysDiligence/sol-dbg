pragma solidity ^0.4.24;

contract A {
    function getPrice(string memory x) public returns (int) {
        return 1;
    }
}

contract B is A {
    function getPrice(string memory dsn, int cost) public returns (int) {
        return cost + 100;
    }
}

contract AmbiguousFunctions is B {
    function test1() public {
        int x = this.getPrice("abc", 1);
        assert(x == 101);
    }

    function test2() public {
        B b = new B();
        int x = b.getPrice("def");
        assert(x == 1);
    }

    function main() public {
        test1();
        test2();
    }
}

contract __IRTest__ {
    function main() public {
        AmbiguousFunctions __this__ = new AmbiguousFunctions();
        __testCase97__(__this__);
    }

    function __testCase97__(AmbiguousFunctions __this__) internal {
        __this__.main();
    }
}