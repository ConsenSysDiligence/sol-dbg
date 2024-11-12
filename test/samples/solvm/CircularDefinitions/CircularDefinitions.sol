pragma solidity ^0.4.24;

contract A {
    B internal b;

    function setB(address addr) public {
        b = B(addr);
    }
}

contract B {
    A internal a;

    function setA(address addr) public {
        a = A(addr);
    }
}

contract CircularDefinitions {
    function main() public {
        A a = new A();
        B b = new B();
        a.setB(b);
        b.setA(a);
    }
}

contract __IRTest__ {
    function main() public {
        CircularDefinitions __this__ = new CircularDefinitions();
        __testCase74__(__this__);
    }

    function __testCase74__(CircularDefinitions __this__) internal {
        __this__.main();
    }
}