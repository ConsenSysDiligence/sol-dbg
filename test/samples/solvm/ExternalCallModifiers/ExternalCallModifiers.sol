pragma solidity ^0.4.24;

contract A {
    constructor() public payable {}

    function arr() public payable {}
}

contract ExternalCallModifiers {
    constructor() public payable {}

    function main() public {
        A a = new A();
        a = (new A).value(5)();
        a.arr();
        a.arr.value(5)();
        a.arr.gas(100)();
        a.arr.gas(200).value(10)();
    }
}

contract __IRTest__ {
    function main() public {
        ExternalCallModifiers __this__ = (new ExternalCallModifiers).value(20)();
        __testCase88__(__this__);
    }

    function __testCase88__(ExternalCallModifiers __this__) internal {
        __this__.main();
    }
}