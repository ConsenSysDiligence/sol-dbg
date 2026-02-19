pragma solidity 0.8.29;

library Lib {
    function callAndAddOne(function () internal returns (uint) a) internal returns (uint) {
        return a() + 1;
    }
}

contract Foo {
    using { Lib.callAndAddOne } for function () internal returns (uint);

    function getOne() public returns (uint) {
        return 1;
    }

    function getTwo() public returns (uint) {
        return 2;
    }

    function main() public payable {
        assert(getOne.callAndAddOne() == 2);
        assert(getTwo.callAndAddOne() == 3);
        assert(Lib.callAndAddOne(getOne) == 2);
    }
}

contract __IRTest__ {
    function main() public payable {
        Foo __this__ = new Foo();
        __this__.main();
    }
}
