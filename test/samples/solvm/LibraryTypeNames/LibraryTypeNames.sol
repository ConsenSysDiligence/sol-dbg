pragma solidity ^0.5.0;

library L {
    enum SomeEnum { A, B, C }

    struct SomeStruct {
        string name;
        int value;
    }
}

contract LibraryTypeNames {
    function main() public {
        L.SomeEnum x = L.SomeEnum.A;
        assert(x == L.SomeEnum.A);
        L.SomeStruct memory s = L.SomeStruct("test", 1);
        assert(s.value == 1);
    }
}

contract __IRTest__ {
    function main() public {
        LibraryTypeNames __this__ = new LibraryTypeNames();
        __testCase63__(__this__);
    }

    function __testCase63__(LibraryTypeNames __this__) internal {
        __this__.main();
    }
}