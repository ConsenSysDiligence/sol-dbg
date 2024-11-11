pragma solidity ^0.4.21;

contract Structs {
    struct Record {
        uint id;
        string data;
    }
}

contract CompoundTypeNames {
    struct Record {
        int y;
        uint8[] z;
    }

    Structs.Record[] internal recs;
    Record internal recs2;

    function main() public {
        recs.push(Structs.Record(1, "test"));
        assert(recs.length == 1);
        assert(recs[0].id == 1);
    }
}

contract __IRTest__ {
    function main() public {
        CompoundTypeNames __this__ = new CompoundTypeNames();
        __testCase63__(__this__);
    }

    function __testCase63__(CompoundTypeNames __this__) internal {
        __this__.main();
    }
}