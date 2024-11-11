pragma solidity <0.7.0;

library UseStructs {
    struct Some {
        mapping(uint8 => bool) field;
        uint256 other;
    }
}

contract Test {
    UseStructs.Some internal s;
    UseStructs.Some internal r;

    function verify() public {
        UseStructs.Some memory x = UseStructs.Some(10);
        assert(x.other == 10);
        s.other = 20;
        x = s;
        assert(x.other == 20);
        s.other = 30;
        assert(x.other == 20);
        assert(s.other == 30);
        s = x;
        s.field[10] = true;
        s.other = 100;
        assert(s.other == 100);
        assert(s.field[10] == true);
        r = s;
        assert(r.other == 100);
        assert(r.field[10] == false);
        s.other = 200;
        s.field[15] = true;
        r.field[20] = true;
        assert(r.other == 100);
        assert(s.other == 200);
        assert(r.field[10] == false);
        assert(s.field[10] == true);
        assert(r.field[15] == false);
        assert(s.field[15] == true);
        assert(r.field[20] == true);
        assert(s.field[20] == false);
    }
}

contract __IRTest__ {
    function main() public {
        Test __this__ = new Test();
        __testCase229__(__this__);
    }

    function __testCase229__(Test __this__) internal {
        __this__.verify();
    }
}