pragma solidity 0.7.6;

contract Test {
    function some() internal returns (uint[3] memory) {
        return [uint(1), 2, 3];
    }

    function verify() public {
        uint[3] memory x = some();
        assert(x[0] == 1);
        assert(x[1] == 2);
        assert(x[2] == 3);
    }
}

contract __IRTest__ {
    function main() public {
        Test __this__ = new Test();
        __testCase69__(__this__);
    }

    function __testCase69__(Test __this__) internal {
        __this__.verify();
    }
}