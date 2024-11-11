pragma solidity >=0.4.13<0.6.0;

contract A {
    function add(uint a, uint b) internal pure returns (uint256) {
        return 1;
    }
}

contract B is A {
    function add(uint a, uint b) internal pure returns (uint256) {
        return a;
    }
}

contract Test is B {
    function add(uint a, uint b) internal pure returns (uint256) {
        return a + b;
    }

    function verify() public {
        uint256 a = 10;
        uint256 b = 15;
        assert(A.add(a, b) == 1);
        assert(B.add(a, b) == 10);
        assert(Test.add(a, b) == 25);
    }
}

contract __IRTest__ {
    function main() public {
        Test __this__ = new Test();
        __testCase102__(__this__);
    }

    function __testCase102__(Test __this__) internal {
        __this__.verify();
    }
}