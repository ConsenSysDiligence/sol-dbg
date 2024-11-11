pragma solidity 0.7.6;

library Safe {
    function add(uint256 a, uint256 b) internal pure returns (uint256) {
        uint256 c = a + b;
        require(c > a, "Overflow");
        return c;
    }

    function add(uint256 a, uint256 b, string memory message) internal pure returns (uint256) {
        uint256 c = a + b;
        require(c > a, message);
        return c;
    }
}

contract Test {
    using Safe for uint256;

    function main() public pure {
        uint256 a = 10;
        uint256 b = 15;
        uint256 c = a.add(b);
        assert(c == 25);
        uint256 d = b.add(a, "Error");
        assert(d == 25);
    }
}

contract __IRTest__ {
    function main() public {
        Test __this__ = new Test();
        __testCase111__(__this__);
    }

    function __testCase111__(Test __this__) internal {
        __this__.main();
    }
}