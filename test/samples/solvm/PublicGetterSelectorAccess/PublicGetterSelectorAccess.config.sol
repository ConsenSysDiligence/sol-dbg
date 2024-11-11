pragma solidity 0.7.6;

import "./utils.sol";

contract Some {
    mapping(address => uint256) public balances;
}

contract Test {
    Some internal s = new Some();

    function verify() public {
        bytes memory encoded = abi.encodePacked(s.balances.selector, 0x4797705893e47003ce44ac917c3Bb650c4eD11A6);
        assert(BytesLib.isSame(encoded, hex"27e235e34797705893e47003ce44ac917c3bb650c4ed11a6"));
    }
}

contract __IRTest__ {
    function main() public {
        Test __this__ = new Test();
        __testCase97__(__this__);
    }

    function __testCase97__(Test __this__) internal {
        __this__.verify();
    }
}