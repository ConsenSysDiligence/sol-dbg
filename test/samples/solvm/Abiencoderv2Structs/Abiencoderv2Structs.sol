pragma solidity ^0.5.0;
pragma experimental ABIEncoderV2;

contract Test {
    struct Arg {
        uint val;
    }

    function encode() public returns (bytes memory) {
        Arg[] memory args = new Arg[](3);
        args[0].val = 1;
        args[1].val = 2;
        args[2].val = 3;
        return abi.encode(args);
    }

    function structInArgs(Arg memory a) public pure returns (Arg memory) {
        return a;
    }

    function verify() public {
        bytes memory encoded = encode();
        Arg[] memory args = abi.decode(encoded, (Arg[]));
        assert(args.length == 3);
        assert(args[0].val == 1);
        assert(args[1].val == 2);
        assert(args[2].val == 3);
        assert(structInArgs(args[1]).val == 2);
    }
}

contract __IRTest__ {
    function main() public {
        Test __this__ = new Test();
        __testCase138__(__this__);
    }

    function __testCase138__(Test __this__) internal {
        __this__.verify();
    }
}