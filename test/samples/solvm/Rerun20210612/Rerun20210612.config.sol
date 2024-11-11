pragma solidity ^0.5.0;

library Lib {
    function add(uint a, uint b) internal pure returns (uint) {
        return a + b;
    }
}

interface IStandard {
    /// This is important: there is a variable declaration with same name
    /// in an implementor contract.
    ///      * ExpressionBuilder will try to resolve functions first,
    /// So it will do a function lookup first, then do a variable lookup
    /// if there are no function matches the signature.
    ///      * So in this case `token.suppliesSize()` will be a function call
    /// rather than a getter call.
    function suppliesSize() external pure returns (uint);
}

contract Token is IStandard {
    uint public suppliesSize = 10;
}

contract Some {
    function _over(uint a, uint b) internal returns (uint) {
        return a + b;
    }

    function _over(uint a) internal returns (uint) {
        return _over(a, 1);
    }
}

contract Rerun20210612 is Some {
    using Lib for uint;

    uint public initsByLibInvocation = uint(1).add(2);
    uint public initsByMethodCall = super._over(1);

    function testStateVarsAreInitialized() public {
        assert(initsByLibInvocation == 3);
        assert(initsByMethodCall == 3);
    }

    function testNumberEdgeCases() public {
        int a = -0xffffffffffffffff;
        assert(a == (-18446744073709551615));
        uint b = 1e30;
        assert(b == 1000000000000000000000000000000);
        int c = -1e30;
        assert(c == (-1000000000000000000000000000000));
    }

    function testModOp() public {
        int8 a = -11;
        uint8 b = uint8(a % 2);
        assert(b == 255);
    }

    function _over(uint a, uint b) internal returns (uint) {
        return super._over(a, b) + 1;
    }

    function testOverloads() public {
        assert(super._over(1, 2) == 3);
        assert(_over(3, 4) == 8);
    }

    function testEncodeWithSelector() public {
        bytes memory a = abi.encodeWithSelector(0x00000001);
        assert(a[0] == 0x00);
        assert(a[1] == 0x00);
        assert(a[2] == 0x00);
        assert(a[3] == 0x01);
        assert(a.length == 4);
    }

    function testCallGetterViaInterfaceCast() public {
        Token token = new Token();
        assert(10 == token.suppliesSize());
    }

    function test() public {
        testStateVarsAreInitialized();
        testNumberEdgeCases();
        testModOp();
        testOverloads();
        testEncodeWithSelector();
        testCallGetterViaInterfaceCast();
    }
}

contract __IRTest__ {
    function main() public {
        Rerun20210612 __this__ = new Rerun20210612();
        __testCase298__(__this__);
    }

    function __testCase298__(Rerun20210612 __this__) internal {
        __this__.test();
    }
}