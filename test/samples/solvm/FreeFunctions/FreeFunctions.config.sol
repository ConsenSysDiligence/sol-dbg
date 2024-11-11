pragma solidity 0.7.4;

function mul(uint x, uint y) returns (uint) {
    return x * y;
}

function mul(uint x, uint y, uint z) returns (uint) {
    return (x * y) * z;
}

function callInner(Foo f, uint x) returns (uint) {
    return f.double(x);
}

contract Foo {
    function double(uint x) external returns (uint) {
        return mul(x, 2);
    }

    function indirectReentry(uint x) public returns (uint) {
        return callInner(this, x);
    }

    function quadruple(uint x) external returns (uint) {
        return mul(x, 2, 2);
    }
}

contract __IRTest__ {
    function main() public {
        Foo __this__ = new Foo();
        __testCase106__(__this__);
        __testCase134__(__this__);
        __testCase162__(__this__);
    }

    function __testCase106__(Foo __this__) internal {
        uint256 ret_106_0 = __this__.double(uint256(5));
        assert(ret_106_0 == uint256(10));
    }

    function __testCase134__(Foo __this__) internal {
        uint256 ret_134_0 = __this__.indirectReentry(uint256(7));
        assert(ret_134_0 == uint256(14));
    }

    function __testCase162__(Foo __this__) internal {
        uint256 ret_162_0 = __this__.quadruple(uint256(5));
        assert(ret_162_0 == uint256(20));
    }
}