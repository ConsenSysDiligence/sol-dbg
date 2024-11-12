pragma solidity ^0.6.0;

enum GlobalEnum { A, B, C }

struct GlobalStruct {
    int a;
    uint[] b;
    mapping(address => uint) c;
}

contract NegativeExponentiation {
    function testSignedBaseExponentiation() public {
        int base_i256 = -3;
        uint16 power_u16 = 3;
        int expected_result_i256 = -27;
        assert((base_i256 ** power_u16) == expected_result_i256);
        int8 base_i8 = -7;
        uint power_u256 = 3;
        int8 expected_result_i8 = -87;
        assert((base_i8 ** power_u256) == expected_result_i8);
    }
}

abstract contract SampleAbstract {
    function abstractFunc(address a) virtual internal returns (address payable);
}

contract Empty {}

contract SampleBase is SampleAbstract {
    function abstractFunc(address a) override(SampleAbstract) internal returns (address payable) {
        return payable(a);
    }

    function testSlices() public pure {
        (uint a, uint b) = abi.decode(msg.data[0:4], (uint, uint));
        (uint c, uint d) = abi.decode(msg.data[:4], (uint, uint));
        (uint e, uint f) = abi.decode(msg.data[4:], (uint, uint));
        (uint g, uint h) = abi.decode(msg.data[:], (uint, uint));
        (uint i, uint j) = abi.decode(msg.data, (uint, uint));
    }

    function testTryCatch() public {
        try new Empty() {
            int a = 1;
        } catch {
            int b = 2;
        }
        try new Empty() returns (Empty x) {
            int a = 1;
        } catch Error(string memory reason) {} catch (bytes memory lowLevelData) {}
    }

    receive() external payable {}

    fallback() external {}
}

contract __IRTest__ {
    function main() public {
        NegativeExponentiation __this__ = new NegativeExponentiation();
        __testCase243__(__this__);
    }

    function __testCase243__(NegativeExponentiation __this__) internal {
        __this__.testSignedBaseExponentiation();
    }
}