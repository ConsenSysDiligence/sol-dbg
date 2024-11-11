pragma solidity 0.8.19;

contract Test {
    function main() public {
        uint a;
        uint b;
        (a, b) = abi.decode(bytes("0x01"), (uint, uint));
    }
}

contract __IRTest__ {
    function main() public {
        Test __this__ = new Test();
        __testCase43__(__this__);
    }

    function __testCase43__(Test __this__) internal {
        try __this__.main() {
            assert(false);
        } catch Error(string memory reason) {
            assert(false);
        } catch {
            assert(true);
        }
    }
}