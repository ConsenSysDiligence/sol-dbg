pragma solidity ^0.6.0;

contract TryCatch {
    uint public x;

    function catchHighLevelException() public returns (string memory) {
        try this.requireFail("test") {
            assert(false);
        } catch Error(string memory m) {
            return m;
        } catch (bytes memory b) {
            assert(false);
        }
    }

    function requireFail(string memory msg) public {
        require(false, msg);
    }

    function catchLowLevelException() public returns (uint) {
        try this.assertFail() {
            assert(false);
        } catch Error(string memory m) {
            assert(false);
        } catch (bytes memory b) {
            return 42;
        }
    }

    function assertFail() public {
        assert(false);
    }

    function catchHighLevelExceptionUnnamedArgs() public {
        try this.requireFail("test") {
            assert(false);
        } catch Error(string memory) {} catch (bytes memory b) {
            assert(false);
        }
    }

    function echo(uint x) public returns (uint) {
        return x;
    }

    function successUnnamed() public {
        try this.echo(42) returns (uint) {} catch Error(string memory m) {
            assert(false);
        } catch (bytes memory b) {
            assert(false);
        }
    }

    function successNamed() public returns (uint) {
        try this.echo(42) returns (uint v) {
            return v + 1;
        } catch Error(string memory m) {
            assert(false);
        } catch (bytes memory b) {
            assert(false);
        }
    }

    function reThrowHighLevel() public {
        x = 22;
        try this.requireFail("hi") {
            assert(false);
        } catch Error(string memory m) {
            x = 23;
            assert(keccak256(abi.encodePacked(m)) == keccak256(abi.encodePacked("hi")));
            require(false, "hihi");
        } catch (bytes memory b) {
            assert(false);
        }
    }

    function reThrowTest() public returns (string memory, uint) {
        x = 11;
        try this.reThrowHighLevel() {
            assert(false);
        } catch Error(string memory m) {
            return (m, x);
        } catch (bytes memory b) {
            assert(false);
        }
    }
}

contract __IRTest__ {
    function main() public {
        TryCatch __this__ = new TryCatch();
        __testCase309__(__this__);
        __testCase342__(__this__);
        __testCase367__(__this__);
        __testCase381__(__this__);
        __testCase395__(__this__);
        __testCase420__(__this__);
    }

    function __testCase309__(TryCatch __this__) internal {
        string memory ret_309_0 = __this__.catchHighLevelException();
        assert(keccak256(abi.encodePacked(ret_309_0)) == keccak256(abi.encodePacked("test")));
    }

    function __testCase342__(TryCatch __this__) internal {
        uint256 ret_342_0 = __this__.catchLowLevelException();
        assert(ret_342_0 == uint256(42));
    }

    function __testCase367__(TryCatch __this__) internal {
        __this__.catchHighLevelExceptionUnnamedArgs();
    }

    function __testCase381__(TryCatch __this__) internal {
        __this__.successUnnamed();
    }

    function __testCase395__(TryCatch __this__) internal {
        uint256 ret_395_0 = __this__.successNamed();
        assert(ret_395_0 == uint256(43));
    }

    function __testCase420__(TryCatch __this__) internal {
        (string memory ret_420_0, uint256 ret_420_1) = __this__.reThrowTest();
        assert(keccak256(abi.encodePacked(ret_420_0)) == keccak256(abi.encodePacked("hihi")));
        assert(ret_420_1 == uint256(11));
    }
}