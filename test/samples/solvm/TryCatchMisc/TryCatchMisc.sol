pragma solidity ^0.6.0;

contract TryCatchMisc {
    uint256 public x;

    function echo(uint x) public returns (uint) {
        return x;
    }

    function fail(uint x) public returns (uint) {
        assert(false);
        return x;
    }

    function argException() public returns (uint) {
        uint x = 1;
        uint y = 0;
        try this.echo(x / y) returns (uint t) {
            return t;
        } catch {
            return 0;
        }
    }

    function successException() public returns (uint) {
        uint y = 0;
        try this.echo(1) returns (uint t) {
            return t / y;
        } catch {
            return 0;
        }
    }

    function catchException() public returns (uint) {
        uint x = 1;
        uint y = 0;
        try this.fail(1) returns (uint t) {
            return 0;
        } catch {
            return x / y;
        }
    }

    function main1() public {
        try this.argException() returns (uint) {
            assert(false);
        } catch Error(string memory m) {
            assert(false);
        } catch (bytes memory err) {}
    }

    function main2() public {
        try this.successException() returns (uint) {
            assert(false);
        } catch Error(string memory m) {
            assert(false);
        } catch (bytes memory err) {}
    }

    function main3() public {
        try this.catchException() returns (uint) {
            assert(false);
        } catch Error(string memory m) {
            assert(false);
        } catch (bytes memory err) {}
    }

    function throwMsg(string memory m) public {
        require(false, m);
    }

    function main4() public {
        try this.throwMsg("foo") {} catch Error(string memory y) {
            try this.throwMsg("bar") {} catch Error(string memory x) {
                assert(keccak256(abi.encode(x)) == keccak256(abi.encode("bar")));
            }
            assert(keccak256(abi.encode(y)) == keccak256(abi.encode("foo")));
        }
    }

    function main5() public returns (string memory) {
        string memory a = "abcd";
        bytes memory b = bytes(a);
        try this.throwMsg("oops") {} catch {}
        b[0] = 0x7a;
        return a;
    }
}

contract __IRTest__ {
    function main() public {
        TryCatchMisc __this__ = new TryCatchMisc();
        __testCase332__(__this__);
        __testCase346__(__this__);
    }

    function __testCase332__(TryCatchMisc __this__) internal {
        __this__.main4();
    }

    function __testCase346__(TryCatchMisc __this__) internal {
        string memory ret_346_0 = __this__.main5();
        assert(keccak256(abi.encodePacked(ret_346_0)) == keccak256(abi.encodePacked("zbcd")));
    }
}