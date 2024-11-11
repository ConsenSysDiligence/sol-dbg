pragma solidity ^0.8.4;

contract TryCatch08 {
    enum E { A, B, C }

    error CustomError(uint8 a, int16 b, address c);

    uint internal x;
    uint[] internal arr;

    function throwString(string memory str) external {
        require(false, str);
    }

    function throwAssert() external {
        assert(false);
    }

    function throwOverflow() external {
        uint t = 2 ** 255;
        t * 3;
    }

    function throwDivByZero() external {
        uint t = 0;
        1234 / t;
    }

    function throwEnumCast() external {
        uint t = 3;
        E(t);
    }

    function throwPopEmpty() external {
        arr.pop();
    }

    function throwIdxOoB1() external {
        arr[1] = 0;
    }

    function throwIdxOoB2() external {
        uint t = arr[1];
    }

    function throwIdxOoB3() external {
        bytes10 a;
        uint t = 10;
        a[t];
    }

    function throwAllocTooMuch() external {
        uint[] memory m = new uint[](2 ** 255);
    }

    function noCatchAll() external {
        try this.throwOverflow() {
            x = 1;
        } catch Error(string memory s) {
            x = 2;
        }
    }

    function throwCustom() external {
        revert CustomError(1, -1, address(0x0));
    }

    function main() public {
        x = 0;
        try this.throwString("abc") {
            assert(false);
        } catch Panic(uint code) {
            assert(false);
        } catch {
            assert(false);
        } catch Error(string memory m) {
            assert(keccak256(bytes(m)) == keccak256(bytes("abc")));
        }
        try this.throwString("abc") {
            assert(false);
        } catch Panic(uint code) {
            assert(false);
        } catch (bytes memory err) {
            assert((((err[0] == 0x08) && (err[1] == 0xc3)) && (err[2] == 0x79)) && (err[3] == 0xa0));
        }
        try this.throwOverflow() {
            assert(false);
        } catch Error(string memory m) {
            assert(false);
        } catch {
            assert(false);
        } catch Panic(uint code) {
            assert(code == 0x11);
        }
        try this.throwOverflow() {
            assert(false);
        } catch Error(string memory s) {
            assert(false);
        } catch (bytes memory err) {
            assert((((err[0] == 0x4e) && (err[1] == 0x48)) && (err[2] == 0x7b)) && (err[3] == 0x71));
        }
        try this.noCatchAll() {
            assert(false);
        } catch Error(string memory x) {
            assert(false);
        } catch Panic(uint code) {
            assert((code == 0x11) && (x == 0));
        } catch {
            assert(false);
        }
        try this.throwAssert() {
            assert(false);
        } catch Panic(uint code) {
            assert(code == 0x1);
        }
        try this.noCatchAll() {
            assert(false);
        } catch Panic(uint code) {
            assert(code == 0x11);
        }
        try this.throwDivByZero() {
            assert(false);
        } catch Panic(uint code) {
            assert(code == 0x12);
        }
        try this.throwEnumCast() {
            assert(false);
        } catch Panic(uint code) {
            assert(code == 0x21);
        }
        try this.throwPopEmpty() {
            assert(false);
        } catch Panic(uint code) {
            assert(code == 0x31);
        }
        try this.throwIdxOoB1() {
            assert(false);
        } catch Panic(uint code) {
            assert(code == 0x32);
        }
        try this.throwIdxOoB2() {
            assert(false);
        } catch Panic(uint code) {
            assert(code == 0x32);
        }
        try this.throwIdxOoB3() {
            assert(false);
        } catch Panic(uint code) {
            assert(code == 0x32);
        }
        try this.throwCustom() {
            assert(false);
        } catch (bytes memory data) {
            bytes memory expected = hex"5eb2ac070000000000000000000000000000000000000000000000000000000000000001ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000";
            assert(keccak256(data) == keccak256(expected));
        }
    }
}

contract __IRTest__ {
    function main() public {
        TryCatch08 __this__ = new TryCatch08();
        __testCase617__(__this__);
    }

    function __testCase617__(TryCatch08 __this__) internal {
        __this__.main();
    }
}