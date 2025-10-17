pragma solidity 0.8.29;

library Lib {
    function checkFields(address expThis, address expSender, bool nested) external {
        assert(address(this) == expThis);
        assert(expSender == msg.sender);

        if (nested) {
            Lib1.checkFields(expThis, expSender);
        }
    }

    function push(uint[] storage p, uint v, bool nested) external {
        if (nested) {
            Lib1.push(p, v);
        }

        p.push(v);
    }
}

library Lib1 {
    function checkFields(address expThis, address expSender) external {
        assert(address(this) == expThis);
        assert(expSender == msg.sender);
    }

    function push(uint[] storage p, uint v) external {
        p.push(v);
    }
}

contract Foo {
    uint[] a;

    function checkThisAndSender() public returns (address) {
        Lib.checkFields(address(this), msg.sender, false);
        Lib.checkFields(address(this), msg.sender, true);
        return address(Lib);
    }

    function checkStorageMod() public {
        uint oldLen = a.length;
        Lib.push(a, 13, false);
        assert(a.length == oldLen + 1);
        assert(a[oldLen] == 13);

        Lib.push(a, 14, true);
        assert(a.length == oldLen + 2);
        assert(a[oldLen + 1] == 14);
    }

    function main() public {
        checkThisAndSender();
        checkStorageMod();
    }
}