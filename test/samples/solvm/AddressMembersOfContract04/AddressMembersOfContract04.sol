pragma solidity ^0.4.13;

contract Some {
    function () public payable {}
}

contract AddressMembersOfContract {
    function verify() public {
        Some s = new Some();
        assert(s.balance == 0);
        s.transfer(0 ether);
        assert(s.balance == 0);
        s.send(0 ether);
        assert(s.balance == 0);
        bool res = s.call();
        assert(res);
    }
}

contract __IRTest__ {
    function main() public {
        AddressMembersOfContract __this__ = new AddressMembersOfContract();
        __testCase74__(__this__);
    }

    function __testCase74__(AddressMembersOfContract __this__) internal {
        __this__.verify();
    }
}