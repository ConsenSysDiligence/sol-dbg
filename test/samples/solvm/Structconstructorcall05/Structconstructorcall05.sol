pragma solidity ^0.5.0;

contract StructConstructorCall {
    struct Member {
        address member;
        string name;
        uint memberSince;
    }

    function main() public {
        address targetMember = address(0xdeadbeef);
        string memory memberName = "boo";
        Member memory m = Member({memberSince: 0, name: memberName, member: targetMember});
        Member memory m1 = Member(targetMember, memberName, 0);
    }
}

contract __IRTest__ {
    function main() public {
        StructConstructorCall __this__ = new StructConstructorCall();
        __testCase53__(__this__);
    }

    function __testCase53__(StructConstructorCall __this__) internal {
        __this__.main();
    }
}