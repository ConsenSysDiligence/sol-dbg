pragma solidity 0.7.6;

contract Provider {
    uint256 public v;

    constructor(uint256 val) {
        v = val;
    }
}

contract Some {
    mapping(uint256 => Provider) public providers;

    constructor() {
        providers[1] = new Provider(uint256(100));
        providers[5] = new Provider(uint256(500));
    }
}

contract Test {
    Some public some;

    constructor() {
        some = new Some();
    }

    function verify() public {
        Some s = this.some();
        Provider p1 = s.providers(1);
        Provider p5 = s.providers(5);
        assert(p1.v() == 100);
        assert(p5.v() == 500);
    }
}

contract __IRTest__ {
    function main() public {
        Test __this__ = new Test();
        __testCase114__(__this__);
    }

    function __testCase114__(Test __this__) internal {
        __this__.verify();
    }
}