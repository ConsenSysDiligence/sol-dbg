pragma solidity ^0.6.0;

interface A {
    function doA(uint arg) external returns (uint ret);
}

interface B is A {
    function doB(uint arg) external returns (uint ret);
}

interface C {
    function doX() external;

    function doY() external returns (uint);

    function doZ(uint a) external;
}

contract Test {
    function verify() public {
        bytes4 idA = type(A).interfaceId;
        assert(idA == A.doA.selector);
        assert(idA == 0x092f1b5f);
        bytes4 idB = type(B).interfaceId;
        assert(idB == B.doB.selector);
        assert(idB == 0x4b981cb5);
        bytes4 idC = type(C).interfaceId;
        assert(idC == ((C.doX.selector ^ C.doY.selector) ^ C.doZ.selector));
        assert(idC == 0x0a474301);
    }
}

contract __IRTest__ {
    function main() public {
        Test __this__ = new Test();
        __testCase123__(__this__);
    }

    function __testCase123__(Test __this__) internal {
        __this__.verify();
    }
}