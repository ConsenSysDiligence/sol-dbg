pragma solidity ^0.4.13;

contract Test {
    function verify() public {
        throw;
    }
}

contract __IRTest__ {
    function main() public {
        Test __this__ = new Test();
        __testCase21__(__this__);
    }

    function __testCase21__(Test __this__) internal {
        bool res;
        bytes memory retData;
        bytes memory data;
        data = abi.encodeWithSignature("verify()");
        res = address(__this__).call(data);
        assert(!res);
    }
}