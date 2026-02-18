pragma solidity 0.4.24;

contract Test {
    function verify() public {
        throw;
    }
}

contract __IRTest__ {
    function main() public payable {
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
