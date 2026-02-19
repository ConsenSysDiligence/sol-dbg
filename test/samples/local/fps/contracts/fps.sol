contract FunctionPointers {
    function (uint) internal returns (uint) a;
    function (uint) external returns (uint) b;

    struct FPs {
        function (uint) internal returns (uint) a;
        function (uint) external returns (uint) b;
    }

    function id(uint x) internal returns (uint) {
        return x;
    }

    function idExt(uint x) external returns (uint) {
        return x;
    }

    function funArgs(function (uint) internal returns (uint) x, function (uint) external returns (uint) y, FPs memory fps) internal {
        a = x;
        b = y;
        this.extFunArg(fps.b);
    }

    function extFunArg(function (uint) external returns (uint) y) external {
        assert(false);
    }

    function main() external {
        FPs memory f = FPs(id, this.idExt);
        funArgs(id, this.idExt, f);
    }
}
