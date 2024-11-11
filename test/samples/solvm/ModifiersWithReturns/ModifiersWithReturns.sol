contract __IRTest__ {
    modifier noExecBefore() {
        return;
        _;
    }
    modifier returnInmod() {
        _;
        return;
    }


    function return1() public returnInmod returns (uint)   {
        return 1;
    }


    function crashInBodyPrevented() public noExecBefore {
        uint a = 1;
        uint b;

        a/b;
    }

    function main() public {
        crashInBodyPrevented();
        uint v = return1();
        assert(v == 1);
    }
}
