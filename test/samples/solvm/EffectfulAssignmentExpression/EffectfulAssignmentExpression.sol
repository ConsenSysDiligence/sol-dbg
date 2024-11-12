pragma solidity ^0.4.24;

contract EffectfulAssignmentExpression {
    uint internal x;

    function main() public {
        assert((x = x + 1) == 1);
    }
}

contract __IRTest__ {
    function main() public {
        EffectfulAssignmentExpression __this__ = new EffectfulAssignmentExpression();
        __testCase33__(__this__);
    }

    function __testCase33__(EffectfulAssignmentExpression __this__) internal {
        __this__.main();
    }
}