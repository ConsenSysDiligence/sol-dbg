pragma solidity ^0.8.0;

type Foo is uint256;

contract ExpressionStatement {
    struct Foo {
        uint256 f;
    }

    event E();
    error Err();

    function main() external {
        uint256 a;
        uint256 b;
        uint256[] memory arr = new uint256[](3);
        int256 c;
        Foo memory f;
        uint256;
        a;
        -c;
        a + b;
        a > b;
        (uint256);
        (uint256, 1);
        (uint256, (bool, 1, string, "foo"));
        (a > b) ? a : b;
        (a, b);
        ((a > b) ? (a, b) : (b, a));
        arr[a];
        arr[arr[a]];
        1;
        true;
        f.f;
        ExpressionStatement;
        ExpressionStatement.main;
        ExpressionStatement.Foo;
        ExpressionStatement.E;
        Err;
        Foo;

        (ExpressionStatement, 1);
        (ExpressionStatement.main, 1);
        (ExpressionStatement.Foo, 1);
        (ExpressionStatement.E, 1);
        (Err, 1);
        (Foo, 1);
    }
}

contract __IRTest__ {
    function main() public {
        ExpressionStatement __this__ = new ExpressionStatement();
        __testCase120__(__this__);
    }

    function __testCase120__(ExpressionStatement __this__) internal {
        __this__.main();
    }
}
