contract Foo {
    struct S_static {
        int8 x;
        uint256 y;
        bool b;
        address addrs;
    }


    struct S1 {
        int8 x;
        uint256 y;
        bool b;
        address[] addrs;
    }

    struct S_nested_static_static {
        int16 t;
        S_static s;
        bytes3 b;
    }

    struct S_nested_dynamic_static {
        int16[] t;
        S_static s;
        bytes3 b;
    }

    struct S_nested_static_dynamic {
        int16 t;
        S1 s;
        bytes3 b;
    }

    struct S_struct_arr {
        int8 x;
        S1[] sArr;
    }

    enum E {
        A,
        B,
        C
    }

    function takeUint256(uint256 x) public {}
    function takeInt256(int256 x) public {}
    function takeFourInts(uint16 x, int8 y, uint24 z, int48 w) public {}
    function takeBool(bool b) public {}
    function takeAddr(address a) public {}
    function takeFixedBytes(bytes4 b) public {}
    function takeMixed(bool b, uint8 d, address a, bytes1 b1, int16 i) public {}
    function takeBytes(bytes calldata b) public {}
    function takeString(string calldata b) public {}
    function takeArray(int16[] calldata a) public {}
    function takeFixedSizeArray(int8[4] calldata a) public {}
    function take2DArray(bytes2[][] calldata a) public {}
    function takeFixedSize2DArray1(int8[4][2] calldata a) public {}
    function takeFixedSize2DArray2(int8[][2] calldata a) public {}
    function takeFixedSize2DArray3(int8[4][] calldata a) public {}
    function takeStruct(S1 calldata s) public {}
    function takeStaticStruct(S_static calldata s) public {}
    function takeNestedStaticStaticStruct(S_nested_static_static calldata s) public {}
    function takeNestedDynamicStaticStruct(S_nested_dynamic_static calldata s) public {}
    function takeNestedStaticDynamicStruct(S_nested_static_dynamic calldata s) public {}
    function takeStructArrStatic(S_static[] calldata s) public {}
    function takeStructArrDynamic(S1[] calldata s) public {}
    function takeStructStructArr(S_struct_arr[] calldata s) public {}
    function takeEnum(E s) public {}
    function takeContract(Foo f) public {}
}
