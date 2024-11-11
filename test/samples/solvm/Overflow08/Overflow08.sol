pragma solidity 0.8.3;

contract InlineInitializerArithmetic {
    uint8 internal a = 255;
    uint8 internal b = a + 1;
}

contract ModifierArgArithmetic {
    modifier M(int8 m) {
        _;
    }

    function foo(int8 x) public M(x + 1) {}
}

contract Base {
    constructor(int8 x) {}
}

contract BaseConstructorArgArithmetic is Base {
    constructor(int8 x) Base(x + 1) {}
}

contract Overflow08 {
    function add_u8(uint8 x, uint8 y) external returns (uint8) {
        return x + y;
    }

    function add_u8_unchecked(uint8 x, uint8 y) external returns (uint8) {
        unchecked {
            return x + y;
        }
    }

    function add_i8(int8 x, int8 y) external returns (int8) {
        return x + y;
    }

    function add_i8_unchecked(int8 x, int8 y) external returns (int8) {
        unchecked {
            return x + y;
        }
    }

    function sub_u8(uint8 x, uint8 y) external returns (uint8) {
        return x - y;
    }

    function sub_u8_unchecked(uint8 x, uint8 y) external returns (uint8) {
        unchecked {
            return x - y;
        }
    }

    function sub_i8(int8 x, int8 y) external returns (int8) {
        return x - y;
    }

    function sub_i8_unchecked(int8 x, int8 y) external returns (int8) {
        unchecked {
            return x - y;
        }
    }

    function mul_u8(uint8 x, uint8 y) external returns (uint8) {
        return x * y;
    }

    function mul_u8_unchecked(uint8 x, uint8 y) external returns (uint8) {
        unchecked {
            return x * y;
        }
    }

    function mul_i8(int8 x, int8 y) external returns (int8) {
        return x * y;
    }

    function mul_i8_unchecked(int8 x, int8 y) external returns (int8) {
        unchecked {
            return x * y;
        }
    }

    function div_i8(int8 x, int8 y) external returns (int8) {
        return x / y;
    }

    function div_i8_unchecked(int8 x, int8 y) external returns (int8) {
        unchecked {
            return x / y;
        }
    }

    function neg_i8(int8 x) external returns (int8) {
        return -x;
    }

    function neg_i8_unchecked(int8 x) external returns (int8) {
        unchecked {
            return -x;
        }
    }

    function exp_i8(int8 x, uint8 exp) external returns (int8) {
        return x ** exp;
    }

    function exp_i8_unchecked(int8 x, uint8 exp) external returns (int8) {
        unchecked {
            return x ** exp;
        }
    }

    function inc_i8(int8 x) external returns (int8) {
        return x++;
    }

    function inc_i8_unchecked(int8 x) external returns (int8) {
        unchecked {
            return x++;
        }
    }

    function dec_i8(int8 x) external returns (int8) {
        return --x;
    }

    function dec_i8_unchecked(int8 x) external returns (int8) {
        unchecked {
            return --x;
        }
    }

    function comp_assign_add(int8 x, int8 y) external returns (int8) {
        return x += y;
    }

    function comp_assign_sub(int8 x, int8 y) external returns (int8) {
        return x -= y;
    }

    function comp_assign_mul(int8 x, int8 y) external returns (int8) {
        return x *= y;
    }

    function comp_assign_div(int8 x, int8 y) external returns (int8) {
        return x /= y;
    }
}

contract __IRTest__ {
    function main() public {
        Overflow08 __this__ = new Overflow08();
        __testCase427__(__this__);
        __testCase458__(__this__);
        __testCase499__(__this__);
        __testCase530__(__this__);
        __testCase561__(__this__);
        __testCase592__(__this__);
        __testCase633__(__this__);
        __testCase664__(__this__);
        __testCase705__(__this__);
        __testCase736__(__this__);
        __testCase767__(__this__);
        __testCase808__(__this__);
        __testCase839__(__this__);
        __testCase870__(__this__);
        __testCase911__(__this__);
        __testCase942__(__this__);
        __testCase983__(__this__);
        __testCase1014__(__this__);
        __testCase1045__(__this__);
        __testCase1076__(__this__);
        __testCase1117__(__this__);
        __testCase1148__(__this__);
        __testCase1189__(__this__);
        __testCase1220__(__this__);
        __testCase1261__(__this__);
        __testCase1302__(__this__);
        __testCase1330__(__this__);
        __testCase1358__(__this__);
        __testCase1386__(__this__);
        __testCase1414__(__this__);
        __testCase1442__(__this__);
        __testCase1483__(__this__);
        __testCase1514__(__this__);
        __testCase1545__(__this__);
        __testCase1586__(__this__);
        __testCase1617__(__this__);
        __testCase1648__(__this__);
        __testCase1686__(__this__);
        __testCase1714__(__this__);
        __testCase1742__(__this__);
        __testCase1780__(__this__);
        __testCase1808__(__this__);
        __testCase1836__(__this__);
        __testCase1877__(__this__);
        __testCase1918__(__this__);
        __testCase1959__(__this__);
        __testCase2000__(__this__);

        try new InlineInitializerArithmetic() {
            assert(false);
        } catch {
            assert(true);
        }

        ModifierArgArithmetic __this2__ = new ModifierArgArithmetic();
        __testCase2048__(__this2__);
        __testCase2071__(__this2__);
        BaseConstructorArgArithmetic __this3__ = new BaseConstructorArgArithmetic(int8(126));

        try new BaseConstructorArgArithmetic(int8(127)) {
            assert(false);
        } catch {
            assert(true);
        }
    }

    function __testCase427__(Overflow08 __this__) internal {
        uint8 ret_427_0 = __this__.add_u8(uint8(254), uint8(1));
        assert(ret_427_0 == uint8(255));
    }

    function __testCase458__(Overflow08 __this__) internal {
        try __this__.add_u8(uint8(254), uint8(2)) {
            assert(false);
        } catch Error(string memory reason) {
            assert(false);
        } catch {
            assert(true);
        }
    }

    function __testCase499__(Overflow08 __this__) internal {
        uint8 ret_499_0 = __this__.add_u8_unchecked(uint8(254), uint8(2));
        assert(ret_499_0 == uint8(0));
    }

    function __testCase530__(Overflow08 __this__) internal {
        int8 ret_530_0 = __this__.add_i8(int8(126), int8(1));
        assert(ret_530_0 == int8(127));
    }

    function __testCase561__(Overflow08 __this__) internal {
        int8 ret_561_0 = __this__.add_i8(int8(-127), int8(-1));
        assert(ret_561_0 == int8(-128));
    }

    function __testCase592__(Overflow08 __this__) internal {
        try __this__.add_i8(int8(126), int8(2)) {
            assert(false);
        } catch Error(string memory reason) {
            assert(false);
        } catch {
            assert(true);
        }
    }

    function __testCase633__(Overflow08 __this__) internal {
        int8 ret_633_0 = __this__.add_i8_unchecked(int8(126), int8(2));
        assert(ret_633_0 == int8(-128));
    }

    function __testCase664__(Overflow08 __this__) internal {
        try __this__.add_i8(int8(-127), int8(-2)) {
            assert(false);
        } catch Error(string memory reason) {
            assert(false);
        } catch {
            assert(true);
        }
    }

    function __testCase705__(Overflow08 __this__) internal {
        int8 ret_705_0 = __this__.add_i8_unchecked(int8(-127), int8(-2));
        assert(ret_705_0 == int8(127));
    }

    function __testCase736__(Overflow08 __this__) internal {
        uint8 ret_736_0 = __this__.sub_u8(uint8(3), uint8(1));
        assert(ret_736_0 == uint8(2));
    }

    function __testCase767__(Overflow08 __this__) internal {
        try __this__.sub_u8(uint8(1), uint8(3)) {
            assert(false);
        } catch Error(string memory reason) {
            assert(false);
        } catch {
            assert(true);
        }
    }

    function __testCase808__(Overflow08 __this__) internal {
        uint8 ret_808_0 = __this__.sub_u8_unchecked(uint8(1), uint8(3));
        assert(ret_808_0 == uint8(254));
    }

    function __testCase839__(Overflow08 __this__) internal {
        int8 ret_839_0 = __this__.sub_i8(int8(1), int8(3));
        assert(ret_839_0 == int8(-2));
    }

    function __testCase870__(Overflow08 __this__) internal {
        try __this__.sub_i8(int8(-2), int8(127)) {
            assert(false);
        } catch Error(string memory reason) {
            assert(false);
        } catch {
            assert(true);
        }
    }

    function __testCase911__(Overflow08 __this__) internal {
        int8 ret_911_0 = __this__.sub_i8_unchecked(int8(-2), int8(127));
        assert(ret_911_0 == int8(127));
    }

    function __testCase942__(Overflow08 __this__) internal {
        try __this__.sub_i8(int8(0), int8(-128)) {
            assert(false);
        } catch Error(string memory reason) {
            assert(false);
        } catch {
            assert(true);
        }
    }

    function __testCase983__(Overflow08 __this__) internal {
        int8 ret_983_0 = __this__.sub_i8_unchecked(int8(0), int8(-128));
        assert(ret_983_0 == int8(-128));
    }

    function __testCase1014__(Overflow08 __this__) internal {
        uint8 ret_1014_0 = __this__.mul_u8_unchecked(uint8(2), uint8(128));
        assert(ret_1014_0 == uint8(0));
    }

    function __testCase1045__(Overflow08 __this__) internal {
        uint8 ret_1045_0 = __this__.mul_u8(uint8(2), uint8(127));
        assert(ret_1045_0 == uint8(254));
    }

    function __testCase1076__(Overflow08 __this__) internal {
        try __this__.mul_u8(uint8(2), uint8(128)) {
            assert(false);
        } catch Error(string memory reason) {
            assert(false);
        } catch {
            assert(true);
        }
    }

    function __testCase1117__(Overflow08 __this__) internal {
        int8 ret_1117_0 = __this__.mul_i8_unchecked(int8(2), int8(64));
        assert(ret_1117_0 == int8(-128));
    }

    function __testCase1148__(Overflow08 __this__) internal {
        try __this__.mul_i8(int8(2), int8(64)) {
            assert(false);
        } catch Error(string memory reason) {
            assert(false);
        } catch {
            assert(true);
        }
    }

    function __testCase1189__(Overflow08 __this__) internal {
        int8 ret_1189_0 = __this__.mul_i8_unchecked(int8(-2), int8(65));
        assert(ret_1189_0 == int8(126));
    }

    function __testCase1220__(Overflow08 __this__) internal {
        try __this__.mul_i8(int8(-2), int8(65)) {
            assert(false);
        } catch Error(string memory reason) {
            assert(false);
        } catch {
            assert(true);
        }
    }

    function __testCase1261__(Overflow08 __this__) internal {
        try __this__.div_i8(int8(-128), int8(-1)) {
            assert(false);
        } catch Error(string memory reason) {
            assert(false);
        } catch {
            assert(true);
        }
    }

    function __testCase1302__(Overflow08 __this__) internal {
        int8 ret_1302_0 = __this__.div_i8_unchecked(int8(-128), int8(-1));
        assert(ret_1302_0 == int8(-128));
    }

    function __testCase1330__(Overflow08 __this__) internal {
        int8 ret_1330_0 = __this__.neg_i8_unchecked(int8(-128));
        assert(ret_1330_0 == int8(-128));
    }

    function __testCase1358__(Overflow08 __this__) internal {
        int8 ret_1358_0 = __this__.neg_i8(int8(-127));
        assert(ret_1358_0 == int8(127));
    }

    function __testCase1386__(Overflow08 __this__) internal {
        int8 ret_1386_0 = __this__.neg_i8(int8(-127));
        assert(ret_1386_0 == int8(127));
    }

    function __testCase1414__(Overflow08 __this__) internal {
        int8 ret_1414_0 = __this__.neg_i8(int8(127));
        assert(ret_1414_0 == int8(-127));
    }

    function __testCase1442__(Overflow08 __this__) internal {
        try __this__.neg_i8(int8(-128)) {
            assert(false);
        } catch Error(string memory reason) {
            assert(false);
        } catch {
            assert(true);
        }
    }

    function __testCase1483__(Overflow08 __this__) internal {
        int8 ret_1483_0 = __this__.exp_i8_unchecked(int8(2), uint8(7));
        assert(ret_1483_0 == int8(-128));
    }

    function __testCase1514__(Overflow08 __this__) internal {
        int8 ret_1514_0 = __this__.exp_i8_unchecked(int8(2), uint8(8));
        assert(ret_1514_0 == int8(0));
    }

    function __testCase1545__(Overflow08 __this__) internal {
        try __this__.exp_i8(int8(2), uint8(7)) {
            assert(false);
        } catch Error(string memory reason) {
            assert(false);
        } catch {
            assert(true);
        }
    }

    function __testCase1586__(Overflow08 __this__) internal {
        int8 ret_1586_0 = __this__.exp_i8(int8(-2), uint8(7));
        assert(ret_1586_0 == int8(-128));
    }

    function __testCase1617__(Overflow08 __this__) internal {
        int8 ret_1617_0 = __this__.exp_i8_unchecked(int8(-3), uint8(5));
        assert(ret_1617_0 == int8(13));
    }

    function __testCase1648__(Overflow08 __this__) internal {
        try __this__.exp_i8(int8(-3), uint8(5)) {
            assert(false);
        } catch Error(string memory reason) {
            assert(false);
        } catch {
            assert(true);
        }
    }

    function __testCase1686__(Overflow08 __this__) internal {
        int8 ret_1686_0 = __this__.inc_i8_unchecked(int8(127));
        assert(ret_1686_0 == int8(127));
    }

    function __testCase1714__(Overflow08 __this__) internal {
        int8 ret_1714_0 = __this__.inc_i8(int8(126));
        assert(ret_1714_0 == int8(126));
    }

    function __testCase1742__(Overflow08 __this__) internal {
        try __this__.inc_i8(int8(127)) {
            assert(false);
        } catch Error(string memory reason) {
            assert(false);
        } catch {
            assert(true);
        }
    }

    function __testCase1780__(Overflow08 __this__) internal {
        int8 ret_1780_0 = __this__.dec_i8_unchecked(int8(-128));
        assert(ret_1780_0 == int8(127));
    }

    function __testCase1808__(Overflow08 __this__) internal {
        int8 ret_1808_0 = __this__.dec_i8(int8(-127));
        assert(ret_1808_0 == int8(-128));
    }

    function __testCase1836__(Overflow08 __this__) internal {
        try __this__.dec_i8(int8(-128)) {
            assert(false);
        } catch Error(string memory reason) {
            assert(false);
        } catch {
            assert(true);
        }
    }

    function __testCase1877__(Overflow08 __this__) internal {
        try __this__.comp_assign_add(int8(127), int8(1)) {
            assert(false);
        } catch Error(string memory reason) {
            assert(false);
        } catch {
            assert(true);
        }
    }

    function __testCase1918__(Overflow08 __this__) internal {
        try __this__.comp_assign_sub(int8(-127), int8(2)) {
            assert(false);
        } catch Error(string memory reason) {
            assert(false);
        } catch {
            assert(true);
        }
    }

    function __testCase1959__(Overflow08 __this__) internal {
        try __this__.comp_assign_mul(int8(65), int8(2)) {
            assert(false);
        } catch Error(string memory reason) {
            assert(false);
        } catch {
            assert(true);
        }
    }

    function __testCase2000__(Overflow08 __this__) internal {
        try __this__.comp_assign_div(int8(-128), int8(-1)) {
            assert(false);
        } catch Error(string memory reason) {
            assert(false);
        } catch {
            assert(true);
        }
    }

    function __testCase2048__(ModifierArgArithmetic __this2__) internal {
        __this2__.foo(int8(126));
    }

    function __testCase2071__(ModifierArgArithmetic __this2__) internal {
        try __this2__.foo(int8(127)) {
            assert(false);
        } catch Error(string memory reason) {
            assert(false);
        } catch {
            assert(true);
        }
    }
}
