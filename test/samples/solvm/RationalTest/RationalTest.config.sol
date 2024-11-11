pragma solidity ^0.4.24;

contract RationalTest {
    function subdenominationEtherTest() public {
        assert(1 wei == 1);
        assert(1 szabo == 1e12);
        assert(1 finney == 1e15);
        assert(1 ether == 1e18);
        assert(1 szabo == 1000000000000 wei);
        assert(1 finney == 1000 szabo);
        assert(1 ether == 1000 finney);
        assert(0.5 ether == 500 finney);
        assert(0.5 finney == 500 szabo);
        assert(0.5 szabo == 500000000000 wei);
        assert(1.5 ether < 2.5 ether);
        assert((-1.5 finney) > (-2 finney));
        assert(0.001 ether == 1 finney);
    }

    function subdenominationTimeTest() public {
        assert(1 == 1 seconds);
        assert(1 minutes == 60 seconds);
        assert(1 hours == 60 minutes);
        assert(1 days == 24 hours);
        assert(1 weeks == 7 days);
        assert(1 years == 365 days);
        assert(0.5 hours == 30 minutes);
        assert(0.1 minutes == 6 seconds);
        assert((2 * 6 seconds) == 0.2 minutes);
    }

    function mathTest() public {
        assert((((-0.7) * (0.8)) * 100 finney) == (-0.056 ether));
        assert(((0.7 ** 2) * 2 ether) == 0.98 ether);
        assert(((0.15 + 10.05) * 10) == 102);
        assert(((+(0.03 + (-0.005))) * 1000) == 25);
        assert(((1.3 / 0.5) * 10) == 26);
        assert(((1.3 / 0.5) * 10) == 26);
        assert(((3.3 % 3) * 10) == 3);
        assert(((0.142857 * (-1000)) * (-1000)) == 142857);
        assert(((-413.0) % 59.0) == (-0));
        assert(((0.1 + 0.2) * 10) == (0.3 * 10));
        assert((0xA * 0.5) == 5);
        assert((0xFE * 0.5) == 127);
    }

    function extremeDenominatedValues() public {
        assert(100000000000000000000000000000000000000000000000000000000000 ether == 100000000000000000000000000000000000000000000000000000000000000000000000000000);
        assert(100000000000000000000000000000000000000000000000000000000000000 finney == 100000000000000000000000000000000000000000000000000000000000000000000000000000);
        assert(100000000000000000000000000000000000000000000000000000000000000000 szabo == 100000000000000000000000000000000000000000000000000000000000000000000000000000);
        assert(100000000000000000000000000000000000000000000000000000000000000000000000000000 wei == 100000000000000000000000000000000000000000000000000000000000000000000000000000);
        assert(100000000000000000000000000000000000000000000000000000000000000000000000000000 seconds == 100000000000000000000000000000000000000000000000000000000000000000000000000000);
        assert(1000000000000000000000000000000000000000000000000000000000000000000000000000 minutes == 60000000000000000000000000000000000000000000000000000000000000000000000000000);
        assert(10000000000000000000000000000000000000000000000000000000000000000000000000 hours == 36000000000000000000000000000000000000000000000000000000000000000000000000000);
        assert(1000000000000000000000000000000000000000000000000000000000000000000000000 days == 86400000000000000000000000000000000000000000000000000000000000000000000000000);
        assert(100000000000000000000000000000000000000000000000000000000000000000000000 weeks == 60480000000000000000000000000000000000000000000000000000000000000000000000000);
        assert(1000000000000000000000000000000000000000000000000000000000000000000000 years == 31536000000000000000000000000000000000000000000000000000000000000000000000000);
    }
}

contract __IRTest__ {
    function main() public {
        RationalTest __this__ = new RationalTest();
        __testCase359__(__this__);
        __testCase373__(__this__);
        __testCase387__(__this__);
        __testCase401__(__this__);
    }

    function __testCase359__(RationalTest __this__) internal {
        __this__.subdenominationEtherTest();
    }

    function __testCase373__(RationalTest __this__) internal {
        __this__.subdenominationTimeTest();
    }

    function __testCase387__(RationalTest __this__) internal {
        __this__.mathTest();
    }

    function __testCase401__(RationalTest __this__) internal {
        __this__.extremeDenominatedValues();
    }
}