pragma solidity 0.8.26;

contract __IRTest__ {
    function retOne() public returns (uint) {
        return 43;
    }

    function retTwo() public returns (uint, uint) {
        return (45, 46);
    }

    function add(uint x, uint y) public returns (uint) {
        return x + y;
    }

    function retThree(uint t) public returns (uint, uint, uint) {
        return (t + 1, t, t - 1);
    }

    function abs(int t) public returns (uint) {
        if (t < 0) {
            return uint(-t);
        } else {
            return uint(t);
        }
    }

    function main() public returns (uint res) {
        retOne();
        (uint a, uint b) = retTwo();
        // 45, 46
        uint c = add(a, b);
        // 91
        (uint x, , uint y) = (1, 2, 3);

        res = c + x + y;
        // 95

        res += 1;
        // 96

        uint d;
        uint e;

        bool t = !true;

        uint f;
        if (t) {
            f = 1000;
        } else {
            f = 2000;
        }

        (d, , e) = retThree(res);
        // 192

        res = f + d + e;
        // 2192

        res -= abs(-1000);
        // 1192
        return res;
    }
}
