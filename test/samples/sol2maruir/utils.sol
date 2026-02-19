/**
 * Utility library to create byte values from strings and compare byte values.
 */
library BytesLib {
    function isSame(bytes memory a, bytes memory b) public pure returns (bool) {
        if (a.length != b.length) {
            return false;
        }

        for (uint256 i = 0; i < a.length; i++) {
            if (a[i] != b[i]) {
                return false;
            }
        }

        return true;
    }
}
