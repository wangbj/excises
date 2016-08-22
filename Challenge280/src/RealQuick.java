/**
 * Created by wangbj on 8/22/16.
 */
public final class RealQuick {
    private static final int numbersLefthand[] = {
            0, 5, 1, 6, -1, -1, 2, 7, -1, -1, -1, -1, -1, -1, 3, 8,
            -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 4, 9,
    };
    private static final int numbersRighthand[] = {
            0, -1, -1, -1, -1, -1, -1, -1, 1, -1, -1, -1, 2, -1, 3, 4,
            5, -1, -1, -1, -1, -1, -1, -1, 6, -1, -1, -1, 7, -1, 8, 9,
    };

    public static int count(int x) {
        int l, r;
        l = (x >> 5) & 0x1f;
        r = x & 0x1f;
        if (numbersLefthand[l] >= 0 &&
                numbersRighthand[r] >= 0) {
            return numbersLefthand[l] * 10 + numbersRighthand[r];
        }
        return -1;
    }
}
