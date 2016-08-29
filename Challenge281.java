import java.util.*;

public class Main {

    public static int fromHex(char c) {
        if (c >= '0' && c <= '9') {
            return (int)c - '0';
        } else if (c >= 'a' && c <= 'f') {
            return (int)c - 'a' + 10;
        } else if (c >= 'A' && c <= 'F') {
            return (int)c - 'A' + 10;
        } else {
            return -1;
        }
    }
    public static Integer[] baseConvert(String number) {
        Integer[] converted = new Integer[] {
            -1,
              0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0,
        };

        for (char c : number.toCharArray()) {
            for (int i = 1; i <= 16; i++) {
                if (converted[i] == -1) continue;

                int x = fromHex(c);
                if (x >= i) {
                    converted[i] = -1;
                    continue;
                }
                converted[i] = i * converted[i] + x;
            }
        }
        return converted;
    }
    public static void main(String[] args) {
        String ex1 = "e123F";

        Integer [] ex1Converted = baseConvert(ex1);

        for (int i = 1; i <= 16; i++) {
            if (ex1Converted[i] != -1) {
                System.out.println("base " + i + " => " + ex1Converted[i]);
            }
        }
    }
}
