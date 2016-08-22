/**
 * Created by wangbj on 8/22/16.
 */

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class Main {
    public static void main(String[] args) {
        BufferedReader stdin = null;
        try {
            stdin = new BufferedReader(new InputStreamReader(System.in));

            getlines:
            while (true) {
                String line = stdin.readLine();
                if (line == null || line.isEmpty()) break;
                byte[] bs = line.getBytes();
                int x = 0, res;

                for (int i = 0; i < bs.length; i++) {
                    if (bs[i] == '0') {
                        x = 2 * x;
                    } else if (bs[i] == '1') {
                        x = 2 * x + 1;
                    } else {
                        System.out.println("Invalid");
                        break getlines;
                    }
                }
                if (x > 0x3ff || bs.length != 10) {
                    System.out.println("Invalid");
                    break getlines;
                }
                res = RealQuick.count(x);
                if (res < 0) {
                    System.out.println("Invalid");
                } else {
                    System.out.println(String.format("%d", res));
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            if (stdin != null) {
                try {
                    stdin.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }
    }
}
