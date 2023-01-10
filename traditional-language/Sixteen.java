import java.math.BigInteger;

public class Sixteen {
    public static void main(String[] args) {
        int power = 1000;
        int sum = 0;
        BigInteger num = BigInteger.valueOf(2L);
        num = num.pow(power);
        while(num.compareTo(BigInteger.ZERO) > 0) {
            sum += num.remainder(BigInteger.TEN).intValue();
            num = num.divide(BigInteger.TEN);
        }
        System.out.println(sum);
    }
}
