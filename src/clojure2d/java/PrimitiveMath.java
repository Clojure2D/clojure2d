// Primitive Math operations with all variants
package clojure2d.java;

public final class PrimitiveMath {
    public static long add(long a, long b) { return a+b; }
    public static double add(double a, long b) { return a+b; }
    public static double add(long a, double b) { return a+b; }
    public static double add(double a, double b) { return a+b; }

    public static long subtract(long a, long b) { return a-b; }
    public static double subtract(double a, long b) { return a-b; }
    public static double subtract(long a, double b) { return a-b; }
    public static double subtract(double a, double b) { return a-b; }
    
    public static long negate(long a) { return -a; }
    public static double negate(double a) { return -a; }
    
    public static long multiply(long a, long b) { return a*b; }
    public static double multiply(double a, long b) { return a*b; }
    public static double multiply(long a, double b) { return a*b; }
    public static double multiply(double a, double b) { return a*b; }

    public static long divide(long a, long b) { return a/b; }
    public static double divide(double a, long b) { return a/b; }
    public static double divide(long a, double b) { return a/b; }
    public static double divide(double a, double b) { return a/b; }
    
    public static long shiftLeft(long a, long n) { return a << n; }
    public static long shiftRight(long a, long n) { return a >> n; }
    public static long unsignedShiftRight(long a, long n) { return a >>> n; }
    public static int unsignedShiftRight(int a, long n) { return a >>> n; }

    public static long bitAnd(long a, long b) { return a & b; }
    public static long bitOr(long a, long b) { return a | b; }
    public static long bitXor(long a, long b) { return a ^ b; }
    public static long bitNot(long a) { return ~a; }

    public static long inc(long a) { return a+1L; }
    public static double inc(double a) { return a+1.0; }
    public static long dec(long a) { return a-1L; }
    public static double dec(double a) { return a-1.0; }

    public static double reciprocal(long a) { return 1.0/a; }
    public static double reciprocal(double a) { return 1.0/a; }
    
    public static long remainder(long a, long b) { return a%b; }
    public static double remainder(double a, long b) { return a%b; }
    public static double remainder(long a, double b) { return a%b; }
    public static double remainder(double a, double b) { return a%b; }

    public static long modulus(long a, long b) { long t=a%b; return ((t==0L) || (a>0L)==(b>0L))?t:t+b; }
    public static double modulus(double a, long b) { double t=a%b; return ((t==0.0) || (a>0.0)==(b>0L))?t:t+b; }
    public static double modulus(long a, double b) { double t=a%b; return ((t==0.0) || (a>0L)==(b>0.0))?t:t+b; }
    public static double modulus(double a, double b) { double t=a%b; return ((t==0.0) || (a>0.0)==(b>0.0))?t:t+b; }

    public static long quotient(long a, long b) { return a/b; }
    public static double quotient(double a, long b) { return (double)((long)(a/b)); }
    public static double quotient(long a, double b) { return (double)((long)(a/b)); }
    public static double quotient(double a, double b) { return (double)((long)(a/b)); }

    public static boolean lt(long a, long b) { return a<b; }
    public static boolean lt(double a, long b) { return a<b; }
    public static boolean lt(long a, double b) { return a<b; }
    public static boolean lt(double a, double b) { return a<b; }

    public static boolean gt(long a, long b) { return a>b; }
    public static boolean gt(double a, long b) { return a>b; }
    public static boolean gt(long a, double b) { return a>b; }
    public static boolean gt(double a, double b) { return a>b; }

    public static boolean lte(long a, long b) { return a<=b; }
    public static boolean lte(double a, long b) { return a<=b; }
    public static boolean lte(long a, double b) { return a<=b; }
    public static boolean lte(double a, double b) { return a<=b; }

    public static boolean gte(long a, long b) { return a>=b; }
    public static boolean gte(double a, long b) { return a>=b; }
    public static boolean gte(long a, double b) { return a>=b; }
    public static boolean gte(double a, double b) { return a>=b; }

    public static boolean eq(long a, long b) { return a==b; }
    public static boolean eq(double a, long b) { return a==b; }
    public static boolean eq(long a, double b) { return a==b; }
    public static boolean eq(double a, double b) { return a==b; }

    public static boolean neq(long a, long b) { return a!=b; }
    public static boolean neq(double a, long b) { return a!=b; }
    public static boolean neq(long a, double b) { return a!=b; }
    public static boolean neq(double a, double b) { return a!=b; }

    public static boolean isZero(long a) { return a==0L; }
    public static boolean isZero(double a) { return a==0.0; }
    public static boolean isNeg(long a) { return a<0L; }
    public static boolean isNeg(double a) { return a<0.0; }
    public static boolean isPos(long a) { return a>0L; }
    public static boolean isPos(double a) { return a>0.0; }
    
    public static boolean and(boolean a, boolean b) { return a && b; }
    public static boolean or(boolean a, boolean b) { return a || b; }
    public static boolean not(boolean a) { return !a; }
    public static boolean xor(boolean a, boolean b) { return (a || b) && !(a && b); }

    public static long min(long a, long b) { return a<b?a:b; }
    public static double min(double a, long b) { return a<b?a:b; }
    public static double min(long a, double b) { return a<b?a:b; }
    public static double min(double a, double b) { return a<b?a:b; }
    
    public static long max(long a, long b) { return a>b?a:b; }
    public static double max(double a, long b) { return a>b?a:b; }
    public static double max(long a, double b) { return a>b?a:b; }
    public static double max(double a, double b) { return a>b?a:b; }
}
