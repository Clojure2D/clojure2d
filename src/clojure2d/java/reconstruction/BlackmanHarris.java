package clojure2d.java.reconstruction;

import net.jafama.FastMath;

public class BlackmanHarris extends AFilter {
    private final static double A0 =  0.35875;
    private final static double A1 = -0.48829;
    private final static double A2 =  0.14128;
    private final static double A3 = -0.01168;

    private double spread;
    
    public BlackmanHarris(double radius, double spread) {
        super(radius);
        this.spread = 1.0/spread;
        init();
    }

    public BlackmanHarris() {
        this(2.0,1.5);
    }
    
    private double BlackmanHarris1d(double x) {
        if (x < -1.0 || x > 1.0)
            return 0.0;
        x = (x + 1.0) * 0.5;
        x *= FastMath.PI;

        return A0 + A1 * FastMath.cos(2.0 * x) + A2 * FastMath.cos(4.0 * x) + A3 * FastMath.cos(6.0 * x);
    }

    public double evaluate(double x, double y) {
        return BlackmanHarris1d(x*spread) * BlackmanHarris1d(y*spread);
    }

    public String getName() {
        return "Blackmann-Harris";
    }
}
