package clojure2d.java.reconstruction;

import net.jafama.FastMath;

public class Gaussian extends AFilter {
    double alpha;
    double exp;

    public Gaussian(double radius, double alpha) {
        super(radius);
        this.alpha = alpha;

        exp = FastMath.exp(-alpha*radius*radius);
        init();
    }

    public Gaussian() {
        this(2.0, 2.0);
    }
    
    public double evaluate(double x, double y) {
        return Math.max(0, FastMath.exp(-alpha*x*x)-exp) * Math.max(0, FastMath.exp(-alpha*y*y)-exp);
    }

    public String getName() {
        return "Gaussian";
    }
}
