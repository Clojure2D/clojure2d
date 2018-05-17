package clojure2d.java.reconstruction;

import net.jafama.FastMath;

public class Gaussian extends AFilter {
    double alpha;
    double expx, expy;

    public Gaussian(double radius, double alpha) {
        super(radius);
        this.alpha = alpha;

        expx = FastMath.exp(-alpha*radius*radius);
        expy = FastMath.exp(-alpha*radius*radius);
        init();
    }

    public double evaluate(double x, double y) {
        return Math.max(0, FastMath.exp(-alpha*x*x)-expx) * Math.max(0, FastMath.exp(-alpha*y*y)-expy);
    }
}
