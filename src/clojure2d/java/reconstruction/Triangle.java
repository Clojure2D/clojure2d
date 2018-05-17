package clojure2d.java.reconstruction;

import net.jafama.FastMath;

public class Triangle extends AFilter {

    public Triangle(double radius) {
        super(radius);
        init();
    }

    public double evaluate(double x, double y) {
        return Math.max(0.0, radius-FastMath.abs(x))*Math.max(0.0, radius-FastMath.abs(y));
    }
}
