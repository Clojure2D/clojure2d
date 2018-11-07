package clojure2d.java.reconstruction;

import net.jafama.FastMath;

public class Triangle extends AFilter {
    private double spread;
    
    public Triangle(double radius, double spread) {
        super(radius);
        this.spread = 1.0/spread;
        init();
    }

    public Triangle() {
        this(2.0,1.2);
    }

    public double evaluate(double x, double y) {
        return Math.max(0.0, 1.0-FastMath.abs(x*spread))*Math.max(0.0, 1.0-FastMath.abs(y*spread));
    }

    public String getName() {
        return "Triangle";
    }
}
