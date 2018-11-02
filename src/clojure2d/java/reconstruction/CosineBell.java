package clojure2d.java.reconstruction;

import net.jafama.FastMath;

public class CosineBell extends AFilter {
    double xm;

    public CosineBell(double radius, double xm) {
        super(radius);
        this.xm = xm;
        init();
    }

    public CosineBell() {
        this(2.0,1.2);
    }
    
    private double cosinebell(double x) {
        double xx = FastMath.abs(x);
        if (xx>xm) return 0;
        return 0.5*(1.0+FastMath.cos(FastMath.PI*xx/xm));
    }

    public double evaluate(double x, double y) {
        return cosinebell(FastMath.sqrt(x*x+y*y));
    }

    public String getName() {
        return "Cosine Bell";
    }
}
