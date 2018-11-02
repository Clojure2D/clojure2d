package clojure2d.java.reconstruction;

import net.jafama.FastMath;

public class Hann extends AFilter {
    private double spread;

    public Hann(double radius, double spread) {
        super(radius);
        this.spread = 1.0/spread;
        init();
    }

    public Hann() {
        this(2.0,1.2);
    }
    
    private double sinc(double v) {
        if (v<1.0e-5) return 1.0;
        double pv = FastMath.PI*v;
        return FastMath.sin(pv)/pv;
    }

    private double hannSinc(double v) {
        double vv = FastMath.abs(v);
        if (vv>radius) return 0.0;
        return sinc(vv)*(0.5+0.5*FastMath.cos(FastMath.PI*vv/radius));
    }

    public double evaluate(double x, double y) {
        return hannSinc(x*spread)*hannSinc(y*spread);
    }

    public String getName() {
        return "Hann";
    }
}
