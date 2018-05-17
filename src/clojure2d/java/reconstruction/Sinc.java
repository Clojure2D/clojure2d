package clojure2d.java.reconstruction;

import net.jafama.FastMath;

public class Sinc extends AFilter {
    double tau;

    public Sinc(double radius, double tau) {
        super(radius);
        this.tau = tau;
        init();
    }

    private double sinc(double v) {
        double vv = FastMath.abs(v);
        if (vv<1.0e-5) return 1.0;
        double pv = FastMath.PI*vv;
        return FastMath.sin(pv)/pv;
    }

    private double windowedSinc(double v, double r) {
        double vv = FastMath.abs(v);
        if (v>r) return 0.0;
        double lanczos = sinc(v/tau);
        return sinc(v)*lanczos;
    }

    public double evaluate(double x, double y) {
        return windowedSinc(x, radius)*windowedSinc(y, radius);
    }
}
