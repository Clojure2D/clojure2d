package clojure2d.java.reconstruction;

public class Box extends AFilter {
    public Box(double radius) {
        super(radius);
        init();
    }

    public double evaluate(double x, double y) { 
        return 1.0;
    }
}
