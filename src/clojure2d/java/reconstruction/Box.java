package clojure2d.java.reconstruction;

public class Box extends AFilter {
    public Box(double radius) {
        super(radius);
        init();
    }

    public Box() {
        this(0.5);
    }
    
    public double evaluate(double x, double y) { 
        return 1.0;
    }

    public String getName() {
        return "Box";
    }
}
