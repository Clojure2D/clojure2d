package clojure2d.java.filter;

import clojure2d.java.Pixels;

public final class Threshold {

    public static void process(int[] in, int[] out, int ch, double amount) {
        int val = (int)(amount * 255 + 0.5);
        for(int i=ch;i<in.length;i+=4) {
            out[i] = in[i] < val ? 0 : 255;
        }
    }
    
    public static void process(int[] in, int[] out, int ch, double amount_low, double amount_high) {
        int val_low = (int)(amount_low * 255 + 0.5);
        int val_high = (int)(amount_high * 255 + 0.5);
        
        for(int i=ch;i<in.length;i+=4) {
            double t = Pixels.smoothStep(val_low, val_high, in[i]);
            out[i] = (int)(0.5+fastmath.java.PrimitiveMath.lerp(0.0,255.0,t));
        }
    }
}
