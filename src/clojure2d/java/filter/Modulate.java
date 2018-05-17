package clojure2d.java.filter;

import clojure2d.java.Pixels;

public final class Modulate {
    
    public static void process(int[] in, int[] out, int ch, double val) {
        for(int i=ch; i<in.length; i+=4) {
            out[i] = Pixels.constrain( (int)(in[i]*val+0.5),0,255);
        }
    }
        
}
