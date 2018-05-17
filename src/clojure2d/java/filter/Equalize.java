package clojure2d.java.filter;

import clojure2d.java.Pixels;

public final class Equalize {

    public static void process(int[] in, int[] out, int ch) {
        double[] histogram = new double[256];
        double d = (1.0 / (in.length >> 2));
        
        for(int i=ch;i<in.length;i+=4) {
            histogram[in[i]] += d;
        }

        int[] lookup = new int[256];
        double sum = 0;
        for(int i=0;i<256;i++) {
            sum += histogram[i];
            lookup[i] = (int)(0.5 + (sum * 255.0));
        }

        for(int i=ch;i<in.length;i+=4) {
            out[i] = lookup[in[i]];
        }

    }
    
}
