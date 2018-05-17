package clojure2d.java.filter;

import clojure2d.java.Pixels;

public final class Normalize {

    public static void process(int[] in, int[] out, int ch) {
        int mn = Integer.MAX_VALUE;
        int mx = Integer.MIN_VALUE;
        
        for(int i=ch;i<in.length;i+=4) {
            int v = in[i];
            mn = v < mn ? v : mn;
            mx = v > mx ? v : mx;
        }

        for(int i=ch;i<in.length;i+=4) {
            int v = in[i];
            out[i] = (int)(0.5 + 255.0 * fastmath.java.PrimitiveMath.norm(v,mn,mx));
        }
    }
    
}
