package clojure2d.java.filter;

import fastmath.java.PrimitiveMath;

public final class Tint {
    
    public static void process1(int[] in, int[] out, int ch, int val) {
        int[] lookup = new int[256];

        for(int i=0; i<256; i++) {
            lookup[i] = (0xff00 & ((i+1) * val)) >> 8;
        }

        for(int i=ch; i<in.length; i+=4) {
            out[i] = lookup[in[i]];
        }
    }

    public static void process3(int[] in, int[] out, int ch, int low_v, int mid_v, int high_v) {
        for(int i=ch;i<in.length;i+=4) {
            int v = in[i];
            double res = v < 128 ? PrimitiveMath.norm(v,0,127,low_v,mid_v) : PrimitiveMath.norm(v,128,255,mid_v,high_v);
            out[i] = (int)(0.5 + res);
        }
    }

    
    public static void process2(int[] in, int[] out, int ch, int low_v, int high_v) {
        for(int i=ch;i<in.length;i+=4) {
            int v = in[i];
            double res = PrimitiveMath.norm(v,0,255,low_v,high_v);
            out[i] = (int)(0.5 + res);
        }
    }

}
