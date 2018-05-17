package clojure2d.java.filter;

import clojure2d.java.Pixels;

public final class ContrastBrightness {

    public static void process(int[] in, int[] out, int ch, double brightness, double contrast) {
        int[] lut = new int[256];
        
        for(int i=0;i<256;i++) {
            lut[i] = Pixels.constrain((int)(0.5 + 255.0 * (((i / 255.0) * brightness - 0.5) * contrast + 0.5)),0,255);
        }
        
        for(int i=ch;i<in.length;i+=4) {
            out[i] = lut[in[i]];
        }
    }
    
}
