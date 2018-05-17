package clojure2d.java.filter;

import clojure2d.java.Pixels;

public final class Posterize {

        public static void process(int[] in, int[] out, int ch, int num_levels) {
            int[] levels = new int[256];

            for(int i=0;i<256;i++) {
                levels[i] = 255 * ( (num_levels * i) >> 8) / (num_levels - 1);
            }

            for(int i=ch;i<in.length;i+=4) {
                out[i] = levels[in[i]];
            }
        }
}
