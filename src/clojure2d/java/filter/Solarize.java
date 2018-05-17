package clojure2d.java.filter;

public final class Solarize {

    public static void process(int[] in, int[] out, int ch) {
        for(int i=ch;i<in.length;i+=4) {
            int v = in[i];
            out[i] = (int)(0.5+(v > 127.5 ? 2.0*(v-127.5) : 2.0*(127.5-v)));
        }
    }
   
}
