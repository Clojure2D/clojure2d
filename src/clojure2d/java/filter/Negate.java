package clojure2d.java.filter;

public final class Negate {

    public static void process(int[] in, int[] out, int ch) {
        for(int i=ch;i<in.length;i+=4) {
            out[i] = 255-in[i];
        }
    }
   
}
