package clojure2d.java.filter;

import clojure2d.java.Pixels;

public final class Quantile {

    private static void SWAP(int[] tmp, int a, int b) {
        if (tmp[a]>tmp[b]) {
            int t = tmp[a];
            tmp[a]=tmp[b];
            tmp[b]=t;
        }
    }

    private static void sort(int[] vals) {
        SWAP(vals, 0, 1);
        SWAP(vals, 3, 4);
        SWAP(vals, 6, 7);
        SWAP(vals, 1, 2);
        SWAP(vals, 4, 5);
        SWAP(vals, 7, 8);
        SWAP(vals, 0, 1);
        SWAP(vals, 3, 4);
        SWAP(vals, 6, 7);
        SWAP(vals, 0, 3);
        SWAP(vals, 3, 6);
        SWAP(vals, 0, 3);
        SWAP(vals, 1, 4);
        SWAP(vals, 4, 7);
        SWAP(vals, 1, 4);
        SWAP(vals, 2, 5);
        SWAP(vals, 5, 8);
        SWAP(vals, 2, 5);
        SWAP(vals, 1, 3);
        SWAP(vals, 5, 7);
        SWAP(vals, 2, 6);
        SWAP(vals, 4, 6);
        SWAP(vals, 2, 4);
        SWAP(vals, 2, 3);
        SWAP(vals, 5, 6);
    }

    private static void sortCross(int[] vals) {
        SWAP(vals, 0, 1);
        SWAP(vals, 3, 4);
        SWAP(vals, 2, 4);
        SWAP(vals, 2, 3);
        SWAP(vals, 0, 3);
        SWAP(vals, 0, 2);
        SWAP(vals, 1, 4);
        SWAP(vals, 1, 3);
        SWAP(vals, 1, 2);
    }
    
    public static void process(int[] in, int[] out, int ch, int w, int h, int selector) {
        int[] vals = new int[9];

        for(int x=0;x<w;x++) {
            for(int y=0;y<h;y++) {

                vals[0] = Pixels.getValue(in, ch, x-1, y-1, w, h, -1);
                vals[1] = Pixels.getValue(in, ch, x, y-1, w, h, -1);
                vals[2] = Pixels.getValue(in, ch, x+1, y-1, w, h, -1);

                vals[3] = Pixels.getValue(in, ch, x-1, y, w, h, -1);
                vals[4] = Pixels.getValue(in, ch, x, y, w, h, -1);
                vals[5] = Pixels.getValue(in, ch, x+1, y, w, h, -1);

                vals[6] = Pixels.getValue(in, ch, x-1, y+1, w, h, -1);
                vals[7] = Pixels.getValue(in, ch, x, y+1, w, h, -1);
                vals[8] = Pixels.getValue(in, ch, x+1, y+1, w, h, -1);

                sort(vals);

                Pixels.setValue(out, ch, x, y, w, vals[selector]);
            }
        }
    }

    public static void processCross(int[] in, int[] out, int ch, int w, int h, int selector) {
        int[] vals = new int[5];
        
        for(int x=0;x<w;x++) {
            for(int y=0;y<h;y++) {


                vals[0] = Pixels.getValue(in, ch, x, y-1, w, h, -1);
                vals[1] = Pixels.getValue(in, ch, x-1, y, w, h, -1);
                vals[2] = Pixels.getValue(in, ch, x, y, w, h, -1);
                vals[3] = Pixels.getValue(in, ch, x+1, y, w, h, -1);
                vals[4] = Pixels.getValue(in, ch, x, y+1, w, h, -1);

                sortCross(vals);

                Pixels.setValue(out, ch, x, y, w, vals[selector]);
            }
        }
    }
    
}
