package clojure2d.java.noise;

import java.util.Random;

import static clojure2d.java.PrimitiveMath.*;

public final class NoiseConfig {
    public final static int INTERPOLATE_NONE = 0;
    public final static int INTERPOLATE_LINEAR = 1;
    public final static int INTERPOLATE_HERMITE = 2;
    public final static int INTERPOLATE_QUINTIC = 3;

    public final static double[] GRADX = {1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 0.0, 0.0, 0.0, 0.0};
    public final static double[] GRADY = {1.0, 1.0, -1.0, -1.0, 0.0, 0.0, 0.0, 0.0, 1.0, -1.0, 1.0, -1.0};
    public final static double[] GRADZ = {0.0, 0.0, 0.0, 0.0, 1.0, 1.0, -1.0, -1.0, 1.0, 1.0, -1.0, -1.0};

    public int interpolate_type;
    public int octaves;
    public double lacunarity;
    public double gain;
    public int seed;
    public boolean normalize;

    public double fractalBounding;

    public int[] perm, perm12;
    public double[] valueLUT;
    
    public NoiseConfig(int seed, int interpolate_type, int octaves, double lacunarity, double gain, boolean normalize) {
        this.seed = seed;
        this.interpolate_type = interpolate_type;
        this.octaves = octaves;
        this.lacunarity = lacunarity;
        this.gain = gain;
        this.normalize = normalize;
        
        calcFractalBounding();

        perm = new int[512];
        perm12 = new int[512];

        Random rng = new Random(seed);

        for (int i=0; i<256; i++) {
            perm[i] = i;
        }
        
        for (int i = 0; i < 256; i++) {
            int r = rng.nextInt(Integer.MAX_VALUE) % (256 - i);
            int k = r + i;
            int l = perm[i];
            perm[i] = perm[i + 256] = perm[k];
            perm[k] = l;
            perm12[i] = perm12[i + 256] = perm[i] % 12;
        }

        valueLUT = new double[256];

        double mn = Double.MAX_VALUE;
        double mx = Double.MIN_VALUE;
    
        for (int i=0; i<256; i++) {
            valueLUT[i] = rng.nextDouble()*2.0-1.0;
            mn = min(mn,valueLUT[i]);
            mx = max(mx,valueLUT[i]);
        }
        
        for(int i=0; i<256;i++) {
            valueLUT[i] = norm(valueLUT[i],mn,mx,-1.0,1.0);
        }
    }

    private void calcFractalBounding() {
        double amp = 1.0;
        double ampf = 1.0;
        for (int i=1; i<octaves; i++) {
            amp *= gain;
            ampf += amp;
        }
        fractalBounding = 1.0/ampf;
    }
}

