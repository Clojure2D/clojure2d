package clojure2d.java.noise;

import java.util.Random;

import static clojure2d.java.PrimitiveMath.*;

public final class NoiseConfig {
    public final static int INTERPOLATE_NONE = 0;
    public final static int INTERPOLATE_LINEAR = 1;
    public final static int INTERPOLATE_HERMITE = 2;
    public final static int INTERPOLATE_QUINTIC = 3;

    public final static int NOISE_VALUE = 0;
    public final static int NOISE_GRADIENT = 1;
    public final static int NOISE_SIMPLEX = 2;
    
    public final static double[] GRAD1dX = {2.0, -2.0};
    
    public final static double[] GRAD2dX = {1.0, -1.0, 1.0, -1.0};
    public final static double[] GRAD2dY = {1.0, -1.0, -1.0, 1.0};
    
    public final static double[] GRAD3dX = {1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 0.0, 0.0, 0.0, 0.0};
    public final static double[] GRAD3dY = {1.0, 1.0, -1.0, -1.0, 0.0, 0.0, 0.0, 0.0, 1.0, -1.0, 1.0, -1.0};
    public final static double[] GRAD3dZ = {0.0, 0.0, 0.0, 0.0, 1.0, 1.0, -1.0, -1.0, 1.0, 1.0, -1.0, -1.0};

    public final static double[] SIMPLEX1d = {1.0, -1.0, 2.0, -2.0, 3.0, -3.0, 4.0, -4.0,
                                              5.0, -5.0, 6.0, -6.0, 7.0, -7.0, 8.0, -8.0};

    private static final int X_NOISE_GEN = 1619;
    private static final int Y_NOISE_GEN = 31337;
    private static final int Z_NOISE_GEN = 6971;
    private static final int SEED_NOISE_GEN = 1013;
    private static final int SHIFT_NOISE_GEN = 13;
    
    public int noise_type;
    public int interpolate_type;
    public int octaves;
    public double lacunarity;
    public double gain;
    public int seed;
    public boolean normalize;

    public double fractalBounding;
    public double rev_octaves;

    public int[] perm, perm12;
    public double[] valueLUT;

    public NoiseConfig(int seed, int noise_type, int interpolate_type, int octaves, double lacunarity, double gain, boolean normalize) {
        this.seed = seed;
        this.noise_type = noise_type;
        this.interpolate_type = interpolate_type;
        this.octaves = Math.max(octaves,1);
        this.lacunarity = lacunarity;
        this.gain = gain;
        this.normalize = normalize;
        
        calcFractalBounding();
        rev_octaves = 1.0 / octaves;

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

    public final static double interpolate(NoiseConfig cfg, double v) {
        if(cfg.interpolate_type == INTERPOLATE_QUINTIC) return v * v * v * (v * (v * 6.0 - 15.0) + 10.0);
        if(cfg.interpolate_type == INTERPOLATE_HERMITE) return v * v * (3.0 - 2.0 * v);

        return v;
    }

    public final static int hash(NoiseConfig cfg, int offset, int x, int y) {
        int vectorIndex = ((X_NOISE_GEN * x) ^ (Y_NOISE_GEN * y) ^ cfg.seed ^ (SEED_NOISE_GEN * offset));
        vectorIndex = vectorIndex * vectorIndex * vectorIndex * 60493;
        return vectorIndex ^ (vectorIndex >> SHIFT_NOISE_GEN);
    }

    public final static int hash(NoiseConfig cfg, int offset, int x, int y, int z) {
        int vectorIndex = ((X_NOISE_GEN * x) ^ (Y_NOISE_GEN * y) ^ (Z_NOISE_GEN * z) ^ cfg.seed ^ (SEED_NOISE_GEN * offset));
        vectorIndex = vectorIndex * vectorIndex * vectorIndex * 60493;
        return vectorIndex ^ (vectorIndex >> SHIFT_NOISE_GEN);
    }

}
