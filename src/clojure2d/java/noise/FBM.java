package clojure2d.java.noise;

public final class FBM {
    public final static double noise(NoiseConfig cfg, double x) {
        double sum = Noise.value(cfg, cfg.perm[0], x);
        double amp = 1.0;

        double xx = x;

        int i=0;
        while(++i < cfg.octaves) {
            xx *= cfg.lacunarity;
            
            amp *= cfg.gain;
            sum += Noise.value(cfg, cfg.perm[i], xx) * amp;
        }

        return cfg.normalize ? ((sum * cfg.fractalBounding) + 1.0) * 0.5 : sum * cfg.fractalBounding;
    }

    public final static double noise(NoiseConfig cfg, double x, double y) {
        double sum = Noise.value(cfg, cfg.perm[0], x, y);
        double amp = 1.0;

        double xx = x;
        double yy = y;

        int i=0;
        while(++i < cfg.octaves) {
            xx *= cfg.lacunarity;
            yy *= cfg.lacunarity;
            
            amp *= cfg.gain;
            sum += Noise.value(cfg, cfg.perm[i], xx, yy) * amp;
        }

        return cfg.normalize ? ((sum * cfg.fractalBounding) + 1.0) * 0.5 : sum * cfg.fractalBounding;
    }

    public final static double noise(NoiseConfig cfg, double x, double y, double z) {
        double sum = Noise.value(cfg, cfg.perm[0], x, y, z);
        double amp = 1.0;

        double xx = x;
        double yy = y;
        double zz = z;

        int i=0;
        while(++i < cfg.octaves) {
            xx *= cfg.lacunarity;
            yy *= cfg.lacunarity;
            zz *= cfg.lacunarity;
            
            amp *= cfg.gain;
            sum += Noise.value(cfg, cfg.perm[i], xx, yy, zz) * amp;
        }

        return cfg.normalize ? ((sum * cfg.fractalBounding) + 1.0) * 0.5 : sum * cfg.fractalBounding;
    }

}
