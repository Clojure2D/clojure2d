package clojure2d.java.noise;

public final class Noise {
    public static double value(NoiseConfig cfg, int offset, double x) {
        switch (cfg.noise_type) {
            
        case NoiseConfig.NOISE_VALUE: return ValueNoise.value(cfg, offset, x);
        case NoiseConfig.NOISE_SIMPLEX: return SimplexNoise.value(cfg, offset, x);
        default: return GradientNoise.value(cfg, offset, x);
        }
    }

    public static double noise(NoiseConfig cfg, double x) {
        return cfg.normalize ? (value(cfg, 0, x) + 1.0) * 0.5 : value(cfg, 0, x);
    }
    
    public static double value(NoiseConfig cfg, int offset, double x, double y) {
        switch (cfg.noise_type) {
            
        case NoiseConfig.NOISE_VALUE: return ValueNoise.value(cfg, offset, x, y);
        case NoiseConfig.NOISE_SIMPLEX: return SimplexNoise.value(cfg, offset, x, y);
        default: return GradientNoise.value(cfg, offset, x, y);
        }
    }

    public static double noise(NoiseConfig cfg, double x, double y) {
        return cfg.normalize ? (value(cfg, 0, x, y) + 1.0) * 0.5 : value(cfg, 0, x, y);
    }

    public static double value(NoiseConfig cfg, int offset, double x, double y, double z) {
        switch (cfg.noise_type) {
            
        case NoiseConfig.NOISE_VALUE: return ValueNoise.value(cfg, offset, x, y, z);
        case NoiseConfig.NOISE_SIMPLEX: return SimplexNoise.value(cfg, offset, x, y, z);
        default: return GradientNoise.value(cfg, offset, x, y, z);
        }
    }

    public static double noise(NoiseConfig cfg, double x, double y, double z) {
        return cfg.normalize ? (value(cfg, 0, x, y, z) + 1.0) * 0.5 : value(cfg, 0, x, y, z);
    }
}
