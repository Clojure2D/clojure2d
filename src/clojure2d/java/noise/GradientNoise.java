package clojure2d.java.noise;

import static clojure2d.java.PrimitiveMath.*;

public final class GradientNoise {
    // 1D
    
    public static double value(NoiseConfig cfg, int offset, double x) {
        int x0 = x > 0.0 ? (int)x : (int)x - 1;
        
        if(cfg.interpolate_type == NoiseConfig.INTERPOLATE_NONE) {
            return cfg.valueLUT[cfg.perm[(x0 & 0xff) + offset]];
        } else {
            int x1 = x0 + 1;
            
            double xs;
            
            switch (cfg.interpolate_type) {
                
            case NoiseConfig.INTERPOLATE_HERMITE: xs = hermite(x - x0); break;
            case NoiseConfig.INTERPOLATE_QUINTIC: xs = quintic(x - x0); break;
            default: xs = x - x0;
            
            }

            return lerp(cfg.valueLUT[cfg.perm[(x0 & 0xff) + offset]],
                        cfg.valueLUT[cfg.perm[(x1 & 0xff) + offset]], xs);
        }
    }

    public static double fbm(NoiseConfig cfg, double x) {
        double sum = value(cfg, cfg.perm[0], x);
        double amp = 1.0;

        double xx = x;

        int i=0;
        while(++i < cfg.octaves) {
            xx *= cfg.lacunarity;
            
            amp *= cfg.gain;
            sum += value(cfg, cfg.perm[i], xx) * amp;
        }

        return cfg.normalize ? ((sum * cfg.fractalBounding) + 1.0) * 0.5 : sum * cfg.fractalBounding;
    }
    
    // 2D
    public static double grad(NoiseConfig cfg, int offset, int x, int y, double xd, double yd) {
        int idx = cfg.perm12[(x & 0xff) + cfg.perm[(y & 0xff) + offset]];
        return xd * NoiseConfig.GRADX[idx] + yd * NoiseConfig.GRADY[idx];
    }
    
    public static double value(NoiseConfig cfg, int offset, double x, double y) {
        int x0 = x > 0.0 ? (int)x : (int)x - 1;
        int y0 = y > 0.0 ? (int)y : (int)y - 1;

        double xd0 = x - x0;
        double yd0 = y - y0;
        
        if(cfg.interpolate_type == NoiseConfig.INTERPOLATE_NONE) {
            return grad(cfg, offset, x0, y0, xd0, yd0);
        } else {
            int x1 = x0 + 1;
            int y1 = y0 + 1;

            double xd1 = xd0 - 1.0;
            double yd1 = yd0 - 1.0;
            
            double xs, ys;
            switch (cfg.interpolate_type) {

            case NoiseConfig.INTERPOLATE_HERMITE:
                xs = hermite(xd0);
                ys = hermite(yd0);
                break;

            case NoiseConfig.INTERPOLATE_QUINTIC:
                xs = quintic(xd0);
                ys = quintic(yd0);
                break;

            default:
                xs = xd0;
                ys = yd0;
            }

            return lerp(lerp(grad(cfg, offset, x0, y0, xd0, yd0), grad(cfg, offset, x1, y0, xd1, yd0), xs), 
                        lerp(grad(cfg, offset, x0, y1, xd0, yd1), grad(cfg, offset, x1, y1, xd1, yd1), xs), ys);
        }
    }

    public static double fbm(NoiseConfig cfg, double x, double y) {
        double sum = value(cfg, cfg.perm[0], x, y);
        double amp = 1.0;

        double xx = x;
        double yy = y;

        int i=0;
        while(++i < cfg.octaves) {
            xx *= cfg.lacunarity;
            yy *= cfg.lacunarity;
            
            amp *= cfg.gain;
            sum += value(cfg, cfg.perm[i], xx, yy) * amp;
        }

        return cfg.normalize ? ((sum * cfg.fractalBounding) + 1.0) * 0.5 : sum * cfg.fractalBounding;
    }

    // 3D
    public static double grad(NoiseConfig cfg, int offset, int x, int y, int z, double xd, double yd, double zd) {
        int idx = cfg.perm12[(x & 0xff) + cfg.perm[(y & 0xff) + cfg.perm[(z & 0xff) + offset]]];
        return xd * NoiseConfig.GRADX[idx] + yd * NoiseConfig.GRADY[idx] + zd * NoiseConfig.GRADZ[idx];
    }

    public static double value(NoiseConfig cfg, int offset, double x, double y, double z) {
        int x0 = x > 0.0 ? (int)x : (int)x - 1;
        int y0 = y > 0.0 ? (int)y : (int)y - 1;
        int z0 = z > 0.0 ? (int)z : (int)z - 1;

        double xd0 = x - x0;
        double yd0 = y - y0;
        double zd0 = z - z0;
        
        if(cfg.interpolate_type == NoiseConfig.INTERPOLATE_NONE) {
            return grad(cfg, offset, x0, y0, z0, xd0, yd0, zd0);
        } else {
            int x1 = x0 + 1;
            int y1 = y0 + 1;
            int z1 = z0 + 1;

            double xd1 = xd0 - 1.0;
            double yd1 = yd0 - 1.0;
            double zd1 = zd0 - 1.0;
            
            double xs, ys, zs;

            switch (cfg.interpolate_type) {

            case NoiseConfig.INTERPOLATE_HERMITE:
                xs = hermite(xd0);
                ys = hermite(yd0);
                zs = hermite(zd0);
                break;

            case NoiseConfig.INTERPOLATE_QUINTIC:
                xs = quintic(xd0);
                ys = quintic(yd0);
                zs = quintic(zd0);
                break;

            default:
                xs = xd0;
                ys = yd0;
                zs = zd0;
            }

            return lerp(lerp(lerp(grad(cfg, offset, x0, y0, z0, xd0, yd0, zd0), grad(cfg, offset, x1, y0, z0, xd1, yd0, zd0), xs), 
                             lerp(grad(cfg, offset, x0, y1, z0, xd0, yd1, zd0), grad(cfg, offset, x1, y1, z0, xd1, yd1, zd0), xs), ys),
                        lerp(lerp(grad(cfg, offset, x0, y0, z1, xd0, yd0, zd1), grad(cfg, offset, x1, y0, z1, xd1, yd0, zd1), xs), 
                             lerp(grad(cfg, offset, x0, y1, z1, xd0, yd1, zd1), grad(cfg, offset, x1, y1, z1, xd1, yd1, zd1), xs), ys), zs);
        }
    }

    public static double fbm(NoiseConfig cfg, double x, double y, double z) {
        double sum = value(cfg, cfg.perm[0], x, y, z);
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
            sum += value(cfg, cfg.perm[i], xx, yy, zz) * amp;
        }

        return cfg.normalize ? ((sum * cfg.fractalBounding) + 1.0) * 0.5 : sum * cfg.fractalBounding;
    }
}
