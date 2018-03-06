package clojure2d.java.noise;

import static clojure2d.java.PrimitiveMath.*;

public final class ValueNoise {
    // 1D
    
    public static double value(NoiseConfig cfg, int offset, double x) {
        int x0 = x > 0.0 ? (int)x : (int)x - 1;
        
        if(cfg.interpolate_type == NoiseConfig.INTERPOLATE_NONE) {
            return cfg.valueLUT[cfg.perm[(x0 & 0xff) + offset]];
        } else {
            int x1 = x0 + 1;
            
            double xs = NoiseConfig.interpolate(cfg, x - x0);
            
            return lerp(cfg.valueLUT[cfg.perm[(x0 & 0xff) + offset]],
                        cfg.valueLUT[cfg.perm[(x1 & 0xff) + offset]], xs);
        }
    }
    
    // 2D
    public static double lut(NoiseConfig cfg, int offset, int x, int y) {
        return cfg.valueLUT[NoiseConfig.hash(cfg, offset, x, y) & 0xff];
    }
    
    public static double value(NoiseConfig cfg, int offset, double x, double y) {
        int x0 = x > 0.0 ? (int)x : (int)x - 1;
        int y0 = y > 0.0 ? (int)y : (int)y - 1;

        if(cfg.interpolate_type == NoiseConfig.INTERPOLATE_NONE) {
            return lut(cfg,offset,x0,y0);
        } else {
            int x1 = x0 + 1;
            int y1 = y0 + 1;

            double xs = NoiseConfig.interpolate(cfg, x - x0);
            double ys = NoiseConfig.interpolate(cfg, y - y0);

            return lerp(lerp(lut(cfg, offset, x0, y0), lut(cfg, offset, x1, y0), xs), 
                        lerp(lut(cfg, offset, x0, y1), lut(cfg, offset, x1, y1), xs), ys);
        }
    }

    public static double lut(NoiseConfig cfg, int offset, int x, int y, int z) {
        return cfg.valueLUT[NoiseConfig.hash(cfg, offset, x, y, z) & 0xff];
    }
    
    public static double value(NoiseConfig cfg, int offset, double x, double y, double z) {
        int x0 = x > 0.0 ? (int)x : (int)x - 1;
        int y0 = y > 0.0 ? (int)y : (int)y - 1;
        int z0 = z > 0.0 ? (int)z : (int)z - 1;

        if(cfg.interpolate_type == NoiseConfig.INTERPOLATE_NONE) {
            return lut(cfg,offset,x0,y0,z0);
        } else {
            int x1 = x0 + 1;
            int y1 = y0 + 1;
            int z1 = z0 + 1;

            double xs = NoiseConfig.interpolate(cfg, x - x0);
            double ys = NoiseConfig.interpolate(cfg, y - y0);
            double zs = NoiseConfig.interpolate(cfg, z - z0);
            
            return lerp(lerp(lerp(lut(cfg, offset, x0, y0, z0), lut(cfg, offset, x1, y0, z0), xs), 
                             lerp(lut(cfg, offset, x0, y1, z0), lut(cfg, offset, x1, y1, z0), xs), ys),
                        lerp(lerp(lut(cfg, offset, x0, y0, z1), lut(cfg, offset, x1, y0, z1), xs), 
                             lerp(lut(cfg, offset, x0, y1, z1), lut(cfg, offset, x1, y1, z1), xs), ys), zs);
        }
    }
}
