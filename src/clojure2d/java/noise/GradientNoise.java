package clojure2d.java.noise;

import static clojure2d.java.PrimitiveMath.*;

public final class GradientNoise {
    // 1D

    public final static double grad(NoiseConfig cfg, int offset, int x, double xd) {
        int idx = cfg.perm[(x & 0xff) + offset] & 0x1;
        return xd * NoiseConfig.GRAD1dX[idx];
    }
    
    public final static double value(NoiseConfig cfg, int offset, double x) {
        int x0 = x > 0.0 ? (int)x : (int)x - 1;

        double xd0 = x - x0;
        
        if(cfg.interpolate_type == NoiseConfig.INTERPOLATE_NONE) {
            return grad(cfg, offset, x0, xd0) * 0.5;
        } else {
            int x1 = x0 + 1;

            double xd1 = xd0 - 1.0;
            
            double xs = NoiseConfig.interpolate(cfg, xd0);
            
            return lerp(grad(cfg, offset, x0, xd0),
                        grad(cfg, offset, x1, xd1), xs);
        }
    }
    
    // 2D
    public final static double grad(NoiseConfig cfg, int offset, int x, int y, double xd, double yd) {
        int idx = NoiseConfig.hash(cfg, offset, x, y) & 0x3;
        return xd * NoiseConfig.GRAD2dX[idx] + yd * NoiseConfig.GRAD2dY[idx];
    }
    
    public final static double value(NoiseConfig cfg, int offset, double x, double y) {
        int x0 = x > 0.0 ? (int)x : (int)x - 1;
        int y0 = y > 0.0 ? (int)y : (int)y - 1;

        double xd0 = x - x0;
        double yd0 = y - y0;
        
        if(cfg.interpolate_type == NoiseConfig.INTERPOLATE_NONE) {
            return grad(cfg, offset, x0, y0, xd0, yd0) * 0.5;
        } else {
            int x1 = x0 + 1;
            int y1 = y0 + 1;

            double xd1 = xd0 - 1.0;
            double yd1 = yd0 - 1.0;

            double xs = NoiseConfig.interpolate(cfg, xd0);
            double ys = NoiseConfig.interpolate(cfg, yd0);
            
            return lerp(lerp(grad(cfg, offset, x0, y0, xd0, yd0), grad(cfg, offset, x1, y0, xd1, yd0), xs), 
                        lerp(grad(cfg, offset, x0, y1, xd0, yd1), grad(cfg, offset, x1, y1, xd1, yd1), xs), ys);
        }
    }

    // 3D
    public final static double grad(NoiseConfig cfg, int offset, int x, int y, int z, double xd, double yd, double zd) {
        int idx = cfg.perm12[(NoiseConfig.hash(cfg, offset, x, y, z) & 511)];
        return xd * NoiseConfig.GRAD3dX[idx] + yd * NoiseConfig.GRAD3dY[idx] + zd * NoiseConfig.GRAD3dZ[idx];
    }

    public final static double value(NoiseConfig cfg, int offset, double x, double y, double z) {
        int x0 = x > 0.0 ? (int)x : (int)x - 1;
        int y0 = y > 0.0 ? (int)y : (int)y - 1;
        int z0 = z > 0.0 ? (int)z : (int)z - 1;

        double xd0 = x - x0;
        double yd0 = y - y0;
        double zd0 = z - z0;
        
        if(cfg.interpolate_type == NoiseConfig.INTERPOLATE_NONE) {
            return grad(cfg, offset, x0, y0, z0, xd0, yd0, zd0) * 0.5;
        } else {
            int x1 = x0 + 1;
            int y1 = y0 + 1;
            int z1 = z0 + 1;

            double xd1 = xd0 - 1.0;
            double yd1 = yd0 - 1.0;
            double zd1 = zd0 - 1.0;

            double xs = NoiseConfig.interpolate(cfg, xd0);
            double ys = NoiseConfig.interpolate(cfg, yd0);
            double zs = NoiseConfig.interpolate(cfg, zd0);

            return lerp(lerp(lerp(grad(cfg, offset, x0, y0, z0, xd0, yd0, zd0), grad(cfg, offset, x1, y0, z0, xd1, yd0, zd0), xs), 
                             lerp(grad(cfg, offset, x0, y1, z0, xd0, yd1, zd0), grad(cfg, offset, x1, y1, z0, xd1, yd1, zd0), xs), ys),
                        lerp(lerp(grad(cfg, offset, x0, y0, z1, xd0, yd0, zd1), grad(cfg, offset, x1, y0, z1, xd1, yd0, zd1), xs), 
                             lerp(grad(cfg, offset, x0, y1, z1, xd0, yd1, zd1), grad(cfg, offset, x1, y1, z1, xd1, yd1, zd1), xs), ys), zs);
        }
    }

}
