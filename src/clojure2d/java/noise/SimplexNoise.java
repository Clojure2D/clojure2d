package clojure2d.java.noise;

import static clojure2d.java.PrimitiveMath.*;

public final class SimplexNoise {
    // 1D

    public static double grad(NoiseConfig cfg, int offset, int x, double xd) {
        int idx = cfg.perm[(x & 0xff) + offset] & 0x7;
        return xd * NoiseConfig.SIMPLEX1d[idx];
    }
    
    public static double value(NoiseConfig cfg, int offset, double x) {
        int i0 = x > 0.0 ? (int)x : (int)x - 1;
        int i1 = i0 + 1;

        double x0 = x - i0;
        double x1 = x0 - 1.0;

        double t0 = 1.0 - x0 * x0;
        t0 *= t0;
        double n0 = t0 * t0 * grad(cfg, offset, i0, x0);

        double t1 = 1.0 - x1 * x1;
        t1 *= t1;
        double n1 = t1 * t1 * grad(cfg, offset, i1, x1);

        return 0.79 * (n0 + n1);
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

    public static final double SQRT3 = Math.sqrt(3.0);
    public static final double F2 = 0.5 * (SQRT3 - 1.0);
    public static final double G2 = (3.0 - SQRT3) / 6.0;
    public static final double G2_2 = G2 + G2;
    
    public static double grad(NoiseConfig cfg, int offset, int x, int y, double xd, double yd) {
        int idx = cfg.perm12[(x & 0xff) + cfg.perm[(y & 0xff) + offset]];
        return xd * NoiseConfig.GRAD3dX[idx] + yd * NoiseConfig.GRAD3dY[idx];
    }
    
    public static double value(NoiseConfig cfg, int offset, double x, double y) {
        double t = (x + y) * F2;

        double xt = x + t;
        double yt = y + t;
        
        int i = xt > 0.0 ? (int)xt : (int)xt - 1;
        int j = yt > 0.0 ? (int)yt : (int)yt - 1;

        t = (i + j) * G2;

        double x0 = x - i + t;
        double y0 = y - j + t;

        int i1, j1;
	if(x0 > y0) {
            i1 = 1; j1 = 0;
        } else {
            i1 = 0; j1 = 1;
	}

	double x1 = x0 - i1 + G2;
	double y1 = y0 - j1 + G2;
	double x2 = x0 - 1.0 + G2_2;
	double y2 = y0 - 1.0 + G2_2;

	double n0, n1, n2;

	t = 0.5 - x0*x0 - y0*y0;
	if (t < 0) {
            n0 = 0;
        } else {
            t *= t;
            n0 = t * t * grad(cfg, offset, i, j, x0, y0);
	}

	t = 0.5 - x1*x1 - y1*y1;
	if (t < 0) {
            n1 = 0;
        } else {
            t *= t;
            n1 = t * t * grad(cfg, offset, i + i1, j + j1, x1, y1);
	}

	t = 0.5 - x2*x2 - y2*y2;
	if (t < 0) {
            n2 = 0;
        } else {
            t *= t;
            n2 = t * t * grad(cfg, offset, i + 1, j + 1, x2, y2);
	}

	return 70.0 * (n0 + n1 + n2);
  
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

    public static final double F3 = 1.0 / 3.0;
    public static final double G3 = 1.0 / 6.0;
    public static final double G3_2 = 1.0 / 3.0;
    public static final double G3_3 = 0.5;
    
    public static double grad(NoiseConfig cfg, int offset, int x, int y, int z, double xd, double yd, double zd) {
        int idx = cfg.perm12[(x & 0xff) + cfg.perm[(y & 0xff) + cfg.perm[(z & 0xff) + offset]]];
        return xd * NoiseConfig.GRAD3dX[idx] + yd * NoiseConfig.GRAD3dY[idx] + zd * NoiseConfig.GRAD3dZ[idx];
    }

    public static double value(NoiseConfig cfg, int offset, double x, double y, double z) {
        double t = (x + y + z) * F3;

        double xt = x + t;
        double yt = y + t;
        double zt = z + t;

        int i = xt > 0.0 ? (int)xt : (int)xt - 1;
        int j = yt > 0.0 ? (int)yt : (int)yt - 1;
        int k = zt > 0.0 ? (int)zt : (int)zt - 1;

        t = (i + j + k) * G3;
        
        double x0 = x - i + t;
        double y0 = y - j + t;
        double z0 = z - k + t;;

        int i1, j1, k1;
	int i2, j2, k2;

	if(x0 >= y0) {
            if(y0 >= z0) {
                i1 = 1; j1 = 0; k1 = 0; i2 = 1; j2 = 1; k2 = 0;
            } else if(x0 >= z0) {
                i1 = 1; j1 = 0; k1 = 0; i2 = 1; j2 = 0; k2 = 1;
            } else {
                i1 = 0; j1 = 0; k1 = 1; i2 = 1; j2 = 0; k2 = 1;
            }
	} else {
            if(y0 < z0) {
                i1 = 0; j1 = 0; k1 = 1; i2 = 0; j2 = 1; k2 = 1;
            } else if(x0 < z0) {
                i1 = 0; j1 = 1; k1 = 0; i2 = 0; j2 = 1; k2 = 1;
            } else {
                i1 = 0; j1 = 1; k1 = 0; i2 = 1; j2 = 1; k2 = 0;
            }
	}

	double x1 = x0 - i1 + G3;
	double y1 = y0 - j1 + G3;
	double z1 = z0 - k1 + G3;
	double x2 = x0 - i2 + G3_2;
	double y2 = y0 - j2 + G3_2;
	double z2 = z0 - k2 + G3_2;
	double x3 = x0 - 1.0 + G3_3;
	double y3 = y0 - 1.0 + G3_3;
	double z3 = z0 - 1.0 + G3_3;

	double n0, n1, n2, n3;

	t = 0.6 - x0*x0 - y0*y0 - z0*z0;
	if(t < 0) {
            n0 = 0;
	} else {
            t *= t;
            n0 = t * t * grad(cfg, offset, i, j, k, x0, y0, z0);
	}

	t = 0.6 - x1*x1 - y1*y1 - z1*z1;
	if(t < 0) {
            n1 = 0;
	} else {
            t *= t;
            n1 = t * t * grad(cfg, offset, i + i1, j + j1, k + k1, x1, y1, z1);
	}

	t = 0.6 - x2*x2 - y2*y2 - z2*z2;
	if(t < 0) {
            n2 = 0;
	} else {
            t *= t;
            n2 = t * t * grad(cfg, offset, i + i2, j + j2, k + k2, x2, y2, z2);
	}

	t = 0.6 - x3*x3 - y3*y3 - z3*z3;
	if(t < 0) {
            n3 = 0;
	} else {
            t *= t;
            n3 = t * t * grad(cfg, offset, i + 1, j + 1, k + 1, x3, y3, z3);
	}

	return 32.0 * (n0 + n1 + n2 + n3);
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
