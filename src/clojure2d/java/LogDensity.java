package clojure2d.java;

import fastmath.vector.Vec4;
import net.jafama.FastMath;
import fastmath.java.PrimitiveMath;
import java.awt.Color;

import clojure2d.java.reconstruction.AFilter;

public final class LogDensity {    
    public int w,h,size;
    public AFilter f;

    private int wdec, hdec;
    
    public double[] r,g,b,a;

    public LogDensity(int w, int h, AFilter f) {
        this.f = f;
        this.w = w;
        this.h = h;

        wdec = w - 1;
        hdec = h - 1;
        
        size = w * h;

        r = new double[size];
        g = new double[size];
        b = new double[size];
        a = new double[size];
    }

    public LogDensity(int w, int h) {
        this(w, h, null);
    }
    
    public void addPixel(double px, double py, Vec4 c) {
        if(f==null) {
            set(px,py,c);
        } else {
            setFiltered(px,py,c);
        }
    }
    
    // pbrt way
    public void setFiltered(double fx, double fy, Vec4 c) {
        int p0x = Math.max((int)FastMath.ceil(fx-0.5-f.radius), 0);
        int p0y = Math.max((int)FastMath.ceil(fy-0.5-f.radius), 0);
        int p1x = Math.min((int)FastMath.floor(fx-0.5+f.radius)+1, wdec);
        int p1y = Math.min((int)FastMath.floor(fy-0.5+f.radius)+1, hdec);

        int diffx = p1x-p0x;
        int diffy = p1y-p0y;

        if (diffx>0 && diffy>0) {
            for (int y=p0y; y<p1y; y++) {
                int yy = Math.min((int)FastMath.floor(FastMath.abs((y-fy)*f.iradius16)), 15) << 4;
                for (int x=p0x; x<p1x; x++) {
                    int xx = Math.min((int)FastMath.floor(FastMath.abs((x-fx)*f.iradius16)), 15);
                    set(x, y, c, f.filterTable[yy+xx]);
                }
            }
        }
    }

    public void set(double x, double y, Vec4 c) {
        int xx = (int)(x+0.5);
        int yy = (int)(y+0.5);
        if(xx>=0 && xx<w && yy>=0 && yy<h) {
            int off = yy*w+xx;
            double w = (c.w >= 255.0) ? 1.0 : (c.w / 255.0);
            r[off] += w*c.x;
            g[off] += w*c.y;
            b[off] += w*c.z;
            a[off] += w;
        }
    }

    public void set(int x, int y, Vec4 c, double weight) {
        int off = y*w+x;
        double w = (c.w >= 255.0) ? weight : weight * (c.w / 255.0);
        r[off] += w*c.x;
        g[off] += w*c.y;
        b[off] += w*c.z;
        a[off] += w;
    }


    public class Config {
        public double agamma, cgamma, vibrancy, saturation;
        public double mx, lmx;
        public boolean do_saturation, do_bc, do_vibrancy, do_gamma;
        public int[] bc_lut;

        public Config(double gamma_alpha, double gamma_color, double vibrancy, double brightness, double contrast, double saturation) {
            agamma = 1.0 / gamma_alpha;
            cgamma = 1.0 / gamma_color;
            this.vibrancy = vibrancy<0.0 ? 0.0 : (vibrancy > 1.0 ? 1.0 : vibrancy);
            this.saturation = saturation;

            do_saturation = saturation != 1.0;
            do_bc = (brightness != 1.0) || (contrast != 1.0);
            do_vibrancy = (this.vibrancy > 0.0) && (gamma_color != 1.0);
            do_gamma = gamma_alpha != 1.0;

            calcBrightnessContrastLut(brightness, contrast);

            mx = Double.MIN_VALUE;
            
            for (int i=0; i<size; i++) {
                if (a[i]>mx) mx=a[i];
            }
            
            lmx = 1.0 / FastMath.log1p(mx);
        }
        
        private void calcBrightnessContrastLut(double brightness, double contrast) {
            bc_lut = new int[256];
            
            for (int i=0; i<256; i++) {
                bc_lut[i] = Pixels.constrain((int)(0.5 + 255.0 * (((i / 255.0) * brightness - 0.5) * contrast + 0.5)), 0, 255);
            }
        }        
    }

    public void toPixels(int[] res, int start, int end, Config conf, Vec4 bg) {
        
        for (int i=start; i<end; i++) {
            int rr,gg,bb;
            double hit = a[i];
            
            if (hit > 0.0) {
                double rhit = 1.0/hit;

                double alpha = conf.do_gamma ? FastMath.pow(FastMath.log(hit+1.0) * conf.lmx, conf.agamma) : FastMath.log(hit+1.0) * conf.lmx;

                double fr = Math.min(Math.max(r[i]*rhit,0.0),255.0);
                double fg = Math.min(Math.max(g[i]*rhit,0.0),255.0);
                double fb = Math.min(Math.max(b[i]*rhit,0.0),255.0);

                if(conf.do_vibrancy) {
                    fr = PrimitiveMath.lerp(fr, 255.0*Math.pow(fr/255.0, conf.cgamma), conf.vibrancy);
                    fg = PrimitiveMath.lerp(fg, 255.0*Math.pow(fg/255.0, conf.cgamma), conf.vibrancy);
                    fb = PrimitiveMath.lerp(fb, 255.0*Math.pow(fb/255.0, conf.cgamma), conf.vibrancy);
                }
                
                rr = (int)PrimitiveMath.lerp(bg.x, fr, alpha);
                gg = (int)PrimitiveMath.lerp(bg.y, fg, alpha);
                bb = (int)PrimitiveMath.lerp(bg.z, fb, alpha);

            } else {
                rr = (int)bg.x;
                gg = (int)bg.y;
                bb = (int)bg.z;
            }

            if(conf.do_bc) {
                rr = conf.bc_lut[rr];
                gg = conf.bc_lut[gg];
                bb = conf.bc_lut[bb];
            }

            if (conf.do_saturation) {
                float[] hsb = Color.RGBtoHSB(rr, gg, bb, null);
                hsb[1] = (float)Math.min(Math.max(hsb[1]*conf.saturation, 0.0), 1.0);
                int s = Color.HSBtoRGB(hsb[0], hsb[1], hsb[2]);
                Pixels.setColor(res, i, new Vec4((s>>16)&0xff, (s>>8)&0xff, s&0xff, 255.0));
            } else {      
                Pixels.setColor(res, i, new Vec4(rr,gg,bb,255.0));
            }
        }
    }

    private void mergeChannel(double[] a, double[] b) {
        for(int i=0; i<size; i++) { a[i] += b[i]; }
    }
    
    public void merge(LogDensity d, int ch) {
        switch(ch) {
        case 0: mergeChannel(r,d.r); break;
        case 1: mergeChannel(g,d.g); break;
        case 2: mergeChannel(b,d.b); break;
        case 3: mergeChannel(a,d.a); break;
        }
    }
    
}
