package clojure2d.java;

import fastmath.vector.Vec4;
import net.jafama.FastMath;
import fastmath.java.PrimitiveMath;
import java.awt.Color;
import clojure.lang.IFn;

import clojure2d.java.reconstruction.AFilter;

public class GradientDensity {    
    public int w,h,size;
    public AFilter f;

    private int wdec, hdec;
    
    public double[] cnt;

    public GradientDensity(int w, int h, AFilter f) {
        this.f = f;
        this.w = w;
        this.h = h;

        wdec = w - 1;
        hdec = h - 1;
        
        size = w * h;

        cnt = new double[size];
    }

    public GradientDensity(int w, int h) {
        this(w, h, null);
    }
    
    public void addPixel(double px, double py) {
        if(f==null) {
            set(px,py);
        } else {
            setFiltered(px,py);
        }
    }
    
    // pbrt way
    public void setFiltered(double fx, double fy) {
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
                    set(x, y, f.filterTable[yy+xx]);
                }
            }
        }
    }

    public void set(double x, double y) {
        int xx = (int)(x+0.5);
        int yy = (int)(y+0.5);
        if(xx>=0 && xx<w && yy>=0 && yy<h) {
            cnt[yy*w+xx] ++;
        }
    }

    public void set(int x, int y, double weight) {
        cnt[y*w+x] += weight;
    }


    public class Config {
        public double mx, lmx;
        public boolean is_log;
        public IFn gradient;

        public Config(boolean is_log_, IFn gradient_) {
            is_log = is_log_;
            gradient = gradient_;
            
            mx = Double.MIN_VALUE;
            
            for (int i=0; i<size; i++) {
                if (cnt[i]>mx) mx=cnt[i];
            }

            lmx = 1.0 / FastMath.log1p(mx);
            mx = 1.0 / mx;
        }       
    }

    public void toPixels(int[] res, int start, int end, Config conf) {
        for (int i=start; i<end; i++) {
            double hit = cnt[i];
            double pos = conf.is_log ? FastMath.log(hit+1.0) * conf.lmx : hit * conf.mx;  
            Pixels.setColor(res, i, (Vec4)conf.gradient.invoke(pos));
        }
    }

    public void toPixels(int[] res, Config conf) {
        toPixels(res, 0, cnt.length, conf);
    }
        
    public void merge(GradientDensity d) {
        for(int i=0; i<size; i++) { cnt[i] += d.cnt[i]; }
    }
    
}
