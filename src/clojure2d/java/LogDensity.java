package clojure2d.java;

import fastmath.vector.Vec4;
import net.jafama.FastMath;

import clojure2d.java.reconstruction.AFilter;

public class LogDensity {    
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
        int p1x = Math.min((int)FastMath.floor(fx-0.5+f.radius)+1,  wdec);
        int p1y = Math.min((int)FastMath.floor(fy-0.5+f.radius)+1, hdec);

        int diffx = p1x-p0x;
        int diffy = p1y-p0y;

        if (diffx>0 && diffy>0) {

            int[] ifx = new int[diffx];
            int[] ify = new int[diffy];

            for (int x=p0x; x<p1x; x++) {
                double v = FastMath.abs( (x-fx)*f.iradius*16.0);
                ifx[x-p0x] = Math.min((int)FastMath.floor(v), 15);
            }

            for (int y=p0y; y<p1y; y++) {
                double v = FastMath.abs( (y-fy)*f.iradius*16.0);
                ify[y-p0y] = Math.min((int)FastMath.floor(v), 15);
            }


            for (int x=p0x; x<p1x; x++) {
                for (int y=p0y; y<p1y; y++) {
                    int off = (ify[y-p0y]<<4)+ifx[x-p0x];
                    set(x, y, c, f.filterTable[off]);
                }
            }
        }
    }

    public void set(double x, double y, Vec4 c) {
        int xx = (int)x;
        int yy = (int)y;
        if(xx>=0 && xx<w && yy>=0 && yy<h) {
            int off = yy*w+xx;
            r[off] += c.x;
            g[off] += c.y;
            b[off] += c.z;
            a[off]++;
        }
    }

    public void set(int x, int y, Vec4 c, double weight) {
        int off = y*w+x;
        r[off] += weight*c.x;
        g[off] += weight*c.y;
        b[off] += weight*c.z;
        a[off] += weight;
    }

    public int[] toPixels(Vec4 bg, double gamma_alpha, double gamma_color, double intensity) {
        double agamma = 1.0 / gamma_alpha;
        double cgamma = 1.0 / gamma_color;
        double rintensity = 1.0 - intensity;

        double mx = Double.MIN_VALUE;

        for (int i=0; i<size; i++) {
            double n=a[i];
            mx = n > mx ? n : mx;
        }

        double mxlog = 1.0 / FastMath.log(mx+1);

        int[] res = new int[size*4];

        for (int i=0; i<size; i++) {
            double hit = a[i];

            if (hit > 0.0) {
                double rhit = 1.0/hit;
                double loghit = FastMath.log(hit+1) * mxlog;

                double alpha = FastMath.pow(loghit, agamma);
                double ralpha = 1.0-alpha;

                double abr = ralpha * bg.x;
                double abg = ralpha * bg.y;
                double abb = ralpha * bg.z;

                double rr = rhit * r[i];
                double gg = rhit * g[i];
                double bb = rhit * b[i];
        
                if (intensity == 0.0) {
                    rr = 255.0 * FastMath.pow(Math.max(0,rr)/255.0, cgamma);
                    gg = 255.0 * FastMath.pow(Math.max(0,gg)/255.0, cgamma);
                    bb = 255.0 * FastMath.pow(Math.max(0,bb)/255.0, cgamma);
                } else 
                    if (intensity < 1.0) {
                        rr = rr*intensity + (255.0 * FastMath.pow(Math.max(0,rr)/255.0, cgamma) * rintensity);
                        gg = gg*intensity + (255.0 * FastMath.pow(Math.max(0,gg)/255.0, cgamma) * rintensity);
                        bb = bb*intensity + (255.0 * FastMath.pow(Math.max(0,bb)/255.0, cgamma) * rintensity);
                    }

                if (alpha < 1.0) {
                    rr = rr * alpha + abr;
                    gg = gg * alpha + abg;
                    bb = bb * alpha + abb;
                }

                Pixels.setColor(res,i,new Vec4(rr,gg,bb,255.0));
                
            } else {
                Pixels.setColor(res,i,bg);
            }
        }

        return res;
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
