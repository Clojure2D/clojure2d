package clojure2d.java;

import java.awt.image.BufferedImage;

import fastmath.vector.Vec4;

public final class Pixels {

    public static BufferedImage imageFromPixels(int[] p, int w, int h) {
        BufferedImage img = new BufferedImage(w,h,BufferedImage.TYPE_INT_ARGB);
        setImagePixels(img,p);
        return img;
    }
    
    public static int[] getImagePixels(BufferedImage img, int x, int y, int w, int h) {
        int[] buff = new int[4 * w * h];
        return img.getRaster().getPixels(x,y,w,h,buff);
    }

    public static int[] getImagePixels(BufferedImage img) { return getImagePixels(img, 0, 0, img.getWidth(), img.getHeight()); }

    public static void setImagePixels(BufferedImage img, int x, int y, int w, int h, int[] p) {
        img.getRaster().setPixels(x,y,w,h,p);
    }

    public static void setImagePixels(BufferedImage img, int[] p) { setImagePixels(img, 0, 0, img.getWidth(), img.getHeight(), p); }

    public static int[] getChannel(int[] p, int ch) {
        int[] buff = new int[p.length >> 2];

        for(int i=ch, iter=0;i<p.length;i+=4,iter++) {
            buff[iter] = p[i];
        }

        return buff;
    }

    public static void setChannel(int[] p, int ch, int[] buff) {
        for(int i=ch, iter=0;i<p.length;i+=4,iter++) {
            p[i] = buff[iter];
        }
    }

    public static int getValue(int[] p, int ch, int x, int y, int w, int h, int edge) {
        if( (x<0) || (x>=w) || (y<0) || (y>=h)) {
            switch(edge) {
            case -1: return p[((constrain(y,0,h-1) * w + constrain(x,0,w-1)) << 2) + ch];
            case -2: return p[((mod(y,h) * w + mod(x,w)) << 2) + ch];
            default: return edge;
            }
        } else {
            return p[((y * w + x) << 2) + ch];
        }
    }

    public static int unsafeGetValue(int[] p, int ch, int x, int y, int w) {
        return p[((y * w + x) << 2) + ch];
    }

    public static int getValue(int[] p, int ch, int idx) {
        return p[(idx << 2) + ch];
    }
    
    public static void setValue(int[] p, int ch, int x, int y, int w, int v) {
        p[((y * w + x) << 2) + ch]=v;
    }

    public static void setValue(int[] p, int ch, int idx, int v) {
        p[(idx << 2) + ch]=v;
    }

    public static Vec4 getColor(int[] p, int x, int y, int w, int h, int edge) {
        int idx=0;

        if( (x<0) || (x>=w) || (y<0) || (y>=h)) {
            switch(edge) {
            case -1: idx=(constrain(y,0,h-1) * w + constrain(x,0,w-1)) << 2; break;
            case -2: idx=(mod(y,h) * w + mod(x,w)) << 2; break;
            default: return new Vec4(edge,edge,edge,255);
            }
        } else {
            idx=(y * w + x) << 2;
        }

        return new Vec4(p[idx],p[idx+1],p[idx+2],p[idx+3]);
    }

    public static Vec4 getColor(int[] p, int idx) {
        int i = idx << 2;
        return new Vec4(p[i],p[i+1],p[i+2],p[i+3]);
    }
    
    public static Vec4 unsafeGetColor(int[] p, int x, int y, int w) {
        int idx=(y * w + x) << 2;
        return new Vec4(p[idx],p[idx+1],p[idx+2],p[idx+3]);
    }

    public static void setColor(int [] p, int x, int y, int w, Vec4 c) {
        int idx = (y * w + x) << 2;

        p[idx] = constrain((int)(c.x+0.5),0,255);
        p[idx+1] = constrain((int)(c.y+0.5),0,255);
        p[idx+2] = constrain((int)(c.z+0.5),0,255);
        p[idx+3] = constrain((int)(c.w+0.5),0,255);
    }

    public static void setColor(int [] p, int idx, Vec4 c) {
        int i = idx << 2;

        p[i] = constrain((int)(c.x+0.5),0,255);
        p[i+1] = constrain((int)(c.y+0.5),0,255);
        p[i+2] = constrain((int)(c.z+0.5),0,255);
        p[i+3] = constrain((int)(c.w+0.5),0,255);
    }
        
    // some basic utilities
    
    public static int constrain(int v, int mn, int mx) {
        if(v<mn) {
            return mn;
        } else if (v>mx) {
            return mx;
        } else {
            return v;
        }
    }

    public static double smoothStep(double a, double b, double x) {
        if (x < a)
            return 0;
        if (x >= b)
            return 1;
        x = (x - a) / (b - a);
        return x*x * (3 - 2*x);
    }
    
    public static int mod(int x, int y) {
        int result = x % y;
        return result < 0 ? result + y : result;
    }

}
