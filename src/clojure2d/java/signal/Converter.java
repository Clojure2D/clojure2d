package clojure2d.java.signal;

import net.jafama.FastMath;
import fastmath.java.PrimitiveMath;

import fastmath.vector.*;

public final class Converter {
    private static final double alaw_A = 87.6;
    private static final double alaw_rA = 1.0 / alaw_A;
    private static final double alaw_lA = 1.0 + FastMath.log(alaw_A);
    private static final double alaw_rlA = 1.0 / alaw_lA;

    private static final double ulaw_U = 255.0;
    private static final double ulaw_rU = 1.0 / 255.0;
    private static final double ulaw_U1 = ulaw_U + 1.0;
    private static final double ulaw_rlnU1 = 1.0 / FastMath.log(ulaw_U1);

    public static final int CODING_ALAW=1;
    public static final int CODING_ALAW_REV=2;
    public static final int CODING_ULAW=3;
    public static final int CODING_ULAW_REV=4;
    
    public static double alaw(double x) {
        double absx = FastMath.abs(x);
        double f = x < 0.0 ? -alaw_rlA : alaw_rlA;
        return f * (absx < alaw_rA ? (alaw_A * absx) : (alaw_lA + FastMath.log(absx)));
    }
    
    public static double alaw_rev(double y) {
        double absy = FastMath.abs(y);
        double f = y < 0.0 ? -alaw_rA : alaw_rA;
        double v = absy * alaw_lA;
        return f * (absy < alaw_rlA ? v : FastMath.exp(v-1));
    }

    public static double ulaw(double x) {
        double sgn_rlnU1 = x < 0.0 ? -ulaw_rlnU1 : ulaw_rlnU1;
        return sgn_rlnU1 * FastMath.log(1.0 + ulaw_U * FastMath.abs(x));
    }

    public static double ulaw_rev(double y) {
        double sgn_rU = y < 0.0 ? -ulaw_rU : ulaw_rU;
        return sgn_rU * (FastMath.pow(ulaw_U1, FastMath.abs(y)) - 1.0);
    }

    public static double encode(int coding, double v) {
        switch(coding) {
        case CODING_ALAW: return alaw(v);
        case CODING_ALAW_REV: return alaw_rev(v);
        case CODING_ULAW: return ulaw(v);
        case CODING_ULAW_REV: return ulaw_rev(v);
        default: return v;
        }
    }
    
    public static int applySign(int in, int bits) {
        int sh = 32 - bits;
        return (in << sh) >> sh;
    }

    public static int pack(int x, boolean isSigned) {
        return isSigned ? applySign(x,8) : (x & 0xff);
    }

    public static int pack(int x, int y, boolean isLittleEndian, boolean isSigned) {
        int a,b;
        if(isLittleEndian) {
            a=y;
            b=x;
        } else {
            a=x;
            b=y;
        }

        int val = ((a & 0xff) << 8) | (b & 0xff);
        return isSigned ? applySign(val,16) : val;
    }

    public static int pack(int x, int y, int z, boolean isLittleEndian, boolean isSigned) {
        int a,c;
        if(isLittleEndian) {
            a=z;
            c=x;
        } else {
            a=x;
            c=z;
        }

        int val = ((a & 0xff) << 16) | ((y & 0xff) << 8) |  (c & 0xff);
        return isSigned ? applySign(val,24) : val;
    }

    public static final int s8min = applySign(0x80,8);
    public static final int s8max = applySign(0x7f,8);
    public static final int s16min = applySign(0x8000,16);
    public static final int s16max = applySign(0x7fff,16);
    public static final int s24min = applySign(0x800000,24);
    public static final int s24max = applySign(0x7fffff,24);

    public static double toDouble(int x, boolean isSigned) {
        int v = pack(x, isSigned);
        return isSigned ? PrimitiveMath.norm(v,s8min,s8max,-1.0,1.0) : PrimitiveMath.norm(v,0.0,0xff,-1.0,1.0);
    }

    public static double toDouble(int x, int y, boolean isLittleEndian, boolean isSigned) {
        int v = pack(x, y, isLittleEndian, isSigned);
        return isSigned ? PrimitiveMath.norm(v,s16min,s16max,-1.0,1.0) : PrimitiveMath.norm(v,0.0,0xffff,-1.0,1.0);
    }

    public static double toDouble(int x, int y, int z, boolean isLittleEndian, boolean isSigned) {
        int v = pack(x, y, z, isLittleEndian, isSigned);
        return isSigned ? PrimitiveMath.norm(v,s24min,s24max,-1.0,1.0) : PrimitiveMath.norm(v,0.0,0xffffff,-1.0,1.0);
    }

    public static final int BITS_8 = 0;
    public static final int BITS_16 = 1;
    public static final int BITS_24 = 2;

    
    
    public static int fromDouble(double x, int bits, boolean isSigned) {
        double xx = x < -1.0 ? -1.0 : (x > 1.0 ? 1.0 : x);
        switch(bits) {
        case BITS_8: return (int)FastMath.round(isSigned ? PrimitiveMath.norm(xx,-1.0,1.0,s8min,s8max) : PrimitiveMath.norm(xx,-1.0,1.0,0,0xff));
        case BITS_16: return (int)FastMath.round(isSigned ? PrimitiveMath.norm(xx,-1.0,1.0,s16min,s16max) : PrimitiveMath.norm(xx,-1.0,1.0,0,0xffff));
        case BITS_24: return (int)FastMath.round(isSigned ? PrimitiveMath.norm(xx,-1.0,1.0,s24min,s24max) : PrimitiveMath.norm(xx,-1.0,1.0,0,0xffffff));
        }
        return 0;
    }

    private static int[] layoutPlanar(int[] in, int[] channels) {
        int channelLen = in.length >> 2;

        int[] out = new int[channelLen*channels.length];

        int outIter = 0;
        for(int channel : channels) {
            for(int i=0; i<channelLen;i++) {
                out[outIter++] = in[(i<<2)+channel];
            }
        }

        return out;
    }

    private static int[] layoutInterleaved(int[] in, int[] channels) {
        int channelLen = in.length >> 2;

        int[] out = new int[channelLen*channels.length];

        int outIter = 0;
        for(int i=0; i<channelLen;i++) {
            int off = i<<2;
            for(int channel : channels) {
                out[outIter++] = in[off+channel];
            }
        }

        return out;
    }

    
    public static double[] toSignal(int[] in, int[] channels, int bits, boolean isLittleEndian, boolean isSigned, boolean isPlanar, int coding) {
        int[] data = isPlanar ? layoutPlanar(in, channels) : layoutInterleaved(in, channels);

        double[] out = new double[data.length/(bits+1)];

        int lenLimit = data.length-bits;
        
        int outIter = 0;
        int inIter = 0;
        while( (outIter < out.length) && (inIter < lenLimit) ) {
            switch(bits) {
            case BITS_8: {
                int x = data[inIter++];
                out[outIter] = encode(coding, toDouble(x, isSigned));
                break;
            }
            case BITS_16: {
                int x = data[inIter++];
                int y = data[inIter++];
                out[outIter] = encode(coding, toDouble(x, y, isLittleEndian, isSigned));
                break;
            }
            case BITS_24: {
                int x = data[inIter++];
                int y = data[inIter++];
                int z = data[inIter++];
                out[outIter] = encode(coding, toDouble(x, y, z, isLittleEndian, isSigned));
                break;
            }
            }
            outIter++;
        }
            
        return out;
    }

    //

    public static int[] fromSignal(double[] in, int[] target, int[] channels, int bits, boolean isLittleEndian, boolean isSigned, boolean isPlanar, int coding) {
        int channelLen = target.length >> 2;
        int[] out = new int[channels.length * channelLen];
        
        int outIter = 0;

        for(double v : in) {
            int d = fromDouble(encode(coding, v), bits, isSigned);

            switch(bits) {
            case BITS_8: {
                out[outIter++] = d & 0xff;
                break;
            }
            case BITS_16: {
                int x = d & 0xff;
                int y = (d >> 8) & 0xff;
                out[outIter++] = isLittleEndian ? x : y;
                out[outIter++] = isLittleEndian ? y : x;
                break;
            }
            case BITS_24: {
                int x = d & 0xff;
                int y = (d >> 8) & 0xff;
                int z = (d >> 16) & 0xff;
                out[outIter++] = isLittleEndian ? x : z;
                out[outIter++] = y;
                out[outIter++] = isLittleEndian ? z : x;
                break;
            }

            }
        }

        int limit = outIter;
        outIter = 0;
        if(isPlanar) {
            for(int ch : channels) {
                for(int i=0;i<channelLen;i++) {
                    if(outIter<limit) target[(i<<2)+ch] = out[outIter++];
                }
            }    
        } else {
            for(int i=0;i<channelLen;i++) {
                int off = i<<2;
                for(int ch : channels) {
                    if(outIter<limit) target[off+ch] = out[outIter++];
                }
            }
        }

        return target;
    }
    
}
