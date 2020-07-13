package clojure2d.java.glitch;

import clojure2d.java.Pixels;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Arrays;
import clojure.lang.IFn;

import fastmath.vector.Vec4;

public final class Segmentation {
    private int[] img;
    private int w,h,no_vertices;

    private ArrayList<Edge> edges = new ArrayList<Edge>();
    private SegmentNode[] elements;
    
    public Segmentation(int[] img, int w, int h) {
        this.img = img;
        this.w = w;
        this.h = h;
        no_vertices = w * h;
    }
        
    class Edge implements Comparable<Edge> {
        int a,b;
        double weight;
        
        public int compareTo(Edge o) {
            return this.weight<o.weight?-1:this.weight>o.weight?1:0;
        }
    }

    class SegmentNode {
        int rank,parent,size;
    }
 
    public void makeEdges(IFn distance) {
        edges.clear();
        for(int x=0;x<w;x++)
            for(int y=0;y<h;y++) {
                Vec4 c = Pixels.getColor(img,x,y,w,h,0);
                
                if(x<w-1) {
                    Edge e = new Edge();
                    e.a = y*w+x;
                    e.b = y*w+x+1;
                    e.weight = (double)distance.invoke(c,Pixels.getColor(img,x+1,y,w,h,0));
                    edges.add(e);
                }
                
                if(y<h-1) {
                    Edge e = new Edge();
                    e.a = y*w+x;
                    e.b = (y+1)*w+x;
                    e.weight = (double)distance.invoke(c,Pixels.getColor(img,x,y+1,w,h,0));
                    edges.add(e);
                }
                
                if( (x<w-1) && (y<h-1)) {
                    Edge e = new Edge();
                    e.a = y*w+x;
                    e.b = (y+1)*w+x+1;
                    e.weight = (double)distance.invoke(c,Pixels.getColor(img,x+1,y+1,w,h,0));
                    edges.add(e);
                }
                
                if( (x<w-1) && (y>0)) {
                    Edge e = new Edge();
                    e.a = y*w+x;
                    e.b = (y-1)*w+x+1;
                    e.weight = (double)distance.invoke(c,Pixels.getColor(img,x+1,y-1,w,h,0));
                    edges.add(e);
                }
            }
        
        // sort edges
        Collections.sort(edges);
    }

    private int findEnd(int x) {
        int y = x;
        while(y != elements[y].parent) y = elements[y].parent;
        elements[x].parent = y;
        return y;
    }

    public int getSegment(int x, int y) {
        return findEnd(y*w+x);
    }
    
    private void joinSegments(int x, int y) {
        if(elements[x].rank > elements[y].rank) {
            elements[y].parent = x;
            elements[x].size += elements[y].size;
        } else {
            elements[x].parent = y;
            elements[y].size += elements[x].size;
            if(elements[x].rank == elements[y].rank) elements[y].rank++;
        }
    }

    public int calculateSegmentation(IFn distance, double threshold, int min_size) {
        makeEdges(distance);
        return calculateSegmentation(threshold,min_size);
    }
    
    public int calculateSegmentation(double threshold, int min_size) {
        int num = no_vertices;

        elements = new SegmentNode[no_vertices];
        
        // init nodes
        for(int i=0;i<no_vertices;i++) {
            SegmentNode s = new SegmentNode();
            s.rank = 0;
            s.size = 1;
            s.parent = i;
            elements[i]=s;
        }
  
        double[] thresholds = new double[no_vertices];
        Arrays.fill(thresholds,threshold);
  
        for(Edge edge: edges) {
            int a = findEnd(edge.a);
            int b = findEnd(edge.b);
            
            if(a!=b) {
                if(edge.weight <= thresholds[a] && edge.weight <= thresholds[b]) {
                    joinSegments(a,b);
                    a = findEnd(a);
                    thresholds[a] = edge.weight + threshold/elements[a].size;
                    num--;
                }
            }
        }
        
        for(Edge edge: edges) {
            int a = findEnd(edge.a);
            int b = findEnd(edge.b);
            if( (a != b) && ((elements[a].size < min_size) || (elements[b].size < min_size))) {
                joinSegments(a,b);
                num--;
            } 
        }
        
        return num;
  
    }
}
