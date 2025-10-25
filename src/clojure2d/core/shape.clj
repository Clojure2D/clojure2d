(ns clojure2d.core.shape
  "All shapes creating functions"
  (:require [clojure2d.protocols :as pr]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [fastmath.grid :as grid])
  (:import [java.awt Shape Graphics2D]
           [java.awt.font GlyphVector]
           [java.awt.geom PathIterator Ellipse2D Ellipse2D$Double Line2D Line2D$Double Path2D Path2D$Double Rectangle2D Rectangle2D$Double Point2D Point2D$Double Arc2D Arc2D$Double Area]))

(set! *unchecked-math* :warn-on-boxed)
(set! *warn-on-reflection* true)

(defn rectangle2d->box
  [^Rectangle2D box]
  [(.getX box) (.getY box)
   (.getWidth box) (.getHeight box)])

(extend-type Shape
  pr/ShapeProto
  (bounding-box [shape]
    (rectangle2d->box (.getBounds2D ^Shape shape)))
  (contains-point? [shape x y]
    (.contains ^Shape shape ^double x ^double y))
  (contains-rectangle? [shape x y w h]
    (.contains ^Shape shape ^double x ^double y ^double w ^double h))
  (intersects-rectangle? [shape x y w h]
    (.intersects ^Shape shape ^double x ^double y ^double w ^double h)))

(defn add-shape
  ([] (Area.))
  ([sh] (Area. sh))
  ([sh1 sh2] (doto (Area. sh1) (.add (Area. sh2)))))

(defn subtract-shape
  ([] (Area.))
  ([sh] (Area. sh))
  ([sh1 sh2] (doto (Area. sh1) (.subtract (Area. sh2)))))

(defn intersect-shape
  ([] (Area.))
  ([sh] (Area. sh))
  ([sh1 sh2] (doto (Area. sh1) (.intersect (Area. sh2)))))

(defn xor-shape
  ([] (Area.))
  ([sh] (Area. sh))
  ([sh1 sh2] (doto (Area. sh1) (.exclusiveOr (Area. sh2)))))

(defn line
  ([l x1 y1 x2 y2] (.setLine ^Line2D l x1 y1 x2 y2) l)
  ([[x1 y1] [x2 y2]] (line (Line2D$Double.) x1 y1 x2 y2))
  ([x1 y1 x2 y2] (line (Line2D$Double.) x1 y1 x2 y2))
  ([l [x1 y1] [x2 y2]] (line l x1 y1 x2 y2)))

(defn rect
  ([r x y w h] (.setFrame ^Rectangle2D r x y w h) r)
  ([x y w h] (rect (Rectangle2D$Double.) x y w h))
  ([[x y] w h] (rect x y w h)))

(defn crect
  ([r x y w h]
   (.setFrame ^Rectangle2D r
              (m/- (double x) (* 0.5 (double w)))
              (m/- (double y) (* 0.5 (double h))) w h)
   r)
  ([x y w h] (crect (Rectangle2D$Double.) x y w h))
  ([[x y] w h] (crect x y w h)))

(defn prect
  ([r x1 y1 x2 y2]
   (.setFrame ^Rectangle2D r x1 y1
              (m/- (double x2) (double x1))
              (m/- (double y2) (double y1)))
   r)
  ([x y w h] (prect (Rectangle2D$Double.) x y w h))
  ([[x y] w h] (prect x y w h)))

(defn ellipse
  ([e x y w h]
   (.setFrame ^Ellipse2D e
              (m/- (double x) (m/* (double w) 0.5))
              (m/- (double y) (m/* (double h) 0.5)) w h)
   e)
  ([x y w h] (ellipse (Ellipse2D$Double.) x y w h))
  ([[x y] w h] (ellipse x y w h)))

(defn arc
  ([a x y w h start extent type]
   (.setArc ^Arc2D a
            (m/- (double x) (m/* (double w) 0.5))
            (m/- (double y) (m/* (double h) 0.5)) w h
            (m/degrees (double start))
            (m/- (m/degrees (double extent)))
            (case type
              :chord Arc2D/CHORD
              :pie Arc2D/PIE
              Arc2D/OPEN))
   a)
  ([x y w h start extent type] (arc (Arc2D$Double.) x y w h start extent type))
  ([x y w h start extent] (arc x y w h start extent :open))
  ([[x y] w h start extent] (arc x y w h start extent)))

(defn rarc
  ([a x y r start extent type]
   (.setArcByCenter ^Arc2D a x y r
                    (m/degrees (double start))
                    (m/- (m/degrees (double extent)))
                    (case type
                      :chord Arc2D/CHORD
                      :pie Arc2D/PIE
                      Arc2D/OPEN))
   a)
  ([x y r start extent type] (rarc (Arc2D$Double.) x y r start extent type))
  ([x y r start extent] (rarc x y r start extent :open))
  ([[x y] r start extent] (rarc x y r start extent)))

(defn triangle
  ([x1 y1 x2 y2 x3 y3]
   (doto (Path2D$Double.)
     (.moveTo x1 y1)
     (.lineTo x2 y2)
     (.lineTo x3 y3)
     (.closePath)))
  ([[x1 y1] [x2 y2] [x3 y3]] (triangle x1 y1 x2 y2 x3 y3)))

(defn path
  "Create shape object out of path."
  ([vs] (path vs true))
  ([vs close?]
   (let [^Path2D p (Path2D$Double.)]
     (when (seq vs)
       (let [
             m (first vs)]
         (.moveTo p (first m) (second m))
         (doseq [v (next vs)]
           (.lineTo p (first v) (second v)))
         (when close? (.closePath p))))
     p)))

(defn- calculate-bezier-control-points
  "Calculate bezier spline control points. http://www.antigrain.com/research/bezier_interpolation/index.html"
  [v0 v1 v2 v3]
  (let [c1 (v/mult (v/add v0 v1) 0.5)
        c2 (v/mult (v/add v1 v2) 0.5)
        c3 (v/mult (v/add v2 v3) 0.5)
        len1 (v/mag c1)
        len2 (v/mag c2)
        len3 (v/mag c3)
        k1 (m// len1 (m/+ len1 len2))
        k2 (m// len2 (m/+ len2 len3))
        m1 (v/add c1 (v/mult (v/sub c2 c1) k1))
        m2 (v/add c2 (v/mult (v/sub c3 c2) k2))
        cp1 (-> c2
                (v/sub m1)
                (v/add m1)
                (v/add v1)
                (v/sub m1))
        cp2 (-> c2
                (v/sub m2)
                (v/add m2)
                (v/add v2)
                (v/sub m2))]
    [cp1 cp2]))

(defn path-bezier
  ([vs] (path-bezier vs false))
  ([vs close?]
   (let [^Path2D p (Path2D$Double.)]
     (when (m/> (count vs) 3)
       (let [vs (mapv v/vec2 vs)
             m0 (vs 0)
             m1 (vs 1)
             m2 (vs 2) 
             f0 (if close? m0 m0)
             f1 (if close? m1 m0)
             f2 (if close? m2 m1)
             f3 (if close? (vs 3) m2)
             vs (if close? (subvec vs 1) vs)]
         (.moveTo p (f1 0) (f1 1))
         (loop [v0 f0
                v1 f1
                v2 f2
                v3 f3
                nvs (subvec vs 3)]
           (let [[cp1 cp2] (calculate-bezier-control-points v0 v1 v2 v3)]
             (.curveTo p (cp1 0) (cp1 1) (cp2 0) (cp2 1) (v2 0) (v2 1))
             (if-not (empty? nvs)
               (recur v1 v2 v3 (nvs 0) (subvec nvs 1))
               (if close?
                 (let [[cp1 cp2] (calculate-bezier-control-points v1 v2 v3 m0)
                       [cp3 cp4] (calculate-bezier-control-points v2 v3 m0 m1)
                       [cp5 cp6] (calculate-bezier-control-points v3 m0 m1 m2)]
                   (.curveTo p (cp1 0) (cp1 1) (cp2 0) (cp2 1) (v3 0) (v3 1))
                   (.curveTo p (cp3 0) (cp3 1) (cp4 0) (cp4 1) (m0 0) (m0 1))
                   (.curveTo p (cp5 0) (cp5 1) (cp6 0) (cp6 1) (m1 0) (m1 1)))
                 (let [[cp1 cp2] (calculate-bezier-control-points v1 v2 v3 v3)]
                   (.curveTo p (cp1 0) (cp1 1) (cp2 0) (cp2 1) (v3 0) (v3 1)))))))))
     p)))

(defn bezier
  ([x1 y1 x2 y2 x3 y3 x4 y4]
   (doto (Path2D$Double.)
     (.moveTo x1 y1)
     (.curveTo x2 y2 x3 y3 x4 y4)))
  ([[x1 y1] [x2 y2] [x3 y3] [x4 y4]] (bezier x1 y1 x2 y2 x3 y3 x4 y4)))

(defn curve
  ([x1 y1 x2 y2 x3 y3]
   (doto (Path2D$Double.)
     (.moveTo x1 y1)
     (.quadTo x2 y2 x3 y3)))
  ([[x1 y1] [x2 y2] [x3 y3]] (curve x1 y1 x2 y2 x3 y3)))

(defn quad
  ([x1 y1 x2 y2 x3 y3 x4 y4]
   (doto (Path2D$Double.)
     (.moveTo x1 y1)
     (.lineTo x2 y2)
     (.lineTo x3 y3)
     (.lineTo x4 y4)
     (.closePath)))
  ([[x1 y1] [x2 y2] [x3 y3] [x4 y4]] (quad x1 y1 x2 y2 x3 y3 x4 y4)))

(defn pointy-hex
  ([x y size]
   (path (grid/pointy-hex-corners size x y) true))
  ([[x y] size] (pointy-hex x y size)))

(defn flat-hex
  ([x y size]
   (path (grid/flat-hex-corners size x y) true))
  ([[x y] size] (flat-hex x y size)))

(defn grid-cell
  ([grid x y]
   (path (grid/corners grid [x y]) true))
  ([grid x y scale]
   (path (grid/corners grid x y scale) true)))

(defn- get-glyph-vector
  ^GlyphVector [canvas ^String txt]
  (let [^Graphics2D g (pr/graphics2d canvas)]
    (.createGlyphVector (.getFont g) (.getFontRenderContext g) txt)))

(defn text-shape
  "Returns shape for given text. Should be called withing context."
  {:metadoc/categories #{:write}}
  ([canvas txt]
   (.getOutline (get-glyph-vector canvas txt)))
  ([canvas txt x y]
   (.getOutline (get-glyph-vector canvas txt) (float x) (float y))))
