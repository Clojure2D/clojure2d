(ns clojure2d.core.shape
  "All shapes creating functions"
  (:require [clojure2d.protocols :as pr]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [fastmath.grid :as grid])
  (:import [java.awt Font RenderingHints Shape Graphics2D]
           [java.awt.font FontRenderContext GlyphVector TextAttribute TextLayout]
           [java.awt.geom AffineTransform Ellipse2D Ellipse2D$Double Line2D Line2D$Double Path2D Path2D$Double Rectangle2D Rectangle2D$Double Arc2D Arc2D$Double Area]
           [java.io StringReader]
           [java.text AttributedString]
           [org.apache.batik.parser AWTPathProducer]))

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

(defn- length-ratio
  "`a / (a + b)` of segment lengths, 0.5 when both are 0 (coincident points)."
  ^double [^double a ^double b]
  (let [s (m/+ a b)] (if (m/zero? s) 0.5 (m// a s))))

(defn- calculate-bezier-control-points
  "Calculate bezier spline control points of the curve from `v1` to `v2` (neighbours `v0`, `v3`): the midpoints of the
  segments, split by the ratio of the segment lengths. http://www.antigrain.com/research/bezier_interpolation/index.html"
  [v0 v1 v2 v3]
  (let [c1 (v/mult (v/add v0 v1) 0.5)
        c2 (v/mult (v/add v1 v2) 0.5)
        c3 (v/mult (v/add v2 v3) 0.5)
        len1 (v/dist v0 v1)
        len2 (v/dist v1 v2)
        len3 (v/dist v2 v3)
        k1 (length-ratio len1 len2)
        k2 (length-ratio len2 len3)
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
  "Smooth path through the points `vs` (cubic bezier curves, antigrain control points); `close?` closes it smoothly.
  An open path needs 2 points (2 points give a straight line), a closed one 4; fewer give an empty path."
  ([vs] (path-bezier vs false))
  ([vs close?]
   (let [^Path2D p (Path2D$Double.)
         n (count vs)]
     (when (and (not close?) (m/== n 2))
       (let [[a b] (mapv v/vec2 vs)]
         (.moveTo p (a 0) (a 1))
         (.lineTo p (b 0) (b 1))))
     (when (if close? (m/> n 3) (m/> n 2))
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

;; transformations

(defn- ops->affine-transform
  ^AffineTransform [ops]
  (let [t (AffineTransform.)]
    (doseq [[op a b] ops]
      (case op
        :translate (.translate t (double a) (double b))
        :scale (.scale t (double a) (double (or b a)))
        :rotate (.rotate t (double a))
        :shear (.shear t (double a) (double b))))
    t))

(defn transform-shape
  "Returns a new shape: `shape` transformed by `transformation`.

  `transformation` is a `java.awt.geom.AffineTransform` or a sequence of operations composed like the canvas transformations (`translate`, `scale`, `rotate`, `shear`): the later operation applies first to the shape coordinates.

  * `[:translate tx ty]`
  * `[:scale s]` or `[:scale sx sy]`
  * `[:rotate angle]` - radians
  * `[:shear sx sy]`

  For example `[[:translate x y] [:scale s]]` scales the shape around the origin, then moves it by `[x y]`."
  ^Shape [^Shape shape transformation]
  (.createTransformedShape (if (instance? AffineTransform transformation)
                             ^AffineTransform transformation
                             (ops->affine-transform transformation)) shape))

;; rounded rectangle

(def ^{:private true :const true :tag 'double} kappa
  "Control point distance of a cubic Bezier quarter circle, as a fraction of the radius."
  0.5522847498307936)

(defn- corner-radii
  "Four corner radii `[tl tr br bl]` of `r` (a number or four numbers) clamped to `[0 limit]`."
  [r ^double limit]
  (let [radii (cond
                (number? r) [r r r r]
                (and (sequential? r) (m/== 4 (count r)) (every? number? r)) r
                :else (throw (ex-info (str "Corner radius should be a number or four numbers, got " (pr-str r)) {:radius r})))]
    (mapv (fn [v] (m/constrain (double v) 0.0 limit)) radii)))

(defn rounded-rect
  "Rectangle with rounded corners, the corners quarter circles.

  * `x`, `y` - top left corner (or `[x y]`), `w`, `h` - width and height
  * `r` - corner radius: a number (all corners) or `[top-left top-right bottom-right bottom-left]`, each clamped to `[0, half of the shorter side]`: 0 or a negative radius gives a square corner, a negative `w` or `h` square corners

  With one radius not above half of the shorter side the shape is the same as `java.awt.geom.RoundRectangle2D` with arcs `2r`.

  Throws `ex-info` when `r` isn't a number or four numbers."
  (^Shape [[x y] w h r] (rounded-rect x y w h r))
  (^Shape [x y w h r]
   (let [x (double x) y (double y) w (double w) h (double h)
         [^double tl ^double tr ^double br ^double bl] (corner-radii r (m/max 0.0 (m/* 0.5 (m/min w h))))
         x1 (m/+ x w) y1 (m/+ y h)
         p (Path2D$Double.)]
     (.moveTo p (m/+ x tl) y)
     (.lineTo p (m/- x1 tr) y)
     (.curveTo p (m/+ (m/- x1 tr) (m/* kappa tr)) y x1 (m/- (m/+ y tr) (m/* kappa tr)) x1 (m/+ y tr))
     (.lineTo p x1 (m/- y1 br))
     (.curveTo p x1 (m/+ (m/- y1 br) (m/* kappa br)) (m/+ (m/- x1 br) (m/* kappa br)) y1 (m/- x1 br) y1)
     (.lineTo p (m/+ x bl) y1)
     (.curveTo p (m/- (m/+ x bl) (m/* kappa bl)) y1 x (m/+ (m/- y1 bl) (m/* kappa bl)) x (m/- y1 bl))
     (.lineTo p x (m/+ y tl))
     (.curveTo p x (m/- (m/+ y tl) (m/* kappa tl)) (m/- (m/+ x tl) (m/* kappa tl)) y (m/+ x tl) y)
     (.closePath p)
     p)))

;; svg path

(def ^{:doc "Path winding rules by name: `:non-zero` and `:even-odd` to the `java.awt.geom.Path2D` constants."}
  winding-rules
  {:non-zero Path2D/WIND_NON_ZERO
   :even-odd Path2D/WIND_EVEN_ODD})

(defn svg-path
  "Returns a shape of SVG path data (the `d` attribute of a `path` element, eg. `M0 0 L10 0 L5 8 Z`).

  `winding` is `:non-zero` (default) or `:even-odd` (see [[winding-rules]]).

  Throws `ex-info` for another winding, and when the data doesn't parse."
  (^Shape [d] (svg-path d :non-zero))
  (^Shape [^String d winding]
   (let [rule (winding-rules winding)]
     (when-not rule
       (throw (ex-info (str "Unknown winding rule " winding) {:winding winding :windings (set (keys winding-rules))})))
     (AWTPathProducer/createShape (StringReader. d) (int rule)))))

;; text outline

(def ^:private outline-render-context
  (FontRenderContext. nil RenderingHints/VALUE_TEXT_ANTIALIAS_ON RenderingHints/VALUE_FRACTIONALMETRICS_ON))

(defn- font-runs
  "`[start end font]` runs of `text`: `font` where it can show the character, `fallback` elsewhere."
  [^String text ^Font font ^Font fallback]
  (let [n (.length text)]
    (loop [i (long 0)
           runs []]
      (if (m/>= i n)
        runs
        (let [cp (.codePointAt text (int i))
              j (m/long-add i (Character/charCount cp))
              f (if (or (nil? fallback) (.canDisplay font (int cp))) font fallback)
              [s _ g :as last-run] (peek runs)]
          (recur j (if (and last-run (identical? g f)) (conj (pop runs) [s j f]) (conj runs [i j f]))))))))

(defn text-outline
  "Returns the outline shape of `text` laid out in `font` (a `java.awt.Font`), without a canvas.

  Characters `font` can't show are taken from `fallback` font (when given), eg. an emoji font. The layout uses antialiased text and fractional metrics, the baseline starts at the origin.

  Empty `text` gives an empty outline.

  See also [[text-shape]] (canvas font)."
  (^Shape [font text] (text-outline font text nil))
  (^Shape [^Font font ^String text fallback]
   (let [attributed (AttributedString. text)]
     (doseq [[s e f] (font-runs text font fallback)]
       (.addAttribute attributed TextAttribute/FONT f (int s) (int e)))
     (.getOutline (TextLayout. (.getIterator attributed) ^FontRenderContext outline-render-context) nil))))
