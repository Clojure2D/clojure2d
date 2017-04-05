;; ## n-dimentional vector utilities
;;
;; Main goal for this namespace is to provide various utility functions which operate on
;; mathematical vectors: 2d, 3d and 4d 
;;
;; Concept for API is taken from [Proceessing](https://github.com/processing/processing/blob/master/core/src/processing/core/PVector.java) and [openFrameworks](https://github.com/openframeworks/openFrameworks/tree/master/libs/openFrameworks/math)
;;

;; Tolerance for vector comparison used in `is-near-zero?` and `aligned?` functions
;; to mitigate approximation errors
(ns clojure2d.math.vector
  (:require [clojure2d.math :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def ^:const ^double TOLERANCE 1.0e-6)

(defprotocol VectorProto
  (applyf [v f])
  (magsq [v1])
  (mag [v1])
  (dot [v1 v2])
  (add [v1] [v1 v2])
  (sub [v1] [v1 v2])
  (mult [v1 v])
  (emult [v1 v])
  (div [v1 v])
  (abs [v1])
  (mx [v1])
  (mn [v1])
  (emx [v1 v2])
  (emn [v1 v2])
  (maxdim [v])
  (mindim [v])
  (base-from [v])
  (sum [v1])  
  (interpolate [v1 v2 t] [v1 v2 t f])
  (is-zero? [v1])
  (is-near-zero? [v1])
  (heading [v1])
  (cross [v1 v2])
  (rotate [v1 angle] [v1 anglex angley anglez])
  (perpendicular [v1] [v1 v2])
  (axis-rotate [v1 angle axis] [v1 angle axis pivot])
  (transform [v1 o vx vy] [v1 o vx vy vz])
  (to-polar [v1])
  (from-polar [v1]))

;; transform -   "Map point to coordinate system defined by origin, vx and vy (as bases)"

(declare angle-between)
(declare normalize)

(deftype Vec2 [^double x ^double y]
  Object
  (toString [_] (str "[" x ", " y "]"))
  VectorProto
  (applyf [_ f] (Vec2. (f x) (f y)))
  (magsq [_] (+ (* x x) (* y y)))
  (mag [_] (m/hypot x y))
  (dot [_ v2] 
    (let [^Vec2 v2 v2] (+ (* x (.x v2)) (* y (.y v2)))))
  (add [_] (Vec2. x y))
  (add [_ v2] 
    (let [^Vec2 v2 v2] (Vec2. (+ x (.x v2)) (+ y (.y v2)))))
  (sub [_] (Vec2. (- x) (- y)))
  (sub [_ v2]
    (let [^Vec2 v2 v2] (Vec2. (- x (.x v2)) (- y (.y v2)))))
  (mult [_ v] (Vec2. (* x ^double v) (* y ^double v)))
  (emult [_ v] 
    (let [^Vec2 v v] (Vec2. (* x (.x v)) (* y (.y v)))))
  (div [_ v] 
    (let [v1 (/ 1.0 ^double v)] (Vec2. (* x v1) (* y v1))))
  (abs [_] (Vec2. (m/abs x) (m/abs y)))
  (mx [_] (max x y))
  (mn [_] (min x y))
  (emx [_ v]
    (let [^Vec2 v v] (Vec2. (max (.x v) x) (max (.y v) y))))
  (emn [_ v]
    (let [^Vec2 v v] (Vec2. (min (.x v) x) (min (.y v) y))))
  (maxdim [_]
    (if (> x y) 0 1))
  (mindim [_]
    (if (< x y) 0 1))
  (base-from [v]
    [v (perpendicular v)])
  (sum [_] (+ x y)) 
  (interpolate [_ v2 t f]
    (let [^Vec2 v2 v2] (Vec2. (f x (.x v2) t)
                              (f y (.y v2) t))))
  (interpolate [v1 v2 t] (interpolate v1 v2 t m/lerp))
  (is-zero? [_] (and (zero? x) (zero? y)))
  (is-near-zero? [_] (and (< (m/abs x) TOLERANCE)
                          (< (m/abs y) TOLERANCE)))
  (heading [_] (m/atan2 y x))
  (rotate [_ angle]
    (let [sa (m/sin angle)
          ca (m/cos angle)
          nx (- (* x ca) (* y sa))
          ny (+ (* x sa) (* y ca))]
      (Vec2. nx ny)))
  (perpendicular [_]
    (normalize (Vec2. (- y) x)))
  (transform [_ o vx vy]
    (let [^Vec2 o o
          ^Vec2 vx vx
          ^Vec2 vy vy]
      (Vec2. (+ (.x o) (* x (.x vx)) (* y (.x vy))) (+ (.y o) (* x (.y vx)) (* y (.y vy))))))
  (to-polar [v]
    (Vec2. (mag v) (heading v)))
  (from-polar [_]
    (Vec2. (* x (m/cos y))
           (* x (m/sin y)))))

(deftype Vec3 [^double x ^double y ^double z]
  Object
  (toString [_] (str "[" x ", " y ", " z "]"))
  VectorProto
  (applyf [_ f] (Vec3. (f x) (f y) (f z)))
  (magsq [_] (+ (* x x) (* y y) (* z z)))
  (mag [_] (m/hypot x y z))
  (dot [_ v2]
    (let [^Vec3 v2 v2] (+ (* x (.x v2)) (* y (.y v2)) (* z (.z v2)))))
  (add [_] (Vec3. x y z))
  (add [_ v2] 
    (let [^Vec3 v2 v2] (Vec3. (+ x (.x v2)) (+ y (.y v2)) (+ z (.z v2)))))
  (sub [_] (Vec3. (- x) (- y) (- z)))
  (sub [_ v2]
    (let [^Vec3 v2 v2] (Vec3. (- x (.x v2)) (- y (.y v2)) (- z (.z v2)))))
  (mult [_ v] (Vec3. (* x ^double v) (* y ^double v) (* z ^double v)))
  (emult [_ v] 
    (let [^Vec3 v v] (Vec3. (* x (.x v)) (* y (.y v)) (* z (.z v)))))
  (div [_ v] 
    (let [v1 (/ 1.0 ^double v)] (Vec3. (* x v1) (*  y v1) (* z v1))))
  (abs [_] (Vec3. (m/abs x) (m/abs y) (m/abs z)))
  (mx [_] (max x y z))
  (mn [_] (min x y z))
  (emx [_ v]
    (let [^Vec3 v v] (Vec3. (max (.x v) x) (max (.y v) y) (max (.z v) z))))
  (emn [_ v]
    (let [^Vec3 v v] (Vec3. (min (.x v) x) (min (.y v) y) (min (.z v) z))))
  (maxdim [_]
    (if (> x y)
      (if (> x z) 0 2)
      (if (> y z) 1 2)))
  (mindim [_]
    (if (< x y)
      (if (< x z) 0 2)
      (if (< y z) 1 2)))
  (base-from [v]
    (let [v2 (if (> (m/abs x) (m/abs y))
               (div (Vec3. (- z) 0.0 x) (m/hypot x z))
               (div (Vec3. 0.0 z (- y)) (m/hypot y z)))]
      [v v2 (cross v v2)]))
  (sum [_] (+ x y z)) 
  (interpolate [_ v2 t f]
    (let [^Vec3 v2 v2] (Vec3. (f x (.x v2) t)
                              (f y (.y v2) t)
                              (f z (.z v2) t))))
  (interpolate [v1 v2 t] (interpolate v1 v2 t m/lerp))
  (is-zero? [_] (and (zero? x) (zero? y) (zero? z)))
  (is-near-zero? [_] (and (< (m/abs x) TOLERANCE)
                          (< (m/abs y) TOLERANCE)
                          (< (m/abs z) TOLERANCE)))
  (heading [v1] (angle-between v1 (Vec3. 1 0 0)))
  (cross [_ v2]
   (let [^Vec3 v2 v2
         cx (- (* y (.z v2)) (* (.y v2) z))
         cy (- (* z (.x v2)) (* (.z v2) x))
         cz (- (* x (.y v2)) (* (.x v2) y))]
     (Vec3. cx cy cz)))
  (perpendicular [v1 v2]
    (normalize (cross v1 v2)))
  (transform [_ o vx vy vz]
    (let [^Vec3 o o
          ^Vec3 vx vx
          ^Vec3 vy vy
          ^Vec3 vz vz]
      (Vec3. (+ (.x o) (* x (.x vx)) (* y (.x vy)) (* z (.x vz)))
             (+ (.y o) (* x (.y vx)) (* y (.y vy)) (* z (.y vz)))
             (+ (.z o) (* x (.z vx)) (* y (.z vy)) (* z (.z vz))))))
  (axis-rotate [_ angle axis]
    (let [^Vec3 axis axis
          ^Vec3 ax (normalize axis)
          axx (.x ax)
          axy (.y ax)
          axz (.z ax)
          cosa (m/cos angle)
          ^Vec3 sa (mult ax (m/sin angle))
          sax (.x sa)
          say (.y sa)
          saz (.z sa)
          ^Vec3 cb (mult ax (- 1.0 cosa))
          cbx (.x cb)
          cby (.y cb)
          cbz (.z cb)
          nx (+ (* x (+ (* axx cbx) cosa))
                (* y (- (* axx cby) saz))
                (* z (+ (* axx cbz) say)))
          ny (+ (* x (+ (* axy cbx) saz))
                (* y (+ (* axy cby) cosa))
                (* z (- (* axy cbz) sax)))
          nz (+ (* x (- (* axz cbx) say))
                (* y (+ (* axz cby) sax))
                (* z (+ (* axz cbz) cosa)))]
      (Vec3. nx ny nz)))
  (axis-rotate [v1 angle axis pivot]
    (add (axis-rotate (sub v1 pivot) angle axis) pivot))
  (rotate [_ anglex angley anglez]
    (let [a (m/cos anglex)
          b (m/sin anglex)
          c (m/cos angley)
          d (m/sin angley)
          e (m/cos anglez)
          f (m/sin anglez)
          cex (* c x e)
          cf (* c f)
          dz (* d z)
          nx (+ (- cex cf) dz)
          af (* a f)
          de (* d e)
          bde (* b de)
          ae (* a e)
          bdf (* b d f)
          bcz (* b c z)
          ny (- (+ (* (+ af bde) x) (* (- ae bdf) y)) bcz)
          bf (* b f)
          ade (* a de)
          adf (* a d f)
          be (* b e)
          acz (* a c z)
          nz (+ (* (- bf ade) x) (* (+ adf be) y) acz)]
      (Vec3. nx ny nz)))
  (to-polar [v1]
    (let [r (mag v1)
          zr (/ z ^double r)
          theta (cond
                  (<= zr -1) m/PI
                  (>= zr 1) 0
                  :else (m/acos zr))
          phi (m/atan2 y x)]
      (Vec3. r theta phi)))
  (from-polar [_]
    (let [st (m/sin y)
          ct (m/cos y)
          sp (m/sin z)
          cp (m/cos z)]
      (Vec3. (* x st cp)
             (* x st sp)
             (* x ct)))))

(deftype Vec4 [^double x ^double y ^double z ^double w]
  Object
  (toString [_] (str "[" x ", " y ", " z ", " w "]"))
  VectorProto
  (applyf [_ f] (Vec4. (f x) (f y) (f z) (f w)))
  (magsq [_] (+ (* x x) (* y y) (* z z) (* w w)))
  (mag [v1] (m/sqrt (magsq v1)))
  (dot [_ v2]
    (let [^Vec4 v2 v2] (+ (* x (.x v2)) (* y (.y v2)) (* z (.z v2)) (* w (.w v2)))))
  (add [_] (Vec4. x y z w))
  (add [_ v2]
    (let [^Vec4 v2 v2] (Vec4. (+ x (.x v2)) (+ y (.y v2)) (+ z (.z v2)) (+ w (.w v2)))))
  (sub [_] (Vec4. (- x) (- y) (- z) (- w)))
  (sub [_ v2] 
    (let [^Vec4 v2 v2] (Vec4. (- x (.x v2)) (- y (.y v2)) (- z (.z v2)) (- w (.w v2)))))
  (mult [_ v] (Vec4. (* x ^double v) (* y ^double v) (* z ^double v) (* w ^double v)))
  (emult [_ v]
    (let [^Vec4 v v] (Vec4. (* x (.x v)) (* y (.y v)) (* z (.z v)) (* w (.w v)))))
  (div [_ v]
    (let [v1 (/ 1.0 ^double v)] (Vec4. (* x v1) (* y v1) (* z v1) (* w v1))))
  (abs [_] (Vec4. (m/abs x) (m/abs y) (m/abs z) (m/abs w)))
  (mx [_] (max x y z w))
  (mn [_] (min x y z w))
  (emx [_ v]
    (let [^Vec4 v v] (Vec4. (max (.x v) x) (max (.y v) y) (max (.z v) z) (max (.w v) w))))
  (emn [_ v]
    (let [^Vec4 v v] (Vec4. (min (.x v) x) (min (.y v) y) (min (.z v) z) (min (.w v) w))))
  (sum [_] (+ x y z w)) 
  (interpolate [_ v2 t f]
    (let [^Vec4 v2 v2] (Vec4. (f x (.x v2) t)
                              (f y (.y v2) t)
                              (f z (.z v2) t)
                              (f w (.w v2) t))))
  (interpolate [v1 v2 t] (interpolate v1 v2 t m/lerp))
  (is-zero? [_] (and (zero? x) (zero? y) (zero? z) (zero? w)))
  (is-near-zero? [_] (and (< (m/abs x) TOLERANCE)
                          (< (m/abs y) TOLERANCE)
                          (< (m/abs z) TOLERANCE)
                          (< (m/abs w) TOLERANCE)))
  (heading [v1] (angle-between v1 (Vec4. 1 0 0 0))))

;; common functions

(defn average-vectors
  "Average / centroid of vectors"
  [init vs]
  (div (reduce add init vs) (count vs)))

(defn dist
  "Euclidean distance between vectors"
  [v1 v2]
  (mag (sub v1 v2)))

(defn dist-sq
  "Euclidean distance between vectors squared"
  [v1 v2]
  (magsq (sub v1 v2)))

(defn dist-abs
  "Manhattan distance between vectors"
  [v1 v2]
  (sum (abs (sub v1 v2))))

(defn dist-cheb
  "Chebyshev distance between 2d vectors"
  [v1 v2]
  (mx (abs (sub v1 v2))))

(defn normalize
  "Normalize vector"
  [v]
  (div v (mag v)))

(defn scale
  "Create new vector with given length"
  [v len]
  (mult (normalize v) len))

(defn limit
  "Limit length of the vector by given value"
  [v ^double len]
  (if (> ^double (magsq v) (* len len))
    (scale v len)
    v))

(defn angle-between
  "Angle between two vectors"
  [v1 v2]
  (if (or (is-zero? v1) (is-zero? v2))
    0
    (let [d (dot v1 v2)
          amt (/ ^double d (* ^double (mag v1) ^double (mag v2)))]
      (cond
        (<= amt -1) m/PI
        (>= amt 1) 0
        :else (m/acos amt)))))

(defn aligned?
  "Are vectors aligned (have the same direction)?"
  [v1 v2]
  (< ^double (angle-between v1 v2) TOLERANCE))

(defn faceforward
  "flip normal"
  [n v]
  (if (neg? ^double (dot n v))
    n
    (sub n)))

(defn generate-vec2
  ""
  ([f1 f2]
   (Vec2. (f1) (f2)))
  ([f]
   (Vec2. (f) (f))))

(defn generate-vec3
  ""
  ([f1 f2 f3]
   (Vec3. (f1) (f2) (f3)))
  ([f]
   (Vec3. (f) (f) (f))))

(defn generate-vec4
  ""
  ([f1 f2 f3 f4]
   (Vec4. (f1) (f2) (f3) (f4)))
  ([f]
   (Vec4. (f) (f) (f) (f))))

(defn array->vec2
  ""
  [^doubles a]
  (Vec2. (aget a 0) (aget a 1)))

(defn array->vec3
  ""
  [^doubles a]
  (Vec3. (aget a 0) (aget a 1) (aget a 2)))

(defn array->vec4
  ""
  [^doubles a]
  (Vec4. (aget a 0) (aget a 1) (aget a 2) (aget a 3)))


;;(perpendicular (Vec3. 1 2 0) (Vec3. 4 3 0))
;;(perpendicular (Vec2. 1 0))

;;(transform (Vec3. 1 2 3) (Vec3. 1 2 3) (Vec3. 1 1 2) (Vec3. -1 1 2) (Vec3. 1 -5 1))

;;(transform (Vec2. 1 2) (Vec2. 1 2) (Vec2. 1 1) (Vec2. 1 -5))

;; (axis-rotate (Vec3. 1 2 3) PI (Vec3. 0 1 0))
;;(axis-rotate (Vec3. 1 2 3) PI (Vec3. -1 1 1) (Vec3. 2 2 2))

;;(rotate (Vec2. 1 1) (/ PI 4))

;;(rotate (Vec3. 1 2 3) PI 0 0)
