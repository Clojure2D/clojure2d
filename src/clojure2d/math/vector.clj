;; ## n-dimentional vector utilities
;;
;; Main goal for this namespace is to provide various utility functions which operate on
;; mathematical vectors: 2d, 3d and 4d 
;;
;; Concept for API is taken from [Proceessing](https://github.com/processing/processing/blob/master/core/src/processing/core/PVector.java) and [openFrameworks](https://github.com/openframeworks/openFrameworks/tree/master/libs/openFrameworks/math)
;;
;; All vectors are equipped with Counted (`count`), Sequential, Sequable (`seq`) and IFn protocols. Additionally Clojure vector is equipped with defined here `VectorProto`.

(ns clojure2d.math.vector
  (:require [clojure2d.math :as m]
            [primitive-math :as prim])
  (:import [clojure.lang Counted IFn PersistentVector Seqable Sequential]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(prim/use-primitive-operators)

;; Tolerance (epsilon), used in `is-near-zero?` fn
(def ^:const ^double TOLERANCE 1.0e-6)

;; ## Vector definitions

(defprotocol VectorProto
  "Vector operations"
  (to-vec [v] "Convert to Clojure vector")
  (applyf [v f] "Apply function to all vector values (map)")
  (magsq [v1] "length squared")
  (mag [v1] "length")
  (dot [v1 v2] "dot product of two vectors")
  (add [v1] [v1 v2] "sum of two vectors")
  (sub [v1] [v1 v2] "difference of two vectors")
  (mult [v1 v] "multiply vector with value")
  (emult [v1 v] "multiply vectors element by element")
  (div [v1 v] "divide vector with value")
  (abs [v1] "absolute value of vector elements")
  (mx [v1] "maximum value of vector elements")
  (mn [v1] "minimum value of vector elements")
  (emx [v1 v2] "create vector with maximum values of coordinates")
  (emn [v1 v2] "create vector with minimum values of coordinates")
  (maxdim [v] "dimension/position of maximum value")
  (mindim [v] "dimension/position of minimum value")
  (base-from [v] "return list of perpendicular vectors (basis)")
  (sum [v1] "sum of elements")
  (permute [v idxs] "permute vector elements")
  (reciprocal [v] "reciprocal of elements")
  (interpolate [v1 v2 t] [v1 v2 t f] "interpolate vectors with value `t`; optionally set interpolation fn")
  (einterpolate [v1 v2 v] [v1 v2 v f] "interpolate vectors elementwise; optionally set interpolation fn")
  (econstrain [v val1 val2] "elementwise constrain")
  (is-zero? [v1] "is vector zero?")
  (is-near-zero? [v1] "is vector almost zero? (all elements are less than `TOLERANCE`)")
  (heading [v1] "vector angle in polar coordinates")
  (cross [v1 v2] "cross product")
  (rotate [v1 angle] [v1 anglex angley anglez] "rotate vector")
  (perpendicular [v1] [v1 v2] "find perpendicular vector")
  (axis-rotate [v1 angle axis] [v1 angle axis pivot] "rotate around axis")
  (transform [v1 o vx vy] [v1 o vx vy vz] "transform vector; map point to coordinate system defined by origin, vx and vy (as bases)")
  (to-polar [v1] "to polar coordinates")
  (from-polar [v1] "from polar coordinates"))

(declare angle-between)
(declare normalize)

(defn- find-idx-reducer-fn 
  "Helper function for reduce to find index for maximum/minimum value in vector."
  [f]
  #(let [[^double midx ^double curr v] %1]
     (if (f %2 v)
       [curr (inc curr) %2]
       [midx (inc curr) v])))

(defn- near-zero?
  "Is your value less than TOLERANCE?"
  [^double v]
  (< (m/abs v) TOLERANCE))

;; Add `VectorProto` to Clojure vector using map/reduce terms.
(extend PersistentVector
  VectorProto
  {:to-vec identity
   :applyf #(mapv %2 %1)
   :magsq (fn [v] (reduce #(+ (double %1) (* (double %2) (double %2))) v))
   :mag #(m/sqrt (magsq %))
   :dot #(reduce clojure.core/+ (map clojure.core/* %1 %2))
   :add #(mapv clojure.core/+ %1 %2)
   :sub #(mapv clojure.core/- %1 %2)
   :mult (fn [v1 v] (map #(* (double %) ^double v) v1))
   :emult #(mapv clojure.core/* %1 %2)
   :div #(mult %1 (/ 1.0 (double %2)))
   :abs #(mapv m/abs %)
   :mx #(reduce clojure.core/max %)
   :mn #(reduce clojure.core/min %)
   :emx #(mapv clojure.core/max %1 %2)
   :emn #(mapv clojure.core/min %1 %2)
   :maxdim #(first (reduce (find-idx-reducer-fn clojure.core/>) [0.0 0.0 (first %)] %))
   :mindim #(first (reduce (find-idx-reducer-fn clojure.core/<) [0.0 0.0 (first %)] %))
   :sum #(reduce clojure.core/+ %)
   :permute #(mapv (fn [idx] (%1 idx)) %2)
   :reciprocal #(mapv (fn [v] (/ 1.0 ^double v)) %)
   :interpolate (fn
                  ([v1 v2 t f]
                   (mapv #(f %1 %2 t) v1 v2))
                  ([v1 v2 t] (interpolate v1 v2 t m/lerp)))
   :einterpolate (fn
                   ([v1 v2 v f]
                    (mapv #(f %1 %2 %3) v1 v2 v))
                   ([v1 v2 v] (einterpolate v1 v2 v m/lerp)))
   :econstrain (fn [v val1 val2] (mapv #(m/constrain %1 val1 val2)) v)
   :is-zero? #(every? clojure.core/zero? %)
   :is-near-zero? #(every? near-zero? %)})

;; Create Vec2 and add all necessary protocols
(deftype Vec2 [^double x ^double y]
  Object
  (toString [_] (str "[" x ", " y "]"))
  (equals [_ v]
    (and (instance? Vec2 v)
         (let [^Vec2 v v]
           (and (== x (.x v))
                (== y (.y v))))))
  Sequential
  Seqable
  (seq [_] (list x y))
  Counted
  (count [_] 2)
  IFn
  (invoke [_ id]
    (case (int id)
      0 x
      1 y
      nil))
  VectorProto
  (to-vec [_] [x y])
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
  (permute [p [^long i1 ^long i2]]
    (Vec2. (p i1) (p i2)))
  (reciprocal [_] (Vec2. (/ 1.0 x) (/ 1.0 y)))
  (interpolate [_ v2 t f]
    (let [^Vec2 v2 v2] (Vec2. (f x (.x v2) t)
                              (f y (.y v2) t))))
  (interpolate [v1 v2 t] (interpolate v1 v2 t m/lerp))
  (einterpolate [_ v2 v f]
    (let [^Vec2 v2 v2
          ^Vec2 v v]
      (Vec2. (f x (.x v2) (.x v))
             (f y (.y v2) (.y v)))))
  (einterpolate [v1 v2 v] (einterpolate v1 v2 v m/lerp))
  (econstrain [_ val1 val2] (Vec2. (m/constrain x val1 val2)
                                   (m/constrain y val1 val2)))
  (is-zero? [_] (and (zero? x) (zero? y)))
  (is-near-zero? [_] (and (near-zero? x) (near-zero? y)))
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

;; Create Vec3 and add all necessary protocols
(deftype Vec3 [^double x ^double y ^double z]
  Object
  (toString [_] (str "[" x ", " y ", " z "]"))
  (equals [_ v]
    (and (instance? Vec3 v)
         (let [^Vec3 v v]
           (and (== x (.x v))
                (== y (.y v))
                (== z (.z v))))))
  Sequential
  Seqable
  (seq [_] (list x y z))
  Counted
  (count [_] 3)
  IFn
  (invoke [_ id]
    (case (int id)
      0 x
      1 y
      2 z
      nil))
  VectorProto
  (to-vec [_] [x y z])
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
  (permute [p [^long i1 ^long i2 ^long i3]]
    (Vec3. (p i1) (p i2) (p i3)))
  (reciprocal [_] (Vec3. (/ 1.0 x) (/ 1.0 y) (/ 1.0 z)))
  (interpolate [_ v2 t f]
    (let [^Vec3 v2 v2] (Vec3. (f x (.x v2) t)
                              (f y (.y v2) t)
                              (f z (.z v2) t))))
  (interpolate [v1 v2 t] (interpolate v1 v2 t m/lerp))
  (einterpolate [_ v2 v f]
    (let [^Vec3 v2 v2
          ^Vec3 v v]
      (Vec3. (f x (.x v2) (.x v))
             (f y (.y v2) (.y v))
             (f z (.z v2) (.z v)))))
  (einterpolate [v1 v2 v] (einterpolate v1 v2 v m/lerp))
  (econstrain [_ val1 val2] (Vec3. (m/constrain x val1 val2)
                                   (m/constrain y val1 val2)
                                   (m/constrain z val1 val2)))
  (is-zero? [_] (and (zero? x) (zero? y) (zero? z)))
  (is-near-zero? [_] (and (near-zero? x) (near-zero? y) (near-zero? z)))
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
    (let [^double r (mag v1)
          zr (/ z r)
          theta (cond
                  (<= zr -1.0) m/PI
                  (>= zr 1.0) 0
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

;; Create Vec4 and add all necessary protocols
(deftype Vec4 [^double x ^double y ^double z ^double w]
  Object
  (toString [_] (str "[" x ", " y ", " z ", " w "]"))
  (equals [_ v]
    (and (instance? Vec4 v)
         (let [^Vec4 v v]
           (and (== x (.x v))
                (== y (.y v))
                (== z (.z v))
                (== w (.w v))))))
  Sequential
  Seqable
  (seq [_] (list x y z w))
  Counted
  (count [_] 4)
  IFn
  (invoke [_ id]
    (case (int id)
      0 x
      1 y
      2 z
      3 w
      nil))
  VectorProto
  (to-vec [_] [x y z w])
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
  (maxdim [_]
    (max-key [x y z w] 0 1 2 3))
  (mindim [_]
    (min-key [x y z w] 0 1 2 3))
  (sum [_] (+ x y z w))
  (permute [p [^long i1 ^long i2 ^long i3 ^long i4]]
    (Vec4. (p i1) (p i2) (p i3) (p i4)))
  (reciprocal [_] (Vec4. (/ 1.0 x) (/ 1.0 y) (/ 1.0 z) (/ 1.0 w)))
  (interpolate [_ v2 t f]
    (let [^Vec4 v2 v2] (Vec4. (f x (.x v2) t)
                              (f y (.y v2) t)
                              (f z (.z v2) t)
                              (f w (.w v2) t))))
  (interpolate [v1 v2 t] (interpolate v1 v2 t m/lerp))
  (einterpolate [_ v2 v f]
    (let [^Vec4 v2 v2
          ^Vec4 v v]
      (Vec4. (f x (.x v2) (.x v))
             (f y (.y v2) (.y v))
             (f z (.z v2) (.z v))
             (f w (.w v2) (.w v)))))
  (einterpolate [v1 v2 v] (einterpolate v1 v2 v m/lerp))
  (econstrain [_ val1 val2] (Vec4. (m/constrain x val1 val2)
                                   (m/constrain y val1 val2)
                                   (m/constrain z val1 val2)
                                   (m/constrain w val1 val2)))
  (is-zero? [_] (and (zero? x) (zero? y) (zero? z) (zero? w)))
  (is-near-zero? [_] (and (near-zero? x) (near-zero? y) (near-zero? z) (near-zero? w)))
  (heading [v1] (angle-between v1 (Vec4. 1 0 0 0))))

;; ## Common vector functions

(defn ediv
  "Elementwise division"
  [v1 v2]
  (emult v1 (reciprocal v2)))

(defn average-vectors
  "Average / centroid of vectors. Input: initial vector (optional), list of vectors"
  ([init vs]
   (div (reduce add init vs) (count vs)))
  ([vs] (average-vectors (first vs) (rest vs))))

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

(defn dist-canberra
  "Canberra distance"
  [v1 v2]
  (let [num (abs (sub v1 v2))
        denom (applyf (add (abs v1) (abs v2)) #(if (zero? ^double %) 0.0 (/ 1.0 ^double %)))]
    (sum (emult num denom))))

(defn dist-emd
  "Earth Mover's Distance"
  [v1 v2]
  (first (reduce #(let [[^double s ^double l] %1
                        [^double a ^double b] %2
                        n (- (+ a l) b)]
                    [(+ s (m/abs l)) n]) [0.0 0.0] (map vector v1 v2))))

;; List of distance fn
(def distances {:euclid dist
                :euclid-sq dist-sq
                :abs dist-abs
                :cheb dist-cheb
                :canberra dist-canberra
                :emd dist-emd})

(defn normalize
  "Normalize vector"
  [v]
  (let [^double m (mag v)]
    (if (zero? m)
      (Vec2. 0.0 0.0)
      (div v m))))

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
    (let [^double d (dot v1 v2)
          amt (/ d (* ^double (mag v1) ^double (mag v2)))]
      (cond
        (<= amt -1.0) m/PI
        (>= amt 1.0) 0
        :else (m/acos amt)))))

(defn aligned?
  "Are vectors aligned (have the same direction)?"
  [v1 v2]
  (< ^double (angle-between v1 v2) TOLERANCE))

(defn faceforward
  "Flip normal"
  [n v]
  (if (neg? ^double (dot n v))
    n
    (sub n)))

(defn generate-vec2
  "Generate Vec2 with fn(s)"
  ([f1 f2]
   (Vec2. (f1) (f2)))
  ([f]
   (Vec2. (f) (f))))

(defn generate-vec3
  "Generate Vec3 with fn(s)"
  ([f1 f2 f3]
   (Vec3. (f1) (f2) (f3)))
  ([f]
   (Vec3. (f) (f) (f))))

(defn generate-vec4
  "Generate Vec4 with fn(s)"
  ([f1 f2 f3 f4]
   (Vec4. (f1) (f2) (f3) (f4)))
  ([f]
   (Vec4. (f) (f) (f) (f))))

(defn array->vec2
  "Doubles array to Vec2"
  [^doubles a]
  (Vec2. (aget a 0) (aget a 1)))

(defn array->vec3
  "Doubles array to Vec3"
  [^doubles a]
  (Vec3. (aget a 0) (aget a 1) (aget a 2)))

(defn array->vec4
  "Doubles array to Vec4"
  [^doubles a]
  (Vec4. (aget a 0) (aget a 1) (aget a 2) (aget a 3)))

(defn vec4
  "Make Vec4 vector"
  ([x y z w] (Vec4. x y z w))
  ([^Vec3 v w] (Vec4. (.x v) (.y v) (.z v) w))
  ([^Vec2 v z w] (Vec4. (.x v) (.y v) z w)))

(defn vec3
  "Make Vec2 vector"
  ([x y z] (Vec3. x y z))
  ([^Vec2 v z] (Vec3. (.x v) (.y v) z)))

(defn vec2
  "Make Vec2 vector"
  [x y] (Vec2. x y))
