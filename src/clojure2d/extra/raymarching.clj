;; http://www.iquilezles.org/www/index.htm
;; http://mercury.sexy/hg_sdf/
;; cheap&dirty ray marching helper library

(ns clojure2d.extra.raymarching
  (:require [clojure2d.math :as m]
            [clojure2d.math.vector :as v])
  (:import [clojure2d.math.vector Vec2 Vec3]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; signed distances

(defmulti make-primitive (fn [n matid conf] n))

(defmethod make-primitive :sphere [_ matid conf]
  (let [s (:r conf)]
    (fn [p]
      (Vec2. (- (v/mag p) s) matid))))

(defmethod make-primitive :plane [_ matid conf]
  (let [^Vec3 n (v/normalize (:normal conf))
        dist (:dist-from-origin conf)]
    (fn [p]
      (Vec2. (+ dist (v/dot p n)) matid))))

(def ^Vec3 vzero (Vec3. 0.0 0.0 0.0))

(defmethod make-primitive :box [_ matid conf]
  (let [^Vec3 b (:box conf)]
    (fn [p]
      (let [^Vec3 d (v/sub (v/abs p) b)]
        (Vec2. (+ (v/mag (v/emx d vzero)) (min 0.0 (v/mx d))) matid)))))

(defmethod make-primitive :torus [_ matid conf]
  (let [small-radius (:small-radius conf)
        large-radius (:large-radius conf)]
    (fn [^Vec3 p]
      (let [l1 (m/hypot (.x p) (.z p))
            l2 (m/hypot (- l1 large-radius) (.y p))]
        (Vec2. (- l2 small-radius) matid)))))

;; operations

(defn op-interpolate
  ""
  ([f1 f2 amount lerp]
   (fn [^Vec3 p]
     (let [^Vec2 r1 (f1 p)
           ^Vec2 r2 (f2 p)]
       (Vec2. (lerp (.x r1) (.x r2) amount)
              (lerp (.y r1) (.y r2) amount)))))
  ([f1 f2 amount]
   (op-interpolate f1 f2 amount m/lerp))
  ([f1 f2]
   (op-interpolate f1 f2 0.5)))

(defn op-rotate
  ""
  [f ^Vec3 axis angle]
  (let [^Vec3 ax (v/normalize axis)
        ^Vec3 sa (v/mult ax (m/sin angle))
        cosa (m/cos angle)
        ^Vec3 cb (v/mult ax (- 1.0 cosa))]
    (fn [^Vec3 p]
      (let [nx (+ (* (.x p) (+ (* (.x ax) (.x cb)) cosa))
                  (* (.y p) (- (* (.x ax) (.y cb)) (.z sa)))
                  (* (.z p) (+ (* (.x ax) (.z cb)) (.y sa))))
            ny (+ (* (.x p) (+ (* (.y ax) (.x cb)) (.z sa)))
                  (* (.y p) (+ (* (.y ax) (.y cb)) cosa))
                  (* (.z p) (- (* (.y ax) (.z cb)) (.x sa))))
            nz (+ (* (.x p) (- (* (.z ax) (.x cb)) (.y sa)))
                  (* (.y p) (+ (* (.z ax) (.y cb)) (.x sa)))
                  (* (.z p) (+ (* (.z ax) (.z cb)) cosa)))]
        (f (Vec3. nx ny nz))))))


(defn op-transform
  ""
  [f ^Vec3 t]
  (fn [^Vec3 p]
    (f (v/sub p t))))

(defn op-union
  ""
  ([f1 f2]
   (fn [^Vec3 p]
     (let [^Vec2 r1 (f1 p)
           ^Vec2 r2 (f2 p)]
       (if (< (.x r1) (.x r2)) r1 r2))))
  ([fs]
   (reduce #(op-union %2 %1) fs)))

(defn op-subtract
  ""
  [f1 f2]
  (fn [^Vec3 p]
    (let [^Vec2 r1 (f1 p)
          ^Vec2 r2 (f2 p)
          vr2 (- (.x r2))]
      (if (> vr2 (.x r1))
        (Vec2. vr2 (.y r2))
        r1))))

(defn op-intersect
  ""
  [f1 f2]
    (fn [^Vec3 p]
    (let [^Vec2 r1 (f1 p)
          ^Vec2 r2 (f2 p)]
      (if (> (.x r1) (.x r2))
        r1
        r2))))

(defn op-repeat
  ""
  [f ^Vec3 c]
  (fn [^Vec3 p]
    (let [q (Vec3. (mod (.x p) (.x c))
                   (mod (.y p) (.y c))
                   (mod (.z p) (.z c)))
          q (v/sub q (v/mult c 0.5))]
      (f q))))

;; camera

(defn make-camera
  "create camera function.
  * ro - ray origin
  * ch - camera heading
  * rot - camera rotation
  returns function which should be used to determine ray direction, based on pixel position"
  [ro ch rot]
  (let [cw (v/normalize (v/sub ch ro))
        cp (Vec3. (m/sin rot) (m/cos rot) 0.0)
        cu (v/normalize (v/cross cw cp))
        cv (v/normalize (v/cross cu cw))]
    (fn [point]
      (Vec3. (v/dot cu point)
             (v/dot cv point)
             (v/dot cw point)))))

;; normal

(defn normal
  ""
  [eps f p]
  (let [epsx (Vec3. eps 0.0 0.0)
        epsy (Vec3. 0.0 eps 0.0)
        epsz (Vec3. 0.0 0.0 eps)
        ^Vec2 fx- (f (v/sub p epsx))
        ^Vec2 fx+ (f (v/add p epsx))
        ^Vec2 fy- (f (v/sub p epsy))
        ^Vec2 fy+ (f (v/add p epsy))
        ^Vec2 fz- (f (v/sub p epsz))
        ^Vec2 fz+ (f (v/add p epsz))]
    (v/normalize (Vec3. (- (.x fx+) (.x fx-))
                        (- (.x fy+) (.x fy-))
                        (- (.x fz+) (.x fz-))))))

(defn make-normal
  ""
  [eps]
  (let [epsx (Vec3. eps 0.0 0.0)
        epsy (Vec3. 0.0 eps 0.0)
        epsz (Vec3. 0.0 0.0 eps)]
    (fn [f p]
      (let [^Vec2 fx- (f (v/sub p epsx))
            ^Vec2 fx+ (f (v/add p epsx))
            ^Vec2 fy- (f (v/sub p epsy))
            ^Vec2 fy+ (f (v/add p epsy))
            ^Vec2 fz- (f (v/sub p epsz))
            ^Vec2 fz+ (f (v/add p epsz))]
        (v/normalize (Vec3. (- (.x fx+) (.x fx-))
                            (- (.x fy+) (.x fy-))
                            (- (.x fz+) (.x fz-))))))))

;; ray

(defn ray
  ""
  [ro rd t]
  (v/add ro (v/mult rd t)))

;; ambient occlusion / global illumination
;; http://www.pouet.net/topic.php?which=6675

(defn make-ao
  "f - distance field
   p - current position
   n - normal 
   k - scaling factor (0.01-1.0)
   falloff - 0.5-0.95
   steps - how many steps (2-10)"
  [k falloff steps]
  (fn [f p n]
    (loop [i (int 0)
           occ 0.0
           pw 1.0]
      (let [fac (+ 0.01 (* i k))
            ^Vec2 d (f (ray p n fac))]
        (if (< i steps)
          (recur (unchecked-inc i)
                 (->> pw
                      (* (- fac (.x d)))
                      (+ occ))
                 (* pw falloff))
          (m/constrain (- 1.0 occ) 0.0 1.0))))))

;; fog
(defn make-distance-fog
  ""
  [fcol factor]
  (fn [t col]
    (let [ft (- 1.0 (m/exp (* factor t t)))]
      (v/interpolate col fcol ft))))

;; light

(defn make-soft-shadow
  ""
  [k steps]
  (fn [f ^Vec3 pos ^Vec3 light]
    (loop [i (int 0)
           res 1.0
           t 0.01]
      (let [^Vec3 r (ray pos light t)
            ^Vec2 sh (f r)
            h (max 0.0 (.x sh))
            newres (min res (/ (* k h) t))]
        (if (or (< h 0.001)
                (> i steps))
          (m/constrain newres 0.0 1.0)
          (recur (unchecked-inc i)
                 newres
                 (+ t (m/constrain h 0.02 0.5))))))))

;; other functions

(defn reflect
  ""
  [I N]
  (->> I
       (v/dot N)
       (* 2.0)
       (v/mult N)
       (v/sub I)))

;; ray marching

(defn make-ray-marching
  "depth field ray marching
  tmin - starting distance > 0.005
  tmax - final distance
  steps - how many steps
  stepf - step factor (0.1-1.0)
  precision - how close is enough (0.0001 - 0.01)"
  ([tmin tmax steps stepf precision]
   (fn [f ro rd]
     (loop [t (min tmin 0.005)
            i (int 0)
            m 0.0]
       (let [r (ray ro rd t)
             ^Vec2 d (f r)]
         (if (or (> i steps)
                 (< (.x d) precision)
                 (> t tmax))
           (Vec2. t (if (> t tmax) 0.0 m))
           (recur (+ t (* stepf (.x d)))
                  (unchecked-inc i)
                  (.y d)))))))
  ([tmin tmax steps]
   (make-ray-marching tmin tmax steps 0.95 0.001)))
