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

(defn op-scale
  ""
  [f s]
  (fn [^Vec3 p]
    (let [^Vec2 r (f (v/div p s))]
      (Vec2. (* (.x r) s) (.y r)))))

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
  (let [^Vec3 cw (v/normalize (v/sub ch ro))
        cp (Vec3. (m/sin rot) (m/cos rot) 0.0)
        ^Vec3 cu (v/normalize (v/cross cw cp))
        ^Vec3 cv (v/normalize (v/cross cu cw))
        row1 (Vec3. (.x cu) (.x cv) (.x cw))
        row2 (Vec3. (.y cu) (.y cv) (.y cw))
        row3 (Vec3. (.z cu) (.z cv) (.z cw))]
    (fn [point]
      (Vec3. (v/dot row1 point)
             (v/dot row2 point)
             (v/dot row3 point)))))

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
  [k steps max-depth]
  (fn [f ^Vec3 pos ^Vec3 light]
    (loop [i (int 0)
           res 1.0
           t 0.01]
      (let [^Vec3 r (ray pos light t)
            ^Vec2 sh (f r)
            h (max 0.0 (.x sh))
            newres (min res (/ (* k h) t))]
        (if (or (< h 0.001)
                (> t max-depth)
                (> i steps))
          (m/constrain (if (< h 0.001) 0.0 newres) 0.0 1.0)
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

;; lighting

(def specular-exp 8.0)
(def specular-exp-f (/ (+ 2.0 specular-exp) 8.0))

(defn make-light
  ""
  [^Vec3 ldir dstr astr sstr]
  (fn [^Vec3 color shadow ^Vec3 snormal ^Vec3 rdir]
    (let [ndotl (max 0.0 (v/dot snormal ldir))
          half-vec (v/normalize (v/sub ldir rdir))
          diffuse (* ndotl dstr)
          ambient (* astr (max 0.0 (v/dot (v/sub snormal) rdir)))
          hdotn (max 0.0 (v/dot snormal half-vec))
          fn (* ndotl (+ specular-exp (* (- 1.0 specular-exp) (m/pow (- 1.0 hdotn) 5.0))))
          specular (* sstr (* specular-exp-f (m/pow hdotn specular-exp) fn))

          diffuse (* diffuse shadow)
          ambient (* ambient shadow)
          specular (if (< shadow 1.0) 0.0 specular)]
      (v/emult color (v/add (Vec3. specular specular specular) (v/add (Vec3. diffuse diffuse diffuse) (Vec3. ambient ambient ambient)))))))


(defn make-light2
  ""
  [L f0 pows diff-color spec-color astr dstr sstr]
  (let [f0 (m/sq (/ (- 1.0 pows) (inc pows)))
        ZERO (Vec3. 0.0 0.0 0.0)
        diff-f (/ (- 1.0 f0) m/PI)
        spec-f (+ 0.0856832 (* 0.0397436 pows))]
    (fn [^Vec3 color shadow ^Vec3 N ^Vec3 E]
      (let [NL (v/dot N L)
            H (v/normalize (v/sub L E))
            NH (v/dot N H)
            EH (v/dot E H)
            NE (v/dot N E)
            m (/ 1.0 (max NL NE))
            Ff0 (+ f0 (* (- 1.0 f0) (m/pow (- 1.0 EH) 5.0)))

            diffuse (v/mult diff-color (* dstr diff-f))
            specular (v/mult spec-color (* sstr spec-f (* Ff0 m (m/pow NH pows))))
            ambient (v/mult color astr)

            diffuse (v/interpolate ZERO diffuse shadow)
            specular (if (< shadow 1.0) ZERO specular)
            ambient (v/interpolate ZERO ambient shadow)]
        (v/emult color (v/add ambient (v/add diffuse specular)))))))

;; ray marching

(defn make-ray-marching
  "depth field ray marching
  tmin - starting distance > 0.005
  tmax - final distance
  steps - how many steps
  stepf - step factor (0.1-1.0)
  precision - how close is enough (0.0001 - 0.01)
  return Vec3, where
  - x - distance
  - y - material value
  - z - fake AO based on steps made"
  ([tmin tmax steps stepf precision]
   (fn [f ro rd]
     (loop [t (min tmin 0.001)
            i (int 0)
            m 0.0]
       (let [r (ray ro rd t)
             ^Vec2 d (f r)
             dist (if (zero? i) (* (.x d) (m/drand 0.3 0.7)) (.x d))]
         (if (or (> i steps)
                 (< dist precision)
                 (> t tmax))
           (Vec3. t (if (> t tmax) -1.0 m) (- 1.0 (/ (double i) steps)))
           (recur (+ t (* stepf dist))
                  (unchecked-inc i)
                  (.y d)))))))
  ([tmin tmax steps]
   (make-ray-marching tmin tmax steps 1.0 0.0001)))
