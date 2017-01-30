;; http://www.iquilezles.org/www/index.htm
;; http://mercury.sexy/hg_sdf/
;; cheap&dirty ray marching helper library

(ns clojure2d.extra.raymarching
  (:require [clojure2d.math :as m]
            [clojure2d.math.vector :as v])
  (:import [clojure2d.math.vector Vec2 Vec3]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def ^Vec3 vzero (Vec3. 0.0 0.0 0.0))

(deftype Material [^Vec3 color ^double diffusion ^double specular ^double specularf0 ^double specularpow ^double reflection])
(deftype HitData [^double d ^Material mat])

(defn make-material
  "Create material information:
  - color - Vec3 with RGB: 0.0-1.0
  - diffusion - diffusion ratio: 0.0 - 1.0
  - specular - specular ratio: 0.0 - 1.0
  - specularf0 - f0, intensity ratio: 0.0 - 1.0
  - specularpow - strength of specular: 1.0 - 10k
  - reflection - reflection ratio: 0.0 - 1.0"
  [conf]
  (Material. (:color conf)
             (:diffusion conf)
             (:specular conf)
             (:specularf0 conf)
             (:specularpow conf)
             (:reflection conf)))


(defn interpolate-material
  ""
  ([^Material m1 ^Material m2 amt f]
   (Material. (v/interpolate (.color m1) (.color m2) amt f)
              (f (.diffusion m1) (.diffusion m2) amt)
              (f (.specular m1) (.specular m2) amt)
              (f (.specularf0 m1) (.specularf0 m2) amt)
              (f (.specularpow m1) (.specularpow m2) amt)
              (f (.reflection m1) (.reflection m2) amt)))
  ([m1 m2 amt]
   (interpolate-material m1 m2 amt m/lerp)))

;; signed distances

(defmulti make-primitive (fn [n mat conf] n))

(defmethod make-primitive :sphere [_ mat conf]
  (let [^double s (:r conf)]
    (fn [p]
      (HitData. (- ^double (v/mag p) s) mat))))

(defmethod make-primitive :plane [_ mat conf]
  (let [n (v/normalize (:normal conf))
        ^double dist (:dist-from-origin conf)]
    (fn [p]
      (HitData. (+ dist ^double (v/dot p n)) mat))))

(defmethod make-primitive :box [_ mat conf]
  (let [b (:box conf)]
    (fn [p]
      (let [d (v/sub (v/abs p) b)]
        (HitData. (+ ^double (v/mag (v/emx d vzero)) (min 0.0 ^double (v/mx d))) mat)))))

(defmethod make-primitive :torus [_ mat conf]
  (let [^double small-radius (:small-radius conf)
        ^double large-radius (:large-radius conf)]
    (fn [^Vec3 p]
      (let [l1 (m/hypot (.x p) (.z p))
            l2 (m/hypot (- l1 large-radius) (.y p))]
        (HitData. (- l2 small-radius) mat)))))

;; operations

(defn op-interpolate
  ""
  ([f1 f2 amount lerp]
   (fn [p]
     (let [^HitData r1 (f1 p)
           ^HitData r2 (f2 p)]
       (HitData. (lerp (.d r1) (.d r2) amount)
                 (interpolate-material (.mat r1) (.mat r2) amount lerp)))))
  ([f1 f2 amount]
   (op-interpolate f1 f2 amount m/lerp))
  ([f1 f2]
   (op-interpolate f1 f2 0.5 m/lerp)))

(defn op-rotate
  ""
  [f axis angle]
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
  [f ^double s]
  (fn [p]
    (let [^HitData r (f (v/div p s))]
      (HitData. (* (.d r) s) (.mat r)))))

(defn op-transform
  ""
  [f t]
  (fn [p]
    (f (v/sub p t))))

(defn op-union
  ""
  ([f1 f2]
   (fn [p]
     (let [^HitData r1 (f1 p)
           ^HitData r2 (f2 p)]
       (if (< (.d r1) (.d r2)) r1 r2))))
  ([fs]
   (reduce #(op-union %2 %1) fs)))

(defn op-blend
  ""
  ([f1 f2 ^double k]
   (let [rk (/ 0.5 k)]
     (fn [p]
       (let [^HitData r1 (f1 p)
             ^HitData r2 (f2 p)
             h (m/constrain (+ 0.5 (* rk (- (.d r2) (.d r1)))) 0.0 1.0)
             fac (* k h (- 1.0 h))
             v (- (m/lerp (.d r2) (.d r1) h) fac)
             m (interpolate-material (.mat r2) (.mat r1) h m/lerp)]
         (HitData. v m)))))
  ([fs k]
   (reduce #(op-blend %2 %1 k) fs)))

(defn op-subtract
  ""
  [f1 f2]
  (fn [p]
    (let [^HitData r1 (f1 p)
          ^HitData r2 (f2 p)
          vr2 (- (.d r2))]
      (if (> vr2 (.d r1))
        (HitData. vr2 (.mat r2))
        r1))))

(defn op-intersect
  ""
  [f1 f2]
    (fn [p]
    (let [^HitData r1 (f1 p)
          ^HitData r2 (f2 p)]
      (if (> (.d r1) (.d r2))
        r1
        r2))))

;; still wrong...
(defn op-repeat
  ""
  [f ^Vec3 c]
  (fn [^Vec3 p]
    (let [q (Vec3. (mod (.x p) (.x c))
                   (mod (.y p) (.y c))
                   (mod (.z p) (.z c)))
          q (v/sub q (v/mult c 0.5))]
      (f q))))

(defn op-displace
  ""
  [f dispf scalein scaleout]
  (fn [p]
    (f (v/add p (v/mult (dispf (v/mult p scalein)) scaleout)))))

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
        ^HitData fx- (f (v/sub p epsx))
        ^HitData fx+ (f (v/add p epsx))
        ^HitData fy- (f (v/sub p epsy))
        ^HitData fy+ (f (v/add p epsy))
        ^HitData fz- (f (v/sub p epsz))
        ^HitData fz+ (f (v/add p epsz))]
    (v/normalize (Vec3. (- (.d fx+) (.d fx-))
                        (- (.d fy+) (.d fy-))
                        (- (.d fz+) (.d fz-))))))

(defn make-normal
  ""
  [f eps]
  (let [epsx (Vec3. eps 0.0 0.0)
        epsy (Vec3. 0.0 eps 0.0)
        epsz (Vec3. 0.0 0.0 eps)]
    (fn [p]
      (let [^HitData fx- (f (v/sub p epsx))
            ^HitData fx+ (f (v/add p epsx))
            ^HitData fy- (f (v/sub p epsy))
            ^HitData fy+ (f (v/add p epsy))
            ^HitData fz- (f (v/sub p epsz))
            ^HitData fz+ (f (v/add p epsz))]
        (v/normalize (Vec3. (- (.d fx+) (.d fx-))
                            (- (.d fy+) (.d fy-))
                            (- (.d fz+) (.d fz-))))))))

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
  [^double k ^double falloff ^long steps]
  (fn ^double [f p n]
    (loop [i (int 0)
           occ 0.0
           pw 1.0]
      (let [ik (* i k)
            sh (if (odd? i) ik (- ik))
            yshift (v/mult (Vec3. sh ik (- sh)) 0.2) ;; vary a normal a little bit
            fac (+ 0.01 ik)
            ^HitData d (f (ray p (v/add n yshift) fac))]
        (if (< i steps)
          (recur (unchecked-inc i)
                 (->> pw
                      (* (- fac (.d d)))
                      (+ occ))
                 (* pw falloff))
          (m/constrain (- 1.0 occ) 0.0 1.0))))))

;; fog
(defn make-distance-fog
  ""
  [fcol ^double factor]
  (fn ^Vec3 [^double t col]
    (let [ft (- 1.0 (m/exp (* factor t t)))]
      (v/interpolate col fcol ft))))

;; shadow

(defn make-soft-shadow
  ""
  [^double k ^long steps ^double max-depth]
  (fn [f pos light]
    (loop [i (int 0)
           res 1.0
           t 0.01]
      (let [r (ray pos light t)
            ^HitData sh (f r)
            h (.d sh)
            newres (min res (/ (* k h) t))]
        (if (or (< h 0.001)
                (> t max-depth)
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
       ^double (v/dot N)
       (* 2.0)
       (v/mult N)
       (v/sub I)
       (v/normalize)))

;; ray marching

(defn make-ray-marching
  "depth field ray marching
  tmin - starting distance > 0.005
  tmax - final distance
  steps - how many steps
  stepf - step factor (0.1-1.0)
  precision - how close is enough (0.0001 - 0.01)
  background - background material
  returns vector, where index:
  - 0 - HitData
  - 1 - fake AO based on steps made"
  ([scene background tmin tmax steps stepf precision]
   (let [^double tmin tmin
         ^double tmax tmax
         ^long steps steps
         ^double stepf stepf
         ^double precision precision]
     (fn [ro rd]
       (loop [t (min tmin 0.001)
              i (int 0)
              m background]
         (let [r (ray ro rd t)
               ^HitData d (scene r)
               dist (if (zero? i) (* (.d d) 0.3) (.d d))]
           (if (or (> i steps)
                   (< dist precision)
                   (> t tmax))
             [(HitData. t (if (> t tmax) background m)) (- 1.0 (/ (double i) steps))]
             (recur (+ t (* stepf dist))
                    (unchecked-inc i)
                    (.mat d))))))))
  ([scene background tmin tmax steps]
   (make-ray-marching scene background tmin tmax steps 1.0 0.00001)))


;; light
;; ad-hoc Blinn-Phong model
;; http://renderwonk.com/publications/s2010-shading-course/gotanda/course_note_practical_implementation_at_triace.pdf
(defn make-light
  ""
  [L scene diff-color spec-color astr]
  (fn [^Material mat shadow-f N D pos]
    (let [shadow (shadow-f scene ^Vec3 pos L)
          E (v/sub D)
          NL (max 0.0 ^double (v/dot N L))
          H (v/normalize (v/add L E))
          NH (max 0.0 ^double (v/dot N H))
          EH (max 0.0 ^double (v/dot E H))
          Ff0 (+ (.specularf0 mat) (* (- 1.0 (.specularf0 mat)) (m/pow (- 1.0 EH) 5.0)))

          diffuse (v/mult diff-color (* (.diffusion mat) NL))
          specular (v/mult spec-color (* (.specular mat) (* Ff0 NL (m/pow NH (.specularpow mat)))))
          ambient (v/mult (.color mat) astr)

          diffuse (v/mult diffuse shadow)
          specular (v/mult specular (m/pow shadow 8))
          ambient (v/mult ambient shadow)]
      (v/emult (.color mat) (v/add ambient (v/add diffuse specular))))))

