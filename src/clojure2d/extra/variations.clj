;; ## Variations
;;
;; Vec2 -> Vec2 are 2d vector fields, functions used in fractal flames rendering as variations.  
;; Code is taken from [jWildfire](https://github.com/tsulej/JWildfire/tree/master/src/org/jwildfire/create/tina/variation).
;;
;;  More on this:
;;
;; * [Folds explained](https://generateme.wordpress.com/2016/04/11/folds/)
;; * [Folds page](http://folds2d.tumblr.com/)
;; * examples/ex08-folds
;;
;; List of currently implemented variations is in the `variation-list`
;;
(ns clojure2d.extra.variations
  "Variations namespace"
  (:require [fastmath.core :as m]
            [fastmath.complex :as c]
            [fastmath.random :refer :all]
            [fastmath.vector :as v])
  (:import [fastmath.vector Vec2]
           [org.apache.commons.math3.special BesselJ Beta Erf Gamma]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

;; Every variation consist of variation configuration and creator function.
;;
;; Configuration is a map with named values passed to variation creator
;; Map can be empty or nil if you want random parametrization or when function don't have parametrization.
;;
;; To create configuration call `make-configuration :variation-name configuration-map`. configuration-map is optional, when you skip it you'll get random configuration.  
;; For example:
;;
;; `(make-configuration :auger)`  
;; `=> {:freq 1.9878973615162039, :weight 0.21075539658278108, :sym 1.7967705799271059, :scale -0.6747741927127615}`
;; 
;; `(make-configuration :auger {:freq 2.0 :sym 1.5})`  
;; `=> {:freq 2.0, :weight 0.4927899160909188, :sym 1.5, :scale -0.6046162305689989}`
;;
;; To see what parameters are used in given variation just call `(make-configuration :variation-name)` or refer configuration fn definition.
;; Function returns map with full configuration necessary to create variation. In case when some of the keys are missing, random values are created.
;;
;; Note: this function is called implicitly when you create variation with `make-variation`. Call it explicitly when you want to record or reuse configuration.
(defmulti make-configuration (fn [key & _] key))
(defmethod make-configuration :default
  ([_ _] {})
  ([_] {}))

;; To obtain variation function with given configuration you have to call `make-variation` multimethod.
;;
;; `make-variation` accepts three attributes:
;;
;; * function name (as a key, see the list above)
;; * scale value, used to scale result (optional, default 1.0)
;; * default configuration map (optional, default random configuration)
;;
;; `(def auger-1 (make-variation :auger))`
;; `(auger-1 (Vec2. 1.0 1.0))`  
;; `=> #clojure2d.utils.vector.Vec2{:x 1.108066895814639, :y 1.1378531686602176}`
(defmulti make-variation (fn [key & _] key))
(defmethod make-variation :default
  ([_ s _] (fn [v] (v/mult v s)))
  ([_ s] (fn [v] (v/mult v s)))
  ([_] identity))

;; Two following macros help to generate proper `make-configuration` and `make-variation` multimethods for given variation name. They are used internally.
(defmacro make-config-method
  "Add new multimethod for variation configuration"
  [sym m]
  (let [k (keyword sym)]
    `(defmethod make-configuration ~k
       ([k# p#] (merge ~m p#))
       ([k#] ~m))))

(defmacro make-var-method
  "Add new multimethod for variation factory function and store variation fn in global list."
  [sym t]
  (let [k (keyword sym)
        m (symbol (str "make-" sym))]
    `(do (defmethod make-variation ~k
           ([k# a# p#] (~m a# (make-configuration ~k p#)))
           ([k# a#] (~m a# (make-configuration ~k)))
           ([k#] (~m 1.0 (make-configuration ~k))))
         ~(if (= t :regular)
            `(register-regular-var ~k)
            `(register-random-var ~k)))))

;; Locally used random function for some configuration parameters. Mostly used to avoid `0` value.
(defn- srandom
  "Symetric random from [-mx -mn] and [mn mx]"
  ^double  [^double mn ^double mx]
  (let [rand (drand mn mx)]
    (randval rand (* -1.0 rand))))

;; Local noise generating functions (for various noise implementations)

(defn- make-noise-variation
  "Calculate shift by angle taken from noise"
  [^double amount scale noise-fn]
  (fn [v]
    (let [^Vec2 vv (v/mult v scale)
          angle (* m/TWO_PI ^double (noise-fn (.x vv) (.y vv)))]
      (v/add v (Vec2. (* amount (m/cos angle))
                      (* amount (m/sin angle)))))))

(defn- make-noise-variation2
  "Calculate shift by vector taken from noise"
  [^double amount scale noise-fn]
  (fn [v]
    (let [^Vec2 vv (v/mult v scale)
          x1 (- ^double (noise-fn (.x vv) (.y vv)) 0.5)
          y1 (- ^double (noise-fn (.y vv) m/E (.x vv)) 0.5)]
      (v/add v (Vec2. (* amount x1)
                      (* amount y1))))))

(def ^Vec2 unitx (Vec2. 1.0 0.0))
(def ^Vec2 zerov (Vec2. 0.0 0.0))

;; Two atoms to store variation names. One for non-random functions and second for random.

(def ^:private regular-var (atom [:default]))
(def ^:private random-var (atom []))

(defn- register-var
  "Add `name` to the atom `what`"
  [what name]
  (swap! what conj name))

(def register-random-var (partial register-var random-var))
(def register-regular-var (partial register-var regular-var))

;; ## A

;; ### Arch

(defn make-arch
  "Arch"
  [^double amount _]
  (fn [^Vec2 v]
    (let [ang (* amount (drand m/PI))
          sinr (m/sin ang)
          cosr (m/cos ang)]
      (if (zero? cosr) zerov
          (Vec2. (* amount sinr)
                 (* amount (/ (m/sq sinr) cosr)))))))
(make-var-method arch :random)

;; ### Asteria
(make-config-method asteria {:alpha (drand -5 5)})

(defn make-asteria
  "asteria by dark-beam, http://jwildfire.org/forum/viewtopic.php?f=23&t=1464"
  [^double amount {:keys [^double alpha]}]
  (let [sina (m/sin (* m/PI alpha))
        cosa (m/cos (* m/PI alpha))]
    (fn [^Vec2 v]
      (let [^Vec2 v0 (v/mult v amount)
            ^double r (v/magsq v0)
            xx (m/sq (dec (m/abs (.x v0))))
            yy (m/sq (dec (m/abs (.y v0))))
            r2 (m/sqrt (+ xx yy))
            in (< r 1.0)
            in1 (if (and (< r 1.0) (< r2 1.0))
                  (brand 0.65)
                  (not in))]
        (if in1
          v0
          (let [xx (- (* cosa (.x v0))
                      (* sina (.y v0)))
                yy (+ (* sina (.x v0))
                      (* cosa (.y v0)))
                nx (* (/ xx (m/sqrt (- 1.0 (* yy yy))))
                      (- 1.0 (m/sqrt (- 1.0 (m/sq (inc (- (m/abs yy))))))))]
            (Vec2. (+ (* cosa nx)
                      (* sina yy))
                   (+ (- (* sina nx))
                      (* cosa yy)))))))))
(make-var-method asteria :random)

;; ### Atan

(make-config-method atan {:mode (lrand 3)
                          :stretch (srandom 0.01 2.0)})

(defn make-atan
  ""
  [^double amount {:keys [^long mode ^double stretch]}]
  (let [m (m/constrain mode 0 2)
        norm (/ 1.0 (* m/M_PI_2 amount))]
    (fn [^Vec2 v]
      (case m
        0 (Vec2. (.x v)
                 (* norm (m/atan (* stretch (.y v)))))
        1 (Vec2. (* norm (m/atan (* stretch (.x v))))
                 (.y v))
        (Vec2. (* norm (m/atan (* stretch (.x v))))
               (* norm (m/atan (* stretch (.y v)))))))))
(make-var-method atan :regular)

;; ### Auger

(make-config-method auger {:freq (drand -5 5)
                           :weight (drand -1 1)
                           :sym (drand -2 2)
                           :scale (srandom 0.5 2)})

(defn make-auger
  "Auger by Xyrus02"
  [^double amount {:keys [^double freq ^double weight ^double sym ^double scale]}]
  (fn [^Vec2 v]
    (let [x (.x v)
          y (.y v)
          s (m/sin (* freq x))
          t (m/sin (* freq y))
          dy (+ y (* weight (+ (m/abs y) (* 0.5 s scale)) s))
          dx (+ x (* weight (+ (m/abs x) (* 0.5 t scale)) t))
          xx (* amount (+ x (* sym (- dx x))))
          yy (* amount dy)]
      (Vec2. xx yy))))
(make-var-method auger :regular)

;; ## B

(defn- bseries-calc
  "Common calculations for bSeries "
  [^double amount ^double tau ^double sigma]
  (let [sinht (m/sinh tau)
        cosht (m/cosh tau)
        sins (m/sin sigma)
        coss (m/cos sigma)
        temp (- cosht coss)]
    (if (zero? temp)
      (Vec2. 0.0 0.0)
      (Vec2. (* amount (/ sinht temp))
             (* amount (/ sins temp))))))

;; ### bCollide

(make-config-method bcollide {:num (srandom 1 30)
                              :a (drand 2)})

(defn make-bcollide
  "bCollide by Michael Faber, http://michaelfaber.deviantart.com/art/bSeries-320574477"
  [^double amount {:keys [^double num ^double a]}]
  (let [bcn-pi (* num m/M_1_PI)
        pi-bcn (/ m/PI num)
        bca-bcn (/ (* m/PI a) num)]
    (fn [^Vec2 v]
      (let [v+ (v/add v unitx)
            v- (Vec2. (- 1.0 (.x v)) (.y v))
            tau (* 0.5 (- (m/log (v/magsq v+))
                          (m/log (v/magsq v-))))
            pre-sigma (- m/PI ^double (v/heading v+) ^double (v/heading v-))
            alt (double (int (* pre-sigma bcn-pi)))
            sigma (if (even? (int alt))
                    (+ (* alt pi-bcn) (rem (+ pre-sigma bca-bcn) pi-bcn))
                    (+ (* alt pi-bcn) (rem (- pre-sigma bca-bcn) pi-bcn)))]
        (bseries-calc amount tau sigma)))))
(make-var-method bcollide :regular)

;; ### bMod

(make-config-method bmod {:radius (drand 0.5 2.0)
                          :distance (drand 2.0)})

(defn make-bmod
  "bMod by Michael Faber, http://michaelfaber.deviantart.com/art/bSeries-320574477"
  [^double amount {:keys [^double radius ^double distance]}]
  (let [rd (* radius distance)
        r2 (+ radius radius)]
    (fn [^Vec2 v]
      (let [v+ (v/add v unitx)
            v- (Vec2. (- 1.0 (.x v)) (.y v))
            pre-tau (* 0.5 (- (m/log (v/magsq v+))
                              (m/log (v/magsq v-))))
            sigma (- m/PI ^double (v/heading v+) ^double (v/heading v-))
            tau (if (and (< pre-tau radius) (< (- pre-tau) radius))
                  (- (rem (+ pre-tau radius rd) r2) radius)
                  pre-tau)]
        (bseries-calc amount tau sigma)))))
(make-var-method bmod :regular)

;; ### bSplit

(make-config-method bsplit {:x (drand -2.0 2.0)
                            :y (drand -2.0 2.0)})

(defn make-bsplit
  "Raykoid666, transcribed and modded by Nic Anderson, chronologicaldot"
  [^double amount {:keys [^double x ^double y]}]
  (fn [^Vec2 v]
    (let [xx (+ x (.x v))]      
      (Vec2. (* (/ amount (m/tan xx)) (m/cos (+ y (.y v))))
             (* (/ amount (m/sin xx)) (- y (.y v)))))))
(make-var-method bsplit :regular)

;; ### bSwirl

(make-config-method bswirl {:in (drand -2.0 2.0)
                            :out (drand -2.0 2.0)})

(defn make-bswirl
  "bSwirl by Michael Faber, http://michaelfaber.deviantart.com/art/bSeries-320574477"
  [^double amount {:keys [^double in ^double out]}]
  (fn [^Vec2 v]
    (let [v+ (v/add v unitx)
          v- (Vec2. (- 1.0 (.x v)) (.y v))
          tau (* 0.5 (- (m/log (v/magsq v+))
                        (m/log (v/magsq v-))))
          pre-sigma (- m/PI ^double (v/heading v+) ^double (v/heading v-))
          sigma (+ pre-sigma (* tau out) (/ in tau))]
      (bseries-calc amount tau sigma))))
(make-var-method bswirl :regular)

;; ### bTransform

(make-config-method btransform {:rotate (drand m/TWO_PI)
                                :power (randval 0.5 (drand 10.0) (irand 10.0))
                                :move (drand -2.0 2.0)
                                :split (drand -2.0 2.0)})

(defn make-btransform
  "bTransform by Michael Faber, http://michaelfaber.deviantart.com/art/bSeries-320574477"
  [^double amount {:keys [^double rotate ^double power ^double move ^double split]}]
  (let [mp (/ m/TWO_PI power)]
    (fn [^Vec2 v]
      (let [v+ (v/add v unitx)
            v- (Vec2. (- 1.0 (.x v)) (.y v))
            pre-tau (+ (/ (* 0.5 (- (m/log (v/magsq v+))
                                    (m/log (v/magsq v-)))) power) move)
            pre-sigma (+ (- m/PI ^double (v/heading v+) ^double (v/heading v-)) rotate)
            tau (if (neg? (.x v))
                  (- pre-tau split)
                  (+ pre-tau split))
            sigma (+ (/ pre-sigma power)
                     (* mp (m/floor (* power (drand)))))]
        (bseries-calc amount tau sigma)))))
(make-var-method btransform :random)

;; ### BWraps7

(make-config-method bwraps7 {:cellsize (srandom 0.5 2.0)
                             :space (drand -1.0 1.0)
                             :gain (drand -2.0 2.0)
                             :inner-twist (drand -2.0 2.0)
                             :outer-twist (drand -2.0 2.0)})

(defn make-bwraps7
  "http://slobo777.deviantart.com/art/Bubble-Wrap-WIP-Plugin-112370125"
  [^double amount {:keys [^double cellsize ^double space ^double gain ^double inner-twist ^double outer-twist]}]
  (let [radius (* 0.5 (/ cellsize (inc (m/sq space))))
        g2 (+ m/EPSILON (* gain gain))
        max-bubble- (* g2 radius)
        max-bubble (if (> max-bubble- 2.0)
                     1.0
                     (* max-bubble- (/ 1.0 (inc (* 0.25 (m/sq max-bubble-))))))
        r2 (m/sq radius)
        rfactor (/ radius max-bubble)]
    (fn [^Vec2 v]
      (if (< (m/abs cellsize) m/EPSILON)
        (v/mult v amount)
        (let [^Vec2 C (-> v
                          (v/div cellsize)
                          (v/applyf m/floor)
                          (v/add (Vec2. 0.5 0.5))
                          (v/mult cellsize))
              L (v/sub v C)]
          (if (> ^double (v/magsq L) r2)
            (v/mult v amount)
            (let [L (v/mult L g2)
                  r (/ rfactor (inc (* 0.25 ^double (v/magsq L))))
                  ^Vec2 L (v/mult L r)
                  r (/ ^double (v/magsq L) r2)
                  theta (+ (* inner-twist (- 1.0 r))
                           (* outer-twist r))
                  s (m/sin theta)
                  c (m/cos theta)
                  vx (+ (.x C) (* c (.x L)) (* s (.y L)))
                  vy (+ (.y C) (* -1.0 s (.x L)) (* c (.y L)))]
              (v/mult (Vec2. vx vy) amount))))))))
(make-var-method bwraps7 :regular)

;; ### Barycentroid

(make-config-method barycentroid {:a (drand -2.0 2.0)
                                  :b (drand -2.0 2.0)
                                  :c (drand -2.0 2.0)
                                  :d (drand -2.0 2.0)})

(defn make-barycentroid
  "barycentroid from Xyrus02, http://xyrusworx.deviantart.com/art/Barycentroid-Plugin-144832371?q=sort%3Atime+favby%3Amistywisp&qo=0&offset=10"
  [^double amount {:keys [^double a ^double b ^double c ^double d]}]
  (let [dot00 (+ (* a a) (* b b))
        dot01 (+ (* a c) (* b d))
        dot11 (+ (* c c) (* d d))
        inv-denom (/ 1.0 (- (* dot00 dot11) (* dot01 dot01)))]
    (fn [^Vec2 v]
      (let [dot02 (+ (* a (.x v)) (* b (.y v)))
            dot12 (+ (* c (.x v)) (* d (.y v)))
            u (* inv-denom (- (* dot11 dot02) (* dot01 dot12)))
            vv (* inv-denom (- (* dot00 dot12) (* dot01 dot02)))
            um (* (m/signum u) (m/sqrt (+ (* u u) (m/sq (.x v)))))
            vm (* (m/signum vv) (m/sqrt (+ (* vv vv) (m/sq (.y v)))))]
        (Vec2. (* amount um)
               (* amount vm))))))
(make-var-method barycentroid :regular)

;; ### Bent

(defn make-bent
  "Bent"
  [^double amount _]
  (fn [^Vec2 v]
    (let [nx (if (neg? (.x v)) (+ (.x v) (.x v)) (.x v))
          ny (if (neg? (.y v)) (* (.y v) 0.5) (.y v))]
      (Vec2. (* amount nx)
             (* amount ny)))))
(make-var-method bent :regular)

(make-config-method bent2 {:x (srandom 0.5 2.0)
                           :y (srandom 0.5 2.0)})

(defn make-bent2
  "Bent2"
  [^double amount {:keys [^double x ^double y]}]
  (fn [^Vec2 v]
    (let [nx (if (neg? (.x v)) (* (.x v) x) (.x v))
          ny (if (neg? (.y v)) (* (.y v) y) (.y v))]
      (Vec2. (* amount nx)
             (* amount ny)))))
(make-var-method bent2 :regular)

;; ### Bipolar

(make-config-method bipolar {:shift (drand -2.0 2.0)})

(defn make-bipolar
  "Bipolar"
  [^double amount {:keys [^double shift]}]
  (let [ps (* (- m/HALF_PI) shift)]
    (fn [^Vec2 v]
      (let [^double x2y2 (v/magsq v)
            t (inc x2y2)
            x2 (+ (.x v) (.x v))
            pre-y (+ ps (* 0.5 (m/atan2 (+ (.y v) (.y v))
                                        (dec x2y2))))
            y (if (> pre-y m/HALF_PI)
                (- (rem (+ pre-y m/HALF_PI) m/PI) m/HALF_PI)
                (if (< pre-y (- m/HALF_PI))
                  (- m/HALF_PI (rem (- m/HALF_PI pre-y) m/PI))
                  pre-y))
            f (+ t x2)
            g (- t x2)]
        (if (or (zero? g)
                (not (pos? (/ f g))))
          (Vec2. 0.0 0.0)
          (Vec2. (* amount m/M_2_PI 0.25 (m/log (/ f g)))
                 (* amount m/M_2_PI y)))))))
(make-var-method bipolar :regular)

;; ### Blade

(defn make-blade
  "Blade"
  [^double amount _]
  (fn [^Vec2 v]
    (let [r (* (drand amount) ^double (v/mag v))
          sinr (m/sin r)
          cosr (m/cos r)]
      (Vec2. (* amount (.x v) (+ cosr sinr))
             (* amount (.x v) (- cosr sinr))))))
(make-var-method blade :random)

(defn make-blade2
  "Blade2"
  [^double amount _]
  (fn [^Vec2 v]
    (let [r (* (drand amount) ^double (v/mag v))
          sinr (m/sin r)
          cosr (m/cos r)]
      (Vec2. (* amount (.x v) (+ cosr sinr))
             (* amount (.y v) (- cosr sinr))))))
(make-var-method blade2 :random)

;; ### Blob

(make-config-method blob {:low (drand -2.0 2.0)
                          :high (drand -2.0 2.0)
                          :waves (drand -6.0 6.0)})

(defn make-blob
  "Blob"
  [^double amount {:keys [^double low ^double high ^double waves]}]
  (let [hl (- high low)]
    (fn [^Vec2 v]
      (let [^double a (v/heading v)
            ^double r (v/mag v)
            rr (->> (* a waves)
                    (m/sin)
                    (* 0.5)
                    (+ 0.5)
                    (* hl)
                    (+ low)
                    (* r))]
        (Vec2. (* amount rr (m/sin a))
               (* amount rr (m/cos a)))))))
(make-var-method blob :regular)

;; ### Blocky

(make-config-method blocky {:x (srandom 0.5 1.5)
                            :y (srandom 0.5 1.5)
                            :mp (srandom 0.001 6.0)})

(defn make-blocky
  "blocky from FracFx, http://fracfx.deviantart.com/art/FracFx-Plugin-Pack-171806681"
  [^double amount {:keys [^double x ^double y ^double mp]}]
  (let [vv (/ amount m/HALF_PI)]
    (fn [^Vec2 v]
      (let [T (inc (/ (+ (m/cos (.x v)) (m/cos (.y v))) mp))
            r (/ amount T)
            tmp (inc ^double (v/magsq v))
            x2 (+ (.x v) (.x v))
            y2 (+ (.y v) (.y v))
            xmax (* 0.5 (+ (m/sqrt (+ tmp x2)) (m/sqrt (- tmp x2))))
            ymax (* 0.5 (+ (m/sqrt (+ tmp y2)) (m/sqrt (- tmp y2))))
            ax (/ (.x v) xmax)
            bx (m/safe-sqrt (- 1.0 (m/sq ax)))
            ay (/ (.y v) ymax)
            by (m/safe-sqrt (- 1.0 (m/sq ay)))]
        (Vec2. (* vv (m/atan2 ax bx) r x)
               (* vv (m/atan2 ay by) r y))))))
(make-var-method blocky :regular)

;; ### Blur Circle

(defn make-blurcircle
  "Blur circle"
  [^double amount _]
  (fn [^Vec2 v]
    (let [x (drand -1.0 1.0)
          y (drand -1.0 1.0)
          absx (m/abs x)
          absy (m/abs y)
          ^Vec2 ps (if (>= absx absy)
                     (Vec2. (if (>= x absy)
                              (+ absx y)
                              (- (* 5.0 absx) y)) absx)
                     (Vec2. (if (>= y absx)
                              (- (* 3.0 absy) x)
                              (+ (* 7.0 absy) x)) absy))
          r (* amount (.y ps))
          a (-> m/M_PI_4
                (* (.x ps))
                (/ (.y ps))
                (- m/M_PI_4))
          sa (m/sin a)
          ca (m/cos a)]
      (Vec2. (* r ca) (* r sa)))))
(make-var-method blurcircle :random)

;; ### Blur
(defn make-blur
  "Blur"
  [^double amount _]
  (fn [^Vec2 v]
    (let [r (drand m/TWO_PI)
          sr (m/sin r)
          cr (m/cos r)
          r2 (drand amount)]
      (Vec2. (* r2 cr) (* r2 sr)))))
(make-var-method blur :random)

;; ### Blur pixelize

(make-config-method blurpixelize {:size (srandom 0.01 1.2)
                                  :scale (drand -1.2 1.2)})

(defn make-blurpixelize
  "Blur Pixelize from Apo7X15C"
  [^double amount {:keys [^double size ^double scale]}]
  (let [inv-size (/ 1.0 size)
        av (* amount size)
        half (Vec2. 0.5 0.5)]
    (fn [v]
      (-> v
          (v/mult inv-size)
          (v/applyf m/floor)
          (v/add (-> (v/generate-vec2 drand)
                     (v/sub half)
                     (v/mult scale)))
          (v/add half)
          (v/mult av)))))
(make-var-method blurpixelize :random)

;; ### BlurZoom

(make-config-method blurzoom {:length (drand -1.2 1.2)
                              :x (drand -1.2 1.2)
                              :y (drand -1.2 1.2)})

(defn make-blurzoom
  "Blur Zoom from Apo7X15C"
  [^double amount {:keys [^double length ^double x ^double y]}]
  (let [xy (Vec2. x y)
        xy- (Vec2. x (- y))]
    (fn [v]
      (-> v
          (v/sub xy)
          (v/mult (inc (drand length)))
          (v/add xy-)
          (v/mult amount)))))
(make-var-method blurzoom :random)

;; ### Boarders

(defn make-boarders
  "Boarders"
  [^double amount _]
  (fn [^Vec2 v]
    (let [roundx (m/rint (.x v))
          roundy (m/rint (.y v))
          offsetx (- (.x v) roundx)
          offsety (- (.y v) roundy)
          hoffsetx (* 0.5 offsetx)
          hoffsety (* 0.5 offsety)]
      (randval 0.75
               (Vec2. (* amount (+ roundx hoffsetx))
                      (* amount (+ roundy hoffsety)))
               (if (>= (m/abs offsetx) (m/abs offsety))
                 
                 (if (>= offsetx 0.0)
                   (Vec2. (* amount (+ hoffsetx roundx 0.25))
                          (* amount (+ hoffsety roundy (/ (* 0.25 offsety) offsetx))))
                   (Vec2. (* amount (- (+ hoffsetx roundx) 0.25))
                          (* amount (- (+ hoffsety roundy) (/ (* 0.25 offsety) offsetx)))))
                 
                 (if (>= offsety 0.0)
                   (Vec2. (* amount (+ hoffsetx roundx (/ (* 0.25 offsetx) offsety)))
                          (* amount (+ hoffsety roundy 0.25)))
                   (Vec2. (* amount (- (+ hoffsetx roundx) (/ (* 0.25 offsetx) offsety)))
                          (* amount (- (+ hoffsety roundy) 0.25)))))))))
(make-var-method boarders :random)

(make-config-method boarders2 {:c (drand -1.2 1.2)
                               :left (drand -1.2 1.2)
                               :right (drand -1.2 1.2)})

(defn make-boarders2
  "Boarders"
  [^double amount {:keys [^double c ^double left ^double right]}]
  (let [cc (m/abs c)
        cl (m/abs left)
        cr (m/abs right)
        cc (if (zero? cc) m/EPSILON cc)
        cl (if (zero? cl) m/EPSILON cl)
        cr (if (zero? cr) m/EPSILON cr)
        cl (* c cl)
        cr (+ c (* c cr))]
    (fn [^Vec2 v]
      (let [roundx (m/rint (.x v))
            roundy (m/rint (.y v))
            offsetx (- (.x v) roundx)
            offsety (- (.y v) roundy)
            coffsetx (* c offsetx)
            coffsety (* c offsety)]
        (randval cr
                 (Vec2. (* amount (+ roundx coffsetx))
                        (* amount (+ roundy coffsety)))
                 (if (>= (m/abs offsetx) (m/abs offsety))
                   
                   (if (>= offsetx 0.0)
                     (Vec2. (* amount (+ coffsetx roundx cl))
                            (* amount (+ coffsety roundy (/ (* cl offsety) offsetx))))
                     (Vec2. (* amount (- (+ coffsetx roundx) cl))
                            (* amount (- (+ coffsety roundy) (/ (* cl offsety) offsetx)))))
                   
                   (if (>= offsety 0.0)
                     (Vec2. (* amount (+ coffsetx roundx (/ (* cl offsetx) offsety)))
                            (* amount (+ coffsety roundy cl)))
                     (Vec2. (* amount (- (+ coffsetx roundx) (/ (* cl offsetx) offsety)))
                            (* amount (- (+ coffsety roundy) cl))))))))))
(make-var-method boarders2 :random)

;; ### Bubble

(defn make-bubble
  "Bubble"
  [^double amount _]
  (fn [v]
    (v/mult v (/ amount (inc (* 0.25 ^double (v/mag v)))))))
(make-var-method bubble :regular)

;; ### Butterfly

(defn make-butterfly
  "Butterfly"
  [^double amount _]
  (let [wx (* amount 1.3029400317411197908970256609023)]
    (fn [^Vec2 v]
      (let [y2 (* 2.0 (.y v))
            r (* wx (m/sqrt (/ (m/abs (* (.y v) (.x v)))
                             (+ m/EPSILON (m/sq (.x v)) (m/sq y2)))))]
        (Vec2. (* r (.x v))
               (* r y2))))))
(make-var-method butterfly :regular)

;; ### BesselJ

(defn make-besselj
  "Bessel"
  [^double amount _]
  (fn [^Vec2 v]
    (Vec2. (* amount ^double (v/mag v) (BesselJ/value (m/abs (.x v)) (m/abs (.y v))))
           (* amount ^double (v/heading v)))))
(make-var-method besselj :regular)

;; ### Beta

(defn make-beta
  "Beta"
  [^double amount _]
  (fn [^Vec2 v]
    (Vec2. (* amount (Beta/logBeta (+ m/EPSILON (m/abs (.x v))) (+ m/EPSILON (m/abs (.y v)))))
           (* amount ^double (v/heading v)))))
(make-var-method beta :regular)

;; ## C

;; ### CPow3

(make-config-method cpow3 {:r (drand -2 2)
                           :a (drand -2 2)
                           :divisor (srandom 0.1 2)
                           :spread (srandom 0.1 2)
                           :discrete-spread (srandom 0.1 2)
                           :spread2 (drand -2 2)
                           :offset2 (drand -2 2)})

(defn make-cpow3
  "CPow3"
  [^double amount {:keys [^double r ^double a ^double divisor ^double spread ^double discrete-spread ^double spread2 ^double offset2]}]
  (let [ang (/ m/TWO_PI divisor)
        c (/ (* r (m/cos (* m/HALF_PI a))) divisor)
        d (/ (* r (m/sin (* m/HALF_PI a))) divisor)
        half-c (* 0.5 c)
        half-d (* 0.5 d)
        inv-spread (/ 0.5 spread)
        full-spread (* m/TWO_PI spread)
        fac (* c half-d ang)]
    (fn [^Vec2 v]
      (let [^double ai (v/heading v)
            n (drand spread)
            n (double (if (>= discrete-spread 1.0) (int n) n))
            n (if (neg? ai) (inc n) n)
            ai (+ ai (* m/TWO_PI n))
            ai (if (< (m/cos (* ai inv-spread)) (drand -1.0 1.0)) (- ai full-spread) ai)
            lnr2 (m/log (v/magsq v))
            ri (* amount (m/exp (- (* half-c lnr2) (* d ai))))
            ang2 (* fac ai lnr2 (+ (* spread2 (drand)) offset2))]
        (Vec2. (* ri (m/cos ang2))
               (* ri (m/sin ang2)))))))
(make-var-method cpow3 :random)

;; ### CPow

(make-config-method cpow {:r (drand -2 2)
                          :i (drand -2 2)
                          :power (srandom 0.1 12)})

(defn make-cpow
  "CPow"
  [^double amount {:keys [^double r ^double i ^double power]}]
  (let [va (/ m/TWO_PI power)
        vc (/ r power)
        vd (/ i power)]
    (fn [v]
      (let [^double a (v/heading v)
            lnr (* 0.5 (m/log (v/magsq v)))
            ang (+ (* a vc)
                   (* vd lnr)
                   (* va (m/floor (drand power))))
            m (* amount (m/exp (- (* vc lnr)
                                  (* vd a))))]
        (Vec2. (* m (m/cos ang)) (* m (m/sin ang)))))))
(make-var-method cpow :random)

;; ### Cell

(make-config-method cell {:size (srandom 0.1 2.0)})

(defn make-cell
  "Cell"
  [^double amount {:keys [^double size]}]
  (let [inv-cell-size (/ 1.0 size)
        av (Vec2. amount (- amount))]
    (fn [v]
      (let [^Vec2 xy (-> v
                         (v/mult inv-cell-size)
                         (v/applyf m/floor))
            dxy (-> v
                    (v/sub (v/mult xy size)))
            newxy (if-not (neg? (.y xy))
                    (if-not (neg? (.x xy))
                      (v/mult xy 2.0)
                      (Vec2. (- (inc (* 2.0 (.x xy))))
                             (* 2.0 (.y xy))))
                    (if-not (neg? (.x xy))
                      (Vec2. (* 2.0 (.x xy))
                             (- (inc (* 2.0 (.y xy)))))
                      (Vec2. (- (inc (* 2.0 (.x xy))))
                             (- (inc (* 2.0 (.y xy)))))))]
        (-> newxy
            (v/mult size)
            (v/add dxy)
            (v/emult av))))))
(make-var-method cell :regular)

;; ### Checks

(make-config-method checks {:x (drand -10 10)
                            :y (drand -10 10)
                            :size (drand -2.0 2.0)
                            :rnd (randval 0.0 (srandom 0.1 1))})

(defn make-checks
  "Checks"
  [^double amount {:keys [^double x ^double y ^double size ^double rnd]}]
  (let [cs (/ 1.0 (+ size m/EPSILON))
        ncx (* -1.0 x)
        ncy (* -1.0 y)]
    (fn [^Vec2 v]
      (let [isxy (+ (m/round (* cs (.x v)))
                    (m/round (* cs (.y v))))
            dxy (if (even? isxy)
                  (Vec2. (+ ncx (drand rnd)) ncy)
                  (Vec2. x (+ y (drand rnd))))]
        (-> v
            (v/add dxy)
            (v/mult amount))))))
(make-var-method checks :random)

;; ### Chunk

(make-config-method chunk {:a (drand -1.2 1.2)
                           :b (drand -1.2 1.2)
                           :c (drand -1.2 1.2)
                           :d (drand -1.2 1.2)
                           :e (drand -1.2 1.2)
                           :f (drand -1.2 1.2)
                           :mode (brand)})

(defn make-chunk
  "Chunk, by zephyrtronium https://zephyrtronium.deviantart.com/art/Chunk-Apophysis-Plugin-Pack-182375397"
  [^double amount {:keys [^double a ^double b ^double c ^double d ^double e ^double f mode]}]
  (fn [^Vec2 v]
    (let [r (+ (* a (m/sq (.x v)))
               (* b (.x v) (.y v))
               (* c (m/sq (.y v)))
               (* d (.x v))
               (* e (.y v))
               f)]
      (if mode
        (if-not (pos? r) v zerov)
        (if (pos? r) v zerov)))))
(make-var-method chunk :regular)

;; ### Circle blur

(defn make-circleblur
  "Circle blur"
  [^double amount _]
  (fn [_]
    (let [rad (m/sqrt (drand))
          a (drand m/TWO_PI)]
      (Vec2. (* amount (m/cos a) rad)
             (* amount (m/sin a) rad)))))
(make-var-method circleblur :random)

;; ### CircleRand

(make-config-method circlerand {:Sc (srandom 0.1 1.2)
                                :Dens (drand 1)
                                :X (drand -20 20)
                                :Y (drand -20 20)
                                :Seed (irand Integer/MAX_VALUE)})

(defn make-circlerand
  "Circle Rand http://eralex61.deviantart.com/art/Circles-Plugins-126273412"
  [^double amount {:keys [^double Sc ^double Dens ^double X ^double Y ^double Seed]}]
  (let [xy (Vec2. X Y)] 
    (fn [v]
      (loop [iter (int 0)]
        (let [XY (-> (v/generate-vec2 #(drand -1.0 1.0))
                     (v/emult xy))
              ^Vec2 MN (-> XY
                           (v/mult 0.5)
                           (v/div Sc)
                           (v/applyf m/floor))
              XY (v/sub XY (-> MN
                               (v/mult 2.0)
                               (v/applyf #(inc ^double %))
                               (v/mult Sc)))]
          (if (and (< iter 100)
                   (or (> (discrete-noise (+ Seed (.x MN)) (.y MN)) Dens)
                       (> ^double (v/mag XY) (-> (discrete-noise (+ 10.0 (.x MN)) (+ 3.0 (.y MN)))
                                                 (* 0.7)
                                                 (+ 0.3)
                                                 (* Sc)))))
            (recur (inc iter))
            (-> MN
                (v/mult 2.0)
                (v/applyf #(inc ^double %))
                (v/mult Sc)
                (v/add XY)
                (v/mult amount))))))))
(make-var-method circlerand :random)

;; ### CircleLinear

(make-config-method circlelinear {:Sc (drand 1)
                                  :K (drand -2 2)
                                  :Dens1 (drand 1)
                                  :Dens2 (drand 1)
                                  :Reverse (drand -1 1)
                                  :X (drand 20)
                                  :Y (drand 20)
                                  :Seed (irand Integer/MAX_VALUE)})

(defn make-circlelinear
  "CircleLinear by eralex, http://eralex61.deviantart.com/art/Circles-Plugins-126273412"
  [^double amount {:keys [^double Sc ^double K ^double Dens1 ^double Dens2 ^double Reverse ^double X ^double Y ^double Seed]}]
  (let [dd (* Dens1 Dens2)]
    (fn [^Vec2 v]
      (let [M (->> Sc
                   (/ (.x v))
                   (* 0.5)
                   (m/floor))
            N (->> Sc
                   (/ (.y v))
                   (* 0.5)
                   (m/floor))
            X (- (.x v) (->> M
                             (* 2.0)
                             (inc)
                             (* Sc)))
            Y (- (.y v) (->> N
                             (* 2.0)
                             (inc)
                             (* Sc)))
            U (m/hypot X Y)
            V (->> (discrete-noise (+ M 10.0) (+ N 3.0))
                   (* 0.7)
                   (+ 0.3)
                   (* Sc))
            Z1 (discrete-noise (+ M Seed) N)
            ^Vec2 XY (if (and (< Z1 Dens1) (< U V)) 
                       (if (pos? Reverse)
                         (if (< Z1 dd)
                           (Vec2. (* K X) (* K Y))
                           (let [Z (->> K
                                        (- 1.0)
                                        (* U)
                                        (/ V)
                                        (+ K))]
                             (Vec2. (* Z X) (* Z Y))))
                         (if (> Z1 dd)
                           (Vec2. (* K X) (* K Y))
                           (let [Z (->> K
                                        (- 1.0)
                                        (* U)
                                        (/ V)
                                        (+ K))]
                             (Vec2. (* Z X) (* Z Y)))))
                       (Vec2. X Y))]
        (Vec2. (->> 2.0
                    (* M)
                    (inc)
                    (* Sc)
                    (+ (.x XY))
                    (* amount))
               (->> 2.0
                    (* N)
                    (inc)
                    (* Sc)
                    (+ (.y XY))
                    (* amount)))))))
(make-var-method circlelinear :regular)

;; ### Clifford

(make-config-method clifford {:a (drand -2 2)
                              :b (drand -2 2)
                              :c (drand -2 2)
                              :d (drand -2 2)})

(defn make-clifford
  "Clifford Pickover attractor"
  [^double amount {:keys [^double a ^double b ^double c ^double d]}]
  (fn [^Vec2 v]
    (v/mult (Vec2. (+ (m/sin (* a (.y v))) (* c (m/cos (* a (.x v)))))
                   (+ (m/sin (* b (.x v))) (* d (m/cos (* b (.y v)))))) amount)))
(make-var-method clifford :regular)

;; ### Conic

(make-config-method conic {:eccentricity (drand -3 3)
                           :holes (drand -3 3)})

(defn make-conic
  "Conc"
  [^double amount {:keys [^double eccentricity ^double holes]}]
  (fn [^Vec2 v]
    (let [magr (/ 1.0 (+ ^double (v/mag v) m/EPSILON))
          ct (* (.x v) magr)
          r (* (/ (* (* amount (- (drand) holes)) eccentricity) (inc (* eccentricity ct))) magr)]
      (v/mult v r))))
(make-var-method conic :random)

;; ### Cot

(defn make-cot
  "Cot"
  [^double amount _]
  (fn [^Vec2 v]
    (let [cotsin (m/sin (* 2.0 (.x v)))
          cotcos (m/cos (* 2.0 (.x v)))
          cotsinh (m/sinh (* 2.0 (.y v)))
          cotcosh (m/cosh (* 2.0 (.y v)))
          cotden (/ (- cotcosh cotcos))]
      (Vec2. (* amount cotden cotsin)
             (* amount cotden -1.0 cotsinh)))))
(make-var-method cot :regular)

;; ### Cosine

(defn make-cosine
  "Cosine"
  [^double amount _]
  (fn [^Vec2 v]
    (let [r (* m/PI (.x v))]
      (Vec2. (* amount (m/cos r) (m/cosh (.y v)))
             (- (* amount (m/sin r) (m/sinh (.y v))))))))
(make-var-method cosine :regular)

;; ### Curl

(make-config-method curl {:c1 (drand -1 1)
                          :c2 (drand -1 1)})

(defn make-curl
  "Curl"
  [^double amount {:keys [^double c1 ^double c2]}]
  (fn [^Vec2 v]
    (let [re (inc (+ (* c1 (.x v)) (* c2 (- (m/sq (.x v)) (m/sq (.y v))))))
          im (+ (* c1 (.y v)) (* c2 2.0 (.x v) (.y v)))
          r (/ amount (+ (m/sq re) (m/sq im)))]
      (Vec2. (* r (+ (* (.x v) re) (* (.y v) im)))
             (* r (- (* (.y v) re) (* (.x v) im)))))))
(make-var-method curl :regular)

;; ### Curve

(make-config-method curve {:xamp (srandom 0.1 3)
                           :yamp (srandom 0.1 3)
                           :xlength (drand 0.1 1.5)
                           :ylength (drand 0.1 1.5)})

(defn make-curve
  "Curve"
  [amount {:keys [^double xamp ^double yamp ^double xlength ^double ylength]}]
  (let [pc-xlen (/ 1.0 (max m/EPSILON (m/sq xlength)))
        pc-ylen (/ 1.0 (max m/EPSILON (m/sq ylength)))
        len (Vec2. pc-xlen pc-ylen)
        amp (Vec2. xamp yamp)]
    (fn [^Vec2 v]
      (let [rv (Vec2. (.y v) (.x v))]
        (-> rv
            (v/emult rv)
            (v/mult -1.0)
            (v/emult len)
            (v/applyf #(m/exp %))
            (v/emult amp)
            (v/add v)
            (v/mult amount))))))
(make-var-method curve :regular)

;; ### Cross

(defn make-cross
  "Cross"
  [^double amount _]
  (fn [^Vec2 v]
    (let [s (- (m/sq (.x v)) (m/sq (.y v)))
          r (* amount (m/sqrt (/ 1.0 (+ m/EPSILON (* s s)))))]
      (Vec2. (* (.x v) r) (* (.y v) r)))))
(make-var-method cross :regular)

;; ### CSin

(make-config-method csin {:stretch (drand -3 3)})

(defn make-csin
  "CSin by zephyrtronium, http://fractal-resources.deviantart.com/art/CSin-Apophysis-Plugin-158332287"
  [^double amount {:keys [^double stretch]}]
  (let [s-cx (Vec2. stretch 0.0)]
    (fn [^Vec2 v]
      (v/mult (->> v
                   (c/mult s-cx)
                   (c/sin)) amount))))
(make-var-method csin :regular)

;; ### Cayley transform

(defn make-cayley
  "Cayley transform"
  [^double amount _]
  (fn [^Vec2 v]
    (if (== (.y v) -1.0)
      zerov
      (c/div (c/add v c/I-)
             (c/add v c/I)))))
(make-var-method cayley :regular)

;; ### Cylinder

(defn make-cylinder
  "Cylinder"
  [^double amount _]
  (fn [^Vec2 v]
    (v/mult (Vec2. (m/sin (.x v)) (.y v)) amount)))
(make-var-method cylinder :regular)

;; ## D

;; ### Diamond

(defn make-diamond
  "Diamond"
  [^double amount _]
  (fn [^Vec2 v]
    (let [^double length (v/mag v)
          sina (/ (.x v) length)
          cosa (/ (.y v) length)
          sinr (m/sin length)
          cosr (m/cos length)]
      (Vec2. (* amount sina cosr)
             (* amount cosa sinr)))))
(make-var-method diamond :regular)

;; ### Disc

(defn make-disc
  "Disc"
  [^double amount _]
  (let [api (/ amount m/PI)]
    (fn [^Vec2 v]
      (let [rpi (* m/PI ^double (v/mag v))
            sinr (m/sin rpi)
            cosr (m/cos rpi)
            r (* api ^double (v/heading v))]
        (Vec2. (* r sinr) (* r cosr))))))
(make-var-method disc :regular)

(make-config-method disc2 {:rot (drand -3 3)
                           :twist (drand -6 6)})

(defn make-disc2
  "Disc2"
  [^double amount {:keys [^double rot ^double twist]}]
  (let [timespi (* rot m/PI) 
        k1 (if (> twist m/TWO_PI) (- (inc twist) m/TWO_PI) 1.0)
        k2 (if (< twist (- m/TWO_PI)) (+ (inc twist) m/TWO_PI) 1.0) 
        sinadd (* (m/sin twist) k1 k2)
        cosadd (* (dec (m/cos twist)) k1 k2)]
    (fn [^Vec2 v]
      (let [t (* timespi (+ (.x v) (.y v)))
            sinr (m/sin t)
            cosr (m/cos t)
            r (/ (* amount ^double (v/heading v)) m/PI)]
        (Vec2. (* r (+ cosadd sinr))
               (* r (+ sinadd cosr)))))))
(make-var-method disc2 :regular)

;; ## E

;; ### eDisk

(defn make-edisc
  "edisc"
  [^double amount _]
  (let [w (/ amount 11.57034632)]
    (fn [^Vec2 v]
      (let [tmp (inc ^double (v/magsq v))
            tmp2 (* 2.0 (.x v))
            xmax (* 0.5 (+ (m/safe-sqrt (+ tmp tmp2))
                           (m/safe-sqrt (- tmp tmp2))))
            a1 (m/log (+ xmax (m/sqrt (dec xmax))))
            a2 (- (m/acos (/ (.x v) xmax)))
            snv (m/sin a1)
            snv (if (pos? (.y v)) (- snv) snv)]
        (Vec2. (* w (m/cosh a2) (m/cos a1))
               (* w (m/sinh a2) snv))))))
(make-var-method edisc :regular)


;; ### eMod

(make-config-method emod {:radius (drand 0.1 4)
                          :distance (drand 2)})

(defn make-emod
  "eMod by Michael Faber, http://michaelfaber.deviantart.com/art/eSeries-306044892"
  [^double amount {:keys [^double radius ^double distance]}]
  (let [radius2 (* 2.0 radius)
        rdr (+ radius (* distance radius))]
    (fn [^Vec2 v]
      (let [tmp (inc ^double (v/magsq v))
            tmp2 (* 2.0 (.x v))
            xmax-pre (* 0.5 (+ (m/safe-sqrt (+ tmp tmp2))
                               (m/safe-sqrt (- tmp tmp2))))
            xmax (m/constrain xmax-pre 1.0 xmax-pre)
            t (m/constrain (/ (.x v) xmax) -1.0 1.0)
            nu-pre (m/acos t)
            nu (if (neg? (.y v)) (* -1.0 nu-pre) nu-pre)
            mu-pre (m/acosh xmax)          
            mu (if (and (< mu-pre radius) (< (* -1.0 mu-pre) radius))
                 (if (pos? nu)
                   (- (rem (+ mu-pre rdr) radius2) radius)
                   (+ (rem (- mu-pre rdr) radius2) radius))
                 mu-pre)
            xx (* amount (m/cosh mu) (m/cos nu))
            yy (* amount (m/sinh mu) (m/sin nu))]
        (Vec2. xx yy)))))
(make-var-method emod :regular)

;; ### Ennepers

(defn make-ennepers
  ""
  [^double amount _]
  (fn [^Vec2 v]
    (let [sx (* (.x v) (.x v))
          sy (* (.y v) (.y v))
          x (+ (- (.x v) (* 0.3333333 sx (.x v))) (* (.x v) sy))
          y (+ (- (.y v) (* 0.3333333 sy (.y v))) (* (.y v) sx))]
      (Vec2. (* amount x) (* amount y)))))
(make-var-method ennepers :regular)

;; ### Erf

(defn make-erf
  "Erf"
  [^double amount _]
  (fn [^Vec2 v]
    (Vec2. (* amount (Erf/erf (.x v) (.y v)))
           (* amount ^double (v/heading v)))))
(make-var-method erf :regular)

;; ### Elliptic

(defn make-elliptic
  "Elliptic"
  [^double amount _]
  (let [-a (/ amount m/HALF_PI)]
    (fn [^Vec2 v]
      (let [tmp (inc ^double (v/magsq v))
            x2 (+ (.x v) (.x v))
            xmax (* 0.5 (+ (m/sqrt (+ tmp x2)) (m/sqrt (- tmp x2))))
            a (/ (.x v) xmax)
            b (m/safe-sqrt (- 1.0 (* a a)))
            l (m/log (+ xmax (m/safe-sqrt (dec xmax))))
            x (* -a (m/atan2 a b)) 
            y (randval (* -a l) (- (* -a l)))]
        (Vec2. x y)))))
(make-var-method elliptic :random)

;; ### Escher

(make-config-method escher {:beta (drand m/TWO_PI)})

(defn make-escher
  "Escher"
  [^double amount {:keys [beta]}]
  (let [seb (m/sin beta)
        ceb (m/cos beta)
        vc (* 0.5 (inc ceb))
        vd (* 0.5 seb)]
    (fn [^Vec2 v]
      (let [^double a (v/heading v)
            lnr (* 0.5 (m/log (v/magsq v)))
            m (* amount (m/exp (- (* vc lnr)
                                  (* vd a))))
            n (+ (* vc a)
                 (* vd lnr))]
        (Vec2. (* m (m/cos n))
               (* m (m/sin n)))))))
(make-var-method escher :regular)

;; ### Ex

(defn make-ex
  "Ex"
  [^double amount _]
  (fn [^Vec2 v]
    (let [^double r (v/mag v)
          ^double h (v/heading v)
          n0 (m/sin (+ h r))
          n1 (m/cos (- h r))
          m0 (* n0 n0 n0)
          m1 (* n1 n1 n1)
          ar (* amount r)]
      (Vec2. (* ar (+ m0 m1))
             (* ar (- m0 m1))))))
(make-var-method ex :regular)

;; ### Exp

(defn make-exp
  "Exp"
  [^double amount _]
  (fn [^Vec2 v]
    (let [e (* amount (m/exp (.x v)))]
      (Vec2. (* e (m/cos (.y v)))
             (* e (m/sin (.y v)))))))
(make-var-method exp :regular)

;; ### Exponential

(defn make-exponential
  "Exponential"
  [^double amount _]
  (fn [^Vec2 v]
    (let [e (* amount (m/exp (dec (.x v))))
          r (* m/PI (.y v))]
      (Vec2. (* e (m/cos r))
             (* e (m/sin r))))))
(make-var-method exponential :regular)

;; ### Eyefish

(defn make-eyefish
  "Eyefish"
  [^double amount _]
  (fn [^Vec2 v]
    (let [r (/ (* amount 4.0) (inc ^double (v/mag v)))]
      (Vec2. (* r (.x v)) (* r (.y v))))))
(make-var-method eyefish :regular)

;; ## F

;; ### Fan

(make-config-method fan {:coeff20 (drand -2.0 2.0)
                         :coeff21 (drand -2.0 2.0)})

(defn make-fan
  "Fan"
  [^double amount {:keys [^double coeff20 ^double coeff21]}]
  (let [dx (+ m/EPSILON (* m/PI (m/sq coeff20)))
        dx2 (* 0.5 dx)]
    (fn [^Vec2 v]
      (let [^double angle (v/heading v)
            r (* amount ^double (v/mag v))
            ac (+ angle coeff21)
            a (if (> ^double (mod ac dx) dx2)
                (- angle dx2)
                (+ angle dx2))]
        (Vec2. (* r (m/cos a))
               (* r (m/sin a)))))))
(make-var-method fan :regular)

(make-config-method fan2 {:x (drand -1 1)
                          :y (drand -1 1)})

(defn make-fan2
  "Fan2"
  [^double amount {:keys [^double x ^double y]}]
  (fn [^Vec2 v]
    (let [^double r (v/mag v)
          ^double angle (v/heading v)
          ac (+ angle y)
          dx (+ m/EPSILON (* m/PI x x))
          dx2 (* 0.5 dx)
          t (- ac (* dx (double (long (/ ac dx)))))
          a (if (> t dx2)
              (- angle dx2)
              (+ angle dx2))]
      (Vec2. (* amount r (m/sin a))
             (* amount r (m/cos a))))))
(make-var-method fan2 :regular)

;; ### Fisheye

(defn make-fisheye
  "Fisheye"
  [^double amount _]
  (fn [^Vec2 v]
    (let [r (/ (* amount 4.0) (inc ^double (v/mag v)))]
      (Vec2. (* r (.y v)) (* r (.x v))))))
(make-var-method fisheye :regular)

;; ### Flower

(make-config-method flower {:petals (drand -10 10)
                            :holes (drand -1 1)})

(defn make-flower
  "Flower"
  [^double amount {:keys [^double petals ^double holes]}]
  (fn [v]
    (let [^double theta (v/heading v)
          d (/ 1.0 (+ m/EPSILON ^double (v/mag v)))
          r (* amount (- (drand) holes) (m/cos (* petals theta)) d)]
      (v/mult v r))))
(make-var-method flower :random)

;; ### Foci

(defn make-foci
  "Foci"
  [^double amount _]
  (fn [^Vec2 v]
    (let [expx (* 0.5 (m/exp (.x v)))
          expnx (/ 0.25 expx)
          sy (m/sin (.y v))
          cy (m/cos (.y v))
          tmp (- (+ expx expnx) cy)
          tmp (/ amount (if (zero? tmp) m/EPSILON tmp))]
      (Vec2. (* tmp (- expx expnx))
             (* tmp sy)))))
(make-var-method foci :regular)

;; ## G

;; ### Gamma

(defn make-gamma
  "gamma by zephyrtronium, http://fractal-resources.deviantart.com/art/Gamma-Apophysis-Plugin-154808483"
  [^double amount _]
  (fn [^Vec2 v]
    (Vec2. (* amount (Gamma/logGamma (v/mag v)))
           (* amount ^double (v/heading v)))))
(make-var-method gamma :regular)

;; ### GaussianBlur

(defn make-gaussianblur
  "Gaussian"
  [^double amount _]
  (fn [v]
    (let [a (drand m/TWO_PI)
          r (* amount (+ (drand) (drand) (drand) (drand) -2.0))]
      (Vec2. (* r (m/cos a)) (* r (m/sin a))))))
(make-var-method gaussianblur :random)

;; ### GDOffs

(make-config-method gdoffs {:delta-x (drand -6 6)
                            :delta-y (drand -6 6)
                            :area-x (srandom 0.5 3)
                            :area-y (srandom 0.5 3)
                            :center-x (drand -1 1)
                            :center-y (drand -1 1)
                            :gamma (drand -5 5)
                            :square brand})

(def ^:const ^double agdod-- 0.1)
(def ^:const ^double agdoa-- 2.0)
(def ^:const ^double agdoc-- 1.0)

(defn- fclp ^double [^double a] (if (neg? a) (- (rem (m/abs a) 1.0)) (rem (m/abs a) 1.0)))
(defn- fscl ^double [^double a] (fclp (* 0.5 (inc a))))
(defn- fosc ^double [^double p ^double a] (fscl (- (m/cos (* p a m/TWO_PI)))))
(defn- flip ^double [^double a ^double b ^double c] (+ a (* c (- b a))))

(defn make-gdoffs
  "GDOffs"
  [^double amount {:keys [^double delta-x ^double delta-y ^double area-x ^double area-y ^double center-x ^double center-y ^double gamma square]}]
  (let [gdodx (* delta-x agdod--)
        gdody (* delta-y agdod--)
        gdoax (* agdoa-- (if (< (m/abs area-x) 0.1) 0.1 (m/abs area-x)))
        gdoay (* agdoa-- (if (< (m/abs area-y) 0.1) 0.1 (m/abs area-y)))
        gdocx (* center-x agdoc--)
        gdocy (* center-y agdoc--)
        gdog gamma
        gdos square
        gdob (/ (* gdog agdoa--) (max gdoax gdoay))]
    (fn [^Vec2 v]
      (let [osc-x (fosc gdodx 1.0)
            osc-y (if gdos (fosc gdody 1.0) 1.0)
            in-x (+ (.x v) gdocx)
            in-y (+ (.y v) gdocy)]
        (v/mult (if gdos
                  (Vec2. (flip (flip in-x (fosc in-x 4.0) osc-x) (fosc (fclp (* gdob in-x)) 4.0) osc-x)
                         (flip (flip in-y (fosc in-y 4.0) osc-x) (fosc (fclp (* gdob in-y)) 4.0) osc-x))
                  (Vec2. (flip (flip in-x (fosc in-x 4.0) osc-x) (fosc (fclp (* gdob in-x)) 4.0) osc-x)
                         (flip (flip in-y (fosc in-y 4.0) osc-y) (fosc (fclp (* gdob in-y)) 4.0) osc-y))) amount)))))
(make-var-method gdoffs :regular)

;; ## H
;;
;; ### Heart

(defn make-heart
  "Heart"
  [^double amount _]
  (fn [^Vec2 v]
    (let [^double r (v/mag v)
          ^double theta (v/heading v)
          rt (* r theta)
          sr (m/sin rt)
          cr (m/cos rt)]
      (Vec2. (* amount r sr) (- (* amount r cr))))))
(make-var-method heart :regular)

;; ### Handkerchief

(defn make-handkerchief
  "Handkerchief"
  [^double amount _]
  (fn [^Vec2 v]
    (let [^double angle (v/heading v)
          ^double r (v/mag v)]
      (Vec2. (* amount (* r (m/sin (+ angle r))))
             (* amount (* r (m/cos (- angle r))))))))
(make-var-method handkerchief :regular)

;; ### Hemisphere

(defn make-hemisphere
  "Hemisphere"
  [^double amount _]
  (fn [^Vec2 v]
    (let [r (/ amount (m/sqrt (inc ^double (v/magsq v))))]
      (Vec2. (* r (.x v))
             (* r (.y v))))))
(make-var-method hemisphere :regular)

;; ### Hole2

(make-config-method hole2 {:a (drand -2.0 2.0)
                           :b (drand -2.0 2.0)
                           :c (drand 3.0)
                           :d (drand -2.0 2.0)
                           :inside (brand)
                           :shape (irand 10)})

(defn make-hole2
  "Hole2"
  [^double amount {:keys [^double a ^double b ^double c ^double d inside shape]}]
  (fn [v]
    (let [^double rhosq (v/magsq v)
          theta (* d ^double (v/heading v))
          delta (* c (m/pow (inc (/ theta m/PI)) a))
          r (case (unchecked-int shape)
              1 (m/sqrt (+ rhosq delta))
              2 (m/sqrt (+ rhosq (m/sin (* b theta)) delta))
              3 (m/sqrt (+ rhosq (m/sin theta) delta))
              4 (m/sqrt (- (inc (+ rhosq (m/sin theta))) delta))
              5 (m/sqrt (+ rhosq (m/abs (m/tan theta)) delta))
              6 (m/sqrt (+ rhosq (inc (m/sin (* b theta))) delta))
              7 (m/sqrt (+ rhosq (m/abs (m/sin (* 0.5 b theta))) delta))
              8 (m/sqrt (+ rhosq (m/sin (* m/PI (m/sin (* b theta)))) delta))
              9 (m/sqrt (+ rhosq (* 0.5 (+ (m/sin (* b theta))
                                           (m/sin (+ m/M_PI_2 (* 2.0 b theta))))) delta))
              (+ delta (m/sqrt rhosq)))
          r1 (if inside
               (/ amount r)
               (* amount r))]
      (Vec2. (* r (m/cos theta))
             (* r (m/sin theta))))))
(make-var-method hole2 :regular)

;; ### Horseshoe

(defn make-horseshoe
  "Horseshoe"
  [^double amount _]
  (fn [^Vec2 v]
    (let [r (+ m/EPSILON ^double (v/mag v))
          sina (/ (.x v) r)
          cosa (/ (.y v) r)]
      (Vec2. (* amount (- (* sina (.x v)) (* cosa (.y v))))
             (* amount (+ (* cosa (.x v)) (* sina (.y v))))))))
(make-var-method horseshoe :regular)

;; ### Hyperbolic

(defn make-hyperbolic
  "Hyperbolic"
  [^double amount _]
  (fn [^Vec2 v]
    (let [r (+ m/EPSILON ^double (v/mag v))
          theta (v/heading v)]
      (Vec2. (/ (m/sin theta) r)
             (* (m/cos theta) r)))))
(make-var-method hyperbolic :regular)

;;; ### Hypershift

(make-config-method hypershift {:shift (drand -2.0 2.0)
                                :stretch (drand -2.0 2.0)})

(defn make-hypershift
  "Hypershift"
  [^double amount {:keys [^double shift ^double stretch]}]
  (let [scale (- 1.0 (* shift shift))]
    (fn [^Vec2 v]
      (let [rad (/ ^double (v/magsq v))
            x (+ shift (* rad (.x v)))
            y (* rad (.y v))
            r (/ (* amount scale) (+ (* x x) (* y y)))]
        (Vec2. (+ shift (* r x))
               (* r y stretch))))))
(make-var-method hypershift :regular)

;; ## I

;; ### InvTree

(defn make-invtree
  "InvTree"
  [^double amount _]
  (fn [^Vec2 v]
    (cond
      (brand 0.333) (v/mult v (* 0.5 amount))
      (brand 0.666) (v/mult (Vec2. (/ (inc (.x v)))
                                   (/ (.y v) (inc (.y v)))) amount)
      :else (v/mult (Vec2. (/ (.x v) (inc (.x v)))
                           (/ (inc (.y v)))) amount))))
(make-var-method invtree :random)


;; ## J

;; ### Joise noises

;; (make-config-method joise-basis (assoc (j/make-random-basis-conf)
;;                                        :scale (srandom 0.1 1.5)))

;; (defn make-joise-basis
;;   "Joise basis noise (angle version)"
;;   [amount c]
;;   (let [n (j/make-noise (j/make-basis c))]
;;     (make-noise-variation amount (:scale c) n)))
;; (make-var-method joise-basis :regular)

;; (make-config-method joise-basis2 (assoc (j/make-random-basis-conf)
;;                                         :scale (srandom 0.1 1.5)))

;; (defn make-joise-basis2
;;   "Joise basis noise (shift version)"
;;   [amount c]
;;   (let [n (j/make-noise (j/make-basis c))]
;;     (make-noise-variation2 amount (:scale c) n)))
;; (make-var-method joise-basis2 :regular)

;; (make-config-method joise-cell (assoc (j/make-random-cell-conf)
;;                                       :scale (drand)))

;; (defn make-joise-cell
;;   "Joise basis cell (angle version)"
;;   [amount c]
;;   (let [n (j/make-noise (j/make-cell c))]
;;     (make-noise-variation amount (:scale c) n)))
;; (make-var-method joise-cell :regular)

;; (make-config-method joise-cell2 (assoc (j/make-random-cell-conf)
;;                                        :scale (drand)))

;; (defn make-joise-cell2
;;   "Joise basis cell (shift version)"
;;   [amount c]
;;   (let [n (j/make-noise (j/make-cell c))]
;;     (make-noise-variation2 amount (:scale c) n)))
;; (make-var-method joise-cell2 :regular)


;; ### Julia

(defn make-julia
  "Julia"
  [^double amount _]
  (fn [^Vec2 v]
    (let [a (+ (* 0.5 ^double (v/heading v)) (* m/PI (lrand 2)))
          r (* amount (m/sqrt (v/mag v)))]
      (Vec2. (* r (m/cos a)) (* r (m/sin a))))))
(make-var-method julia :random)

(defn make-julia2
  "Julia with different angle calc"
  [^double amount _]
  (fn [^Vec2 v]
    (let [a (+ (* 0.5 (m/atan2 (.x v) (.y v))) (* m/PI (lrand 2)))
          r (* amount (m/sqrt (v/mag v)))]
      (Vec2. (* r (m/cos a)) (* r (m/sin a))))))
(make-var-method julia2 :random)

;; ### JuliaC

(make-config-method juliac {:re (int (srandom 1.0 10.0))
                            :im (* 0.01 (drand -2.0 2.0))
                            :dist (drand -2.0 2.0)})

(defn make-juliac
  "JuliaC"
  [^double amount {:keys [^double re ^double im ^double dist]}]
  (let [rre (/ 1.0 re)]
    (fn [^Vec2 v]
      (let [arg (+ ^double (v/heading v)
                   (* m/TWO_PI ^double (mod (irand) re)))
            lnmod (* dist (m/log (v/magsq v)))
            a (+ (* arg rre)
                 (* lnmod im))
            mod2 (* amount (m/exp (- (* lnmod rre)
                                     (* arg im))))]
        (Vec2. (* mod2 (m/cos a))
               (* mod2 (m/sin a)))))))
(make-var-method juliac :random)

;; ### JuliaN

(make-config-method julian (let [r (srandom 1 10)]
                             {:power (randval r (int r))
                              :dist (drand -4 4)}))

(defn make-julian
  "JuliaN"
  [^double amount {:keys [^double power ^double dist]}]
  (let [abspower (int (m/abs power))
        cpower (* 0.5 (/ dist power))]
    (fn [v]
      (let [a (/ (+ ^double (v/heading v) (* m/TWO_PI (double (irand abspower)))) power)
            r (* amount (m/pow (v/magsq v) cpower))]
        (Vec2. (* r (m/cos a)) (* r (m/sin a)))))))
(make-var-method julian :random)

;; ### JuliaScope

(make-config-method juliascope (let [r (srandom 1 10)]
                                 {:power (randval r (int r))
                                  :dist (drand -4 4)}))

(defn make-juliascope
  "JuliaScope"
  [^double amount {:keys [^double power ^double dist]}]
  (let [abspower (int (m/abs power))
        cpower (* 0.5 (/ dist power))]
    (fn [v]
      (let [rnd (double (lrand abspower))
            a (if (zero? (bit-and rnd 1))
                (/ (+ (* m/TWO_PI rnd) ^double (v/heading v)) power)
                (/ (- (* m/TWO_PI rnd) ^double (v/heading v)) power))
            r (* amount (m/pow (v/magsq v) cpower))]
        (Vec2. (* r (m/cos a)) (* r (m/sin a)))))))
(make-var-method juliascope :random)


;; ### JuliaQ

(make-config-method juliaq {:power (int (srandom 1 10))
                            :divisor (srandom 1 8)})

(defn make-juliaq
  "juliaq by Zueuk, http://zueuk.deviantart.com/art/juliaq-Apophysis-plugins-340813357" 
  [^double amount {:keys [^double divisor ^double power]}]
  (let [inv-power (/ divisor power)
        half-inv-power (* 0.5 inv-power)
        inv-power-2pi (/ m/TWO_PI power)]
    (fn [^Vec2 v]
      (let [a (+ (* inv-power ^double (v/heading v))
                 (* inv-power-2pi (double (irand))))
            r (* amount (m/pow (v/magsq v) half-inv-power))]
        (Vec2. (* r (m/cos a)) (* r (m/sin a)))))))
(make-var-method juliaq :random)

;; ## L

;; ### Lazy Susan

(make-config-method lazysusan {:twist (drand -6 6)
                               :spin (drand -4 4)
                               :space (drand -2 2)
                               :x (drand -1 1)
                               :y (drand -1 1)})

(defn make-lazysusan
  "Lazysusan"
  [^double amount {:keys [^double twist ^double spin ^double space ^double x ^double y]}]
  (fn [^Vec2 v]
    (let [xx (- (.x v) x)
          yy (+ (.y v) y)
          rr (m/hypot xx yy)]
      (if (< rr amount)
        (let [a (+ (m/atan2 yy xx) spin (* twist (- amount rr)))
              nr (* amount rr)]
          (Vec2. (+ (* nr (m/cos a)) x)
                 (- (* nr (m/sin a)) y)))
        (let [nr (* amount (inc (/ space rr)))]
          (Vec2. (+ (* nr xx) x)
                 (- (* nr yy) y)))))))
(make-var-method lazysusan :regular)

;; ### LogApo

(make-config-method logapo {:base (drand 0.01 20)})

(defn make-logapo
  "LogApo"
  [^double amount {:keys [^double base]}]
  (let [denom (/ 0.5 (m/log base))]
    (fn [v]
      (Vec2. (* amount denom (m/log (v/magsq v)))
             (* amount ^double (v/heading v))))))
(make-var-method logapo :regular)

;; ### Log

(defn make-log
  "Log"
  [^double amount _]
  (fn [^Vec2 v]
    (Vec2. (* amount 0.5 (m/log (v/magsq v)))
           (* amount ^double (v/heading v)))))
(make-var-method log :regular)

;; ### Loonie

(defn make-loonie
  "Loonie"
  [^double amount _]
  (let [w2 (m/sq amount)]
    (fn [v]
      (let [^double r2 (v/magsq v)]
        (if (and (< r2 w2) (not (zero? r2)))
          (let [r (* amount (m/sqrt (dec (/ w2 r2))))]
            (v/mult v r))
          (v/mult v amount))))))
(make-var-method loonie :regular)

;; ## M

;; ### Modulus

(make-config-method modulus {:x (srandom 0.01 2.0)
                             :y (srandom 0.01 2.0)})

(defn make-modulus
  "Modulus"
  [amount {:keys [^double x ^double y]}]
  (let [xr (+ x x)
        yr (+ y y)]
    (fn [^Vec2 v]
      (v/mult (Vec2. (cond
                       (> (.x v) x) (+ (- x) (rem (+ (.x v) x) xr))
                       (< (.x v) (- x)) (- x (rem (- x (.x v)) xr))
                       :else (.x v))
                     (cond
                       (> (.y v) y) (+ (- y) (rem (+ (.y v) y) yr))
                       (< (.y v) (- y)) (- y (rem (- y (.y v)) yr))
                       :else (.y v))) amount))))
(make-var-method modulus :regular)


;; ## N

;; ### Ngon

(make-config-method ngon {:circle (drand -2 2)
                          :corners (drand -2 2)
                          :power (drand -10 10)
                          :sides (drand -10 10)})

(defn make-ngon
  "Ngon"
  [^double amount {:keys [^double circle ^double corners ^double power ^double sides]}]
  (let [b (/ m/TWO_PI sides)
        hb (/ b 2.0)
        hpower (/ power 2.0)]
    (fn [v]
      (let [r-factor (m/pow (v/magsq v) hpower)
            ^double theta (v/heading v)
            phi (- theta (* b (m/floor (/ theta b))))
            phi (if (> phi hb) (- phi b) phi)
            amp (/ (+ circle (* corners (dec (/ 1.0 (+ (m/cos phi) m/EPSILON))))) (+ r-factor m/EPSILON))]
        (v/mult v (* amount amp))))))
(make-var-method ngon :regular)

;; ### Noise

(defn make-noise
  "Noise"
  [^double amount _]
  (fn [v]
    (let [a (drand m/TWO_PI)
          r (drand amount)]
      (Vec2. (* r (m/cos a))
             (* r (m/sin a))))))
(make-var-method noise :random)

;; ## P

;; ### Panorama1

(defn make-panorama1
  "Panorama1"
  [^double amount _]
  (fn [v]
    (let [aux (/ (m/sqrt (inc ^double (v/magsq v))))
          nv (v/mult v aux)
          ^double aux (v/mag nv)]
      (Vec2. (* amount m/M_1_PI ^double (v/heading nv))
             (* amount (- aux 0.5))))))
(make-var-method panorama1 :regular)

;; ### Panorama2

(defn make-panorama2
  "Panorama2"
  [^double amount _]
  (fn [v]
    (let [aux (/ (inc (m/sqrt (v/magsq v))))
          nv (v/mult v aux)
          ^double aux (v/mag nv)]
      (Vec2. (* amount m/M_1_PI ^double (v/heading nv))
             (* amount (- aux 0.5))))))
(make-var-method panorama2 :regular)

;; ### Parabola

(make-config-method parabola {:width (srandom 0.5 2.0)
                              :height (srandom 0.5 2.0)})

(defn make-parabola
  "Parabola fn"
  [^double amount {:keys [^double width ^double height]}]
  (fn [v]
    (let [^double r (v/mag v)
          sr (m/sin r)
          cr (m/cos r)]
      (Vec2. (* amount height sr sr (drand))
             (* amount width cr (drand))))))
(make-var-method parabola :random)


;; ### Perlin

(make-config-method perlin {:seed (irand)
                            :scale (srandom 0.1 1.5)
                            :octaves (irand 1 6)})

(defn make-perlin
  "Perlin noise"
  [amount {:keys [seed octaves scale]}]
  (let [n (make-fbm-noise {:seed seed :octaves octaves})]
    (make-noise-variation amount scale n)))
(make-var-method perlin :regular)

(make-config-method perlin2 {:seed (irand)
                             :scale (srandom 0.1 1.5)
                             :octaves (irand 1 6)})

(defn make-perlin2
  "Perlin noise"
  [^double amount {:keys [^int seed ^int octaves ^double scale]}]
  (let [n (make-fbm-noise {:seed seed :octaves octaves})]
    (make-noise-variation2 amount scale n)))
(make-var-method perlin2 :regular)


;; ### Petal

(defn make-petal
  "Petal"
  [^double amount _]
  (fn [^Vec2 v]
    (let [a (m/cos (.x v))
          bx (m/pow (* (m/cos (.x v)) (m/cos (.y v))) 3.0)
          by (m/pow (* (m/sin (.x v)) (m/cos (.y v))) 3.0)]
      (Vec2. (* amount a bx)
             (* amount a by)))))
(make-var-method petal :regular)

;; ### Pie

(make-config-method pie {:slices (srandom 0.01 7.0)
                         :rotation (drand m/TWO_PI)
                         :thickness (drand -2.0 2.0)})

(defn make-pie
  "pie from jwildfire"
  [^double amount {:keys [^double slices ^double rotation ^double thickness]}]
  (fn [^Vec2 v]
    (let [sl (double (m/round (+ 0.5 (* slices (drand)))))
          a (-> thickness
                (* (drand))
                (+ sl)
                (* m/TWO_PI)
                (/ slices)
                (+ rotation))
          r (* amount (drand))]
      (Vec2. (* r (m/cos a))
             (* r (m/sin a))))))
(make-var-method pie :random)

;; ### PDJ

(make-config-method pdj {:a (drand -6.0 6.0)
                         :b (drand -6.0 6.0)
                         :c (drand -6.0 6.0)
                         :d (drand -6.0 6.0)})

(defn make-pdj
  "PDJ"
  [^double amount {:keys [^double a ^double b ^double c ^double d]}]
  (fn [^Vec2 v]
    (Vec2. (* amount (- (m/sin (* a (.y v))) (m/cos (* b (.x v)))))
           (* amount (- (m/sin (* c (.x v))) (m/cos (* d (.y v))))))))
(make-var-method pdj :regular)

;; ### Perspective

(make-config-method perspective {:angle (drand (- m/PI) m/PI)
                                 :dist (drand -5.0 5.0)})

(defn make-perspective
  "Perspective"
  [^double amount {:keys [^double angle ^double dist]}]
  (let [ang (* m/HALF_PI angle)
        vsin (m/sin ang)
        vfcos (* dist (m/cos ang))]
    (fn [^Vec2 v]
      (let [t (/ amount (- dist (* (.y v) vsin)))]
        (Vec2. (* t dist (.x v))
               (* t vfcos (.y v)))))))
(make-var-method perspective :regular)

;; ### Phoenix julia

(make-config-method phoenix-julia {:power (int (srandom 0.51 10))
                                   :dist (drand -2.0 2.0)
                                   :x-distort (drand -2.0 2.0)
                                   :y-distort (drand -2.0 2.0)})

(defn make-phoenix-julia
  "Phoenix julia"
  [^double amount {:keys [^double power ^double dist ^double x-distort ^double y-distort]}]
  (let [inv-n (/ dist power)
        inv2pi-n (/ m/TWO_PI power)
        c-n (* 0.5 inv-n)]
    (fn [^Vec2 v]
      (let [pre-x (* (.x v) x-distort)
            pre-y (* (.y v) y-distort)
            a (+ (* (m/atan2 pre-y pre-x) inv-n)
                 (* (irand) inv2pi-n))
            sina (m/sin a)
            cosa (m/cos a)
            r (* amount (m/pow (v/magsq v) c-n))]
        (Vec2. (* r cosa)
               (* r sina))))))
(make-var-method phoenix-julia :random)

;; ### Polar

(defn make-polar
  "Polar"
  [^double amount _]
  (fn [^Vec2 v]
    (let [ny (dec ^double (v/mag v))]
      (Vec2. (* amount ^double (v/heading v) m/M_1_PI)
             (* amount ny)))))
(make-var-method polar :regular)

(defn make-polar2
  "Polar2"
  [^double amount _]
  (let [p2v (/ amount m/PI)
        p2v2 (* 0.5 p2v)]
    (fn [^Vec2 v] (Vec2. (* p2v ^double (v/heading v)) (* p2v2 (m/log (v/magsq v)))))))
(make-var-method polar2 :regular)

;; ### PowBlock

(make-config-method powblock {:numerator (drand -20 20)
                              :denominator (drand -20 20)
                              :root (drand -6 6)
                              :correctn (drand -2 2)
                              :correctd (drand -2 2)})

(defn make-powblock
  "PowBlock"
  [^double amount {:keys [^double numerator ^double denominator ^double root ^double correctn ^double correctd]}]
  (let [power (/ (* denominator correctn) (+ m/EPSILON (m/abs correctd)))
        power (if (< (m/abs power) m/EPSILON) m/EPSILON power)
        power (/ (* 0.5 numerator) power) 
        deneps (/ (if (< (m/abs denominator) m/EPSILON) m/EPSILON denominator))]
    (fn [^Vec2 v]
      (let [^double theta (v/heading v)
            r2 (* amount (m/pow (v/magsq v) power))
            ran (+ (* numerator (+ (* theta deneps) (* root m/TWO_PI (m/floor (drand denominator)) deneps))))]
        (Vec2. (* r2 (m/cos ran))
               (* r2 (m/sin ran)))))))
(make-var-method powblock :random)

;; ### Power

(defn make-power
  "Power"
  [^double amount _]
  (fn [^Vec2 v]
    (let [theta (v/heading v)
          sa (m/sin theta)
          ca (m/cos theta)
          pow (* amount (m/pow (v/mag v) sa))]
      (Vec2. (* pow ca) (* pow sa)))))
(make-var-method power :regular)

;; ### Popcorn2

(make-config-method popcorn2 {:x (drand -1.5 1.5)
                              :y (drand -1.5 1.5)
                              :c (drand -5.0 5.0)})

(defn make-popcorn2
  "popcorn2 from apophysis"
  [^double amount {:keys [^double x ^double y ^double c]}]
  (fn [^Vec2 v]
    (let [xx (->> (.y v)
                  (* c)
                  (m/tan)
                  (m/sin)
                  (* x)
                  (+ (.x v))
                  (* amount))
          yy (->> (.x v)
                  (* c)
                  (m/tan)
                  (m/sin)
                  (* y)
                  (+ (.y v))
                  (* amount))]
      (Vec2. xx yy))))
(make-var-method popcorn2 :regular)

;; ### Pressure Wave

(make-config-method pressure-wave {:x-freq (drand -6 6)
                                   :y-freq (drand -6 6)})

(defn make-pressure-wave
  "Pressure Wave"
  [^double amount {:keys [^double x-freq ^double y-freq]}]
  (let [[^double pwx ^double ipwx] (if (zero? x-freq) [1.0 1.0] (let [pwx (* x-freq m/TWO_PI)] [pwx (/ pwx)]))
        [^double pwy ^double ipwy] (if (zero? y-freq) [1.0 1.0] (let [pwy (* y-freq m/TWO_PI)] [pwy (/ pwy)]))]
    (fn [^Vec2 v]
      (Vec2. (* amount (+ (.x v) (* ipwx (m/sin (* pwx (.x v))))))
             (* amount (+ (.y v) (* ipwy (m/sin (* pwy (.y v))))))))))
(make-var-method pressure-wave :regular)

;; ## R

;; ### R Circle Blur

(make-config-method r-circleblur {:n (drand -3 3)
                                  :seed (drand Float/MAX_VALUE)
                                  :dist (drand -1 1)
                                  :mn (drand -3 3)
                                  :mx (drand -3 3)})

(defn make-r-circleblur
  "R Circle Blur"
  [^double amount {:keys [^double n ^double seed ^double dist ^double mn ^double mx]}]
  (let [dm (- mx mn)]
    (fn [v]
      (let [^double angle (v/heading v)
            ^double rad (v/mag v)
            rad (mod rad n)
            by (m/sin (+ angle rad))
            bx (m/cos (+ angle rad))
            by (m/round (* by rad))
            bx (m/round (* bx rad))
            rad2 (* 0.5 (m/sqrt (drand)))
            angle2 (drand m/TWO_PI)
            a1 (m/sfrac (* 43758.5453 (m/sin (+ (* bx 127.1) (* by 311.7) seed))))
            a2 (m/sfrac (* 43758.5453 (m/sin (+ (* bx 269.5) (* by 183.3) seed))))
            a3 (m/sfrac (* 43758.5453 (m/sin (+ (* bx 78.233) (* by 12.9898) seed))))
            a3 (+ mn (* a3 dm))
            rad2 (* rad2 a3)]
        (Vec2. (* amount (+ bx (* rad2 (m/cos angle2)) (* dist a1)))
               (* amount (+ by (* rad2 (m/sin angle2)) (* dist a2))))))))
(make-var-method r-circleblur :random)


;; ### Radial Blur

(make-config-method radialblur {:angle (drand (- m/TWO_PI) m/TWO_PI)})

(defn make-radialblur
  "Radial blur"
  [^double amount {:keys [^double angle]}]
  (let [spin (* amount (m/sin (* angle m/HALF_PI)))
        zoom (* amount (m/cos (* angle m/HALF_PI)))]
    (fn [^Vec2 v]
      (let [rnd-g (+ (drand) (drand) (drand) (drand) -2.0)
            ^double ra (v/mag v)
            alpha (+ (* spin rnd-g) ^double (v/heading v))
            rz (dec (* zoom rnd-g))]
        (Vec2. (+ (* rz (.x v)) (* ra (m/cos alpha)))
               (+ (* rz (.y v)) (* ra (m/sin alpha))))))))
(make-var-method radialblur :random)

;; ### Rational3

(make-config-method rational3 {:a (drand -3 3)
                               :b (drand -3 3)
                               :c (drand -3 3)
                               :d (drand -3 3)
                               :e (drand -3 3)
                               :f (drand -3 3)
                               :g (drand -3 3)
                               :h (drand -3 3)})

(defn make-rational3
  "Rational3"
  [^double amount {:keys [^double a ^double b ^double c ^double d
                          ^double e ^double f ^double g ^double h]}]
  (fn [^Vec2 v]
    (let [^Vec2 sqr (v/emult v v)
          ^Vec2 cb (v/emult sqr v)
          zt3 (- (.x cb) (* 3.0 (.x v) (.y sqr)))
          zt2 (- (.x sqr) (.y sqr))
          zb3 (- (* 3.0 (.x sqr) (.y v)) (.y cb))
          zb2 (* 2.0 (.x v) (.y v))
          tr (+ (* a zt3) (* b zt2) (* c (.x v)) d)
          ti (+ (* a zb3) (* b zb2) (* c (.y v )))
          br (+ (* e zt3) (* f zt2) (* g (.x v)) h)
          bi (+ (* e zb3) (* f zb2) (* g (.y v )))
          r3den (/ amount (+ (* br br) (* bi bi)))]
      (Vec2. (* r3den (+ (* tr br) (* ti bi)))
             (* r3den (- (* ti br) (* tr bi)))))))
(make-var-method rational3 :regular)

;; ### Rays

(defn make-rays
  "Rays"
  [^double amount _]
  (fn [^Vec2 v]
    (let [ang (* amount (drand m/PI))
          r (/ amount (+ m/EPSILON ^double (v/magsq v)))
          tanr (* amount r (m/tan ang))]
      (Vec2. (* tanr (m/cos (.x v)))
             (* tanr (m/sin (.y v)))))))
(make-var-method rays :random)

(defn make-rays1
  "Rays1"
  [^double amount _]
  (let [pa (* amount (m/sq m/M_2_PI))]
    (fn [^Vec2 v]
      (let [^double t (v/magsq v)
            u (+ pa (/ (m/tan (m/sqrt t))))
            r (* amount u t)]
        (Vec2. (/ r (.x v))
               (/ r (.y v)))))))
(make-var-method rays1 :regular)

(defn make-rays2
  "Rays2"
  [^double amount _]
  (let [a10 (/ amount 10.0)]
    (fn [^Vec2 v]
      (let [^double t (v/magsq v)
            u (/ (m/cos (* (+ t m/EPSILON) (m/tan (/ (+ m/EPSILON t))))))
            r (* a10 t u)]
        (Vec2. (/ r (.x v))
               (/ r (.y v)))))))
(make-var-method rays2 :regular)

(defn make-rays3
  "Rays3"
  [^double amount _]
  (let [a10 (/ amount 10.0)]
    (fn [^Vec2 v]
      (let [^double t (v/magsq v)
            t2 (* t t)
            u (/ (m/sqrt (m/cos (m/sin (* (+ m/EPSILON t2) (m/sin (/ (+ t2 m/EPSILON))))))))
            r (* a10 t u)]
        (Vec2. (/ (* r (m/cos t)) (.x v))
               (/ (* r (m/tan t)) (.y v)))))))
(make-var-method rays3 :regular)

;; ### Rectangles

(make-config-method rectangles {:x (drand -1.5 1.5)
                                :y (drand -1.5 1.5)})

(defn make-rectangles
  "Rectangles"
  [^double amount {:keys [^double x ^double y]}]
  (fn [^Vec2 v]
    (Vec2. (if (< (m/abs (.x v)) m/EPSILON)
             (* amount (.x v))
             (* amount (-> (.x v)
                           (/ x)
                           (m/floor)
                           (* 2.0)
                           inc
                           (* x)
                           (- (.x v)))))
           (if (< (m/abs (.y v)) m/EPSILON)
             (* amount (.y v))
             (* amount (-> (.y v)
                           (/ y)
                           (m/floor)
                           (* 2.0)
                           inc
                           (* y)
                           (- (.y v))))))))
(make-var-method rectangles :regular)

;; ### Rhodonea

(make-config-method rhodonea {:knumer (randval (int (srandom 1 30)) (srandom 1 30))
                              :kdenom (randval (int (srandom 1 30)) (srandom 1 30))
                              :radial-offset (drand -1 1)
                              :inner-mode (irand 7)
                              :outer-mode (irand 7)
                              :inner-spread (drand -1 1)
                              :outer-spread (drand -1 1)
                              :inner-spread-ratio (drand -2 2)
                              :outer-spread-ratio (drand -2 2)
                              :spread-split (drand -1.5 1.5)
                              :cycle-offset (drand m/TWO_PI)
                              :cycles-param (randval 0 (drand 100))
                              :metacycle-expansion (drand -1 1)
                              :metacycles (drand 10)
                              :fill (randval 0 (drand))})


(defn make-rhodonea
  "Rhodonea"
  [^double amount {:keys [^double knumer ^double kdenom ^double radial-offset ^long inner-mode ^long outer-mode
                          ^double inner-spread ^double outer-spread ^double inner-spread-ratio ^double outer-spread-ratio
                          ^double spread-split ^double cycles-param ^double cycle-offset ^double metacycle-expansion
                          ^double metacycles ^double fill]}]
  (let [kn knumer
        kd kdenom
        k (/ kn kd)
        cycles-to-close (double (if (zero? (mod k 1.0))
                                  (if (zero? (mod k 2.0))
                                    1.0
                                    (if (bool-or (not (zero? radial-offset))
                                                 (not (zero? inner-spread))
                                                 (not (zero? outer-spread))
                                                 (not (zero? fill)))
                                      1.0
                                      0.5))
                                  (if (bool-and (zero? (mod kn 1.0))
                                                (zero? (mod kd 1.0)))
                                    (let [lkn (long kn)
                                          lkd (long kd)
                                          gcd (m/gcd lkn lkd)
                                          [^long kn ^long kd] (if (not== gcd 1)
                                                                [(/ lkn gcd) (/ lkd gcd)]
                                                                [lkn lkd])]
                                      (if (bool-or (zero? (mod kn 2.0))
                                                   (zero? (mod kd 2.0)))
                                        kd
                                        (/ kd 2)))
                                    (if (< cycles-param 16)
                                      16
                                      (* 2 kd kn)))))
        cycles (if (zero? cycles-param)
                 (* cycles-to-close metacycles)
                 cycles-param)]
    (fn [^Vec2 v]
      (let [rin (* spread-split ^double (v/mag v))
            ^double tin (v/heading v)
            t (* cycles (+ tin (* cycle-offset m/TWO_PI)))
            r (+ radial-offset (m/cos (* t k)))
            r (if-not (zero? fill)
                (+ r (* fill (- (drand) 0.5)))
                r)
            x (* r (m/cos t))
            y (* r (m/sin t))
            expansion (m/floor (/ (* cycles (+ tin m/PI))
                                  (* cycles-to-close m/TWO_PI)))
            adjusted-amount (+ amount (* expansion metacycle-expansion))]
        (if (> (m/abs rin) (m/abs r))
          (case (long outer-mode)
            0 (Vec2. (* adjusted-amount x)
                     (* adjusted-amount y))
            1 (let [rinx (inc (* (dec rin) outer-spread outer-spread-ratio))
                    riny (inc (* (dec rin) outer-spread))]
                (Vec2. (* adjusted-amount rinx x)
                       (* adjusted-amount riny y)))
            2 (let [xin (* (m/sgn x) (m/abs (.x v)))
                    yin (* (m/sgn y) (m/abs (.y v)))]
                (Vec2. (* adjusted-amount (+ x (* outer-spread outer-spread-ratio (- xin x))))
                       (* adjusted-amount (+ y (* outer-spread (- yin y))))))
            3 (let [xin (* (m/sgn x) (m/abs (.x v)))
                    yin (* (m/sgn y) (m/abs (.y v)))]
                (Vec2. (* adjusted-amount (+ x (* outer-spread outer-spread-ratio xin)))
                       (* adjusted-amount (+ y (* outer-spread yin)))))
            4 (let [rinx (+ (* 0.5 rin) (* outer-spread outer-spread-ratio))
                    riny (+ (* 0.5 rin) outer-spread)]
                (Vec2. (* adjusted-amount rinx x)
                       (* adjusted-amount riny y)))
            5 v
            6 (Vec2. 0.0 0.0))
          (case (long inner-mode)
            0 (Vec2. (* adjusted-amount x)
                     (* adjusted-amount y))
            1 (let [rinx (inc (* (dec rin) inner-spread inner-spread-ratio))
                    riny (inc (* (dec rin) inner-spread))]
                (Vec2. (* adjusted-amount rinx x)
                       (* adjusted-amount riny y)))
            2 (let [xin (* (m/sgn x) (m/abs (.x v)))
                    yin (* (m/sgn y) (m/abs (.y v)))]
                (Vec2. (* adjusted-amount (+ x (* inner-spread inner-spread-ratio (- xin x))))
                       (* adjusted-amount (+ y (* inner-spread (- yin y))))))
            3 (let [xin (* (m/sgn x) (m/abs (.x v)))
                    yin (* (m/sgn y) (m/abs (.y v)))]
                (Vec2. (* adjusted-amount (+ x (* inner-spread inner-spread-ratio xin)))
                       (* adjusted-amount (+ y (* inner-spread yin)))))
            4 (let [rinx (+ (* 0.5 rin) (* inner-spread inner-spread-ratio))
                    riny (+ (* 0.5 rin) inner-spread)]
                (Vec2. (* adjusted-amount rinx x)
                       (* adjusted-amount riny y)))
            5 v
            6 (Vec2. 0.0 0.0)
            ))))))
(make-var-method rhodonea :random)

;; ### Rings

(make-config-method rings {:coeff20 (drand 1.3)})

(defn make-rings
  "Rings"
  [^double amount {:keys [^double coeff20]}]
  (let [dx (+ m/EPSILON (m/sq coeff20))
        dx2 (+ dx dx)
        rdx (/ 1.0 dx2)
        dx- (- 1.0 dx)]
    (fn [^Vec2 v]
      (let [^double r (v/mag v)
            rr (+ (- r (* dx2 (double (int (* (+ r dx) rdx))))) (* r dx-))]
        (Vec2. (* rr (/ (.x v) r))
               (* rr (/ (.y v) r)))))))
(make-var-method rings :regular)

(make-config-method rings2 {:val (drand -1.0 1.0)})

(defn make-rings2
  "Rings2"
  [^double amount {:keys [^double val]}]
  (let [dx (+ m/EPSILON (m/sq val))]
    (fn [^Vec2 v]
      (let [^double l (v/mag v)
            r (* amount (- 2.0 (* dx (inc (/ (* 2.0 (double (int (* 0.5 (inc (/ l dx)))))) l)))))]
        (v/mult v r)))))
(make-var-method rings2 :regular)

;; ### Ripple

(make-config-method ripple {:frequency (drand -3 3)
                            :velocity (drand -3 3)
                            :amplitude (drand -2 2)
                            :centerx (drand -0.5 0.5)
                            :centery (drand -0.5 0.5)
                            :phase (drand -1 1)
                            :scale (srandom 0.5 4)
                            :fixed-dist-calc (irand 4)})

(defn make-ripple
  "Ripple"
  [^double amount {:keys [^double frequency ^double velocity ^double amplitude ^double centerx
                          ^double centery ^double phase ^double scale ^int fixed-dist-calc]}]
  (let [f (* frequency 5.0)
        a (* amplitude 0.01)
        p (- (* phase m/TWO_PI) m/PI)
        s (if (zero? scale) m/EPSILON scale)
        is (/ s)
        vxp (* velocity p)
        pxa (* p a)
        pixa (* (- m/PI p) a)]
    (fn [^Vec2 v]
      (let [x (- (* s (.x v)) centerx)
            y (- (* s (.y v)) centery)
            ^double d (case (unchecked-int fixed-dist-calc)
                        0 (v/mag v)
                        1 (m/sqrt (* (m/sq (.x v)) (m/sq (.y v))))
                        2 (max (m/abs (.x v)) (m/abs (.y v)))
                        3 (+ (m/abs (.x v)) (m/abs (.y v))))
            d (if (< d m/EPSILON) m/EPSILON d)
            nx (/ x d)
            ny (/ y d)
            wave (m/cos (- (* f d) vxp))
            d1 (+ d (* wave pxa))
            d2 (+ d (* wave pixa))
            u1 (+ centerx (* nx d1))
            u2 (+ centerx (* nx d2))
            v1 (- (* ny d1) centery)
            v2 (- (* ny d2) centery)]
        (Vec2. (* amount is (m/lerp u1 u2 p))
               (* amount is (m/lerp v1 v2 p)))))))
(make-var-method ripple :regular)

;; ### Rippled

(defn make-rippled
  "Rippled"
  [^double amount _]
  (fn [^Vec2 v]
    (let [d (+ m/EPSILON ^double (v/magsq v))]
      (Vec2. (* (* (m/tanh d) (* 2.0 (.x v))) (/ amount 2.0))
             (* (* (m/cos d) (* 2.0 (.y v))) (/ amount 2.0))))))
(make-var-method rippled :regular)

;; ### Round Sphere

(defn make-roundspher
  ""
  [^double amount _]
  (let [s (m/sq m/M_2_PI)]
    (fn [v]
      (let [^double d (v/magsq v)
            re (/ (+ s (/ d)))
            ad (/ amount d)]
        (v/mult v (* amount ad re))))))
(make-var-method roundspher :regular)

;; ## S

;; ### Scry

(defn make-scry
  "Scry"
  [^double amount _]
  (fn [^Vec2 v]
    (let [^double t (v/magsq v)
          d (-> 1.0
                (/ amount)
                (+ t)
                (* (m/sqrt t))
                (+ m/EPSILON))
          r (/ 1.0 d)]
      (v/mult v r))))
(make-var-method scry :regular)

;; ### Sech

(defn make-sech
  "Sech"
  [^double amount _]
  (fn [^Vec2 v]
    (let [sn (m/sin (.y v))
          cn (m/cos (.y v))
          snh (m/sinh (.x v))
          cnh (m/cosh (.x v))
          d (+ (m/cos (* 2.0 (.y v)))
               (m/cosh (* 2.0 (.x v))))
          d (if (zero? d) m/EPSILON d)
          den (/ 2.0 d)]
      (Vec2. (* amount den cn cnh)
             (* (- amount) den sn snh)))))
(make-var-method sech :regular)

;; ### Shreadrad

(make-config-method shreadrad {:n (randval (int (srandom 1 9)) (srandom 0.0001 8.0))
                               :width (drand -2.0 2.0)})

(defn make-shreadrad
  "ShreadRad"
  [^double amount {:keys [^double n ^double width]}]
  (let [sa (/ m/TWO_PI n)
        sa2 (* 0.5 sa)
        sa2sw (* sa2 width)
        pi3 (* 3.0 m/PI)]
    (fn [v]
      (let [^double ang (v/heading v)
            ^double rad (v/mag v)
            xang (/ (+ ang pi3 sa2) sa)
            ixang (unchecked-int xang)
            zang (- (* sa (+ ixang (* width (- xang ixang)))) m/PI sa2sw)]
        (Vec2. (* amount rad (m/cos zang))
               (* amount rad (m/sin zang)))))))
(make-var-method shreadrad :regular)

;; ### Sinusoidal

(defn make-sinusoidal
  "Sinusoidal"
  [^double amount _]
  (fn [^Vec2 v]
    (Vec2. (* amount (m/sin (.x v))) (* amount (m/sin (.y v))))))
(make-var-method sinusoidal :regular)

;; ### Secant

(defn make-secant
  "Secant2"
  [^double amount _]
  (fn [^Vec2 v]
    (let [r (* amount ^double (v/mag v))
          cr (* amount (m/cos r))
          icr (/ 1.0 (if (zero? cr) m/EPSILON cr))]
      (Vec2. (* amount (.x v)) icr))))
(make-var-method secant :regular)

(defn make-secant2
  "Secant2"
  [^double amount _]
  (fn [^Vec2 v]
    (let [r (* amount ^double (v/mag v))
          cr (m/cos r)
          icr (/ 1.0 (if (zero? cr) m/EPSILON cr))
          ny (if (neg? cr)
               (* amount (inc cr))
               (* amount (dec cr)))]
      (Vec2. (* amount (.x v)) ny))))
(make-var-method secant2 :regular)

;; ### Spherical

(defn make-spherical
  "Spherical"
  [^double amount _]
  (fn [^Vec2 v]
    (v/mult v (/ amount (+ m/EPSILON ^double (v/magsq v))))))
(make-var-method spherical :regular)

;; ### Spiral

(defn make-spiral
  "Spiral"
  [^double amount _]
  (fn [^Vec2 v]
    (let [r (+ m/EPSILON ^double (v/mag v))
          revr (/ 1.0 r)
          sina (* (.x v) revr)
          cosa (* (.y v) revr)
          sinr (m/sin r)
          cosr (m/cos r)]
      (Vec2. (* amount revr (+ cosa sinr))
             (* amount revr (- sina cosr))))))
(make-var-method spiral :regular)

;; ### Split

(make-config-method split {:xsplit (drand (- m/TWO_PI) m/TWO_PI)
                           :ysplit (drand (- m/TWO_PI) m/TWO_PI)})

(defn make-split
  "Split"
  [^double amount {:keys [^double xsplit ^double ysplit]}]
  (fn [^Vec2 v]
    (Vec2. (if (pos? (m/cos (* (.x v) xsplit)))
             (* amount (.y v))
             (- (* amount (.y v))))
           (if (pos? (m/cos (* (.y v) ysplit)))
             (* amount (.x v))
             (- (* amount (.x v)))))))
(make-var-method split :regular)

;; ### Splits

(make-config-method splits {:x (drand -1.5 1.5)
                            :y (drand -1.5 1.5)})

(defn make-splits
  "Splits"
  [^double amount {:keys [^double x ^double y]}]
  (fn [^Vec2 v]
    (Vec2. (if (pos? (.x v))
             (* amount (+ (.x v) x))
             (* amount (- (.x v) x)))
           (if (pos? (.y v))
             (* amount (+ (.y v) y))
             (* amount (- (.y v) y))))))
(make-var-method splits :regular)

;; ### Square

(defn make-square
  "Square"
  [^double amount _]
  (fn [v]
    (Vec2. (* amount (drand -0.5 0.5))
           (* amount (drand -0.5 0.5)))))
(make-var-method square :random)

;; ### Squirrel

(make-config-method squirrel {:a (drand m/EPSILON 4.0)
                              :b (drand m/EPSILON 4.0)})

(defn make-squirrel
  "Squirrel"
  [^double amount {:keys [^double a ^double b]}]
  (fn [^Vec2 v]
    (let [u (m/sqrt (+ (* a (m/sq (.x v)))
                       (* b (m/sq (.y v)))))]
      (Vec2. (* amount (m/cos u) (m/tan (.x v)))
             (* amount (m/sin u) (m/tan (.y v)))))))
(make-var-method squirrel :regular)

;; ### STwin

(make-config-method stwin {:distort (drand -6 6)
                           :multiplier (srandom 0.001 3.0)})

(defn make-stwin
  "STwin by Xyrus-02, http://timothy-vincent.deviantart.com/art/STwin-Plugin-136504836"
  [^double amount {:keys [^double distort ^double multiplier]}]
  (fn [^Vec2 v]
    (let [x (* (.x v) amount multiplier)
          y (* (.y v) amount multiplier)
          x2 (* x x)
          y2 (* y y)
          x2+y2 (+ x2 y2)
          x2-y2 (- x2 y2)
          div (if (zero? x2+y2) 1.0 x2+y2)
          result (/ (* x2-y2 (m/sin (* m/TWO_PI distort (+ x y)))) div)]
      (Vec2. (+ (* amount (.x v)) result)
             (+ (* amount (.y v)) result)))))
(make-var-method stwin :regular)

;; ### Supershape

(make-config-method supershape {:rnd (drand -1 1)
                                :m (drand m/TWO_PI)
                                :n1 (drand -5 5)
                                :n2 (drand -5 5)
                                :n3 (drand -5 5)
                                :holes (drand -1 1)})

(defn make-supershape
  "Supershape"
  [^double amount {:keys [^double rnd ^double m ^double n1 ^double n2 ^double n3 ^double holes]}]
  (let [pm-4 (/ m 4.0)
        pneg1-n1 (/ -1.0 n1)]
    (fn [^Vec2 v]
      (let [theta (+ (* pm-4 ^double (v/heading v)) m/M_PI_4)
            st (m/sin theta)
            ct (m/cos theta)
            t1 (m/pow (m/abs ct) n2)
            t2 (m/pow (m/abs st) n3)
            ^double mag (v/mag v)
            r (/ (* (* amount (- (+ (drand rnd) (* (- 1.0 rnd) mag)) holes)) (m/pow (+ t1 t2) pneg1-n1)) mag)]
        (v/mult v r)))))
(make-var-method supershape :random)

;; ### Swirl

(defn make-swirl
  "Swirl"
  [^double amount _]
  (fn [^Vec2 v]
    (let [r (v/magsq v)
          c1 (m/sin r)
          c2 (m/cos r)]
      (Vec2. (* amount (- (* c1 (.x v)) (* c2 (.y v))))
             (* amount (+ (* c2 (.x v)) (* c1 (.y v))))))))
(make-var-method swirl :regular)

;; ## T

;; ### Tangent

(defn make-tangent
  "Tangent"
  [^double amount _]
  (fn [^Vec2 v]
    (let [d (m/cos (.y v))
          id (/ 1.0 (if (zero? d) m/EPSILON d))]
      (Vec2. (* amount (m/sin (.x v)) id)
             (* amount (m/tan (.y v)))))))
(make-var-method tangent :regular)

;; ### Twintrian

(defn make-twintrian
  "Twintrian"
  [^double amount _]
  (fn [^Vec2 v]
    (let [r (* amount (drand) ^double (v/mag v))
          sinr (m/sin r)
          diff (+ (m/cos r) (m/log10 (m/sq sinr)))]
      (Vec2. (* amount diff (.x v))
             (* amount (.x v) (- diff (* m/PI sinr)))))))
(make-var-method twintrian :random)


;; ### Taurus

(make-config-method taurus {:r (drand -5.0 5.0)
                            :n (drand -5.0 5.0)
                            :inv (drand -2.0 2.0)
                            :sor (drand -2.0 2.0)})

(defn make-taurus
  "Taurus"
  [^double amount {:keys [^double r ^double n ^double inv ^double sor]}]
  (let [rinv (* r inv)
        revinv (- 1.0 inv)]
    (fn [^Vec2 v]
      (let [sx (m/sin (.x v))
            cx (m/cos (.x v))
            sy (m/sin (.y v))
            ir (+ rinv (* revinv r (m/cos (* n (.x v)))))
            irsy (+ ir sy)]
        (Vec2. (* amount cx irsy)
               (* amount sx irsy))))))
(make-var-method taurus :regular)

;; ### Trade

(make-config-method trade {:r1 (drand 0.1 3.0)
                           :r2 (drand 0.1 3.0)
                           :d1 (drand -2.0 2.0)
                           :d2 (drand -2.0 2.0)})

(defn make-trade
  "trade by Michael Faber,  http://michaelfaber.deviantart.com/art/The-Lost-Variations-258913970"
  [^double amount {:keys [^double r1 ^double r2 ^double d1 ^double d2]}]
  (let [c1 (+ r1 d1)
        c2 (+ r2 d2)]
    (fn [^Vec2 v]
      (let [[^double cc1 ^double cc2 ^double fr ^double rr] (if (pos? (.x v))
                                                              [c1 (- c2) (/ r2 r1) r1]
                                                              [(- c2) c1 (/ r1 r2) r2])
            nv (Vec2. (- cc1 (.x v)) (.y v))
            ^double rm (v/mag nv)
            r (* rm fr)
            a (v/heading nv)
            res (Vec2. (+ cc2 (* r (m/cos a)))
                       (* r (m/sin a)))]
        (if (<= rm rr)
          (v/mult res amount)
          (v/mult v amount))))))
(make-var-method trade :regular)

;; ## V

;; ### Vibration

(make-config-method vibration {:dir (drand m/TWO_PI)
                               :angle (drand m/TWO_PI)
                               :freq (srandom 0.01 2.0)
                               :amp (srandom 0.1 1.0)
                               :phase (drand)
                               :dir2 (drand m/TWO_PI)
                               :angle2 (drand m/TWO_PI)
                               :freq2 (srandom 0.01 2.0)
                               :amp2 (srandom 0.1 1.0)
                               :phase2 (drand)})

(defn make-vibration
  "Vibration http://fractal-resources.deviantart.com/art/Apo-Plugins-Vibration-1-and-2-252001851"
  [^double amount {:keys [^double dir ^double angle ^double freq ^double amp ^double phase
                          ^double dir2 ^double angle2 ^double freq2 ^double amp2 ^double phase2]}]
  (let [total-angle (+ angle dir)
        cos-dir (m/cos dir)
        sin-dir (m/sin dir)
        cos-tot (m/cos total-angle)
        sin-tot (m/sin total-angle)
        scaled-freq (* m/TWO_PI freq)
        phase-shift (/ (* m/TWO_PI phase) freq)
        total-angle2 (+ angle2 dir2)
        cos-dir2 (m/cos dir2)
        sin-dir2 (m/sin dir2)
        cos-tot2 (m/cos total-angle2)
        sin-tot2 (m/sin total-angle2)
        scaled-freq2 (* m/TWO_PI freq2)
        phase-shift2 (/ (* m/TWO_PI phase2) freq2)]
    (fn [^Vec2 v]
      (let [d-along-dir (+ (* (.x v) cos-dir)
                           (* (.y v) sin-dir))
            local-amp (* amp (m/sin (+ (* d-along-dir scaled-freq) phase-shift)))
            x (+ (.x v) (* local-amp cos-tot))
            y (+ (.y v) (* local-amp sin-tot))
            d-along-dir (+ (* (.x v) cos-dir2)
                           (* (.y v) sin-dir2))
            local-amp (* amp2 (m/sin (+ (* d-along-dir scaled-freq2) phase-shift2)))
            x (+ x (* local-amp cos-tot2))
            y (+ y (* local-amp sin-tot2))]
        (Vec2. (* amount x) (* amount y))))))
(make-var-method vibration :regular)

(make-config-method vibration2 {:dir (drand m/TWO_PI)
                                :angle (drand m/TWO_PI)
                                :freq (srandom 0.01 2.0)
                                :amp (srandom 0.1 1.0)
                                :phase (drand)
                                :dir2 (drand m/TWO_PI)
                                :angle2 (drand m/TWO_PI)
                                :freq2 (srandom 0.01 2.0)
                                :amp2 (srandom 0.1 1.0)
                                :phase2 (drand)
                                :dm (drand -0.5 0.5)
                                :dmfreq (srandom 0.01 1.0)
                                :tm (drand -0.5 0.5)
                                :tmfreq (srandom 0.01 1.0)
                                :fm (drand -0.5 0.5)
                                :fmfreq (srandom 0.01 1.0)
                                :am (drand -0.5 0.5)
                                :amfreq (srandom 0.01 1.0)
                                :d2m (drand -0.5 0.5)
                                :d2mfreq (srandom 0.01 1.0)
                                :t2m (drand -0.5 0.5)
                                :t2mfreq (srandom 0.01 1.0)
                                :f2m (drand -0.5 0.5)
                                :f2mfreq (srandom 0.01 1.0)
                                :a2m (drand -0.5 0.5)
                                :a2mfreq (srandom 0.01 1.0)})

(defn- v-modulate 
  "Modulate"
  ^double [^double amp ^double freq ^double x]
  (* amp (m/cos (* x freq m/TWO_PI))))

(defn make-vibration2
  "Vibration 2 http://fractal-resources.deviantart.com/art/Apo-Plugins-Vibration-1-and-2-252001851"
  [^double amount {:keys [^double dir ^double angle ^double freq ^double amp ^double phase
                          ^double dir2 ^double angle2 ^double freq2 ^double amp2 ^double phase2
                          ^double dm ^double dmfreq
                          ^double tm ^double tmfreq
                          ^double fm ^double fmfreq
                          ^double am ^double amfreq
                          ^double d2m ^double d2mfreq
                          ^double t2m ^double t2mfreq
                          ^double f2m ^double f2mfreq
                          ^double a2m ^double a2mfreq]}]
  (let [cdir (m/cos dir)
        sdir (m/sin dir)
        cdir2 (m/cos dir2)
        sdir2 (m/sin dir2)]
    (fn [^Vec2 v]
      (let [d-along-dir (+ (* (.x v) cdir)
                           (* (.y v) sdir))
            dir-l (+ dir (v-modulate dm dmfreq d-along-dir))
            angle-l (+ angle (v-modulate tm tmfreq d-along-dir))
            freq-l (/ (v-modulate fm fmfreq d-along-dir) freq)
            amp-l (+ amp (* amp (v-modulate am amfreq d-along-dir)))
            total-angle (+ angle-l dir-l)
            cos-dir (m/cos dir-l)
            sin-dir (m/sin dir-l)
            cos-tot (m/cos total-angle)
            sin-tot (m/sin total-angle)
            scaled-freq (* m/TWO_PI freq)
            phase-shift (/ (* m/TWO_PI phase) freq)
            d-along-dir (+ (* (.x v) cos-dir)
                           (* (.y v) sin-dir))
            local-amp (* amp-l (m/sin (+ (* d-along-dir scaled-freq) freq-l phase-shift)))
            x (+ (.x v) (* local-amp cos-tot))
            y (+ (.y v) (* local-amp sin-tot))

            d-along-dir (+ (* (.x v) cdir2)
                           (* (.y v) sdir2))
            dir-l (+ dir2 (v-modulate d2m d2mfreq d-along-dir))
            angle-l (+ angle2 (v-modulate t2m t2mfreq d-along-dir))
            freq-l (/ (v-modulate f2m f2mfreq d-along-dir) freq2)
            amp-l (+ amp2 (* amp2 (v-modulate a2m a2mfreq d-along-dir)))
            total-angle (+ angle-l dir-l)
            cos-dir (m/cos dir-l)
            sin-dir (m/sin dir-l)
            cos-tot (m/cos total-angle)
            sin-tot (m/sin total-angle)
            scaled-freq (* m/TWO_PI freq2)
            phase-shift (/ (* m/TWO_PI phase2) freq2)
            d-along-dir (+ (* (.x v) cos-dir)
                           (* (.y v) sin-dir))
            local-amp (* amp-l (m/sin (+ (* d-along-dir scaled-freq) freq-l phase-shift)))
            x (+ x (* local-amp cos-tot))
            y (+ y (* local-amp sin-tot))]
        (Vec2. (* amount x)
               (* amount y))))))
(make-var-method vibration2 :regular)

;; ### Voron

(make-config-method voron {:k (srandom 0.6 1.3)
                           :step (srandom 0.1 1.2)
                           :num (drand 0.1 25.0)
                           :xseed (irand)
                           :yseed (irand)})

(deftype VoronResType [^double R ^double X0 ^double Y0])
(deftype VoronCalcType [^long M1 ^long N1 ^long k])

(defn make-voron
  "Voron by eralex61, http://eralex61.deviantart.com/art/Voronoi-Diagram-plugin-153126702"
  [^double amount {:keys [^double k ^double step ^double num ^int xseed ^int yseed]}]
  (fn [^Vec2 v]
    (let [fk (fn ^VoronCalcType [^long M1 ^long N1]
               (VoronCalcType. M1 N1
                               (long (inc (m/floor (* (discrete-noise (+ (+ (* M1 19) (* N1 257)) xseed) 0) num))))))
          m (long (m/floor (/ (.x v) step)))
          n (long (m/floor (/ (.y v) step)))
          m- (dec m)
          m+ (inc m)
          n- (dec n)
          n+ (inc n)
          Ks (mapv fk [m- m- m- m m m m+ m+ m+] [n- n n+ n- n n+ n- n n+])
          ^VoronResType res (reduce (fn [^VoronResType curr ^VoronCalcType calc]
                                      (loop [i (long 0)
                                             ^VoronResType currl curr]
                                        (if (< i (.k calc))
                                          (let [X (* step (+ (double (.M1 calc)) (discrete-noise (+
                                                                                                  (+ i (* 64 (.M1 calc)))
                                                                                                  (+ xseed (* 15 (.N1 calc)))) 0)))
                                                Y (* step (+ (double (.N1 calc)) (discrete-noise (+
                                                                                                  (+ i (* 21 (.M1 calc)))
                                                                                                  (+ yseed (* 33 (.N1 calc)))) 0)))
                                                R (m/hypot (- (.x v) X) (- (.y v) Y))]
                                            (recur (unchecked-inc i)
                                                   (if (< R (.R currl))
                                                     (VoronResType. R X Y)
                                                     currl)))
                                          currl))) (VoronResType. 20.0 0.0 0.0) Ks)]
      (Vec2. (* amount (+ (.X0 res) (* k (- (.x v) (.X0 res)))))
             (* amount (+ (.Y0 res) (* k (- (.y v) (.Y0 res)))))))))
(make-var-method voron :regular)

;; ### Waves

(make-config-method waves {:coeff10 (drand -2.0 2.0)
                           :coeff11 (drand -2.0 2.0)
                           :coeff20 (drand -2.0 2.0)
                           :coeff21 (drand -2.0 2.0)})

(defn make-waves
  "Waves"
  [^double amount {:keys [^double coeff10 ^double coeff11 ^double coeff20 ^double coeff21]}]
  (let [c202 (+ m/EPSILON (m/sq coeff20))
        c212 (+ m/EPSILON (m/sq coeff21))]
    (fn [^Vec2 v]
      (Vec2. (->> c202
                  (/ (.y v))
                  (m/sin)
                  (* coeff10)
                  (+ (.x v))
                  (* amount))
             (->> c212
                  (/ (.x v))
                  (m/sin)
                  (* coeff11)
                  (+ (.y v))
                  (* amount))))))
(make-var-method waves :regular)

;; ### Wedge

(make-config-method wedge {:angle (drand m/TWO_PI)
                           :hole (drand -2 2)
                           :count (drand -5 5)
                           :swirl (drand -2 2)})

(defn make-wedge
  "Wedge"
  [^double amount {:keys [^double angle ^double hole ^double count ^double swirl]}]
  (let [hm1p (* m/M_1_PI 0.5)]
    (fn [v]
      (let [^double r (v/mag v)
            a (+ ^double (v/heading v) (* r swirl))
            c (m/floor (* (+ (* count a) m/PI) hm1p))
            comp-fac (- 1.0 (* angle count hm1p))
            a (+ (* a comp-fac) (* c angle))
            r (* amount (+ r hole))]
        (Vec2. (* r (m/cos a))
               (* r (m/sin a)))))))
(make-var-method wedge :regular)


;; ## Additional variations
;;
;; https://github.com/d3/d3-geo-projection/tree/master/src

;; ### Miller

(defn make-miller
  "Miller"
  [^double amount _]
  (fn [^Vec2 v]
    (v/mult (Vec2. (.x v)
                   (->> (m/constrain (.y v) -1.9634 1.9634)
                        (* 0.4)
                        (+ m/QUARTER_PI)
                        (m/tan)
                        (m/log)
                        (* 1.25))) amount)))
(make-var-method miller :regular)

(defn make-millerrev
  "Millerrev"
  [^double amount _]
  (fn [^Vec2 v]
    (v/mult (Vec2. (.x v)
                   (-> (.y v)
                       (* 0.8)
                       (m/exp)
                       (m/atan)
                       (* 2.5)
                       (- (* 0.625 m/PI)))) amount)))
(make-var-method millerrev :regular)

;; ### Foucaut

(defn make-foucaut
  "Foucaut"
  [^double amount _]
  (fn [^Vec2 v]
    (let [k (* 0.5 (.y v))
          cosk (m/cos k)
          xx (->> cosk
                  (* cosk)
                  (* (m/cos (.y v)))
                  (* (/ (.x v) m/SQRTPI))
                  (* 2.0)
                  (* amount))
          yy (* amount m/SQRTPI (m/tan k))]
      (Vec2. xx yy))))
(make-var-method foucaut :regular)

;; ## Lists

;; List of variations based on RNG
(def variation-list-random @random-var)

;; List of variations not random
(def variation-list-not-random @regular-var)

;; list of all variations defined in the file
(def variation-list (concat variation-list-random variation-list-not-random))

;; ## Function arithmetic
;;
;; Using following functions you can create various combination of the functions and derive another ones. Configuration schema goes as follows:
;;
;; Each variation is described by 
;;
;; * `{:type :variation :name NAME :amount AMOUNT :config CONFIG}` where
;;   * NAME is variation name (keyword)
;;   * AMOUNT is scaling factor
;;   * CONFIG is variation configuration
;; * `{:type :operation :name OPERATION :amount AMOUNT :var1 VAR1 :var2 VAR2} where
;;   * OPERATION is one of the operations (see below)
;;   * AMOUNT is scaling factor
;;   * VAR1 and VAR2 two variations to combine
;; * `{:type :operation :name :derivative :amount AMOUNT :var VAR :step STEP} where
;;   * AMOUNT is scaling factor
;;   * VAR variation, subject to calculate derivative
;;   * STEP dx and dy value
;;
;; Possible OPERATIONs are:
;;
;; * `:add` - sum of two variations
;; * `:mult` - multiplication
;; * `:comp` - composition
;;
;; Configuration form a tree and is base for creating new variation function
;;
;; To get random configuration call `make-random-configuration`
;; `=> {:type :operation, :name :add, :var1 {:type :variation, :name :hemisphere, :amount 0.7030921730331494, :config {}}, :var2 {:type :variation, :name :secant2, :amount -0.969787677636873, :config {}}, :amount 0.5977715611790528}`
;;
;; If you want to keep structure and operations and change only variation configs, step (for derivative) and amount call `randomize-parametrization`
;;
;; `(def c (make-random-configuration 1))`
;; `(randomize-parametrization c)`
;; `=> {:type :operation, :name :add, :var1 {:type :variation, :name :stwin, :amount 1.931203996559876, :config {:distort -1.9879093574070819, :multiplier -1.1430930986892986}}, :var2 {:type :variation, :name :miller, :amount 1.2175172903222724, :config {}}, :amount 0.3175892398498681}`
;; `(randomize-parametrization c)`
;; `=> {:type :operation, :name :add, :var1 {:type :variation, :name :stwin, :amount 0.9983039843886115, :config {:distort 1.336493697754337, :multiplier -1.2367301725837616}}, :var2 {:type :variation, :name :miller, :amount -1.6972368966112605, :config {}}, :amount 0.37098305837196743}`
;;
;; To create variation function based on configuration call `make-combination` function and pass configuration. 

;; Set below to true if you want to avoid variations based on RNG
(def ^:dynamic *skip-random-variations* false)

(defn- derivative
  "Calculate derivative of fn using a step."
  ([f ^double amount ^double a]
   (let [^Vec2 d (Vec2. a a)]
     (fn [^Vec2 v]
       (let [v1 (f v)
             v2 (f (v/add v d))]
         (v/mult (v/div (v/sub v2 v1) a) amount)))))
  ([f amount]
   (derivative f amount 0.001))
  ([f]
   (derivative f 1.0 0.001)))

(defn- build-random-variation-step
  "Create variation configuration"
  []
  (let [n (rand-nth (if *skip-random-variations* variation-list-not-random variation-list))]
    {:type :variation :name n :amount 1.0 :config (make-configuration n)}))

(defn- build-random-configuration-step
  "Create configuration tree"
  ([f1 f2]
   (let [operand (rand-nth [:comp :add :comp :add :comp :mult :comp])]
     {:type :operation :name operand :var1 f1 :var2 f2}))
  ([f]
   (randval 0.1 f
            (randval 0.1
                     {:type :operation :name :deriv :var f}
                     (build-random-configuration-step f (build-random-variation-step)))))
  ([]
   (build-random-configuration-step (build-random-variation-step) (build-random-variation-step))))

(defn randomize-parametrization
  "Randomize parametrization: amounts, step (for derivative) and variation config. Keep structure untouched."
  ([f]
   (if (= (:type f) :variation)
     (assoc f :amount 1.0 :config (make-configuration (:name f) {}))
     (let [name (:name f)]
       (if (= name :deriv)
         (assoc f :amount 1.0 :step (m/sq (drand 0.01 1.0)) :var (randomize-parametrization (:var f)))
         (let [amount1 (if (= name :comp) 1.0 (drand -2.0 2.0))
               amount2 (if (= name :comp) 1.0 (drand -2.0 2.0)) 
               amount (case name
                        :add (/ 1.0 (+ (m/abs amount1) (m/abs amount2)))
                        :mult (/ 1.0 (* amount1 amount2))
                        :comp 1.0)]
           (assoc f :amount amount
                  :var1 (assoc (randomize-parametrization (:var1 f)) :amount amount1)
                  :var2 (assoc (randomize-parametrization (:var2 f)) :amount amount2))))))))

(defn make-random-configuration
  "Create random configuration. Optionally with depth passed (0 = only root is created)."
  ([] (make-random-configuration (lrand 5)))
  ([depth] (make-random-configuration depth (build-random-variation-step)))
  ([^long depth f]
   (if (pos? depth)
     (make-random-configuration (dec depth) (randomize-parametrization (build-random-configuration-step f)))
     f)))

(defn make-combination
  "Parse configuration and return new variation function."
  ([{:keys [type name amount config var step var1 var2]}]
   (if (= type :variation)
     (make-variation name amount config)
     (if (= name :deriv)
       (derivative (make-combination var) amount step)
       (let [v1 (make-combination var1)
             v2 (make-combination var2)]
         (case name
           :comp (fn ^Vec2 [^Vec2 v] (v/mult (v1 (v2 v)) amount))
           :add (fn ^Vec2 [^Vec2 v] (v/mult (v/add (v1 v) (v2 v)) amount))
           :mult (fn ^Vec2 [^Vec2 v] (v/mult (v/emult (v1 v) (v2 v)) amount)))))))
  ([] (make-combination (make-random-configuration))))
