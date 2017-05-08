;; ## Variations
;;
;; Vec2 -> Vec2 functions used in fractal flames rendering as variations.  
;; Code is taken from [jWildfire](https://github.com/tsulej/JWildfire/tree/master/src/org/jwildfire/create/tina/variation).
;;
;;  More on this:
;;
;; * [Folds explained](https://generateme.wordpress.com/2016/04/11/folds/)
;; * [Folds page](http://folds2d.tumblr.com/)
;; * examples/ex00-variations
;;
;; List of currently implemented variations is in the sequence `variation-list`
;;
(ns clojure2d.extra.variations
  "Variations namespace"
  (:require [clojure2d.math :refer :all]
            [clojure2d.math.random :refer :all]
            [clojure2d.math.vector :as v]
            [clojure2d.math.complex :as c]
            [clojure2d.math :as m]
            [clojure2d.math.random :as r])
  (:import [clojure2d.math.vector Vec2]
           [clojure2d.math.complex Complex]
           [org.apache.commons.math3.special Gamma Beta Erf BesselJ]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; Every variation consist of variation configuration and function itself.
;;
;; Configuration is a map with named values passed later when creating variation
;; Map can be empty or nil if you want random parametrization or function don't have parametrization.
;;
;; To create configuration call `make-configuration :name configuration-map`.  
;; For example:
;;
;; `(make-configuration :auger nil)`  
;; `=> {:freq 1.9878973615162039, :weight 0.21075539658278108, :sym 1.7967705799271059, :scale -0.6747741927127615}`
;; 
;; `(make-configuration :auger {:freq 2.0 :sym 1.5})`  
;; `=> {:freq 2.0, :weight 0.4927899160909188, :sym 1.5, :scale -0.6046162305689989}`
;;
;; Some configurations precalculate additional parameters used in variations:  
;; `(make-configuration :emod {:distance 2 :radius 2})`  
;; `=> {:radius 2, :distance 2, :radius2 4.0, :rdr 6}`
;; 
;; Parameters are listed in `config-` functions.  
;; Default configuration is an empty map
;; Function returns map with full and precalculated configuration necessary to create variation. In case when some f the keys are missing, random value is created.
;;
;; Note: this function is called implicitly when you create variation with `make-variation`. Call it explicitly when you want to record or reuse configuration.
(defmulti make-configuration (fn [key _] key))
(defmethod make-configuration :default [_ _] {})

;; To obtain variation function with given configuration you have to call `make-variation` multimethod.
;;
;; `make-variation` accepts three attributes:
;;
;; * function name (as a key, see the list above)
;; * scale value, used to scale result
;; * default configuration map, can be empty or nil (in this case random configuration is created)
;;
;; `(def auger-1 (make-variation :auger 1.0 {}))`
;; `(auger-1 (Vec2. 1.0 1.0))`  
;; `=> #clojure2d.utils.vector.Vec2{:x 1.108066895814639, :y 1.1378531686602176}`
(defmulti make-variation (fn [key _ _] key))
(defmethod make-variation :default [_ _ _] (fn [^Vec2 v] v))

;; Two following macros help to generate proper `make-configuration` and `make-variation` multimethods for given variation name. They are used internally.
(defmacro make-config-method
  "Add new multimethod for variation configuration"
  [sym m]
  (let [k (keyword sym)
        s (symbol (str "config-" sym))]
    `(defmethod make-configuration ~k [k# p#] (merge ~m p#))))

(defmacro make-var-method
  "Add new multimethod for variation factory function"
  [sym t]
  (let [k (keyword sym)
        m (symbol (str "make-" sym))]
    `(do (defmethod make-variation ~k [k# a# p#] (~m a# (make-configuration ~k p#)))
         ~(if (= t :regular)
            `(register-regular-var ~k)
            `(register-random-var ~k)))))

;; Locally used random function for some configuration parameters. Mostly used to avoid `0` value.
(defn- srandom
  "Symetric random from [-mx -mn] and [mn mx]"
  ^double  [^double mn ^double mx]
  (let [^double rand (drand mn mx)]
    (if (brand)
      rand
      (* -1 rand))))

(def ^Vec2 unitx (Vec2. 1.0 0.0))
(def ^Vec2 zerov (Vec2. 0.0 0.0))

;; Two atoms to register names. One for non-random functions and second for random

(def random-var (atom []))
(def regular-var (atom [:default]))

(defn- register-var
  "Add `name` to the atom `what`"
  [what name]
  (swap! what conj name))

(def register-random-var (partial register-var random-var))
(def register-regular-var (partial register-var regular-var))

;; ## A
;;
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
          s (sin (* freq x))
          t (sin (* freq y))
          dy (+ y (* weight (+ (abs y) (* 0.5 s scale)) s))
          dx (+ x (* weight (+ (abs x) (* 0.5 t scale)) t))
          xx (* amount (+ x (* sym (- dx x))))
          yy (* amount dy)]
      (Vec2. xx yy))))
(make-var-method auger :regular)

;; ### Arch

(defn make-arch
  ""
  [^double amount _]
  (fn [^Vec2 v]
    (let [ang (* amount ^double (drand PI))
          sinr (sin ang)
          cosr (cos ang)]
      (if (zero? cosr) zerov
          (Vec2. (* amount sinr)
                 (* amount (/ (sq sinr) cosr)))))))
(make-var-method arch :random)

;; ## B

;; ### bCollide
(make-config-method bcollide {:num (srandom 1 30)
                              :a (drand 2)})

(defn make-bcollide
  "bCollide by Michael Faber, http://michaelfaber.deviantart.com/art/bSeries-320574477"
  [^double amount {:keys [^double num ^double a]}]
  (let [bcn-pi (* num M_1_PI)
        pi-bcn (/ PI num)
        bca-bcn (/ (* PI a) num)]
    (fn [^Vec2 v]
      (let [v+ (v/add v unitx)
            v- (Vec2. (- 1.0 (.x v)) (.y v))
            tau (* 0.5 (- (log (v/magsq v+))
                          (log (v/magsq v-))))
            pre-sigma (- PI ^double (v/heading v+) ^double (v/heading v-))
            alt (int (* pre-sigma bcn-pi))
            sigma (if (even? alt)
                    (+ (* alt pi-bcn) (rem (+ pre-sigma bca-bcn) pi-bcn))
                    (+ (* alt pi-bcn) (rem (- pre-sigma bca-bcn) pi-bcn)))
            sinht (sinh tau)
            cosht (cosh tau)
            sins (sin sigma)
            coss (cos sigma)
            temp (/ 1.0 (- cosht coss))
            xx (* amount sinht temp)
            yy (* amount sins temp)]
        (Vec2. xx yy)))))
(make-var-method bcollide :regular)

;; ### Blob
(make-config-method blob {:low (drand -2.0 2.0)
                          :high (drand -2.0 2.0)
                          :waves (drand -6.0 6.0)})

(defn make-blob
  ""
  [^double amount {:keys [^double low ^double high ^double waves]}]
  (let [hl (- high low)]
    (fn [^Vec2 v]
      (let [^double a (v/heading v)
            ^double r (v/mag v)
            rr (->> (* a waves)
                    sin
                    (* 0.5)
                    (+ 0.5)
                    (* hl)
                    (+ low)
                    (* r))]
        (Vec2. (* amount rr (sin a))
               (* amount rr (cos a)))))))
(make-var-method blob :regular)

;; ### bSwirl
(make-config-method bswirl {:in (drand -2.0 2.0)
                            :out (drand -2.0 2.0)})

(defn make-bswirl
  "bSwirl by Michael Faber, http://michaelfaber.deviantart.com/art/bSeries-320574477"
  [^double amount {:keys [^double in ^double out]}]
  (fn [^Vec2 v]
    (let [v+ (v/add v unitx)
          v- (Vec2. (- 1.0 (.x v)) (.y v))
          tau (* 0.5 (- (log (v/magsq v+))
                        (log (v/magsq v-))))
          pre-sigma (- PI ^double (v/heading v+) ^double (v/heading v-))
          sigma (+ pre-sigma (* tau out) (/ in tau))
          sinht (sinh tau)
          cosht (cosh tau)
          sins (sin sigma)
          coss (cos sigma)
          temp (- cosht coss)]
      (if (zero? temp)
        (Vec2. 0.0 0.0)
        (Vec2. (* amount (/ sinht temp))
               (* amount (/ sins temp)))))))
(make-var-method bswirl :regular)

;; ### BesselJ
(defn make-besselj
  ""
  [^double amount _]
  (fn [^Vec2 v]
    (Vec2. (* amount ^double (v/mag v) (BesselJ/value (abs (.x v)) (abs (.y v))))
           (* amount ^double (v/heading v)))))
(make-var-method besselj :regular)

;; ### Beta
(defn make-beta
  ""
  [^double amount _]
  (fn [^Vec2 v]
    (Vec2. (* amount (Beta/logBeta (+ EPSILON (abs (.x v))) (+ EPSILON (abs (.y v)))))
           (* amount ^double (v/heading v)))))
(make-var-method beta :regular)

;; ## Bent
(defn make-bent
  ""
  [^double amount _]
  (fn [^Vec2 v]
    (let [nx (if (neg? (.x v)) (+ (.x v) (.x v)) (.x v))
          ny (if (neg? (.y v)) (* (.y v) 0.5) (.y v))]
      (Vec2. (* amount nx)
             (* amount ny)))))
(make-var-method bent :regular)

;; ## Blade
(defn make-blade
  ""
  [^double amount _]
  (fn [^Vec2 v]
    (let [r (* ^double (drand amount) ^double (v/mag v))
          sinr (sin r)
          cosr (cos r)]
      (Vec2. (* amount (.x v) (+ cosr sinr))
             (* amount (.x v) (- cosr sinr))))))
(make-var-method blade :random)

(defn make-blade2
  ""
  [^double amount _]
  (fn [^Vec2 v]
    (let [r (* ^double (drand amount) ^double (v/mag v))
          sinr (sin r)
          cosr (cos r)]
      (Vec2. (* amount (.x v) (+ cosr sinr))
             (* amount (.y v) (- cosr sinr))))))
(make-var-method blade2 :random)

;; ## Boarders
(defn make-boarders
  ""
  [^double amount _]
  (fn [^Vec2 v]
    (let [roundx (rint (.x v))
          roundy (rint (.y v))
          offsetx (- (.x v) roundx)
          offsety (- (.y v) roundy)
          hoffsetx (* 0.5 offsetx)
          hoffsety (* 0.5 offsety)]
      (if (brand 0.75)
        (Vec2. (* amount (+ roundx hoffsetx))
               (* amount (+ roundy hoffsety)))
        (if (>= (abs offsetx) (abs offsety))
          
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

;; ### Bubble
(defn make-bubble
  ""
  [^double amount _]
  (fn [^Vec2 v]
    (v/mult v (/ amount (inc (* 0.25 ^double (v/mag v)))))))
(make-var-method bubble :regular)


;; ### Butterfly
(defn make-butterfly
  ""
  [^double amount _]
  (fn [^Vec2 v]
    (let [wx (* amount 1.3029400317411197908970256609023)
          y2 (* 2.0 (.y v))
          r (* wx (sqrt (/ (abs (* (.y v) (.x v)))
                           (+ EPSILON (sq (.x v)) (sq y2)))))]
      (Vec2. (* r (.x v))
             (* r y2)))))
(make-var-method butterfly :regular)

;; ## C

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
                   (floor)
                   (long))
            N (->> Sc
                   (/ (.y v))
                   (* 0.5)
                   (floor)
                   (long))
            X (- (.x v) (->> M
                             (* 2.0)
                             (inc)
                             (* Sc)))
            Y (- (.y v) (->> N
                             (* 2.0)
                             (inc)
                             (* Sc)))
            U (hypot X Y)
            V (->> (discrete-noise (+ M 10) (+ N 3))
                   (* 0.7)
                   (+ 0.3)
                   (* Sc))
            Z1 (discrete-noise (+ M Seed) N)
            [^double XX ^double YY] (if (and (< Z1 Dens1) (< U V)) 
                                      (if (pos? Reverse)
                                        (if (< Z1 dd)
                                          [(* K X) (* K Y)]
                                          (let [Z (->> K
                                                       (- 1.0)
                                                       (* U)
                                                       (/ V)
                                                       (+ K))]
                                            [(* Z X) (* Z Y)]))
                                        (if (> Z1 dd)
                                          [(* K X) (* K Y)]
                                          (let [Z (->> K
                                                       (- 1.0)
                                                       (* U)
                                                       (/ V)
                                                       (+ K))]
                                            [(* Z X) (* Z Y)])))
                                      [X Y])]
        (Vec2. (->> 2.0
                    (* M)
                    (inc)
                    (* Sc)
                    (+ XX)
                    (* amount))
               (->> 2.0
                    (* N)
                    (inc)
                    (* Sc)
                    (+ YY)
                    (* amount)))))))
(make-var-method circlelinear :regular)

;; ### Cosine
(defn make-cosine
  ""
  [^double amount _]
  (fn [^Vec2 v]
    (let [r (* PI (.x v))]
      (Vec2. (* amount (cos r) (cosh (.y v)))
             (- (* amount (sin r) (sinh (.y v))))))))
(make-var-method cosine :regular)

;; ### cross
(defn make-cross
  ""
  [^double amount _]
  (fn [^Vec2 v]
    (let [s (- (sq (.x v)) (sq (.y v)))
          r (* amount (sqrt (/ 1.0 (+ EPSILON (* s s)))))]
      (Vec2. (* (.x v) r) (* (.y v) r)))))
(make-var-method cross :regular)

;; ### csin
(make-config-method csin {:stretch (drand -3 3)})

(defn make-csin
  "CSin by zephyrtronium, http://fractal-resources.deviantart.com/art/CSin-Apophysis-Plugin-158332287"
  [^double amount {:keys [^double stretch]}]
  (let [s-cx (Complex. stretch 0.0)]
    (fn [^Vec2 v]
      (v/mult (->> (c/from-vec2 v)
                   (c/mult s-cx)
                   (c/sin)
                   (c/to-vec2)) amount))))
(make-var-method csin :regular)

;; ### Cayley transform

(defn make-cayley
  "Cayley transform"
  [^double amount _]
  (fn [^Vec2 v]
    (if (== (.y v) -1.0)
      zerov
      (let [^Complex c (c/from-vec2 v)]
        (c/to-vec2 (c/div (c/add c c/I-)
                          (c/add c c/I)))))))
(make-var-method cayley :regular)

;; ### Cylinder
(defn make-cylinder
  ""
  [^double amount _]
  (fn [^Vec2 v]
    (v/mult (Vec2. (sin (.x v)) (.y v)) amount)))
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
          sinr (sin length)
          cosr (cos length)]
      (Vec2. (* amount sina cosr)
             (* amount cosa sinr)))))
(make-var-method diamond :regular)

;; ### Disc

(defn make-disc
  ""
  [^double amount _]
  (let [api (/ amount PI)]
    (fn [^Vec2 v]
      (let [rpi (* PI ^double (v/mag v))
            sinr (sin rpi)
            cosr (cos rpi)
            r (* api ^double (v/heading v))]
        (Vec2. (* r sinr) (* r cosr))))))
(make-var-method disc :regular)


;; ## E

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
            xmax-pre (* 0.5 (+ (safe-sqrt (+ tmp tmp2))
                               (safe-sqrt (- tmp tmp2))))
            xmax (constrain xmax-pre 1.0 xmax-pre)
            t (constrain (/ (.x v) xmax) -1.0 1.0)
            nu-pre (acos t)
            nu (if (neg? (.y v)) (* -1.0 nu-pre) nu-pre)
            mu-pre (acosh xmax)          
            mu (if (and (< mu-pre radius) (< (* -1.0 mu-pre) radius))
                 (if (pos? nu)
                   (- (rem (+ mu-pre rdr) radius2) radius)
                   (+ (rem (- mu-pre rdr) radius2) radius))
                 mu-pre)
            xx (* amount (cosh mu) (cos nu))
            yy (* amount (sinh mu) (sin nu))]
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
  ""
  [^double amount _]
  (fn [^Vec2 v]
    (Vec2. (* amount (Erf/erf (.x v) (.y v)))
           (* amount ^double (v/heading v)))))
(make-var-method erf :regular)

;; ### Elliptic
(defn make-elliptic
  ""
  [^double amount _]
  (let [-a (/ amount HALF_PI)]
    (fn [^Vec2 v]
      (let [tmp (inc ^double (v/magsq v))
            x2 (+ (.x v) (.x v))
            xmax (* 0.5 (+ (sqrt (+ tmp x2)) (sqrt (- tmp x2))))
            a (/ (.x v) xmax)
            b (safe-sqrt (- 1.0 (* a a)))
            l (log (+ xmax (safe-sqrt (dec xmax))))
            x (* -a (atan2 a b)) 
            y (if (r/brand)
                (* -a l)
                (- (* -a l)))]
        (Vec2. x y)))))
(make-var-method elliptic :random)

;; ### Ex

(defn make-ex
  ""
  [^double amount _]
  (fn [^Vec2 v]
    (let [^double r (v/mag v)
          ^double h (v/heading v)
          n0 (sin (+ h r))
          n1 (cos (- h r))
          m0 (* n0 n0 n0)
          m1 (* n1 n1 n1)
          ar (* amount r)]
      (Vec2. (* ar (+ m0 m1))
             (* ar (- m0 m1))))))
(make-var-method ex :regular)


;; ### Exp
(defn make-exp
  ""
  [^double amount _]
  (fn [^Vec2 v]
    (let [e (* amount (exp (.x v)))]
      (Vec2. (* e (cos (.y v)))
             (* e (sin (.y v)))))))
(make-var-method exp :regular)

;; ### Exponential
(defn make-exponential
  ""
  [^double amount _]
  (fn [^Vec2 v]
    (let [e (* amount (exp (dec (.x v))))
          r (* PI (.y v))]
      (Vec2. (* e (cos r))
             (* e (sin r))))))
(make-var-method exponential :regular)


;; ### Eyefish
(defn make-eyefish
  ""
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
  ""
  [^double amount {:keys [^double coeff20 ^double coeff21]}]
  (let [dx (+ EPSILON (* PI (sq coeff20)))
        dx2 (* 0.5 dx)]
    (fn [^Vec2 v]
      (let [^double angle (v/heading v)
            r (* amount ^double (v/mag v))
            ac (+ angle coeff21)
            a (if (> ^double (mod ac dx) dx2)
                (- angle dx2)
                (+ angle dx2))]
        (Vec2. (* r (cos a))
               (* r (sin a)))))))
(make-var-method fan :regular)


;; ### Fan2
(make-config-method fan2 {:x (drand -1 1)
                          :y (drand -1 1)})

(defn make-fan2
  ""
  [^double amount {:keys [^double x ^double y]}]
  (fn [^Vec2 v]
    (let [^double r (v/mag v)
          ^double angle (v/heading v)
          ac (+ angle y)
          dx (+ EPSILON (* PI x x))
          dx2 (* 0.5 dx)
          t (- ac (* dx (int (/ ac dx))))
          a (if (> t dx2)
              (- angle dx2)
              (+ angle dx2))]
      (Vec2. (* amount r (sin a))
             (* amount r (cos a))))))
(make-var-method fan2 :regular)

;; ### Fisheye
(defn make-fisheye
  ""
  [^double amount _]
  (fn [^Vec2 v]
    (let [r (/ (* amount 4.0) (inc ^double (v/mag v)))]
      (Vec2. (* r (.y v)) (* r (.x v))))))
(make-var-method fisheye :regular)


;; ### Foci
(defn make-foci
  ""
  [^double amount _]
  (fn [^Vec2 v]
    (let [expx (* 0.5 (exp (.x v)))
          expnx (/ 0.25 expx)
          sy (sin (.y v))
          cy (cos (.y v))
          tmp (- (+ expx expnx) cy)
          tmp (/ amount (if (zero? tmp) EPSILON tmp))]
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

;; ## H
;;

;; ### Heart
(defn make-heart
  ""
  [^double amount _]
  (fn [^Vec2 v]
    (let [^double r (v/mag v)
          ^double theta (v/heading v)
          rt (* r theta)
          sr (sin rt)
          cr (cos rt)]
      (Vec2. (* amount r sr) (- (* amount r cr))))))
(make-var-method heart :regular)

;; ### Handkerchief
(defn make-handkerchief
  ""
  [^double amount _]
  (fn [^Vec2 v]
    (let [^double angle (v/heading v)
          ^double r (v/mag v)]
      (Vec2. (* amount (* r (sin (+ angle r))))
             (* amount (* r (cos (- angle r))))))))
(make-var-method handkerchief :regular)


;; ### Hemisphere

(defn make-hemisphere
  ""
  [^double amount _]
  (fn [^Vec2 v]
    (let [r (/ amount (sqrt (inc ^double (v/magsq v))))]
      (Vec2. (* r (.x v))
             (* r (.y v))))))
(make-var-method hemisphere :regular)

(defn make-horseshoe
  ""
  [^double amount _]
  (fn [^Vec2 v]
    (let [r (+ EPSILON ^double (v/mag v))
          sina (/ (.x v) r)
          cosa (/ (.y v) r)]
      (Vec2. (* amount (- (* sina (.x v)) (* cosa (.y v))))
             (* amount (+ (* cosa (.x v)) (* sina (.y v))))))))
(make-var-method horseshoe :regular)

;; ### Hyperbolic

(defn make-hyperbolic
  ""
  [^double amount _]
  (fn [^Vec2 v]
    (let [r (+ EPSILON ^double (v/mag v))
          theta (v/heading v)]
      (Vec2. (/ (sin theta) r)
             (* (cos theta) r)))))
(make-var-method hyperbolic :regular)

;; ## J
;;
;; ### Julia
(defn make-julia
  "Julia"
  [^double amount _]
  (fn [^Vec2 v]
    (let [a (+ (* 0.5 ^double (v/heading v)) (* PI ^double (irand 2)))
          r (->> (v/mag v)
                 (sqrt)
                 (* amount))]
      (Vec2. (* r (cos a)) (* r (sin a))))))
(make-var-method julia :random)

;; ### JuliaC
(make-config-method juliac {:re (int (srandom 1.0 10.0))
                            :im (* 0.01 ^double (drand -2.0 2.0))
                            :dist (drand -2.0 2.0)})

(defn make-juliac
  ""
  [^double amount {:keys [^double re ^double im ^double dist]}]
  (let [rre (/ 1.0 re)]
    (fn [^Vec2 v]
      (let [arg (+ ^double (v/heading v)
                   (* TWO_PI ^double (mod ^int (irand) re)))
            lnmod (* dist (log (v/magsq v)))
            a (+ (* arg rre)
                 (* lnmod im))
            mod2 (* amount (exp (- (* lnmod rre)
                                   (* arg im))))]
        (Vec2. (* mod2 (cos a))
               (* mod2 (sin a)))))))
(make-var-method juliac :random)

;; ### JuliaN
(make-config-method julian (let [r (srandom 1 10)]
                             {:power (if (brand) r (int r))
                              :dist (drand -4 4)}))

(defn make-julian
  "JuliaN"
  [^double amount {:keys [^double power ^double dist]}]
  (let [abspower (int (abs power))
        cpower (* 0.5 (/ dist power))]
    (fn [^Vec2 v]
      (let [a (/ (+ ^double (v/heading v) (* TWO_PI ^int (irand abspower))) power)
            r (* amount (pow (v/magsq v) cpower))]
        (Vec2. (* r (cos a)) (* r (sin a)))))))
(make-var-method julian :random)

;; ### JuliaQ
(make-config-method juliaq {:power (int (srandom 1 10))
                            :divisor (srandom 1 8)})

(defn make-juliaq
  "juliaq by Zueuk, http://zueuk.deviantart.com/art/juliaq-Apophysis-plugins-340813357" 
  [^double amount {:keys [^double divisor ^double power]}]
  (let [inv-power (/ ^double divisor power)
        half-inv-power (* 0.5 inv-power)
        inv-power-2pi (/ TWO_PI power)]
    (fn [^Vec2 v]
      (let [a (+ (* inv-power ^double (v/heading v))
                 (* inv-power-2pi ^int (irand)))
            r (* amount (pow (v/magsq v) half-inv-power))]
        (Vec2. (* r (cos a)) (* r (sin a)))))))
(make-var-method juliaq :random)

;; ## L

;; ### Log

(defn make-log
  ""
  [^double amount _]
  (fn [^Vec2 v]
    (Vec2. (* amount 0.5 (log ^double (v/magsq v)))
           (* amount ^double (v/heading v)))))
(make-var-method log :regular)

;; ## N


;; ## P

;; ### Pie
(make-config-method pie {:slices (srandom 0.01 7.0)
                         :rotation (drand TWO_PI)
                         :thickness (drand -2.0 2.0)})

(defn make-pie
  "pie from jwildfire"
  [^double amount {:keys [^double slices ^double rotation ^double thickness]}]
  (fn [^Vec2 v]
    (let [sl (round (+ 0.5 (* slices ^double (drand))))
          a (-> thickness
                (* ^double (drand))
                (+ sl)
                (* TWO_PI)
                (/ slices)
                (+ rotation))
          r (* amount ^double (drand))]
      (Vec2. (* r (cos a))
             (* r (sin a))))))
(make-var-method pie :random)

;; ### PDJ
(make-config-method pdj {:a (drand -6.0 6.0)
                         :b (drand -6.0 6.0)
                         :c (drand -6.0 6.0)
                         :d (drand -6.0 6.0)})

(defn make-pdj
  ""
  [^double amount {:keys [^double a ^double b ^double c ^double d]}]
  (fn [^Vec2 v]
    (Vec2. (* amount (- (sin (* a (.y v))) (cos (* b (.x v)))))
           (* amount (- (sin (* c (.x v))) (cos (* d (.y v))))))))
(make-var-method pdj :regular)

;; ### Perspective
(make-config-method perspective {:angle (drand (- PI) PI)
                                 :dist (drand -5.0 5.0)})

(defn make-perspective
  ""
  [^double amount {:keys [^double angle ^double dist]}]
  (let [ang (* HALF_PI angle)
        vsin (sin ang)
        vfcos (* dist (cos ang))]
    (fn [^Vec2 v]
      (let [t (/ amount (- dist (* (.y v) vsin)))]
        (Vec2. (* t dist (.x v))
               (* t vfcos (.y v)))))))
(make-var-method perspective :regular)


;; ### Polar
(defn make-polar
  ""
  [^double amount _]
  (fn [^Vec2 v]
    (let [ny (dec ^double (v/mag v))]
      (Vec2. (* amount ^double (v/heading v) M_1_PI)
             (* amount ny)))))
(make-var-method polar :regular)

;; ### Polar2
(defn make-polar2
  ""
  [^double amount _]
  (let [p2v (/ amount PI)
        p2v2 (* 0.5 p2v)]
    (fn [^Vec2 v] (Vec2. (* p2v ^double (v/heading v)) (* p2v2 (log (v/magsq v)))))))
(make-var-method polar2 :regular)

;; ### Power
(defn make-power
  ""
  [^double amount _]
  (fn [^Vec2 v]
    (let [theta (v/heading v)
          sa (sin theta)
          ca (cos theta)
          pow (* amount (pow (v/mag v) sa))]
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
                  (tan)
                  (sin)
                  (* x)
                  (+ (.x v))
                  (* amount))
          yy (->> (.x v)
                  (* c)
                  (tan)
                  (sin)
                  (* y)
                  (+ (.y v))
                  (* amount))]
      (Vec2. xx yy))))
(make-var-method popcorn2 :regular)

;; ## R

;; ### Rectangles
(make-config-method rectangles {:x (drand -1.5 1.5)
                                :y (drand -1.5 1.5)})

(defn make-rectangles
  ""
  [^double amount {:keys [^double x ^double y]}]
  (fn [^Vec2 v]
    (Vec2. (if (< (abs (.x v)) EPSILON)
             (* amount (.x v))
             (* amount (-> (.x v)
                           (/ x)
                           floor
                           (* 2.0)
                           inc
                           (* x)
                           (- (.x v)))))
           (if (< (abs (.y v)) EPSILON)
             (* amount (.y v))
             (* amount (-> (.y v)
                           (/ y)
                           floor
                           (* 2.0)
                           inc
                           (* y)
                           (- (.y v))))))))
(make-var-method rectangles :regular)

;; ### Rings
(make-config-method rings {:coeff20 (drand 1.3)})

(defn make-rings
  ""
  [^double amount {:keys [^double coeff20]}]
  (let [dx (+ EPSILON (sq coeff20))
        dx2 (+ dx dx)
        rdx (/ 1.0 dx2)
        dx- (- 1.0 dx)]
    (fn [^Vec2 v]
      (let [^double r (v/mag v)
            rr (+ (- r (* dx2 (int (* (+ r dx) rdx)))) (* r dx-))]
        (Vec2. (* rr (/ (.x v) r))
               (* rr (/ (.y v) r)))))))
(make-var-method rings :regular)

;; ### Rings2
(make-config-method rings2 {:val (drand -1.0 1.0)})

(defn make-rings2
  ""
  [^double amount {:keys [^double val]}]
  (let [dx (+ EPSILON (sq val))]
    (fn [^Vec2 v]
      (let [^double l (v/mag v)
            r (* amount (- 2.0 (* dx (inc (/ (* 2.0 (int (* 0.5 (inc (/ l dx))))) l)))))]
        (v/mult v r)))))
(make-var-method rings2 :regular)

;; ## S

;; ### Scry

(defn make-scry
  ""
  [^double amount _]
  (fn [^Vec2 v]
    (let [^double t (v/magsq v)
          d (-> 1.0
                (/ amount)
                (+ t)
                (* (sqrt t))
                (+ EPSILON))
          r (/ 1.0 d)]
      (v/mult v r))))
(make-var-method scry :regular)

;; ### Sinusoidal
(defn make-sinusoidal
  "Sinusoidal"
  [^double amount _]
  (fn [^Vec2 v]
    (Vec2. (* amount (sin (.x v))) (* amount (sin (.y v))))))
(make-var-method sinusoidal :regular)

;; ### Secant
(defn make-secant2
  ""
  [^double amount _]
  (fn [^Vec2 v]
    (let [r (* amount ^double (v/mag v))
          cr (cos r)
          icr (/ 1.0 (if (zero? cr) EPSILON cr))
          ny (if (neg? cr)
               (* amount (inc cr))
               (* amount (dec cr)))]
      (Vec2. (* amount (.x v)) ny))))
(make-var-method secant2 :regular)

;; ### Spherical
(defn make-spherical
  ""
  [^double amount _]
  (fn [^Vec2 v]
    (v/mult v (/ amount (+ EPSILON ^double (v/magsq v))))))
(make-var-method spherical :regular)

;; ### Spiral
(defn make-spiral
  ""
  [^double amount _]
  (fn [^Vec2 v]
    (let [r (+ EPSILON ^double (v/mag v))
          revr (/ 1.0 r)
          sina (* (.x v) revr)
          cosa (* (.y v) revr)
          sinr (sin r)
          cosr (cos r)]
      (Vec2. (* amount revr (+ cosa sinr))
             (* amount revr (- sina cosr))))))
(make-var-method spiral :regular)


;; ### Split
(make-config-method split {:xsplit (* PI ^double (drand -2.0 2.0))
                           :ysplit (* PI ^double (drand -2.0 2.0))})

(defn make-split
  ""
  [^double amount {:keys [^double xsplit ^double ysplit]}]
  (fn [^Vec2 v]
    (Vec2. (if (pos? (cos (* (.x v) xsplit)))
             (* amount (.y v))
             (- (* amount (.y v))))
           (if (pos? (cos (* (.y v) ysplit)))
             (* amount (.x v))
             (- (* amount (.x v)))))))
(make-var-method split :regular)

;; ### Splits
(make-config-method splits {:x (drand -1.5 1.5)
                            :y (drand -1.5 1.5)})

(defn make-splits
  ""
  [^double amount {:keys [^double x ^double y]}]
  (fn [^Vec2 v]
    (Vec2. (if (pos? (.x v))
             (* amount (+ (.x v) x))
             (* amount (- (.x v) x)))
           (if (pos? (.y v))
             (* amount (+ (.y v) y))
             (* amount (- (.y v) y))))))
(make-var-method splits :regular)

;; ### Squirrel
(make-config-method squirrel {:a (drand EPSILON 4.0)
                              :b (drand EPSILON 4.0)})

(defn make-squirrel
  ""
  [^double amount {:keys [^double a ^double b]}]
  (fn [^Vec2 v]
    (let [u (sqrt (+ (* a (sq (.x v)))
                     (* b (sq (.y v)))))]
      (Vec2. (* amount (cos u) (tan (.x v)))
             (* amount (sin u) (tan (.y v)))))))
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
          result (/ (* x2-y2 (sin (* TWO_PI distort (+ x y)))) div)]
      (Vec2. (+ (* amount (.x v)) result)
             (+ (* amount (.y v)) result)))))
(make-var-method stwin :regular)

;; ### Swirl
(defn make-swirl
  ""
  [^double amount _]
  (fn [^Vec2 v]
    (let [r (v/magsq v)
          c1 (sin r)
          c2 (cos r)]
      (Vec2. (* amount (- (* c1 (.x v)) (* c2 (.y v))))
             (* amount (+ (* c2 (.x v)) (* c1 (.y v))))))))
(make-var-method swirl :regular)

;; ## T

;; ### Tangent
(defn make-tangent
  ""
  [^double amount _]
  (fn [^Vec2 v]
    (let [d (cos (.y v))
          id (/ 1.0 (if (zero? d) EPSILON d))]
      (Vec2. (* amount (sin (.x v)) id)
             (* amount (tan (.y v)))))))
(make-var-method tangent :regular)

;; ### Taurus
(make-config-method taurus {:r (drand -5.0 5.0)
                            :n (drand -5.0 5.0)
                            :inv (drand -2.0 2.0)
                            :sor (drand -2.0 2.0)})

(defn make-taurus
  ""
  [^double amount {:keys [^double r ^double n ^double inv ^double sor]}]
  (let [rinv (* r inv)
        revinv (- 1.0 inv)]
    (fn [^Vec2 v]
      (let [sx (sin (.x v))
            cx (cos (.x v))
            sy (sin (.y v))
            ir (+ rinv (* revinv r (cos (* n (.x v)))))
            irsy (+ ir sy)]
        (Vec2. (* amount cx irsy)
               (* amount sx irsy))))))
(make-var-method taurus :regular)


;; ### Trade
(defn config-trade
  "Trade configuration"
  [p]
  (let [m (merge {:r1 (drand 0.1 3.0)
                  :r2 (drand 0.1 3.0)
                  :d1 (drand -2.0 2.0)
                  :d2 (drand -2.0 2.0)} p)]
    (assoc m :c1 (+ ^double (:r1 m) ^double (:d1 m))
           :c2 (+ ^double (:r2 m) ^double (:d2 m)))))
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
            res (Vec2. (+ cc2 (* r (cos a)))
                       (* r (sin a)))]
        (if (<= rm rr)
          (v/mult res amount)
          (v/mult v amount))))))
(make-var-method trade :regular)

;; ## V

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
                               (long (inc (floor (* (discrete-noise (+ (+ (* M1 19) (* N1 257)) xseed) 0) num))))))
          m (long (floor (/ (.x v) step)))
          n (long (floor (/ (.y v) step)))
          m- (dec m)
          m+ (inc m)
          n- (dec n)
          n+ (inc n)
          Ks (mapv fk [m- m- m- m m m m+ m+ m+] [n- n n+ n- n n+ n- n n+])
          ^VoronResType res (reduce (fn [^VoronResType curr ^VoronCalcType calc]
                                      (loop [i (long 0)
                                             ^VoronResType currl curr]
                                        (if (< i (.k calc))
                                          (let [X (* step (+ (.M1 calc) (discrete-noise (+
                                                                                           (+ i (* 64 (.M1 calc)))
                                                                                           (+ xseed (* 15 (.N1 calc)))) 0)))
                                                Y (* step (+ (.N1 calc) (discrete-noise (+
                                                                                           (+ i (* 21 (.M1 calc)))
                                                                                           (+ yseed (* 33 (.N1 calc)))) 0)))
                                                R (hypot (- (.x v) X) (- (.y v) Y))]
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
  ""
  [^double amount {:keys [^double coeff10 ^double coeff11 ^double coeff20 ^double coeff21]}]
  (let [c202 (+ EPSILON (sq coeff20))
        c212 (+ EPSILON (sq coeff21))]
    (fn [^Vec2 v]
      (Vec2. (->> c202
                  (/ (.y v))
                  sin
                  (* coeff10)
                  (+ (.x v))
                  (* amount))
             (->> c212
                  (/ (.x v))
                  sin
                  (* coeff11)
                  (+ (.y v))
                  (* amount))))))
(make-var-method waves :regular)


;;;;; https://github.com/d3/d3-geo-projection/tree/master/src

(defn make-miller
  ""
  [^double amount _]
  (fn [^Vec2 v]
    (v/mult (Vec2. (.x v)
                   (->> (constrain (.y v) -1.9634 1.9634)
                        (* 0.4)
                        (+ QUARTER_PI)
                        (tan)
                        (log)
                        (* 1.25))) amount)))
(make-var-method miller :regular)

(defn make-millerrev
  ""
  [^double amount _]
  (fn [^Vec2 v]
    (v/mult (Vec2. (.x v)
                   (-> (.y v)
                       (* 0.8)
                       (exp)
                       (atan)
                       (* 2.5)
                       (- (* 0.625 PI)))) amount)))
(make-var-method millerrev :regular)

(defn make-foucaut
  ""
  [^double amount _]
  (fn [^Vec2 v]
    (let [k (* 0.5 (.y v))
          cosk (cos k)
          xx (->> cosk
                  (* cosk)
                  (* (cos (.y v)))
                  (* (/ (.x v) SQRTPI))
                  (* 2)
                  (* amount))
          yy (* amount SQRTPI (tan k))]
      (Vec2. xx yy))))
(make-var-method foucaut :regular)

;;;;

(def variation-list-random @random-var)
(def variation-list-not-random @regular-var)

;; list of all variations defined in the file
(def variation-list (concat variation-list-random variation-list-not-random))

;;; combinator & randomizer

(def ^:dynamic *skip-random-variations* false)

(defn- derivative
  ""
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

(defn- make-random-variation-conf
  "Create configuration line for random variation"
  []
  (let [n (rand-nth (if *skip-random-variations* variation-list-not-random variation-list))]
    {:type :variation :name n :amount 1.0 :config (make-configuration n {})}))

(defn- make-random-conf-step
  "Create one step for combined variation configuration, one of: sum, multiplication, combination or derivative"
  ([f1 f2]
   (let [operand (rand-nth [:comp :add :comp :add :comp :mult :comp])
         ^double amount1 (if (= operand :comp) 1.0 (drand -2.0 2.0))
         ^double amount2 (if (= operand :comp) 1.0 (drand -2.0 2.0))
         type1 (:type f1)
         type2 (:type f2)
         var1 (if (= type1 :operation) f1 (merge f1 {:amount amount1}))
         var2 (if (= type2 :operation) f2 (merge f2 {:amount amount2}))
         v {:type :operation :name operand :var1 var1 :var2 var2}]
     (case operand
       :add (merge v {:amount (/ 1.0 (+ (abs amount1) (abs amount2)))})
       :mult (merge v {:amount (/ 1.0 (* amount1 amount2))})
       :comp (merge v {:amount 1.0}))))
  ([f]
   (if (brand 0.1) f ;; skip sometimes
       (if (brand 0.15) ;; derivative?
         {:type :operation :name :deriv :step (sq (drand 0.01 1.0)) :amount 1.0 :var f}
         (make-random-conf-step f (make-random-variation-conf)))))
  ([]
   (make-random-conf-step (make-random-variation-conf) (make-random-variation-conf))))

(defn randomize-parametrization
  ""
  ([f]
   (if (= (:type f) :variation)
     (assoc f :amount 1.0 :config (make-configuration (:name f) {}))
     (let [name (:name f)]
       (println name)
       (if (= name :deriv)
         (assoc f :amount 1.0 :step (sq (drand 0.01 1.0)) :var (randomize-parametrization (:var f)))
         (let [^double amount1 (if (= name :comp) 1.0 (drand -2.0 2.0))
               ^double amount2 (if (= name :comp) 1.0 (drand -2.0 2.0)) 
               amount (case name
                        :add (/ 1.0 (+ (abs amount1) (abs amount2)))
                        :mult (/ 1.0 (* amount1 amount2))
                        :comp 1.0)]
           (assoc f :amount amount
                  :var1 (assoc (randomize-parametrization (:var1 f)) :amount amount1)
                  :var2 (assoc (randomize-parametrization (:var2 f)) :amount amount2)))))))
  ([]
   (randomize-parametrization (make-random-tree))))

(defn get-random-variation-cfg
  ""
  []
  (let [n (rand-nth (if *skip-random-variations* variation-list-not-random variation-list))]
    {:type :variation :name n}))

(defn make-first-step
  ""
  ([f1 f2]
   (let [operand (rand-nth [:comp :add :comp :add :comp :mult :comp])]
     {:type :operation :name operand :var1 f1 :var2 f2}))
  ([f]
   (if (brand 0.1) f
       (if (brand 0.15)
         {:type :operation :name :deriv :var f}
         (make-first-step f (get-random-variation-cfg)))))
  ([]
   (make-first-step (get-random-variation-cfg) (get-random-variation-cfg))))

(defn make-random-tree
  ""
  ([] (make-random-tree (lrand 5)))
  ([depth] (make-random-tree depth (get-random-variation-cfg)))
  ([^long depth f]
   (if (pos? depth)
     (make-random-tree (dec depth) (make-first-step f))
     f)))

(defn make-random-configuration
  "Create full random configuration for combined variations"
  ([^long depth f]
   (if (pos? depth)
     (make-random-configuration (dec depth) (make-random-conf-step f))
     f))
  ([depth] (make-random-configuration depth (make-random-variation-conf)))
  ([] (make-random-configuration (lrand 5))))

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
