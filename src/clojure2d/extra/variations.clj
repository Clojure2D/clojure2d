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
            [clojure2d.math :as m])
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
  [sym]
  (let [k (keyword sym)
        s (symbol (str "config-" sym))]
    `(defmethod make-configuration ~k [k# p#] (~s p#))))

(defmacro make-var-method
  "Add new multimethod for variation factory function"
  [sym]
  (let [k (keyword sym)
        m (symbol (str "make-" sym))]
    `(defmethod make-variation ~k [k# a# p#] (~m a# (make-configuration ~k p#)))))

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
(defn config-auger
  "Auger configuration
  params: `:freq` `:weight` `:sym` `:scale`"
  [p]
  (merge {:freq (drand -5 5)
          :weight (drand -1 1)
          :sym (drand -2 2)
          :scale (srandom 0.5 2)} p))
(make-config-method auger)

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
(make-var-method auger)
(register-regular-var :auger)

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
(make-var-method arch)
(register-random-var :arch)

;; ## B

;; ### bCollide
(defn config-bcollide
  "bCollide configuration
  params: `:num` `:a`"
  [p]
  (let [m (merge {:num (srandom 1 30)
                  :a (drand 2)} p)
        bcn-pi (* ^double (:num m) M_1_PI)
        pi-bcn (/ PI ^double (:num m))
        bca-bcn (/ (* PI ^double (:a m)) ^double (:num m))]
    (assoc m
           :bcn-pi bcn-pi
           :pi-bcn pi-bcn
           :bca-bcn bca-bcn)))
(make-config-method bcollide)

(defn make-bcollide
  "bCollide by Michael Faber, http://michaelfaber.deviantart.com/art/bSeries-320574477"
  [^double amount {:keys [^double bcn-pi ^double pi-bcn ^double bca-bcn]}]
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
      (Vec2. xx yy))))
(make-var-method bcollide)
(register-regular-var :bcollide)

;; ### bSwirl
(defn config-bswirl
  "bSwirl configuration
  params: `:in` `:out`"
  [p]
  (merge {:in (drand -2.0 2.0)
          :out (drand -2.0 2.0)} p))
(make-config-method bswirl)

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
(make-var-method bswirl)
(register-regular-var :bswirl)

;; ### BesselJ
(defn make-besselj
  ""
  [^double amount _]
  (fn [^Vec2 v]
    (Vec2. (* amount ^double (v/mag v) (BesselJ/value (abs (.x v)) (abs (.y v))))
           (* amount ^double (v/heading v)))))
(make-var-method besselj)
(register-regular-var :besselj)

;; ### Beta
(defn make-beta
  ""
  [^double amount _]
  (fn [^Vec2 v]
    (Vec2. (* amount (Beta/logBeta (+ EPSILON (abs (.x v))) (+ EPSILON (abs (.y v)))))
           (* amount ^double (v/heading v)))))
(make-var-method beta)
(register-regular-var :beta)

;; ## Bent

(defn make-bent
  ""
  [^double amount _]
  (fn [^Vec2 v]
    (let [nx (if (neg? (.x v)) (+ (.x v) (.x v)) (.x v))
          ny (if (neg? (.y v)) (* (.y v) 0.5) (.y v))]
      (Vec2. (* amount nx)
             (* amount ny)))))
(make-var-method bent)
(register-regular-var :bent)

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
(make-var-method blade)
(register-random-var :blade)

(defn make-blade2
  ""
  [^double amount _]
  (fn [^Vec2 v]
    (let [r (* ^double (drand amount) ^double (v/mag v))
          sinr (sin r)
          cosr (cos r)]
      (Vec2. (* amount (.x v) (+ cosr sinr))
             (* amount (.y v) (- cosr sinr))))))
(make-var-method blade2)
(register-random-var :blade2)

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
(make-var-method boarders)
(register-random-var :boarders)

;; ## Butterfly

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

(make-var-method butterfly)
(register-regular-var :butterfly)

;; ## C

;; ### CircleLinear
(defn config-circlelinear
  "CircleLinear configuration
  params: `:Sc` `:K` `:Dens1` `:Dens2` `:Reverse` `:X` `:Y` `:Seed`"
  [p]
  (let [m (merge {:Sc (drand 1)
                  :K (drand -2 2)
                  :Dens1 (drand 1)
                  :Dens2 (drand 1)
                  :Reverse (drand -1 1)
                  :X (drand 20)
                  :Y (drand 20)
                  :Seed (irand Integer/MAX_VALUE)} p)]
    (assoc m :dd (* ^double (:Dens1 m) ^double (:Dens2 m)))))
(make-config-method circlelinear)

(defn make-circlelinear
  "CircleLinear by eralex, http://eralex61.deviantart.com/art/Circles-Plugins-126273412"
  [^double amount {:keys [^double Sc ^double K ^double Dens1 ^double Dens2 ^double Reverse ^double X ^double Y ^double Seed ^double dd]}]
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
                  (* amount))))))
(make-var-method circlelinear)
(register-regular-var :circlelinear)

;; ### csin

(defn config-csin
  "CSin configuraion
  params `:stretch`"
  [p]
  (let [m (merge {:stretch (drand -3 3)} p)]
    (assoc m :s-cx (Complex. (:stretch m) 0.0))))
(make-config-method csin)

(defn make-csin
  "CSin by zephyrtronium, http://fractal-resources.deviantart.com/art/CSin-Apophysis-Plugin-158332287"
  [^double amount {:keys [stretch ^Complex s-cx]}]
  (fn [^Vec2 v]
    (v/mult (->> (c/from-vec2 v)
                 (c/mult s-cx)
                 (c/sin)
                 (c/to-vec2)) amount)))
(make-var-method csin)
(register-regular-var :csin)

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
(make-var-method cayley)
(register-regular-var :cayley)

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
(make-var-method diamond)
(register-regular-var :diamond)

;; ## E

;; ### eMod
(defn config-emod
  "eMod configuration
  params: `:radius` `:distance`"
  [p]
  (let [m (merge {:radius (drand 0.1 4)
                  :distance (drand 2)} p)]
    (assoc m :radius2 (* 2.0 ^double (:radius m))
           :rdr (+ ^double (:radius m) (* ^double (:distance m) ^double (:radius m))))))
(make-config-method emod)

(defn make-emod
  "eMod by Michael Faber, http://michaelfaber.deviantart.com/art/eSeries-306044892"
  [^double amount {:keys [^double radius ^double distance ^double radius2 ^double rdr]}]
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
      (Vec2. xx yy))))
(make-var-method emod)
(register-regular-var :emod)

;; ### Erf
(defn make-erf
  ""
  [^double amount _]
  (fn [^Vec2 v]
    (Vec2. (* amount (Erf/erf (.x v) (.y v)))
           (* amount ^double (v/heading v)))))
(make-var-method erf)
(register-regular-var :erf)

;; ### Exp
(defn make-exp
  ""
  [^double amount _]
  (fn [^Vec2 v]
    (let [e (exp (.x v))]
      (Vec2. (* amount e (cos (.y v)))
             (* amount e (sin (.y v)))))))
(make-var-method exp)
(register-regular-var :exp)

;; ## F
(defn config-fan2
  "fan2 configuration
  params: `:x` `:y`"
  [p]
  (merge {:x (drand -1 1)
          :y (drand -1 1)} p))
(make-config-method fan2)

(defn make-fan2
  ""
  [^double amount {:keys [^double x ^double y]}]
  (fn [^Vec2 v]
    (let [^double r (v/mag v)
          ^double angle (v/heading v)
          dy y
          dx (+ 1.0e-6 (* PI x x))
          dx2 (* 0.5 dx)
          t (+ angle (- dy (* dx (long (/ (+ angle dy) dx)))))
          a (if (> t dx2)
              (- angle dx2)
              (+ angle dx2))]
      (Vec2. (* amount r (sin a))
             (* amount r (cos a))))))
(make-var-method fan2)
(register-regular-var :fan2)

;; ## G

;; ### Gamma
(defn make-gamma
  "gamma by zephyrtronium, http://fractal-resources.deviantart.com/art/Gamma-Apophysis-Plugin-154808483"
  [^double amount _]
  (fn [^Vec2 v]
    (Vec2. (* amount (Gamma/logGamma (v/mag v)))
           (* amount ^double (v/heading v)))))
(make-var-method gamma)
(register-regular-var :gamma)

;; ## H
;;
;; ### Hemisphere

(defn make-hemisphere
  ""
  [^double amount _]
  (fn [^Vec2 v]
    (let [r (/ amount (sqrt (inc ^double (v/magsq v))))]
      (Vec2. (* r (.x v))
             (* r (.y v))))))
(make-var-method hemisphere)
(register-regular-var :hemisphere)

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
(make-var-method julia)
(register-random-var :julia)

;; ### JuliaN
(defn config-julian
  "JuliaN configuration
  params: `:power` `:dist`"
  [p]
  (let [m (merge {:power (srandom 1 10)
                  :dist (drand -4 4)} p)]
    (assoc m
           :abspower (abs (:power m))
           :cpower (* 0.5 (/ ^double (:dist m) ^double (:power m))))))
(make-config-method julian)

(defn make-julian
  "JuliaN"
  [^double amount {:keys [^double power ^double abspower ^double cpower]}]
  (fn [^Vec2 v]
    (let [a (/ (+ ^double (v/heading v) (* TWO_PI ^double (drand abspower))) power)
          r (* amount (pow (v/magsq v) cpower))]
      (Vec2. (* r (cos a)) (* r (sin a))))))
(make-var-method julian)
(register-random-var :julian)

;; ### JuliaQ
(defn config-juliaq
  "Juliaq configuration
  params: `:power` `:divisor`"
  [p]
  (let [m (merge {:power (srandom 1 10)
                  :divisor (drand -8 8)} p)
        inv-power (/ ^double (:divisor m) ^double (:power m))
        half-inv-power (* 0.5 inv-power)
        inv-power-2pi (/ TWO_PI ^double (:power m))]
    (assoc m
           :inv-power inv-power
           :half-inv-power half-inv-power
           :inv-power-2pi inv-power-2pi)))
(make-config-method juliaq)

(defn make-juliaq
  "juliaq by Zueuk, http://zueuk.deviantart.com/art/juliaq-Apophysis-plugins-340813357"
  [^double amount {:keys [^double inv-power ^double inv-power-2pi ^double half-inv-power] :as all}]
  (fn [^Vec2 v]
    (let [a (+ (* inv-power ^double (v/heading v))
               (* inv-power-2pi ^double (drand (Integer/MAX_VALUE))))
          r (* amount (pow (v/magsq v) half-inv-power))]
      (Vec2. (* r (cos a)) (* r (sin a))))))
(make-var-method juliaq)
(register-random-var :juliaq)

;; ## N


;; ## P

;; ### Pie
(defn config-pie
  "Pie configuration
  params: `:slices` `:rotation` `:thickness`"
  [p]
  (merge {:slices (srandom 0.01 7.0)
          :rotation (drand TWO_PI)
          :thickness (drand -2.0 2.0)} p))
(make-config-method pie)

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
(make-var-method pie)
(register-random-var :pie)


;; ### Popcorn2
(defn config-popcorn2
  "Popcorn2 configuration
  params: `:x` `:y` `:c`"
  [p]
  (merge {:x (drand -1.5 1.5)
          :y (drand -1.5 1.5)
          :c (drand -5.0 5.0)} p))
(make-config-method popcorn2)

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
(make-var-method popcorn2)
(register-regular-var :popcorn2)

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
(make-var-method scry)
(register-regular-var :scry)

;; ### Sinusoidal
(defn make-sinusoidal
  "Sinusoidal"
  [^double amount _]
  (fn [^Vec2 v]
    (Vec2. (* amount (sin (.x v))) (* amount (sin (.y v))))))
(make-var-method sinusoidal)
(register-regular-var :sinusoidal)

;; ### STwin
(defn config-stwin
  "STwin configuration
  params: `:distort` `:multiplier`"
  [p]
  (merge {:distort (drand -6 6)
          :multiplier (srandom 0.001 3.0)} p))
(make-config-method stwin)

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
(make-var-method stwin)
(register-regular-var :stwin)

;; ## T

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
(make-config-method trade)

(defn make-trade
  "trade by Michael Faber,  http://michaelfaber.deviantart.com/art/The-Lost-Variations-258913970"
  [^double amount {:keys [^double r1 ^double r2 ^double c1 ^double c2]}]
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
        (v/mult v amount)))))
(make-var-method trade)
(register-regular-var :trade)

;; ## V

;; ### Voron

(defn config-voron
  "Voron configuration"
  [p]
  (merge {:k (srandom 0.6 1.3)
          :step (srandom 0.1 1.2)
          :num (drand 0.1 25.0)
          :xseed (irand)
          :yseed (irand)} p))
(make-config-method voron)

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
(make-var-method voron)
(register-regular-var :voron)

(def vv (make-variation :voron 1.0 {}))

(vv (Vec2. 0.9 0.2))
;; => #object[clojure2d.math.vector.Vec2 0x34f5877c "[-0.6144291130318851, -0.03556826075794334]"]

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

(make-var-method miller)
(register-regular-var :miller)

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
(make-var-method millerrev)
(register-regular-var :millerrev)

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
(make-var-method foucaut)
(register-regular-var :foucaut)

;;;;

(def variation-list-random @random-var)
(def variation-list-not-random @regular-var)

;; list of all variations defined in the file
(def variation-list (concat variation-list-random variation-list-not-random))

;;; combinator & randomizer

(def ^:dynamic *skip-random-variations* false)

(defn derivative
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

(defn make-random-variation-conf
  "Create configuration line for random variation"
  []
  (let [n (rand-nth (if *skip-random-variations* variation-list-not-random variation-list))]
    {:type :variation :name n :amount 1.0 :config (make-configuration n {})}))

(defn make-random-conf-step
  "Create one step for combined variation configuration, one of: sum, multiplication, combination or derivative"
  ([f1 f2]
   (let [operand (rand-nth [:add :add :mult :comp :comp :comp])
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
       (if (brand 0.2) ;; derivative?
         {:type :operation :name :deriv :step (sq (drand 0.01 1.0)) :amount 1.0 :var f}
         (make-random-conf-step f (make-random-variation-conf)))))
  ([]
   (make-random-conf-step (make-random-variation-conf) (make-random-variation-conf))))

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
  ([config]
   (let [t (:type config)
         n (:name config)
         a (:amount config)]
     (if (= t :variation)
       (make-variation n a (:config config))
       (if (= n :deriv)
         (derivative (make-combination (:var config)) a (:step config))
         (let [v1 (make-combination (:var1 config))
               v2 (make-combination (:var2 config))]
           (case n
             :comp (fn ^Vec2 [^Vec2 v] (v/mult (v1 (v2 v)) a))
             :add (fn ^Vec2 [^Vec2 v] (v/mult (v/add (v1 v) (v2 v)) a))
             :mult (fn ^Vec2 [^Vec2 v] (v/mult (v/emult (v1 v) (v2 v)) a))))))))
  ([] (make-combination (make-random-configuration))))
