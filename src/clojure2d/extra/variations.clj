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
  (:require [clojure2d.math :as m]
            [clojure2d.math.vector :as v]
            [clojure2d.math.complex :as c])
  (:import [clojure2d.math.vector Vec2]
           [clojure2d.math.complex Complex]
           [org.apache.commons.math3.special Gamma Beta Erf BesselJ]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

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
  [mn mx]
  (let [rand (m/drand mn mx)]
  (if (m/brand)
    rand
    (* -1 rand))))

(def ^Vec2 unitx (Vec2. 1.0 0.0))
(def ^Vec2 zerov (Vec2. 0.0 0.0))

;; ## A
;;
;; ### Auger
(defn config-auger
  "Auger configuration
  params: `:freq` `:weight` `:sym` `:scale`"
  [p]
  (merge {:freq (m/drand -5 5)
          :weight (m/drand -1 1)
          :sym (m/drand -2 2)
          :scale (srandom 0.5 2)} p))
(make-config-method auger)

(defn make-auger
  "Auger by Xyrus02"
  [amount {:keys [freq weight sym scale]}]
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
(make-var-method auger)

;; ### Arch

(defn make-arch
  ""
  [amount _]
  (fn [^Vec2 v]
    (let [ang (* amount (m/drand m/PI))
          sinr (m/sin ang)
          cosr (m/cos ang)]
      (if (zero? cosr) zerov
          (Vec2. (* amount sinr)
                 (* amount (/ (m/sq sinr) cosr)))))))
(make-var-method arch)

;; ## B

;; ### bCollide
(defn config-bcollide
  "bCollide configuration
  params: `:num` `:a`"
  [p]
  (let [m (merge {:num (srandom 1 30)
                  :a (m/drand 2)} p)
        bcn-pi (* (:num m) m/M_1_PI)
        pi-bcn (/ m/PI (:num m))
        bca-bcn (/ (* m/PI (:a m)) (:num m))]
    (assoc m
           :bcn-pi bcn-pi
           :pi-bcn pi-bcn
           :bca-bcn bca-bcn)))
(make-config-method bcollide)

(defn make-bcollide
  "bCollide by Michael Faber, http://michaelfaber.deviantart.com/art/bSeries-320574477"
  [amount {:keys [bcn-pi pi-bcn bca-bcn]}]
  (fn [^Vec2 v]
    (let [v+ (v/add v unitx)
          v- (Vec2. (- 1.0 (.x v)) (.y v))
          tau (* 0.5 (- (m/log (v/magsq v+))
                        (m/log (v/magsq v-))))
          pre-sigma (- m/PI (v/heading v+) (v/heading v-))
          alt (int (* pre-sigma bcn-pi))
          sigma (if (even? alt)
                  (+ (* alt pi-bcn) (rem (+ pre-sigma bca-bcn) pi-bcn))
                  (+ (* alt pi-bcn) (rem (- pre-sigma bca-bcn) pi-bcn)))
          sinht (m/sinh tau)
          cosht (m/cosh tau)
          sins (m/sin sigma)
          coss (m/cos sigma)
          temp (/ 1.0 (- cosht coss))
          xx (* amount sinht temp)
          yy (* amount sins temp)]
      (Vec2. xx yy))))
(make-var-method bcollide)

;; ### bCollide
(defn config-bswirl
  "bSwirl configuration
  params: `:in` `:out`"
  [p]
  (merge {:in (m/drand -2.0 2.0)
          :out (m/drand -2.0 2.0)} p))
(make-config-method bswirl)

(defn make-bswirl
  "bSwirl by Michael Faber, http://michaelfaber.deviantart.com/art/bSeries-320574477"
  [amount {:keys [in out]}]
  (fn [^Vec2 v]
    (let [v+ (v/add v unitx)
          v- (Vec2. (- 1.0 (.x v)) (.y v))
          tau (* 0.5 (- (m/log (v/magsq v+))
                        (m/log (v/magsq v-))))
          pre-sigma (- m/PI (v/heading v+) (v/heading v-))
          sigma (+ pre-sigma (* tau out) (/ in tau))
          sinht (m/sinh tau)
          cosht (m/cosh tau)
          sins (m/sin sigma)
          coss (m/cos sigma)
          temp (- cosht coss)]
      (if (zero? temp)
        (Vec2. 0.0 0.0)
        (Vec2. (* amount (/ sinht temp))
               (* amount (/ sins temp)))))))
(make-var-method bswirl)

;; ### BesselJ
(defn make-besselj
  ""
  [amount _]
  (fn [^Vec2 v]
    (Vec2. (* amount (v/mag v) (BesselJ/value (m/abs (.x v)) (m/abs (.y v))))
           (* amount (v/heading v)))))
(make-var-method besselj)

;; ### Beta
(defn make-beta
  ""
  [amount _]
  (fn [^Vec2 v]
    (Vec2. (* amount (Beta/logBeta (+ m/EPSILON (m/abs (.x v))) (+ m/EPSILON (m/abs (.y v)))))
           (* amount (v/heading v)))))
(make-var-method beta)

;; ## Bent

(defn make-bent
  ""
  [amount _]
  (fn [^Vec2 v]
    (let [nx (if (neg? (.x v)) (+ (.x v) (.x v)) (.x v))
          ny (if (neg? (.y v)) (* (.y v) 0.5) (.y v))]
      (Vec2. (* amount nx)
             (* amount ny)))))
(make-var-method bent)

;; ## Blade

(defn make-blade
  ""
  [amount _]
  (fn [^Vec2 v]
    (let [r (* (m/drand amount) (v/mag v))
          sinr (m/sin r)
          cosr (m/cos r)]
      (Vec2. (* amount (.x v) (+ cosr sinr))
             (* amount (.x v) (- cosr sinr))))))
(make-var-method blade)

(defn make-blade2
  ""
  [amount _]
  (fn [^Vec2 v]
    (let [r (* (m/drand amount) (v/mag v))
          sinr (m/sin r)
          cosr (m/cos r)]
      (Vec2. (* amount (.x v) (+ cosr sinr))
             (* amount (.y v) (- cosr sinr))))))
(make-var-method blade2)

;; ## Boarders

(defn make-boarders
  ""
  [amount _]
  (fn [^Vec2 v]
    (let [roundx (m/rint (.x v))
          roundy (m/rint (.y v))
          offsetx (- (.x v) roundx)
          offsety (- (.y v) roundy)
          hoffsetx (* 0.5 offsetx)
          hoffsety (* 0.5 offsety)]
      (if (m/brand 0.75)
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
(make-var-method boarders)

;; ## Butterfly

(defn make-butterfly
  ""
  [amount _]
  (fn [^Vec2 v]
    (let [wx (* amount 1.3029400317411197908970256609023)
          y2 (* 2.0 (.y v))
          r (* wx (m/sqrt (/ (m/abs (* (.y v) (.x v)))
                             (+ m/EPSILON (m/sq (.x v)) (m/sq y2)))))]
      (Vec2. (* r (.x v))
             (* r y2)))))

(make-var-method butterfly)

;; ## C

;; ### CircleLinear
(defn config-circlelinear
  "CircleLinear configuration
  params: `:Sc` `:K` `:Dens1` `:Dens2` `:Reverse` `:X` `:Y` `:Seed`"
  [p]
  (let [m (merge {:Sc (m/drand 1)
                  :K (m/drand -2 2)
                  :Dens1 (m/drand 1)
                  :Dens2 (m/drand 1)
                  :Reverse (m/drand -1 1)
                  :X (m/drand 20)
                  :Y (m/drand 20)
                  :Seed (m/irand Integer/MAX_VALUE)} p)]
    (assoc m :dd (* (:Dens1 m) (:Dens2 m)))))
(make-config-method circlelinear)

(defn make-circlelinear
  "CircleLinear by eralex, http://eralex61.deviantart.com/art/Circles-Plugins-126273412"
  [amount {:keys [Sc K Dens1 Dens2 Reverse X Y Seed dd]}]
  (fn [^Vec2 v]
    (let [M (->> Sc
                 (/ (.x v))
                 (* 0.5)
                 (m/floor)
                 (int))
          N (->> Sc
                 (/ (.y v))
                 (* 0.5)
                 (m/floor)
                 (int))
          X (- (.x v) (->> M
                           (* 2.0)
                           (inc)
                           (* Sc)))
          Y (- (.y v) (->> N
                           (* 2.0)
                           (inc)
                           (* Sc)))
          U (m/hypot X Y)
          V (->> (m/discrete-noise (+ M 10) (+ N 3))
                 (* 0.7)
                 (+ 0.3)
                 (* Sc))
          Z1 (m/discrete-noise (+ M Seed) N)
          [XX YY] (if (and (< Z1 Dens1) (< U V)) 
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

;; ### csin

(defn config-csin
  "CSin configuraion
  params `:stretch`"
  [p]
  (let [m (merge {:stretch (m/drand -3 3)} p)]
    (assoc m :s-cx (Complex. (:stretch m) 0.0))))
(make-config-method csin)

(defn make-csin
  "CSin by zephyrtronium, http://fractal-resources.deviantart.com/art/CSin-Apophysis-Plugin-158332287"
  [amount {:keys [stretch ^Complex s-cx]}]
  (fn [^Vec2 v]
    (v/mult (->> (c/from-vec2 v)
                 (c/mult s-cx)
                 (c/sin)
                 (c/to-vec2)) amount)))
(make-var-method csin)

;; ## E

;; ### eMod
(defn config-emod
  "eMod configuration
  params: `:radius` `:distance`"
  [p]
  (let [m (merge {:radius (m/drand 0.1 4)
                  :distance (m/drand 2)} p)]
    (assoc m :radius2 (* 2.0 (:radius m))
           :rdr (+ (:radius m) (* (:distance m) (:radius m))))))
(make-config-method emod)

(defn make-emod
  "eMod by Michael Faber, http://michaelfaber.deviantart.com/art/eSeries-306044892"
  [amount {:keys [radius distance radius2 rdr]}]
  (fn [^Vec2 v]
    (let [tmp (+ 1.0 (v/magsq v))
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
      (Vec2. xx yy))))
(make-var-method emod)

;; ### Erf
(defn make-erf
  ""
  [amount _]
  (fn [^Vec2 v]
    (Vec2. (* amount (Erf/erf (.x v) (.y v)))
           (* amount (v/heading v)))))
(make-var-method erf)

;; ### Exp
(defn make-exp
  ""
  [amount _]
  (fn [^Vec2 v]
    (let [e (m/exp (.x v))]
      (Vec2. (* amount e (m/cos (.y v)))
             (* amount e (m/sin (.y v)))))))
(make-var-method exp)

;; ## F
(defn config-fan2
  "fan2 configuration
  params: `:x` `:y`"
  [p]
  (merge {:x (m/drand -1 1)
          :y (m/drand -1 1)} p))
(make-config-method fan2)

(defn make-fan2
  ""
  [amount {:keys [x y]}]
  (fn [^Vec2 v]
    (let [r (v/mag v)
          angle (v/heading v)
          dy y
          dx (+ 1.0e-6 (* m/PI x x))
          dx2 (* 0.5 dx)
          t (+ angle (- dy (* dx (long (/ (+ angle dy) dx)))))
          a (if (> t dx2)
              (- angle dx2)
              (+ angle dx2))]
      (Vec2. (* amount r (m/sin a))
             (* amount r (m/cos a))))))
(make-var-method fan2)

;; ## G

;; ### Gamma
(defn make-gamma
  "gamma by zephyrtronium, http://fractal-resources.deviantart.com/art/Gamma-Apophysis-Plugin-154808483"
  [amount _]
  (fn [^Vec2 v]
    (Vec2. (* amount (Gamma/logGamma (v/mag v)))
           (* amount (v/heading v)))))
(make-var-method gamma)

;; ## H
;;
;; ### Hemisphere

(defn make-hemisphere
  ""
  [amount _]
  (fn [^Vec2 v]
    (let [r (/ amount (m/sqrt (inc (v/magsq v))))]
      (Vec2. (* r (.x v))
             (* r (.y v))))))
(make-var-method hemisphere)

;; ## J
;;
;; ### Julia
(defn make-julia
  "Julia"
  [amount _]
  (fn [^Vec2 v]
    (let [a (+ (* 0.5 (v/heading v)) (* m/PI (m/irand 2)))
          r (->> (v/mag v)
                 (m/sqrt)
                 (* amount))]
      (Vec2. (* r (m/cos a)) (* r (m/sin a))))))
(make-var-method julia)

;; ### JuliaN
(defn config-julian
  "JuliaN configuration
  params: `:power` `:dist`"
  [p]
  (let [m (merge {:power (srandom 1 10)
                  :dist (m/drand -4 4)} p)]
    (assoc m
           :abspower (m/abs (:power m))
           :cpower (* 0.5 (/ (:dist m) (:power m))))))
(make-config-method julian)

(defn make-julian
  "JuliaN"
  [amount {:keys [power abspower cpower]}]
  (fn [^Vec2 v]
    (let [a (/ (+ (v/heading v) (* m/TWO_PI (m/drand abspower))) power)
          r (* amount (m/pow (v/magsq v) cpower))]
      (Vec2. (* r (m/cos a)) (* r (m/sin a))))))
(make-var-method julian)

;; ### JuliaQ
(defn config-juliaq
  "Juliaq configuration
  params: `:power` `:divisor`"
  [p]
  (let [m (merge {:power (srandom 1 10)
                  :divisor (m/drand -8 8)} p)
        inv-power (/ (:divisor m) (:power m))
        half-inv-power (* 0.5 inv-power)
        inv-power-2pi (/ m/TWO_PI (:power m))]
    (assoc m
           :inv-power inv-power
           :half-inv-power half-inv-power
           :inv-power-2pi inv-power-2pi)))
(make-config-method juliaq)

(defn make-juliaq
  "juliaq by Zueuk, http://zueuk.deviantart.com/art/juliaq-Apophysis-plugins-340813357"
  [amount {:keys [inv-power inv-power-2pi half-inv-power] :as all}]
  (fn [^Vec2 v]
    (let [a (+ (* inv-power (v/heading v))
               (* inv-power-2pi (m/drand (Integer/MAX_VALUE))))
          r (* amount (m/pow (v/magsq v) half-inv-power))]
      (Vec2. (* r (m/cos a)) (* r (m/sin a))))))
(make-var-method juliaq)

;; ## N


;; ## P

;; ### Popcorn2
(defn config-popcorn2
  "Popcorn2 configuration
  params: `:x` `:y` `:c`"
  [p]
  (merge {:x (m/drand -1.5 1.5)
          :y (m/drand -1.5 1.5)
          :c (m/drand -5.0 5.0)} p))
(make-config-method popcorn2)

(defn make-popcorn2
  "popcorn2 from apophysis"
  [amount {:keys [x y c]}]
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
(make-var-method popcorn2)

;; ## S

;; ### Sinusoidal
(defn make-sinusoidal
  "Sinusoidal"
  [amount _]
  (fn [^Vec2 v]
    (Vec2. (* amount (m/sin (.x v))) (* amount (m/sin (.y v))))))
(make-var-method sinusoidal)

;; ### STwin
(defn config-stwin
  "STwin configuration
  params: `:distort` `:multiplier`"
  [p]
  (merge {:distort (m/drand -6 6)
          :multiplier (srandom 0.001 3.0)} p))
(make-config-method stwin)

(defn make-stwin
  "STwin by Xyrus-02, http://timothy-vincent.deviantart.com/art/STwin-Plugin-136504836"
  [amount {:keys [distort multiplier]}]
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
(make-var-method stwin)

;; ## T

;; ### Trade
(defn config-trade
  "Trade configuration"
  [p]
  (let [m (merge {:r1 (m/drand 0.1 3.0)
                  :r2 (m/drand 0.1 3.0)
                  :d1 (m/drand -2.0 2.0)
                  :d2 (m/drand -2.0 2.0)} p)]
    (assoc m :c1 (+ (:r1 m) (:d1 m))
           :c2 (+ (:r2 m) (:d2 m)))))
(make-config-method trade)

(defn make-trade
  "trade by Michael Faber,  http://michaelfaber.deviantart.com/art/The-Lost-Variations-258913970"
  [amount {:keys [r1 r2 c1 c2]}]
  (fn [^Vec2 v]
    (let [[cc1 cc2 fr rr] (if (pos? (.x v))
                            [c1 (- c2) (/ r2 r1) r1]
                            [(- c2) c1 (/ r1 r2) r2])
          nv (Vec2. (- cc1 (.x v)) (.y v))
          rm (v/mag nv)
          r (* rm fr)
          a (v/heading nv)
          res (Vec2. (+ cc2 (* r (m/cos a)))
                     (* r (m/sin a)))]
      (if (<= rm rr)
        (v/mult res amount)
        (v/mult v amount)))))
(make-var-method trade)

;;;;; https://github.com/d3/d3-geo-projection/tree/master/src

(defn make-miller
  ""
  [amount _]
  (fn [^Vec2 v]
    (v/mult (Vec2. (.x v)
                   (->> (m/constrain (.y v) -1.9634 1.9634)
                        (* 0.4)
                        (+ m/QUARTER_PI)
                        (m/tan)
                        (m/log)
                        (* 1.25))) amount)))

(make-var-method miller)

(defn make-millerrev
  ""
  [amount _]
  (fn [^Vec2 v]
    (v/mult (Vec2. (.x v)
                   (-> (.y v)
                       (* 0.8)
                       (m/exp)
                       (m/atan)
                       (* 2.5)
                       (- (* 0.625 m/PI)))) amount)))

(make-var-method millerrev)

(defn make-foucaut
  ""
  [amount _]
  (fn [^Vec2 v]
    (let [k (* 0.5 (.y v))
          cosk (m/cos k)
          xx (->> cosk
                  (* cosk)
                  (* (m/cos (.y v)))
                  (* (/ (.x v) m/SQRTPI))
                  (* 2)
                  (* amount))
          yy (* amount m/SQRTPI (m/tan k))]
      (Vec2. xx yy))))

(make-var-method foucaut)

;;;;

(def variation-list-random [:arch
                            :blade :blade2 :boarders
                            :julia :julian :juliaq])

(def variation-list-not-random [:default
                                :auger 
                                :bcollide :besselj :beta :bswirl :bent :butterfly
                                :circlelinear :csin 
                                :emod :erf :exp
                                :foucaut :fan2
                                :gamma 
                                :hemisphere
                                :miller :millerrev
                                :popcorn2 
                                :sinusoidal :stwin
                                :trade])


;; list of all variations defined in the file
(def variation-list (concat variation-list-random variation-list-not-random))


;;; combinator & randomizer

;;;;

(defn derivative
  ""
  ([f amount a]
   (let [^Vec2 d (Vec2. a a)]
     (fn [^Vec2 v]
       (let [v1 (f v)
             v2 (f (v/add v d))]
         (v/mult (v/div (v/sub v2 v1) a) amount)))))
  ([f amount]
   (derivative f amount 0.001))
  ([f]
   (derivative f 1.0)))

;;;;

(defn- binary-op
  ""
  [op f1 f2 amount]
  (fn [^Vec2 v]
    (v/mult (op (f1 v) (f2 v)) amount)))

;;;;
(def addf (partial binary-op v/add))
(def subf (partial binary-op v/sub))
(def mulf (partial binary-op v/emult))

(defn divf
  ""
  [f1 f2 amount]
  (fn [^Vec2 v]
    (let [^Vec2 v1 (f1 v)
          ^Vec2 v2 (f2 v)
          x (if (zero? (.x v2)) 1.0 (.x v2))
          y (if (zero? (.y v2)) 1.0 (.y v2))]
      (v/mult (Vec2. (/ (.x v1) x) (/ (.y v1) y)) amount))))

(defn compf
  ""
  [f1 f2 amount]
  (v/mult (comp f1 f2) amount))

(defn make-random-configuration
  ""
  []
  ())

(defn make-random-combination
  ""
  []
  )
