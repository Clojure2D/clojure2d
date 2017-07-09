;; several analog audio/video filters and effects
;; can be applied in sequence on channels separately (by filter-channels function from pixels)
;;
;; Each effect has following structure:
;; - filter function - operates on sample with some state (and set up configuration made by creator). Each function accepts parameters:
;;    - [[state list] sample] - in this case function returns [result-value state1 state2 ...]
;;    - [[state list]] - returns initial state (if your state list contains `nil` values they are replaced with default value
;; - creator function (multimethod) - creates filter function with set up config parameters, accepts configuration as a map
;;
;; eg. to create lowpass filter you can call `(def lp (make-effect :simple-lowpass {:rate 44100 :cutoff 5000}))`
;; `lp` is now lowpass filter with set up rate and cutoff
;;
;; to get inital state you call `(def lp-state (lp []))`
;; to run filter you call
;; `(lp lp-state 10) => [4.160172294378693 4.160172294378693]`
;; to restore state you have to use destructuring [result & state] to use on another sample
;;
;; See examples 16
;;
;; Check description about each effect to see what configuration is required and what contains state.
;;

(ns clojure2d.extra.signal
  (:require [clojure2d.math :as m]
            [clojure2d.pixels :as p]
            [clojure2d.core :refer :all]
            [clojure2d.color :as c]
            [clojure.java.io :refer :all]
            [clojure2d.math.joise :as j]
            [clojure2d.math.vector :as v]
            [criterium.core :as bench]
            [clojure2d.math.random :as r])
  (:import [clojure2d.pixels Pixels]
           [clojure2d.math.vector Vec2 Vec3]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; represent signal is a collection of arrays of doubles, values -1.0 to 1.0

(deftype Signal [^doubles signal]
  Object
  (toString [_] (str "size=" (alength signal))))

;; helper functions
(def ^:const ^double alaw-A 87.6)
(def ^:const ^double alaw-rA (/ 1.0 alaw-A))
(def ^:const ^double alaw-lA (inc (m/log alaw-A)))
(def ^:const ^double alaw-rlA (/ 1.0 alaw-lA))

(defn alaw
  ""
  ^double [^double x]
  (let [absx (m/abs x)
        f (* alaw-rlA (m/sgn x))]
    (* f (if (< absx alaw-rA)
           (* alaw-A absx)
           (+ alaw-lA (m/log absx))))))

(defn alaw-rev
  ""
  ^double [^double y]
  (let [absy (m/abs y)
        f (* alaw-rA (m/sgn y))
        v (* absy alaw-lA)]
    (* f (if (< absy alaw-rlA)
           v
           (m/exp (dec v))))))

(def ^:const ^double ulaw-U 255.0)
(def ^:const ^double ulaw-rU (/ 1.0 255.0))
(def ^:const ^double ulaw-U1 (inc ulaw-U))
(def ^:const ^double ulaw-rlnU1 (/ 1.0 (m/log ulaw-U1)))

(defn ulaw
  ""
  ^double [^double x]
  (* (m/sgn x)
     ulaw-rlnU1
     (m/log (inc (* ulaw-U (m/abs x))))))

(defn ulaw-rev
  ""
  ^double [^double y]
  (* (m/sgn y)
     ulaw-rU
     (dec (m/pow ulaw-U1 (m/abs y)))))

(defn- apply-sign
  ""
  ^long [^long in ^long bits]
  (let [sh (- 64 bits)]
    (-> in
        (bit-shift-left sh)
        (bit-shift-right sh))))

;; TODO: move me to bit operations!!!
(defn- pack-with-endianess-and-sign
  ""
  (^long [^long x little-endian signed]
   (if signed (apply-sign x 8) x))
  (^long [^long x ^long y little-endian signed]
   (let [[^long a ^long b] (if little-endian [y x] [x y])
         val (bit-or (bit-shift-left (bit-and a 0xff) 8)
                     (bit-and b 0xff))]
     (if signed (apply-sign val 16) val))
   )
  ([x y z little-endian signed]
   (let [[^long a ^long b ^long c] (if little-endian [z y x] [x y z])
         val (bit-or (bit-shift-left (bit-and a 0xff) 16)
                     (bit-shift-left (bit-and b 0xff) 8)
                     (bit-and c 0xff))]
     (if signed (apply-sign val 24) val))))

(def ^:const ^long s8-min (apply-sign 0x80 8))
(def ^:const ^long s8-max (apply-sign 0x7f 8))
(def ^:const ^long s16-min (apply-sign 0x8000 16))
(def ^:const ^long s16-max (apply-sign 0x7fff 16))
(def ^:const ^long s24-min (apply-sign 0x800000 24))
(def ^:const ^long s24-max (apply-sign 0x7fffff 24))

(defn int-to-float
  ""
  (^double [x endianess sign]
   (let [v (pack-with-endianess-and-sign x endianess sign)]
     (if sign
       (m/norm v s8-min s8-max -1.0 1.0)
       (m/norm v 0 0xff -1.0 1.0))))
  (^double [x y endianess sign]
   (let [v (pack-with-endianess-and-sign x y endianess sign)]
     (if sign
       (m/norm v s16-min s16-max -1.0 1.0)
       (m/norm v 0 0xffff -1.0 1.0))))
  ([x y z endianess sign]
   (let [^long v (pack-with-endianess-and-sign x y z endianess sign)]
     (if sign
       (m/norm v s24-min s24-max -1.0 1.0)
       (m/norm v 0 0xffffff -1.0 1.0)))))

(defn float-to-int
  ""
  [^long bits ^double v little-endian sign]
  (let [vv (m/constrain v -1.0 1.0)
        restored (long (condp == bits
                         8 (if sign
                               (m/norm vv -1.0 1.0 s8-min s8-max)
                               (m/norm vv -1.0 1.0 0 0xff))
                         16 (if sign
                                (m/norm vv -1.0 1.0 s16-min s16-max)
                                (m/norm vv -1.0 1.0 0 0xffff))
                         24 (if sign
                                (m/norm vv -1.0 1.0 s24-min s24-max)
                                (m/norm vv -1.0 1.0 0 0xffffff))))]
    (condp == bits
      8 (bit-and restored 0xff)
      16 (let [a (bit-and restored 0xff)
               b (bit-and (bit-shift-right restored 8) 0xff)]
           (if little-endian (Vec2. a b) (Vec2. b a)))
      24 (let [a (bit-and restored 0xff)
               b (bit-and (bit-shift-right restored 8) 0xff)
               c (bit-and (bit-shift-right restored 16) 0xff)]
           (if little-endian (Vec3. a b c) (Vec3. c b a))))))

(defn- pre-layout-planar
  "dir == true: pixels -> buffer"
  ([dir ^Pixels p channels ^ints buff]
   (loop [ch channels
          iter (int 0)]
     (when ch
       (let [channel (first ch)
             curr-iter (loop [idx (int 0)
                              titer (int iter)]
                         (if (< idx (.size p))
                           (do
                             (if dir
                               (aset ^ints buff titer (int (p/get-value p channel idx)))
                               (p/set-value p channel idx (int (aget ^ints buff titer))))
                             (recur (inc idx) (inc titer)))
                           titer))]
         (recur (next ch) (int curr-iter)))))
   buff)
  ([dir ^Pixels p channels]
   (let [buff (int-array (* (count channels) (.size p)))]
     (pre-layout-planar dir p channels buff))))

(defn- pre-layout-interleaved
  "dir == true: pixels -> buffer"
  ([dir ^Pixels p channels ^ints buff]
   (loop [idx (int 0)
          iter (int 0)]
     (when (< idx (.size p))
       (let [curr-iter (loop [ch channels
                              titer (int iter)]
                         (if ch
                           (let [channel (first ch)]
                             (if dir
                               (aset ^ints buff titer (int (p/get-value p channel idx)))
                               (p/set-value p channel idx (int (aget ^ints buff titer))))
                             (recur (next ch) (inc titer)))
                           titer))]
         (recur (inc idx) (int curr-iter)))))
   buff)
  ([dir ^Pixels p channels]
   (let [buff (int-array (* (count channels) (.size p)))]
     (pre-layout-interleaved dir p channels buff))))

;; convert from Pixels to Signal and vice versa
;; :layout [:planar :interleaved]
;; :coding [:none :alaw :ulaw :alaw-rev :ulaw-rev]
;; :signed [true false]
;; :bits [8 16 24]
;; :little-endian [true false]
;; :channels :all or list of channels

(def pixels-default-configuration
  {:layout :planar
   :coding :none
   :signed false
   :bits 8
   :little-endian true
   :channels [0 1 2]})

(defn signal-from-pixels
  ""
  ([^Pixels p conf]
   (let [config (merge pixels-default-configuration conf)
         channels (if (= :all (:channels config)) [0 1 2 3] (:channels config))
         ^long b (:bits config)
         e (:little-endian config)
         s (:signed config)
         nb (bit-shift-right b 3)
         tsize (int (m/ceil (/ (* (count channels) (.size p)) nb)))
         ^doubles buff (double-array tsize)
         ^ints layout (if (= :planar (:layout config))
                        (pre-layout-planar true p channels)
                        (pre-layout-interleaved true p channels))
         limit (- (alength layout) (dec nb))
         coding (condp = (:coding config)
                  :none identity
                  :alaw alaw
                  :ulaw ulaw
                  :alaw-rev alaw-rev
                  :ulaw-rev ulaw-rev)]
     (loop [idx (int 0)
            bidx (int 0)]
       (when (< idx limit)
         (condp == nb
           1 (aset ^doubles buff bidx ^double (coding (int-to-float (aget ^ints layout idx) e s)))
           2 (aset ^doubles buff bidx ^double (coding (int-to-float (aget ^ints layout idx) (aget ^ints layout (inc idx)) e s)))
           3 (aset ^doubles buff bidx ^double (coding (int-to-float (aget ^ints layout idx) (aget ^ints layout (inc idx)) (aget ^ints layout (+ 2 idx)) e s))))
         (recur (+ idx nb) (inc bidx))))

     (Signal. buff)))
  ([p]
   (signal-from-pixels p {})))

(defn signal-to-pixels
  ""
  [^Pixels target ^Signal sig conf]
  (let [config (merge pixels-default-configuration conf)
        channels (if (= :all (:channels config)) [0 1 2 3] (:channels config))
        ^long b (:bits config)
        e (:little-endian config)
        s (:signed config)
        nb (bit-shift-right b 3)
        tsize (int (m/ceil (/ (* (count channels) (.size target)) nb)))
        ^doubles buff (.signal sig)
        ^ints layout (int-array (* (count channels) (.size target)))
        limit (- (alength layout) (dec nb))
        coding (condp = (:coding config)
                 :none identity
                 :alaw alaw
                 :ulaw ulaw
                 :alaw-rev alaw-rev
                 :ulaw-rev ulaw-rev)]

    (loop [idx (int 0)
           bidx (int 0)]
      (when (< idx limit)
        (let [v (coding (aget ^doubles buff bidx))]
          (condp = nb
            1 (aset ^ints layout idx (int (float-to-int 8 v e s)))
            2 (let [^Vec2 v (float-to-int 16 v e s)]
                (aset ^ints layout idx (int (.x v)))
                (aset ^ints layout (inc idx) (int (.y v))))
            3 (let [^Vec3 v (float-to-int 24 v e s)]
                (aset ^ints layout idx (int (.x v)))
                (aset ^ints layout (inc idx) (int (.y v)))
                (aset ^ints layout (+ 2 idx) (int (.z v))))))
        (recur (+ idx nb) (inc bidx))))

    (if (= :planar (:layout config))
      (pre-layout-planar false target channels layout)
      (pre-layout-interleaved false target channels layout))

    target))

;; state management functions and transducer operations
;; minimizing destructuring as much as possible...

(deftype StateWithS [^double sample state]
  Object
  (toString [_] (str sample))) ; sample and effect state, StateWithF or vector of StateWithF
(deftype StateWithF [f state]) ; effect and its state

(defn- next-effect
  ""
  [sample ^StateWithF state]
  (let [^StateWithS res ((.f state) sample (.state state))]
    (StateWithS. (.sample res) (StateWithF. (.f state) (.state res)))))

(defn- process-effects-one-pass
  ""
  [sample effects]
  (let [size (count effects)]
    (loop [i (int 0)
           ^StateWithS acc (StateWithS. sample [])]
      (if (< i size)
        (let [^StateWithS ne (next-effect (.sample acc) (effects i))]
          (recur (inc i) (StateWithS. (.sample ne) (conj (.state acc) (.state ne)))))
        acc))))

(defn- create-state
  ""
  ([effects]
   (vec (map #(StateWithF. % (%)) effects)))
  ([effects initial-state]
   (vec (map #(StateWithF. %1 %2) effects initial-state))))

(defn apply-effects
  "Apply effects on signal"
  ([effects ^Signal s ^long rst]
   (let [len (alength ^doubles (.signal s))
         ^doubles in (.signal s)
         ^doubles out (double-array len)]
     (loop [idx (int 0)
            effects_and_state (create-state effects)]
       (when (< idx len)
         (let [sample (aget ^doubles in idx)
               ^StateWithS res (process-effects-one-pass sample effects_and_state)
               nidx (inc idx)]
           (aset ^doubles out idx (double (.sample res)))
           (recur nidx 
                  (if (and (pos? rst) (zero? ^long (mod nidx rst)))
                    (create-state effects)
                    (.state res))))))
     (Signal. out)))
  ([effects s]
   (apply-effects effects s 0)))

(defn apply-effect
  ""
  ([effect ^Signal s ^long rst]
   (let [len (alength ^doubles (.signal s))
         ^doubles in (.signal s)
         ^doubles out (double-array len)]
     (loop [idx (int 0)
            effect_and_state (effect)]
       (when (< idx len)
         (let [sample (aget ^doubles in idx)
               ^StateWithS state (effect sample effect_and_state)
               nidx (inc idx)]
           (aset ^doubles out idx (double (.sample state)))
           (recur nidx 
                  (if (and (pos? rst) (zero? ^long (mod nidx rst)))
                    (effect)
                    (.state state))))))
     (Signal. out)))
  ([effect s]
   (apply-effect effect s 0)))

(defn- make-filter-fn
  ""
  ([f effects config config-back rst]
   (fn [ch target p]
     (let [c {:channels [ch]}
           sig (signal-from-pixels p (merge config c))
           res (f effects sig rst)]
       (signal-to-pixels target res (merge config-back c)))))
  ([f effects rst]
   (make-filter-fn f effects {} {} rst))
  ([f effects config config-back]
   (make-filter-fn f effects config config-back 0))
  ([f effects]
   (make-filter-fn f effects {} {} 0)))

(def make-effects-filter (partial make-filter-fn apply-effects))
(def make-effect-filter (partial make-filter-fn apply-effect))

;;; test

(deftype EffectList [sample effect state next]
  Object
  (toString [_] (str sample)))

(defn single-pass
  ""
  [^EffectList e sample]
  (if (nil? (.next e))
    (let [^StateWithS r ((.effect e) sample (.state e))]
      (EffectList. (.sample r) (.effect e) (.state r) nil))
    (let [^EffectList prev (single-pass (.next e) sample)]
      (let [^StateWithS r ((.effect e) (.sample prev) (.state e))]
        (EffectList. (.sample r) (.effect e) (.state r) prev)))))

(defn make-effect-list
  ""
  [init f]
  (EffectList. 0.0 f init nil))

(defn compose-effects
  ""
  [^EffectList e & es]
  (if (nil? es)
    e
    (EffectList. (.sample e) (.effect e) (.state e) (apply compose-effects es))))

;; (def lp1 (make-effect :simple-lowpass {:rate 44100 :cutoff 10000}))
;; (def lp2 (make-effect :simple-lowpass {:rate 44100 :cutoff 5000}))
;; (def lp3 (make-effect :simple-lowpass {:rate 44100 :cutoff 2000}))
;; (def dj (make-effect :dj-eq {:lo (r/drand -5 5) :mid (r/drand -5 5) :hi (r/drand -5 5) :peak_bw 1.3 :shelf_slope 1.5 :rate (r/irand 4000 100000)}))

;; (def lll1 lp1)
;; (def lll2 (create-state [lp1 lp2 lp3]))

;; (def lll3 (compose-effects lp1 lp2 lp3))

;; (bench/quick-bench (call lll1 (r/drand -1.0 1.0)))

;; (single-pass lll1 1.0)
;; (process-effects-one-pass 1.0 lll2)

;; (bench/quick-bench (single-pass lll1 (r/drand -1.0 1.0)))
;; (bench/quick-bench (process-effects-one-pass (r/drand -1.0 1.0) lll2))


;;; multimethod - effect creators

(defmulti make-effect (fn [m conf] m))

;;;;;;;;;;;;; EFFECTS


;; SIMPLE LOWPASS/HIGHPASS

(defn- calc-filter-alpha
  ""
  ^double [^double rate ^double cutoff]
  (let [tinterval (/ 1.0 rate)
        tau (/ 1.0 (* cutoff m/TWO_PI))]
    (/ tinterval (+ tau tinterval))))

(defmethod make-effect :simple-lowpass [_ conf]
  (let [alpha (calc-filter-alpha (:rate conf) (:cutoff conf))] 
    (make-effect-list 0.0 (fn
                            ([^double sample ^double prev]
                             (let [s1 (* sample alpha)
                                   s2 (- prev (* prev alpha))
                                   nprev (+ s1 s2)]
                               (StateWithS. nprev nprev)))))))

(defmethod make-effect :simple-highpass [_ conf]
  (let [lpfilter (make-effect :simple-lowpass conf)]
    (make-effect-list lpfilter (fn
                                 ([^double sample lp]
                                  (let [^EffectList res (single-pass lpfilter sample)]
                                    (StateWithS. (- sample (.sample res)) res)))))))

;; BIQUAD FILTERS

(deftype BiquadConf [^double b0 ^double b1 ^double b2 ^double a1 ^double a2])

(defn biquad-eq-params
  "fc - center frequency
   gain
   bw - bandwidth
   fs - sample rate"
  [^double fc ^double gain ^double bw ^double fs]
  (let [w (/ (* m/TWO_PI (m/constrain fc 1.0 (* 0.5 fs))) fs)
        cw (m/cos w)
        sw (m/sin w)
        J (m/pow 10.0 (* gain 0.025))
        g (-> bw
              (m/constrain 0.0001 4.0)
              (* m/LN2_2 w)
              (/ sw)
              (m/sinh)
              (* sw))
        a0r (/ 1.0 (+ 1.0 (/ g J)))

        b0 (* a0r (+ 1.0 (* g J)))
        b1 (* a0r -2.0 cw)
        b2 (* a0r (- 1.0 (* g J)))
        a1 (- b1)
        a2 (* a0r (- (/ g J) 1.0))]
    (BiquadConf. b0 b1 b2 a1 a2)))

(defn biquad-hs-params
  "fc - center frequency
   gain
   slope - shelf slope
   fs - sample rate"
  [^double fc ^double gain ^double slope ^double fs]
  (let [w (/ (* m/TWO_PI (m/constrain fc 1.0 (* 0.5 fs))) fs)
        cw (m/cos w)
        sw (m/sin w)
        A (m/pow 10.0 (* gain 0.025))
        iA (inc A)
        dA (dec A)
        b (m/sqrt (- (/ (inc (* A A)) (m/constrain slope 0.0001 1.0)) (* dA dA)))
        apc (* cw iA)
        amc (* cw dA)
        bs (* b sw)
        a0r (->> amc
                 (- iA)
                 (+ bs)
                 (/ 1.0))

        b0 (* a0r A (+ iA amc bs))
        b1 (* a0r A -2.0 (+ dA apc))
        b2 (* a0r A (- (+ iA amc) bs))
        a1 (* a0r -2.0 (- dA apc))
        a2 (* a0r (+ (dec (- A)) amc bs))]
    (BiquadConf. b0 b1 b2 a1 a2)))

(defn biquad-ls-params
  "fc - center frequency
   gain
   slope - shelf slope
   fs - sample rate"
  [^double fc ^double gain ^double slope ^double fs]
  (let [w (/ (* m/TWO_PI (m/constrain fc 1.0 (* 0.5 fs))) fs)
        cw (m/cos w)
        sw (m/sin w)
        A (m/pow 10.0 (* gain 0.025))
        iA (inc A)
        dA (dec A)
        b (m/sqrt (- (/ (inc (* A A)) (m/constrain slope 0.0001 1.0)) (* dA dA)))
        apc (* cw iA)
        amc (* cw dA)
        bs (* b sw)
        a0r (->> amc
                 (+ iA)
                 (+ bs)
                 (/ 1.0))

        b0 (* a0r A (- (+ iA bs) amc))
        b1 (* a0r A 2.0 (- dA apc))
        b2 (* a0r A (- iA amc bs))
        a1 (* a0r 2.0 (+ dA apc))
        a2 (* a0r (+ bs (- (dec (- A)) amc)))]
    (BiquadConf. b0 b1 b2 a1 a2)))

(defn biquad-lp-params
  ""
  [^double fc ^double bw ^double fs]
  (let [omega (* m/TWO_PI (/ fc fs))
        sn (m/sin omega)
        cs (m/cos omega)
        alpha (* sn (m/sinh (* m/LN2_2 bw (/ omega sn))))
        a0r (/ 1.0 (inc alpha))
        cs- (- 1.0 cs)
        
        b0 (* a0r 0.5 cs-)
        b1 (* a0r cs-)
        b2 b0
        a1 (* a0r 2.0 cs)
        a2 (* a0r (dec alpha))]
    (BiquadConf. b0 b1 b2 a1 a2)))

(defn biquad-hp-params
  ""
  [^double fc ^double bw ^double fs]
  (let [omega (* m/TWO_PI (/ fc fs))
        sn (m/sin omega)
        cs (m/cos omega)
        alpha (* sn (m/sinh (* m/LN2_2 bw (/ omega sn))))
        a0r (/ 1.0 (inc alpha))
        cs+ (inc cs)
        
        b0 (* a0r 0.5 cs+)
        b1 (* a0r (- cs+))
        b2 b0
        a1 (* a0r 2.0 cs)
        a2 (* a0r (dec alpha))]
    (BiquadConf. b0 b1 b2 a1 a2)))

(defn biquad-bp-params
  ""
  [^double fc ^double bw ^double fs]
  (let [omega (* m/TWO_PI (/ fc fs))
        sn (m/sin omega)
        cs (m/cos omega)
        alpha (* sn (m/sinh (* m/LN2_2 bw (/ omega sn))))
        a0r (/ 1.0 (inc alpha))
        
        b0 (* a0r alpha)
        b1 0.0
        b2 (* a0r (- alpha))
        a1 (* a0r 2.0 cs)
        a2 (* a0r (dec alpha))]
    (BiquadConf. b0 b1 b2 a1 a2)))

(deftype StateBiquad [^double x2 ^double x1 ^double y2 ^double y1])

(defn biquad-filter
  ""
  ([^BiquadConf c ^double sample ^StateBiquad state]
   (let [y (-> (* (.b0 c) sample)
               (+ (* (.b1 c) (.x1 state)))
               (+ (* (.b2 c) (.x2 state)))
               (+ (* (.a1 c) (.y1 state)))
               (+ (* (.a2 c) (.y2 state))))]
     (StateWithS. y (StateBiquad. (.x1 state) sample (.y1 state) y))))
  ([_]
   (StateBiquad. 0.0 0.0 0.0 0.0)))

(defmethod make-effect :biquad-eq [_ conf]
  (partial biquad-filter (biquad-eq-params (:fc conf) (:gain conf) (:bw conf) (:fs conf))))

(defmethod make-effect :biquad-hs [_ conf]
  (partial biquad-filter (biquad-hs-params (:fc conf) (:gain conf) (:slope conf) (:fs conf))))

(defmethod make-effect :biquad-ls [_ conf]
  (partial biquad-filter (biquad-ls-params (:fc conf) (:gain conf) (:slope conf) (:fs conf))))

(defmethod make-effect :biquad-lp [_ conf]
  (partial biquad-filter (biquad-lp-params (:fc conf) (:bw conf) (:fs conf))))

(defmethod make-effect :biquad-hp [_ conf]
  (partial biquad-filter (biquad-hp-params (:fc conf) (:bw conf) (:fs conf))))

(defmethod make-effect :biquad-bp [_ conf]
  (partial biquad-filter (biquad-bp-params (:fc conf) (:bw conf) (:fs conf))))

(deftype StateDjEq [^StateBiquad s1 ^StateBiquad s2 ^StateBiquad s3])

(defmethod make-effect :dj-eq [_ conf]
  (let [b1 (make-effect :biquad-eq {:fc 100.0 :gain (:lo conf) :bw (:peak_bw conf) :fs (:rate conf)})
        b2 (make-effect :biquad-eq {:fc 1000.0 :gain (:mid conf) :bw (:peak_bw conf) :fs (:rate conf)})
        b3 (make-effect :biquad-hs {:fc 10000.0 :gain (:hi conf) :slope (:shelf_slope conf) :fs (:rate conf)})]
    (fn
      ([sample ^StateDjEq state]
       (let [^StateWithS r1 (b1 sample (.s1 state))
             ^StateWithS r2 (b2 (.sample r1) (.s2 state))
             ^StateWithS r3 (b3 (.sample r2) (.s3 state))]
         (StateWithS. (.sample r3) (StateDjEq. (.state r1) (.state r2) (.state r3)))))
      ([]
       (StateDjEq. (b1) (b2) (b3))))))

(defmethod make-effect :phaser-allpass [_ conf]
  (let [^double d (:delay conf)
        a1 (/ (- 1.0 d) (inc d))]
    (fn
      ([^double sample ^double zm1]
       (let [y (+ zm1 (* sample (- a1)))
             new-zm1 (+ sample (* y a1))]
         (StateWithS. y new-zm1)))
      ([] 0.0))))
;; 

(deftype StateDivider [^double out ^double amp ^double count ^double lamp ^double last ^int zeroxs])

(defmethod make-effect :divider [_ conf]
  (let [^long denom (:denominator conf)]
    (fn
      ([^double sample ^StateDivider state]
       (let [count (inc (.count state))
             ^StateDivider s1 (if (or (and (> sample 0.0) (<= (.last state) 0.0))
                                      (and (neg? sample) (>= (.last state) 0.0)))
                                (if (== denom 1)
                                  (StateDivider. (if (pos? (.out state)) -1.0 1.0) 0.0 0.0 (/ (.amp state) count) (.last state) 0)
                                  (StateDivider. (.out state) (.amp state) count (.lamp state) (.last state) (inc (.zeroxs state))))
                                (StateDivider. (.out state) (.amp state) count (.lamp state) (.last state) (.zeroxs state)))
             amp (+ (.amp s1) (m/abs sample))
             ^StateDivider s2 (if (and (> denom 1)
                                       (== ^long (rem (.zeroxs s1) denom) (dec denom)))
                                (StateDivider. (if (pos? (.out s1)) -1.0 1.0) 0.0 0 (/ amp (.count s1)) (.last s1) 0)
                                (StateDivider. (.out s1) amp (.count s1) (.lamp s1) (.last s1) (.zeroxs s1)))]
         (StateWithS. (* (.out s2) (.lamp s2)) (StateDivider. (.out s2) (.amp s2) (.count s2) (.lamp s2) sample (.zeroxs s2)))))
      ([]
       (StateDivider. 1.0 0.0 0.0 0.0 0.0 0.0)))))

;; FM
(deftype StateFm [^double pre ^double integral ^int t lp])

(defmethod make-effect :fm [_ conf]
  (let [lp-chain [(make-effect :simple-lowpass {:rate 100000 :cutoff 25000})
                  (make-effect :simple-lowpass {:rate 100000 :cutoff 10000})
                  (make-effect :simple-lowpass {:rate 100000 :cutoff 1000})]
        ^double quant (:quant conf)
        ^double omega (:omega conf)
        ^double phase (:phase conf)]
    (fn
      ([^double sample ^StateFm state]
       (let [sig (* sample phase)
             new-integral (+ (.integral state) sig)
             m (m/cos (+ new-integral (* omega (.t state))))
             ^double m (if (pos? quant)
                         (m/norm (int (m/norm m -1.0 1.0 0.0 quant)) 0.0 quant -1.0 1.0)
                         m)
             dem (m/abs (- m (.pre state)))
             ^StateWithS res (process-effects-one-pass dem (.lp state))
             demf (/ (* 2.0 (- (.sample res) omega)) phase)]
         (StateWithS. (m/constrain demf -1.0 1.0) (StateFm. m new-integral (inc (.t state)) (.state res)))))
      ([]
       (StateFm. 0.0 0.0 0 (create-state lp-chain))))))

;; bandwidth limit
;; https://searchcode.com/file/18573523/cmt/src/lofi.cpp#

(defmethod make-effect :bandwidth-limit [_ conf]
  (let [dx (double (/ (:freq conf) (:rate conf)))]
    (fn
      ([^double sample ^double state]
       (let [res (if (>= sample state)
                   (min (+ state dx) sample)
                   (max (- state dx) sample))]
         (StateWithS. res res)))
      ([] 0.0))))

(defmethod make-effect :distort [_ conf]
  (let [fact (double (:factor conf))
        nfact (inc fact)]
    (fn 
      ([^double sample state]
       (let [div (+ fact (m/abs sample))
             res (* nfact (/ sample div))]
         (StateWithS. res state)))
      ([] nil))))


;; foverdrive

(defmethod make-effect :foverdrive [_ conf]
  (let [^double drive (:drive conf)
        drivem1 (dec drive)]
    (fn
      ([^double sample state]
       (let [fx (m/abs sample)
             res (/ (* sample (+ fx drive)) (inc (+ (* sample sample) (* fx drivem1))))]
         (StateWithS. res state)))
      ([] nil))))

;; decimator

(deftype StateDecimator [^double count ^double last])

(defmethod make-effect :decimator [_ conf]
  (let [step (m/pow 0.5 (- ^double (:bits conf) 0.9999))
        stepr (/ 1.0 step)
        ratio (/ ^double (:fs conf) ^double (:rate conf))]
    (fn
      ([^double sample ^StateDecimator state]
       (let [ncount (+ (.count state) ratio)]
         (if (>= ncount 1.0)
           (let [delta (* step ^double (rem (->> sample
                                                 m/sgn
                                                 (* step 0.5)
                                                 (+ sample)
                                                 (* stepr)) 1.0))
                 last (- sample delta)]
             (StateWithS. last (StateDecimator. (dec ncount) last)))
           (StateWithS. (.last state) (StateDecimator. ncount (.last state))))))
      ([] (StateDecimator. 0.0 0.0)))))

;;; load and save signal
;;; file representation is 16 bit signed, big endian file
;;; please use Audacity/SOX utilities to convert files
(defn save-signal
  ""
  [^Signal s filename]
  (make-parents filename)
  (let [^java.io.DataOutputStream out (java.io.DataOutputStream. (output-stream filename))]
    (try
      (dotimes [i (alength ^doubles (.signal s))]
        (.writeShort out (short (m/cnorm (aget ^doubles (.signal s) i) -1.0 1.0 Short/MIN_VALUE Short/MAX_VALUE))))
      (.flush out)
      (finally (. out clojure.core/close)))
    s))

(defn load-signal
  ""
  [filename]
  (let [^java.io.File f (file filename)
        len (/ (.length f) 2)
        ^java.io.DataInputStream in (java.io.DataInputStream. (input-stream filename))
        ^doubles buffer (double-array len)]
    (try
      (dotimes [i len]
        (aset ^doubles buffer (int i) (double (m/cnorm (.readShort in) Short/MIN_VALUE Short/MAX_VALUE -1.0 1.0))))
      (finally (. in clojure.core/close)))
    (Signal. buffer)))


;;;;;;;;;;;;;;;
;;; wave generators

(def snoise (j/make-noise (j/auto-correct (j/make-basis) 10000 -1.0 1.0)))

(defmulti make-wave (fn [f _ _ _] f))

(defmethod make-wave :sin [_ ^double f ^double a ^double p]
  (fn ^double [^double x]
    (* a
     (m/sin (+ (* p m/TWO_PI) (* x m/TWO_PI f))))))

(defmethod make-wave :noise [_ ^double f ^double a ^double p]
  (fn ^double [^double x]
    (* a
       ^double (snoise (* (+ p x) f)))))

(defmethod make-wave :saw [_ ^double f ^double a ^double p] 
  (fn ^double [^double x]
    (let [rp (* 2.0 a)
          p2 (* f ^double (mod (+ (* a p) a x) 1.0))]
      (* rp (- p2 (m/floor p2) 0.5)))))

(defmethod make-wave :square [_ ^double f ^double a ^double p]
  (fn ^double [^double x]
    (if (< ^double (mod (+ p (* x f)) 1.0) 0.5)
      a
      (- a))))

(defmethod make-wave :triangle [_ ^double f ^double a ^double p]
  (let [saw (make-wave :saw f a p)]
    (fn ^double [^double x]
      (- (* 2.0 (m/abs (saw x))) a))))

(defmethod make-wave :cut-triangle [_ ^double f ^double a ^double p]
  (let [tri (make-wave :triangle f a p)]
    (fn ^double [^double x]
      (let [namp (* 0.5 a)]
        (* 2.0 (m/constrain (tri x) (- namp) namp))))))

(def waves [:sin :noise :saw :square :triangle :cut-triangle])

(defn sum-waves
  ""
  ^double [fs ^double x]
  (reduce #(+ ^double %1 ^double (%2 x)) 0.0 fs))

(defn make-sum-wave
  ""
  [fs]
  (partial sum-waves fs))

(defn make-signal-from-wave
  ""
  [f ^double samplerate ^double seconds]
  (let [len (* samplerate seconds)
        ^doubles buffer (double-array len)
        limit (dec seconds)]
    (dotimes [i len]
      (aset ^doubles buffer (int i) (double (f (m/norm i 0 len 0 limit)))))
    (Signal. buffer)))
