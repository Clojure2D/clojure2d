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
            [clojure2d.math.joise :as j])
  (:import [clojure2d.pixels Pixels]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; represent signal is a collection of arrays of doubles, values -1.0 to 1.0

(deftype Signal [^doubles signal]
  Object
  (toString [_] (str "size=" (alength signal))))

;; helper functions
(def ^:const alaw-A 87.6)
(def ^:const alaw-rA (/ 1.0 alaw-A))
(def ^:const alaw-lA (inc (m/log alaw-A)))
(def ^:const alaw-rlA (/ 1.0 alaw-lA))

(defn alaw
  ""
  [x]
  (let [absx (m/abs x)
        f (* alaw-rlA (m/sgn x))]
    (* f (if (< absx alaw-rA)
           (* alaw-A absx)
           (+ alaw-lA (m/log absx))))))

(defn alaw-rev
  ""
  [y]
  (let [absy (m/abs y)
        f (* alaw-rA (m/sgn y))
        v (* absy alaw-lA)]
    (* f (if (< absy alaw-rlA)
           v
           (m/exp (dec v))))))

(def ^:const ulaw-U 255.0)
(def ^:const ulaw-rU (/ 1.0 255.0))
(def ^:const ulaw-U1 (inc ulaw-U))
(def ^:const ulaw-rlnU1 (/ 1.0 (m/log ulaw-U1)))

(defn ulaw
  ""
  [x]
  (* (m/sgn x)
     ulaw-rlnU1
     (m/log (inc (* ulaw-U (m/abs x))))))

(defn ulaw-rev
  ""
  [y]
  (* (m/sgn y)
     ulaw-rU
     (dec (m/pow ulaw-U1 (m/abs y)))))

(defn- apply-sign
  ""
  [in bits]
  (let [sh (- 64 bits)]
    (-> in
         (bit-shift-left sh)
         (bit-shift-right sh))))

;; TODO: move me to bit operations!!!
(defn- pack-with-endianess-and-sign
  ""
  ([x little-endian signed]
   (if signed (apply-sign x 8) x))
  ([x y little-endian signed]
   (let [[a b] (if little-endian [y x] [x y])
         val (bit-or (bit-shift-left (bit-and a 0xff) 8)
                     (bit-and b 0xff))]
     (if signed (apply-sign val 16) val))
   )
  ([x y z little-endian signed]
   (let [[a b c] (if little-endian [z y x] [x y z])
         val (bit-or (bit-shift-left (bit-and a 0xff) 16)
                     (bit-shift-left (bit-and b 0xff) 8)
                     (bit-and c 0xff))]
     (if signed (apply-sign val 24) val))))


(def s8-min (apply-sign 0x80 8))
(def s8-max (apply-sign 0x7f 8))
(def s16-min (apply-sign 0x8000 16))
(def s16-max (apply-sign 0x7fff 16))
(def s24-min (apply-sign 0x800000 24))
(def s24-max (apply-sign 0x7fffff 24))

(defn- int-to-float
  ""
  ([x endianess sign]
   (let [v (pack-with-endianess-and-sign x endianess sign)]
     (if sign
       (m/norm v s8-min s8-max -1.0 1.0)
       (m/norm v 0 0xff -1.0 1.0))))
  ([x y endianess sign]
   (let [v (pack-with-endianess-and-sign x y endianess sign)]
     (if sign
       (m/norm v s16-min s16-max -1.0 1.0)
       (m/norm v 0 0xffff -1.0 1.0))))
  ([x y z endianess sign]
   (let [v (pack-with-endianess-and-sign x y z endianess sign)]
     (if sign
       (m/norm v s24-min s24-max -1.0 1.0)
       (m/norm v 0 0xffffff -1.0 1.0)))))

(defn- float-to-int
  ""
  [bits v little-endian sign]
  (let [vv (m/constrain v -1.0 1.0)
        restored (long (condp = bits
                         8 (if sign
                               (m/norm vv -1.0 1.0 s8-min s8-max)
                               (m/norm vv -1.0 1.0 0 0xff))
                         16 (if sign
                                (m/norm vv -1.0 1.0 s16-min s16-max)
                                (m/norm vv -1.0 1.0 0 0xffff))
                         24 (if sign
                                (m/norm vv -1.0 1.0 s24-min s24-max)
                                (m/norm vv -1.0 1.0 0 0xffffff))))]
    (condp = bits
      8 (bit-and restored 0xff)
      16 (let [a (bit-and restored 0xff)
                 b (bit-and (bit-shift-right restored 8) 0xff)]
             (if little-endian [a b] [b a]))
      24 (let [a (bit-and restored 0xff)
                 b (bit-and (bit-shift-right restored 8) 0xff)
                 c (bit-and (bit-shift-right restored 16) 0xff)]
             (if little-endian [a b c] [c b a])))))

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
         b (:bits config)
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
         (condp = nb
           1 (aset ^doubles buff bidx (double (coding (int-to-float (aget ^ints layout idx) e s))))
           2 (aset ^doubles buff bidx (double (coding (int-to-float (aget ^ints layout idx) (aget ^ints layout (inc idx)) e s))))
           3 (aset ^doubles buff bidx (double (coding (int-to-float (aget ^ints layout idx) (aget ^ints layout (inc idx)) (aget ^ints layout (+ 2 idx)) e s)))))
         (recur (+ idx nb) (inc bidx))))

     (Signal. buff)))
  ([p]
   (signal-from-pixels p {})))

(defn signal-to-pixels
  ""
  [^Pixels target ^Signal sig conf]
  (let [config (merge pixels-default-configuration conf)
        channels (if (= :all (:channels config)) [0 1 2 3] (:channels config))
        b (:bits config)
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
            2 (let [[a b] (float-to-int 16 v e s)]
                (aset ^ints layout idx (int a))
                (aset ^ints layout (inc idx) (int b)))
            3 (let [[a b c] (float-to-int 24 v e s)]
                (aset ^ints layout idx (int a))
                (aset ^ints layout (inc idx) (int b))
                (aset ^ints layout (+ 2 idx) (int c)))))
        (recur (+ idx nb) (inc bidx))))

    (if (= :planar (:layout config))
      (pre-layout-planar false target channels layout)
      (pre-layout-interleaved false target channels layout))

    target))

;; state management functions and transducer operations

(defn- next-effect
  ""
  [[f state] sample]
  (let [[res & resstate] (f state sample)]
    [res f resstate]))

(defn- process-effects-one-pass
  ""
  [sample effects]
  (let [size (count effects)]
    (loop [i (int 0)
           acc [sample []]]
      (if (< i size)
        (let [[s fs] acc
              [res & ns] (next-effect (effects i) s)]
          (recur (inc i) [res (conj fs ns)]))
        acc))))

(defn- create-state
  ""
  ([effects]
   (vec (map (fn [f] [f (f [])]) effects)))
  ([effects initial-state]
   (vec (map (fn [f c] [f (f c)]) effects initial-state))))

(defn apply-effects
  "Apply effects on signal"
  ([effects ^Signal s rst]
   (let [len (alength ^doubles (.signal s))
         ^doubles in (.signal s)
         ^doubles out (double-array len)]
     (loop [idx (int 0)
            effects_and_state (create-state effects)]
       (when (< idx len)
         (let [sample (aget ^doubles in idx)
               [res conf] (process-effects-one-pass sample effects_and_state)
               nidx (inc idx)]
           (aset ^doubles out idx (double res))
           (recur nidx 
                  (if (and (pos? rst) (zero? (mod nidx rst)))
                    (create-state effects)
                    conf)))))
     (Signal. out)))
  ([effects s]
   (apply-effects effects s 0)))

(defn make-effects-filter
  ""
  ([effects config config-back rst]
   (fn [ch target p]
     (let [c {:channels [ch]}
           sig (signal-from-pixels p (merge config c))
           res (apply-effects effects sig rst)]
       (signal-to-pixels target res (merge config-back c)))))
  ([effects rst]
   (make-effects-filter effects {} {} rst))
  ([effects config config-back]
   (make-effects-filter effects config config-back 0))
  ([effects]
   (make-effects-filter effects {} {} 0)))

;;; multimethod - effect creators

(defmulti make-effect (fn [m conf] m))

;;;;;;;;;;;;; EFFECTS


;; SIMPLE LOWPASS/HIGHPASS

(defn simple-lowpass
  ""
  ([alpha [prev] sample]
   (let [s1 (* sample alpha)
         s2 (- prev (* prev alpha))
         nprev (+ s1 s2)]
     [nprev nprev]))
  ([_ [prev]]
   (if (nil? prev)
     [0]
     [prev])))

(defn simple-highpass
  ""
  ([lpfilter conf sample]
   (let [[res rprev] (lpfilter conf sample)]
     [(- sample res) rprev]))
  ([lpfilter conf]
   (lpfilter conf)))

(defn- calc-filter-alpha
  ""
  [rate cutoff]
  (let [tinterval (/ 1.0 rate)
        tau (/ 1.0 (* cutoff m/TWO_PI))]
    (/ tinterval (+ tau tinterval))))

(defmethod make-effect :simple-lowpass [_ conf]
  (partial simple-lowpass (calc-filter-alpha (:rate conf) (:cutoff conf))))

(defmethod make-effect :simple-highpass [_ conf]
  (partial simple-highpass (make-effect :simple-lowpass conf) ))

;; BIQUAD FILTERS

(defn biquad-eq-params
  "fc - center frequency
   gain
   bw - bandwidth
   fs - sample rate"
  [fc gain bw fs]
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
    [b0 b1 b2 a1 a2]))

(defn biquad-hs-params
  "fc - center frequency
   gain
   slope - shelf slope
   fs - sample rate"
  [fc gain slope fs]
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
    [b0 b1 b2 a1 a2]))

(defn biquad-ls-params
  "fc - center frequency
   gain
   slope - shelf slope
   fs - sample rate"
  [fc gain slope fs]
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
    [b0 b1 b2 a1 a2]))

(defn biquad-lp-params
  ""
  [fc bw fs]
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
    [b0 b1 b2 a1 a2]))

(defn biquad-hp-params
  ""
  [fc bw fs]
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
    [b0 b1 b2 a1 a2]))

(defn biquad-bp-params
  ""
  [fc bw fs]
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
    [b0 b1 b2 a1 a2]))

(defn biquad-filter
  ""
  ([[b0 b1 b2 a1 a2] [x2 x1 y2 y1] sample]
   (let [y (-> (* b0 sample)
               (+ (* b1 x1))
               (+ (* b2 x2))
               (+ (* a1 y1))
               (+ (* a2 y2)))]
     [y x1 sample y1 y]))
  ([_ [x2 x1 y2 y1]]
   (map #(or % 0.0) [x2 x1 y2 y1])))

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

(defn dj-eq
  ""
  ([b1 b2 b3 [s1 s2 s3] sample]
   (let [[r1 & rs1] (b1 s1 sample)
         [r2 & rs2] (b2 s2 r1)
         [r3 & rs3] (b3 s3 r2)]
     [r3 rs1 rs2 rs3]))
  ([b1 b2 b3 [s1 s2 s3]]
   [(b1 s1) (b2 s2) (b3 s3)]))

(defmethod make-effect :dj-eq [_ conf]
  (let [b1 (make-effect :biquad-eq {:fc 100.0 :gain (:lo conf) :bw (:peak_bw conf) :fs (:rate conf)})
        b2 (make-effect :biquad-eq {:fc 1000.0 :gain (:mid conf) :bw (:peak_bw conf) :fs (:rate conf)})
        b3 (make-effect :biquad-hs {:fc 10000.0 :gain (:hi conf) :slope (:shelf_slope conf) :fs (:rate conf)})]
    (partial dj-eq b1 b2 b3)))

(defn phaser-allpass
  ""
  ([a1 [zm1] sample]
   (let [y (+ zm1 (* sample (- a1)))
         new-zm1 (+ sample (* y a1))]
     [y new-zm1]))
  ([a1 [zm1]]
   (if (nil? zm1) [0.0] [zm1])))

(defmethod make-effect :phaser-allpass [_ conf]
  (let [d (:delay conf)]
    (partial phaser-allpass (/ (- 1.0 d) (inc d)))))

(defn divider
  ""
  ([denom [out amp count lamp last zeroxs] sample]
   (let [count (inc count)
         s sample
         [out lamp zeroxs count amp] (if (or (and (> s 0.0) (<= last 0.0))
                                             (and (neg? s) (>= last 0.0)))
                                       (if (== denom 1)
                                         [(if (pos? out) -1.0 1.0) (/ amp count) 0 0.0 0.0]
                                         [out lamp (inc zeroxs) count amp])
                                       [out lamp zeroxs count amp])
         amp (+ amp (m/abs s))
         [out lamp zeroxs count amp] (if (and (> denom 1)
                                              (== (mod zeroxs denom) (dec denom)))
                                       [(if (pos? out) -1.0 1.0) (/ amp count) 0 0.0 0.0]
                                       [out lamp zeroxs count amp])
         last s]
     [(* out lamp) out amp count lamp last zeroxs]))
  ([_ [out amp count lamp last zeroxs]]
   (map #(or %1 %2) [out amp count lamp last zeroxs] [1.0 0.0 0.0 0.0 0.0 0.0])))

(defmethod make-effect :divider [_ conf]
  (partial divider (:denominator conf)))

;; FM

(defn fm
  ""
  ([lp-chain quant omega phase [pre integral t lp] sample]
   (let [sig (* sample phase)
         new-integral (+ integral sig)
         m (m/cos (+ new-integral (* omega t)))
         m (if (pos? quant)
             (m/norm (int (m/norm m -1.0 1.0 0 quant)) 0 quant -1.0 1.0)
             m)
         dem (m/abs (- m pre))
         [res newlp] (process-effects-one-pass dem lp)
         demf (/ (* 2.0 (- res omega)) phase)]
     [(m/constrain demf -1.0 1.0) m new-integral (inc t) newlp]))
  ([lp-chain _ _ _ [pre integral t lp]]
   (map #(or %1 %2) [pre integral t lp] [0.0 0.0 0 (create-state lp-chain)])))

(defmethod make-effect :fm [_ conf]
  (let [lp-chain [(make-effect :simple-lowpass {:rate 100000 :cutoff 25000})
                  (make-effect :simple-lowpass {:rate 100000 :cutoff 10000})
                  (make-effect :simple-lowpass {:rate 100000 :cutoff 1000})]]
    (partial fm lp-chain (:quant conf) (:omega conf) (:phase conf))))

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

(defn sin-wave
  ""
  [freq amp phase x]
  (* amp
     (m/sin (+ (* phase m/TWO_PI) (* x m/TWO_PI freq)))))

(def snoise (j/make-noise (j/auto-correct (j/make-basis) 10000 -1.0 1.0)))

(defn noise-wave
  ""
  [freq amp phase x]
  (* amp
     (snoise (* (+ phase x) freq))))

(defn saw-wave
  ""
  [freq amp phase x]
  (let [rp (* 2.0 amp)
        p2 (* freq (mod (+ (* amp phase) amp x) 1.0))]
    (* rp (- p2 (m/floor p2) 0.5))))

(defn square-wave
  ""
  [freq amp phase x]
  (if (< (mod (+ phase (* x freq)) 1.0) 0.5)
    amp
    (- amp)))

(defn triangle-wave
  ""
  [saw amp x]
  (- (* 2.0 (m/abs (saw x))) amp))

(defn cut-triangle-wave
  ""
  [tri amp x]
  (let [namp (* 0.5 amp)]
    (* 2.0 (m/constrain (tri x) (- namp) namp))))

(defmulti make-wave (fn [f _ _ _] f))
(defmethod make-wave :sin [_ f a p] (partial sin-wave f a p))
(defmethod make-wave :noise [_ f a p] (partial noise-wave f a p))
(defmethod make-wave :saw [_ f a p] (partial saw-wave f a p))
(defmethod make-wave :square [_ f a p] (partial square-wave f a p))
(defmethod make-wave :triangle [_ f a p]
  (let [saw (make-wave :saw f a p)]
    (partial triangle-wave saw a)))
(defmethod make-wave :cut-triangle [_ f a p] (partial cut-triangle-wave (make-wave :triangle f a p) a))

(def waves [:sin :noise :saw :square :triangle :cut-triangle])

(defn sum-waves
  ""
  [fs x]
  (reduce #(+ %1 (%2 x)) 0 fs))

(defn make-sum-wave
  ""
  [fs]
  (partial sum-waves fs))

(defn make-signal-from-wave
  ""
  [f samplerate seconds]
  (let [len (* samplerate seconds)
        ^doubles buffer (double-array len)
        limit (dec seconds)]
    (dotimes [i len]
      (aset ^doubles buffer (int i) (double (f (m/norm i 0 len 0 limit)))))
    (Signal. buffer)))
