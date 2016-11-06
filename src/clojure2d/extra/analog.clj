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
;; You operate on Pixels you can use helper function `make-effects-filter` where you pass list of the filters with or without inital state. Such filter can be used on Pixels like any other filter (like blur, normalize, etc.) with `p/filter-channels`
;;
;; See examples 16
;;
;; Check description about each effect to see what configuration is required and what contains state.
;;

(ns clojure2d.extra.analog
  (:require [clojure2d.math :as m]
            [clojure2d.pixels :as p]
            [clojure2d.core :refer :all]
            [clojure2d.color :as c])
  (:import [clojure2d.pixels Pixels]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; c/clamp255 or c/mod255
(def ^:dynamic *clamp-method* c/clamp255)

;; state management functions and transducer operations

(defn- next-effect
  ""
  [[f state] sample]
  (let [[res & resstate] (f state sample)]
    [res f resstate]))

(defn- process-effects-one-pass
  ""
  [sample effects]
  (reduce #(let [[s fs] %1
                 [res & ns] (next-effect %2 s)]
             [res (conj fs ns)]) [sample []] effects))

(defn- create-state
  ""
  ([effects]
   (map (fn [f] [f (f [])]) effects))
  ([effects initial-state]
   (map (fn [f c] [f (f c)]) effects initial-state)))

(defn- apply-effects
  "Apply effects on Pixels (channel)"
  [state ch ^Pixels target ^Pixels p]
  (loop [idx (int 0)
         effects_and_state state]
    (when (< idx (.size target))
      (let [sample (p/get-value p ch idx)
            [res conf] (process-effects-one-pass sample effects_and_state)]
        (p/set-value target ch idx (*clamp-method* res))
        (recur (inc idx) conf))))
  :done)

(defn make-effects-filter
  ""
  ([effects initial-state]
   (partial apply-effects (create-state effects initial-state)))
  ([effects]
   (partial apply-effects (create-state effects))))

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
        b1 (* a0r (* -2.0 cw))
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

