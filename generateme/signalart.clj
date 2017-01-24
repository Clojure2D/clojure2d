(ns signalart)

(defn calc-filter-alpha
  "Calculate alpha for low/high pass filters."
  [rate cutoff]
  (let [tinterval (/ 1.0 rate)
        tau (/ 1.0 (* cutoff 2.0 Math/PI))]
    (/ tinterval (+ tau tinterval))))

(defn make-lowpass-filter
  "Create lowpass filter function"
  [conf]
  (let [alpha (calc-filter-alpha (:rate conf) (:cutoff conf))]
    (fn
      ([sample state]
       (let [s1 (* sample alpha)
             s2 (- state (* state alpha))
             nprev (+ s1 s2)]
         [nprev nprev]))
      ([] 0.0))))

(def lowpass-filter (make-lowpass-filter {:rate 44100 :cutoff 3000}))

(lowpass-filter -0.432 (lowpass-filter))

(defn make-highpass-filter
  "Create highpass filter"
  [conf]
  (let [lpfilter (make-lowpass-filter conf)]
    (fn
      ([sample state]
       (let [[res state] (lpfilter sample state)]
         [(- sample res) state]))
      ([]
       (lpfilter)))))

(def highpass-filter (make-highpass-filter {:rate 44100 :cutoff 100}))

(highpass-filter -0.432 (highpass-filter))
