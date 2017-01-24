(ns signalart)

;; lowpass

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

;; highpass

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

;; processing

(defn make-filters-state
  "Maps filters to filter-state pairs"
  [fs]
  (map #(vector % (%)) fs))

(def initial-filters-state 
  (make-filters-state
   [lowpass-filter highpass-filter]))

(defn process-one-sample
  "Process one sample"
  [sample filters-state]
  (loop [current-sample sample
         current-state filters-state
         new-state []]
    (if current-state
      (let [[f fstate] (first current-state)
            [res new-fstate] (f current-sample fstate)]
        (recur res (next current-state) (conj new-state [f new-fstate])))
      [current-sample new-state])))

;; let's extract only sample after filtering
((process-one-sample -0.432 initial-filters-state) 0)

(defn process-signal
  "Produce lazy sequence of signal processed with vector of filters"
  [signal filters]
  (let [current-fstate (make-filters-state filters)
        next-step (fn internal-next-step [sig fstate]
                    (when sig
                      (let [[res nfstate] (process-one-sample (first sig) fstate)]
                        (cons res (lazy-seq (internal-next-step (next sig) nfstate))))))]
    (next-step signal current-fstate)))

;; round result to 3rd decimal place
(take 10 (map #(/ (Math/round (* % 1000.0)) 1000.0)
              (process-signal
               (repeatedly #(dec (* 2.0 (rand))))
               [lowpass-filter highpass-filter])))

(process-signal
 [-1.0 0.0 1.0 0.0]
 [lowpass-filter lowpass-filter lowpass-filter])

(process-signal
 [-1.0 0.0 1.0 0.0]
 [highpass-filter highpass-filter highpass-filter])
