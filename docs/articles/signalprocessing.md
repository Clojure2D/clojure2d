# Signal Processing in Clojure

Few months ago I ported my [Sonification](https://github.com/SonifyIt/sonification) Processing project to Clojure as apart of my Clojure2D library. Since I'm quite fresh to Clojure and Functional Programming it has been a great task to work with.

The main issue is to produce kind of stateful map function with transducing capability. Having signal I want to apply sequence of filters and keep filters' state to process next sample.

Here is step by step reconstruction of my approach. I've started with native Clojure constructs (like vectors, destructuring) but finally I created custom types to pass state between calls to speed up processing.

## Filters

Let's start with filters. I introduce two of them: simple low pass and high pass filters. Each filter should have:

* Constructor accepting filter configuration (here: sample rate, cutoff)
* Filter itself which:
  - accepts: sample and current state
  - returns: filtered sample and new state
* Initial state creator. This is done by calling filter function without parameters.

Let's do this.

First lowpass filter.

``` eval-clojure
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
```

And highpass filter (in terms of lowpass filter)


``` eval-clojure
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
```

## Processor

And now time for our signal processor. We need possibility to apply as many filters as we want for one sample, keep the state somewhere and map all samples in the collection. 

To make composition I decided to keep filters with current state in a vector as a pair `[filter state]`. Now let's build function which processes given sample through all filters. Function accepts sample and state, returns resulting sample and new state of all filters.

``` eval-clojure
(defn make-filters-state
  "Maps filters to filter-state pairs"
  [fs]
  (map #(vector % (%)) fs))

(def initial-filters-state (make-filters-state [lowpass-filter highpass-filter]))

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
```

And now it's time to add final signal processing function. I used lazy sequence as a result. Filters state is passed between calls.

<pre><code class="language-eval-clojure" data-loop-msec="2000">(defn process-signal
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
</code></pre>

Too see that filters work let's apply lowpass filter for high frequency signal.

``` eval-clojure
(process-signal
 [-1.0 0.0 1.0 0.0]
 [lowpass-filter lowpass-filter lowpass-filter])

```

Signal is almost filtered out). Now check highpass filter on the same signal.

``` eval-clojure
(process-signal
 [-1.0 0.0 1.0 0.0]
 [highpass-filter highpass-filter highpass-filter])
```

Signal is almost untouched.

## Other approaches

Eric Shull (exupero) proposed on solution based on stateful transducers [-> LINK](http://exupero.org/hazard/post/signal-processing/)

My `Clojure2D` implementation is based on concept described here but based on arrays of doubles and custom types instead of vectors and destructuring.
