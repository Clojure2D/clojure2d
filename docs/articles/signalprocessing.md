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

To make composition I decided to keep filters with current state in the collection as a pair `[filter state]`. Now let's build function which for given collection of filters creates collection with filters and state pairs.
