(ns clojure2d.utils
  (:require [clojure.string :as s]
            [clojure2d.math :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defn repeat-val
  "Repeats given value num times and return String"
  [v cnt]
  (s/join (repeat cnt v)))

(defn pad-left
  "Pad string with given char(s)"
  ([string cnt]
   (pad-left string cnt \space))
  ([string cnt padchr]
   (let [c (count string)]
     (if (< c cnt)
       (str (repeat-val padchr (- cnt c)) string)
       string))))

(defn to-hex
  "return hex value of given number, padded with leading zeroes if given length"
  ([n]
   (s/upper-case 
    (cond 
      (instance? Byte n) (Integer/toHexString (unchecked-byte n))
      (instance? Integer n) (Integer/toHexString (unchecked-int n))
      :else (Long/toHexString (unchecked-long n)))))
  ([n pad]
   (pad-left (to-hex n) pad \0)))

(defmacro time-with-name
  "Evaluates expr and prints the time it took.  Returns the value of
 expr."
  [ss expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     (prn (str ~ss " Elapsed time: " (/ (double (- (. System (nanoTime)) start#)) 1000000.0) " msecs"))
     ret#))

;; array

(defn array-mutate!
  "Mutate int array value with function f"
  [f ^ints array idx]
  (let [v (aget array idx)]
    (aset array idx ^int (f v))))

(def ainc! (partial array-mutate! inc))
(def adec! (partial array-mutate! dec))

(defmacro amap!
  "Mutating version of amap"
  {:added "1.0"}
  [a idx expr]
  `(let [a# ~a]
     (loop  [~idx 0]
       (if (< ~idx  (alength a#))
         (do
           (aset-int a# ~idx ~expr)
           (recur (unchecked-inc ~idx)))
         a#))))

(defn aget-2d
  "Get value from int array, treat as 2d"
  [^ints array w h x y]
  (if (or (neg? x)
          (neg? y)
          (>= x w)
          (>= y h))
    (aget-2d array w h (m/constrain x 0 (dec w)) (m/constrain y 0 (dec h)))
    (aget array (int (+ x (* y w))))))

(defn array-clone
  "Clone array using System/arraycopy"
  [^ints array]
  (let [len (int (alength array))
        res (int-array len)]
      (System/arraycopy array 0 ^ints res 0 len)
      res))

;;
(defn make-counter [v] 
  (let [tick (atom (dec v))]
    #(swap! tick inc)))

(defmacro doloop
  [bindings & body]
  (let [i (first bindings)
        n (second bindings)]
    `(let [n# (int ~n)]
       (loop [~i (int 0)]
         (when (< ~i n#)
           ~@body
           (recur (unchecked-inc ~i)))))))

