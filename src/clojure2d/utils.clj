(ns clojure2d.utils
  (:require [clojure.string :as s]
            [clojure2d.math :as m]
            [clojure.java.io :as io]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defn to-hex
  "return hex value of given number, padded with leading zeroes if given length"
  ([n]
   (format "%X" n))
  ([n pad]
   (format (str "%0" pad "X") n)))

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

(defn make-session-name
  ""
  ([]
   (let [^java.text.SimpleDateFormat sdf (java.text.SimpleDateFormat. "yyyyMMddHHmmss")
         date (java.util.Date.)]
     [(.format sdf date) (to-hex (hash date))])))

(def ^:dynamic *log-to-file* false)

(let [session-name (atom nil) ; store session name as a current date
      session-file (agent nil) ; logger Writer, created and used when *log-to-file* is true
      session-cnt (atom nil) ; counter for next filename fn
      ^java.text.SimpleDateFormat sdf (java.text.SimpleDateFormat. "yyyy-MM-dd HH:mm:ss")
      nilfn (fn [_] nil)]

  (defn close-session
    ""
    []
    (when-not (nil? @session-file)
      (send session-file (fn [^java.io.Writer o]
                           (.flush o)
                           (.close o)
                           nil)))
    (swap! session-name nilfn)
    (swap! session-cnt nilfn))

  (defn make-session
    ""
    []
    (let [nname (make-session-name)]
      (swap! session-name (fn [_] nname))

      (when *log-to-file*
        (let [fname (str "log/" (first nname) ".log")]
          (io/make-parents fname)
          (send session-file (fn [^java.io.Writer o]
                               (do
                                 (when-not (nil? o)
                                   (.flush o)
                                   (.close o))
                                 (let [^java.io.Writer no (io/writer fname :append true)]
                                   (.write no (str "Session id: " (second nname) "\n"))
                                   no))))))

      (swap! session-cnt (fn [_] (make-counter 0)))))

  (defn log
    ""
    [s]
    (when *log-to-file*
      (if (nil? @session-file)
        (do
          (make-session)
          (log s))
        (let [to-log (str (.format sdf (java.util.Date.)) ": " s "\n")]
          (send session-file (fn [^java.io.Writer o]
                               (.write o to-log)
                               (.flush o)
                               o)))))
    true)

  (defn next-filename
    ""
    [prefix]
    (if (nil? @session-name)
      (do
        (make-session)
        (next-filename prefix))
      (str prefix (second @session-name) "_" (format "%06d" (@session-cnt)))))

  (defn get-session-name
    ""
    []
    @session-name)


)
