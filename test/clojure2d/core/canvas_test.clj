(ns clojure2d.core.canvas-test
  "Graphics context lifecycle, clipping, single rendering hints and path definitions (no window needed)."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure2d.core :as c2d]
            [clojure2d.core.shape :as sh])
  (:import [java.awt Color Graphics2D RenderingHints Shape]
           [java.awt.geom Ellipse2D$Double Path2D Path2D$Double Rectangle2D$Double]))

(defn- clip-bounds [canvas] (vec (sh/rectangle2d->box (.getBounds2D (.getClip ^Graphics2D (:graphics canvas))))))

(deftest with-canvas-releases-graphics-test
  (let [flushed (atom 0)
        canvas (c2d/canvas 10 10)]
    (with-redefs [c2d/flush-graphics (fn [c] (swap! flushed inc) (.dispose ^Graphics2D (:graphics c)))]
      (testing "a normal body returns its value and releases once"
        (is (= 3 (c2d/with-canvas [c canvas] 1 2 3)))
        (is (= :red (c2d/with-canvas-> canvas (c2d/set-color :red) ((constantly :red)))))
        (is (= 2 @flushed)))
      (testing "a throwing body still releases"
        (is (thrown? ArithmeticException (c2d/with-canvas [c canvas] (/ 1 0))))
        (is (thrown? ArithmeticException (c2d/with-canvas-> canvas ((fn [_] (/ 1 0))))))
        (is (= 4 @flushed))))))

(deftest with-isolated-canvas-test
  (c2d/with-canvas [c (c2d/canvas 100 100)]
    (let [^Graphics2D g (:graphics c)
          before (fn [] [(.getClip g) (.getTransform g) (.getColor g) (.getStroke g) (.getFont g)
                         (.getRenderingHint g RenderingHints/KEY_ANTIALIASING) @(:transform-stack c)])
          state (before)]
      (c2d/push-matrix c)
      (let [state (before)]
        (testing "every change inside is discarded"
          (is (= :done (c2d/with-isolated-canvas [i c]
                         (-> i
                             (c2d/add-clip 10 10 20 20)
                             (c2d/translate 5 5)
                             (c2d/rotate 1.0)
                             (c2d/set-color :red)
                             (c2d/set-stroke 7.0)
                             (c2d/set-font-attributes 33)
                             (c2d/set-rendering-hint :antialiasing :off)
                             (c2d/push-matrix))
                         :done)))
          (is (= state (before))))
        (testing "a throwing body is discarded too, nested isolation works"
          (is (thrown? ArithmeticException
                       (c2d/with-isolated-canvas [i c]
                         (c2d/set-color i :blue)
                         (c2d/with-isolated-canvas [j i] (c2d/set-color j :green))
                         (is (= (Color. 0 0 255) (.getColor ^Graphics2D (:graphics i))))
                         (/ 1 0))))
          (is (= state (before))))
        (testing "the copy starts from the current state"
          (c2d/translate c 3 4)
          (c2d/with-isolated-canvas [i c]
            (is (= (.getTransform g) (.getTransform ^Graphics2D (:graphics i))))
            (is (= [] @(:transform-stack i))))))
      (c2d/pop-matrix c)
      (is (= state (before))))))

(deftest add-clip-test
  (c2d/with-canvas [c (c2d/canvas 100 100)]
    (testing "without a clip it clips to the given area"
      (c2d/add-clip c 10 20 30 40)
      (is (= [10.0 20.0 30.0 40.0] (clip-bounds c))))
    (testing "nested clips intersect"
      (c2d/add-clip c 20 0 100 50)
      (is (= [20.0 20.0 20.0 30.0] (clip-bounds c))))
    (testing "a disjoint area clips everything"
      (c2d/add-clip c 80 80 5 5)
      (is (.isEmpty (.getBounds2D (.getClip ^Graphics2D (:graphics c))))))
    (testing "clip replaces, a shape intersects"
      (c2d/clip c 0 0 50 50)
      (c2d/add-clip c (Ellipse2D$Double. 25.0 25.0 50.0 50.0))
      (is (= [25.0 25.0 25.0 25.0] (clip-bounds c))))
    (testing "a clip is in user space"
      (c2d/reset-clip c)
      (c2d/translate c 10 10)
      (c2d/add-clip c 0 0 5 5)
      (is (= [0.0 0.0 5.0 5.0] (clip-bounds c))))))

(deftest set-rendering-hint-test
  (c2d/with-canvas [c (c2d/canvas 10 10 :high)]
    (let [^Graphics2D g (:graphics c)]
      (testing "one hint changes, the others stay"
        (is (= c (c2d/set-rendering-hint c :antialiasing :off)))
        (is (= RenderingHints/VALUE_ANTIALIAS_OFF (.getRenderingHint g RenderingHints/KEY_ANTIALIASING)))
        (is (= RenderingHints/VALUE_RENDER_QUALITY (.getRenderingHint g RenderingHints/KEY_RENDERING))))
      (testing "every listed key and value is settable"
        (doseq [[k vs] c2d/rendering-hint-values
                v (keys vs)]
          (is (= c (c2d/set-rendering-hint c k v)))))
      (testing "a map of hints sets them all"
        (is (= c (c2d/set-rendering-hint c {:antialiasing :off :rendering :speed})))
        (is (= RenderingHints/VALUE_ANTIALIAS_OFF (.getRenderingHint g RenderingHints/KEY_ANTIALIASING)))
        (is (= RenderingHints/VALUE_RENDER_SPEED (.getRenderingHint g RenderingHints/KEY_RENDERING)))
        (is (= c (c2d/set-rendering-hint c {})))
        (is (thrown? clojure.lang.ExceptionInfo (c2d/set-rendering-hint c {:antialiasing :on :rendering :fast}))))
      (testing "unknown key or value"
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Unknown rendering hint" (c2d/set-rendering-hint c :antialias :on)))
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Unknown rendering hint" (c2d/set-rendering-hint c :antialiasing :yes)))
        (is (thrown? clojure.lang.ExceptionInfo (c2d/set-rendering-hint c :antialiasing nil)))))))

(defn- path-def [^Shape s] (vec (c2d/shape->path-def s)))

(deftest path-def-round-trip-test
  (testing "round trip keeps segments and winding"
    (doseq [s [(Rectangle2D$Double. 1.0 2.0 3.0 4.0)
               (Ellipse2D$Double. 0.0 0.0 10.0 5.0)
               (doto (Path2D$Double. Path2D/WIND_EVEN_ODD)
                 (.moveTo 0.0 0.0) (.lineTo 10.0 0.0) (.lineTo 10.0 10.0) (.closePath)
                 (.moveTo 2.0 2.0) (.quadTo 3.0 3.0 4.0 2.0) (.closePath))]]
      (is (= (path-def s) (path-def (c2d/path-def->shape (c2d/shape->path-def s)))))))
  (testing "winding rules"
    (is (= Path2D/WIND_EVEN_ODD (.getWindingRule ^Path2D (c2d/path-def->shape [[:move [0 0] :even-odd] [:line [1 1]]]))))
    (is (= Path2D/WIND_NON_ZERO (.getWindingRule ^Path2D (c2d/path-def->shape [[:move [0 0]] [:line [1 1]]]))))
    (is (= Path2D/WIND_NON_ZERO (.getWindingRule ^Path2D (c2d/path-def->shape [[:move [0 0] :unknown] [:line [1 1]]]))))
    (is (= Path2D/WIND_NON_ZERO (.getWindingRule ^Path2D (c2d/path-def->shape [[:move [0 0] :even-odd] [:line [1 1] :non-zero]])))))
  (testing "empty definition gives an empty path"
    (is (= [] (path-def (c2d/path-def->shape []))))))

(deftest shape->points-test
  (testing "lines keep their points, close adds none"
    (is (= [[0.0 0.0] [10.0 0.0] [10.0 10.0]]
           (vec (c2d/shape->points (sh/path [[0 0] [10 0] [10 10]] true) 0.1)))))
  (testing "curves flatten within flatness"
    (let [pts (c2d/shape->points (Ellipse2D$Double. -1.0 -1.0 2.0 2.0) 0.01)]
      (is (< 8 (count pts)))
      (is (every? (fn [[x y]] (< (Math/abs (- 1.0 (Math/hypot x y))) 0.011)) pts))))
  (testing "finer flatness gives more points"
    (let [e (Ellipse2D$Double. 0.0 0.0 100.0 100.0)]
      (is (< (count (c2d/shape->points e 1.0)) (count (c2d/shape->points e 0.01))))))
  (testing "subpaths in order; empty shape"
    (is (= [[0.0 0.0] [1.0 0.0] [5.0 5.0] [6.0 5.0]]
           (vec (c2d/shape->points (doto (Path2D$Double.) (.moveTo 0.0 0.0) (.lineTo 1.0 0.0) (.moveTo 5.0 5.0) (.lineTo 6.0 5.0)) 0.1))))
    (is (empty? (c2d/shape->points (Path2D$Double.) 0.1))))
  (testing "a rectangle's path repeats its first point before closing"
    (is (= [[0.0 0.0] [1.0 0.0] [1.0 1.0] [0.0 1.0] [0.0 0.0]] (vec (c2d/shape->points (Rectangle2D$Double. 0.0 0.0 1.0 1.0) 0.1))))))

(deftest clip-coordinates-test
  (c2d/with-canvas [c (c2d/canvas 20 20)]
    (c2d/clip c 0.6 0.6 5.5 5.5)
    (is (= [0.0 0.0 5.0 5.0] (clip-bounds c)) "clip truncates to whole pixels")
    (c2d/reset-clip c)
    (c2d/add-clip c 0.6 0.6 5.5 5.5)
    (is (every? #(< (Math/abs (double %)) 1e-6) (map - [0.6 0.6 5.5 5.5] (clip-bounds c))) "add-clip keeps fractions (Java2D stores floats)")))

(deftest rounded-rect-drawing-test
  (let [img (fn [f] (let [c (c2d/canvas 40 40)]
                      (c2d/with-canvas-> c (c2d/set-background :white) (c2d/set-color :black) (f))
                      (vec (.getRGB ^java.awt.image.BufferedImage (c2d/get-image c) 0 0 40 40 nil 0 40))))]
    (is (= (img #(c2d/shape % (sh/rounded-rect 5 5 30 20 [4 0 8 2])))
           (img #(c2d/rounded-rect % 5 5 30 20 [4 0 8 2]))
           (img #(c2d/rounded-rect % [5 5] 30 20 [4 0 8 2]))))
    (is (= (img #(c2d/shape % (sh/rounded-rect 5 5 30 20 4) true))
           (img #(c2d/rounded-rect % 5 5 30 20 4 true))))))

(deftest path-def-winding-map-test
  (is (= Path2D/WIND_EVEN_ODD (.getWindingRule ^Path2D (c2d/path-def->shape [[:move [0 0] :even-odd] [:line [1 1] :bogus]])))))
