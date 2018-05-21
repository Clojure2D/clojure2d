(ns clojure2d.core-test
  (:require [clojure.test :refer :all]
            [fastmath.vector :as v]
            [clojure2d.core :refer :all]
            [clojure2d.color :as c]
            [clojure2d.pixels :as p]))

;; global variables
(def ^:const window-name "Testing clojure.core")
(def canvas1 (canvas 100 100))
(def canvas2 (canvas 100 100))

(defn draw-on-canvas
  ""
  [c fps]
  (-> c
      (set-background :black)
      (set-color :maroon)
      (line 0 (inc (mod fps 100)) 100 (inc (mod fps 100)))
      (ellipse 50 50 10 10)))

;; window with draw function
(defn draw
  "Draw function"
  [c window fps _]
  (set-state! window (assoc (get-state window) :fps fps))
  (draw-on-canvas c fps))

(def window (atom nil))
(def window-closed (atom nil))

(defmethod key-pressed [window-name \s] [_ state]
  (assoc state :key-pressed true))
(defmethod key-pressed [window-name \a] [_ state]
  (assoc state :a-pressed true))

(defn build-up
  "Prepare data, events, etc."
  []
  (reset! window (show-window canvas1 window-name 150 150 60 draw)) 
  (set-state! @window {:a 1})
  (reset! window-closed (show-window canvas1 "Testing clojure.core closed"))
  (close-window @window-closed)
  (.dispatchEvent (:frame @window) (java.awt.event.KeyEvent. (:panel @window)
                                                             java.awt.event.KeyEvent/KEY_PRESSED
                                                             1 0
                                                             java.awt.event.KeyEvent/VK_UNDEFINED
                                                             \s))
  (.dispatchEvent (:frame @window) (java.awt.event.KeyEvent. (:panel @window)
                                                             java.awt.event.KeyEvent/KEY_PRESSED
                                                             1 0
                                                             java.awt.event.KeyEvent/VK_UNDEFINED
                                                             \a))
  (with-canvas-> canvas2
    (draw-on-canvas 50)))

(defn clean-up
  "Close window, remove files"
  []
  (close-window @window)
  (when (window-active? @window-closed) (close-window @window-closed)))

(defn window-fixture
  [f]
  (build-up)
  (f)
  (clean-up))

(use-fixtures :once window-fixture)

;; test filenames
(deftest filename-test
  (is (= "jpg" (file-extension "test.jpg")))
  (is (= nil (file-extension "test"))))

;; resize image
(deftest resize-test
  (let [resized (resize canvas1 4 44)]
    (is (= 4 (width resized)))
    (is (= 44 (height resized)))))

;; Image proto
(deftest image-proto-test
  (is (= java.awt.image.BufferedImage (type (get-image canvas1))))
  (is (= 100 (width canvas1)))
  (is (= 100 (height canvas1)))
  (is (= 100 (width (get-image @window)))))

(deftest get-pixel-test
  (is (= (c/to-color :maroon) (p/get-color canvas2 50 50)))
  (is (= (c/to-color :black) (p/get-color canvas2 0 0))))

;; rendering hints available
(deftest rendering-hints-test
  (is (= [:high :highest :low :mid] (sort (keys rendering-hints)))))

;; test transformations
;; :TODO: full workflow and all functions

(defn make-transformation
  "Translate canvas and check point position"
  [v]
  (with-canvas-> canvas1
    (translate 100 100)
    (transform v)))

(defn make-inv-transformation
  "Translate canvas and check point"
  [v]
  (with-canvas-> canvas1
    (translate 100 100)
    (inv-transform v)))

(deftest transformation-test
  (is (= (make-transformation (v/vec2 0 0)) (v/vec2 100 100)))
  (is (= (make-inv-transformation (v/vec2 100 100)) (v/vec2 0 0))))

;; check internal values for window
(deftest internal-window-values-test
  (are [x y] (= x (y @window))
    window-name :window-name
    150 :w
    150 :h
    60.0 :fps))

(deftest panel-size-test
  (is (= 150 (.getWidth (:panel @window))))
  (is (= 150 (.getHeight (:panel @window)))))

;; Test event
(deftest event-test
  (are [x] (contains? (get-state @window) x)
    :key-pressed
    :a-pressed)
  (is (not (contains? (get-state @window) :b-pressed))))

(deftest state-test
  (is (:fps (get-state @window)))
  (is (window-active? @window))
  (is (not (window-active? @window-closed))))

(deftest function-test
  (is (= "D54" (to-hex 3412)))
  (is (= 1045 (let [c (make-counter 100)]
                (reduce + (repeatedly 10 c))))))

;; sesion generation
(deftest session-generation-test
  (is (= [14 8] (mapv count (make-session)))))

;; filename generation
(deftest filename-generation-test
  (is (= "000000" (subs (next-filename "" "") 9)))
  (is (= "000001.jpg" (subs (next-filename "" ".jpg") 9)))
  (is (= "abc" (subs (next-filename "abc") 0 3))))
