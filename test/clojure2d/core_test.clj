(ns clojure2d.core-test
  (:require [expectations :refer :all]
            [clojure2d.math.vector :as v]
            [clojure2d.core :refer :all]
            [clojure2d.math :as m]))


;; global variables
(def canvas (make-canvas 100 100))

(defn draw
  "Draw function"
  [canvas window fps _]
  (set-state! window fps)
  (-> canvas
      (set-background :black)
      (line 0 (mod fps 100) 100 (mod fps 100))))

(def window (show-window canvas "Testing clojure.core" 50 50 60 draw))
(def window-closed (show-window canvas "Testing clojure.core closed"))

(defn build-up
  "Prepare data"
  {:expectations-options :before-run}
  []
  (set-state! window {:a 1})
  (close-window window-closed))

(defn clean-up
  "Close window, remove files"
  {:expectations-options :after-run}
  []
  (close-window window))

;; test filenames

(expect (file-extension "test.jpg") "jpg")
(expect (file-extension "test") nil)

;; resize image
(expect (more-> 4 width
                44 height)
        (resize-image (get-image canvas) 4 44))

;; Image proto on canvas
(expect java.awt.image.BufferedImage (get-image canvas))

;; Image proto on image (from canvas)
(expect (more-> 100 width
                100 height
                java.awt.image.BufferedImage get-image)
        (get-image canvas))

;; rendering hints available
(expect [:high :low :mid] (sort (keys rendering-hints)))

;; resize canvas
(expect (more-> 4 width
                44 height)
        (resize-canvas canvas 4 44))

;; test transformations
;; :TODO: full workflow and all functions

(defn make-transformation
  "Translate canvas and check point position"
  [v]
  (with-canvas canvas
    (translate 100 100)
    (transform v)))

(defn make-inv-transformation
  "Translate canvas and check point"
  [v]
  (with-canvas canvas
    (translate 100 100)
    (inv-transform v)))

(expect (make-transformation (v/vec2 0 0)) (v/vec2 100 100))
(expect (make-inv-transformation (v/vec2 100 100)) (v/vec2 0 0))

;; check internal values for window
(expect (more-> "Testing clojure.core" :window-name
                50 :w
                50 :h
                60.0 :fps)
        window)

;; Image proto on window
(expect (more-> 100 width
                100 height)
        (get-image window))

;; check state
(expect pos? (get-state window)) ; draw function sets state to current fps, should be positive
(expect true (window-active? window))
(expect false (window-active? window-closed))
(expect 1 (count @global-state))

;; various functions
(expect "D54" (to-hex 3412))
(expect 1045 (let [c (make-counter 100)]
               (reduce + (repeatedly 10 c))))

(expect (more-of [s1 s2]
                 14 (count s1)
                 8 (count s2))
        (make-session))

(expect "000000" (subs (next-filename "" "") 9))
(expect "000001" (subs (next-filename "" "") 9))
