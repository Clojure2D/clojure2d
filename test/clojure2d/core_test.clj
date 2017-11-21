(ns clojure2d.core-test
  (:require [expectations :refer :all]
            [clojure2d.math.vector :as v]
            [clojure2d.core :refer :all]
            [clojure2d.color :as c]))

;; global variables
(def ^:const window-name "Testing clojure.core")
(def canvas (make-canvas 100 100))
(def canvas2 (make-canvas 100 100))

(defn draw-on-canvas
  ""
  [canvas fps]
  (-> canvas
      (set-background :black)
      (set-color :maroon)
      (line 0 (inc (mod fps 100)) 100 (inc (mod fps 100)))
      (ellipse 50 50 10 10)))

;; window with draw function
(defn draw
  "Draw function"
  [canvas window fps _]
  (set-state! window (assoc (get-state window) :fps fps))
  (draw-on-canvas canvas fps))

(def window (atom nil))
(def window-closed (atom nil))

(defmethod key-pressed [window-name \s] [_ state]
  (assoc state :key-pressed true))
(defmethod key-pressed [window-name \a] [_ state]
  (assoc state :a-pressed true))

(defn build-up
  "Prepare data, events, etc."
  {:expectations-options :before-run}
  []
  (reset! window (show-window canvas window-name 50 50 60 draw)) 
  (set-state! @window {:a 1})
  (reset! window-closed (show-window canvas "Testing clojure.core closed"))
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
  (with-canvas canvas2
    (draw-on-canvas 50)))

(defn clean-up
  "Close window, remove files"
  {:expectations-options :after-run}
  []
  (close-window @window)
  (when (window-active? @window-closed) (close-window @window-closed)))

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

(expect (c/to-color :maroon) (get-pixel canvas2 50 50))
(expect (c/to-color :black) (get-pixel canvas2 0 0))

;; rendering hints available
(expect [:high :low :mid] (sort (keys rendering-hints)))

;; resize canvas
(expect (more-> 4 width 44 height)
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
(expect (more-> window-name :window-name
                50 :w
                50 :h
                60.0 :fps)
        @window)

;; Image proto on window
(expect (more-> 100 width
                100 height)
        (get-image @window))

;; Test event
(expect :key-pressed (in (keys (get-state @window))))
(expect :a-pressed (in (keys (get-state @window))))

;; check state
(expect :fps (in (keys (get-state @window)))) ; draw function sets state to current fps, should be positive
(expect true (window-active? @window))
(expect false (window-active? @window-closed))
(expect 1 (count @global-state))

;; various functions
(expect "D54" (to-hex 3412))
(expect 1045 (let [c (make-counter 100)]
               (reduce + (repeatedly 10 c))))

;; sesion generation
(expect (more-of [s1 s2]
                 14 (count s1)
                 8 (count s2))
        (make-session))

;; filename generation
(expect "000000" (subs (next-filename "" "") 9))
(expect "000001.jpg" (subs (next-filename "" ".jpg") 9))
(expect "abc" (subs (next-filename "abc") 0 3))
