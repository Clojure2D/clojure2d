;; https://www.funprogramming.org/

(ns examples.funprogramming
  (:require [clojure2d.core :refer :all]
            [clojure2d.math.random :as r]
            [clojure2d.math :as m]))

;; Below setup is not necessary. I use it to be sure everything is as fast as possible
(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

;; https://www.funprogramming.org/2-Download-Processing-Use-point-and-line.html

;; Comments:
;;
;; * Default canvas and window is 200x200
;; * Window and canvas are separate concepts, you draw on canvas, window just displays it
;; * Canvas has different qualities (`:low`, `:mid` and `:high`). Default is `:high`. `point` looks blurred due to antialiasing.
;; * Default background is black, color is white
;; * To draw on canvas you have to create context with `with-canvas->` or `with-canvas` macros
;; * Default window creates canvas for you, to access it use `get-canvas` function

(let [window (show-window)]
  (with-canvas-> (get-canvas window) 
    (point 50 50)
    (point 51 50)
    (point 52 50)
    (point 53 50)
    (point 54 50)
    (point 55 50)
    (line 0 0 199 199)))

;; https://www.funprogramming.org/3-Create-an-animation-use-random.html

;; Comments:
;;
;; * Draw function accepts 4 parameters
;;   * canvas in context (created for you before call, so you don't need to use `with-canvas...` macro)
;;   * window where canvas is drawn
;;   * current frame number (like `frameCount` in Processing)
;;   * state as value returned from previous call or initial state (set during creating window) or nil
;; * `drand` returns `double` type random number. To get `int` call `irand`
;; * To bound `draw` to window pass it as parameter when creating one
;; * Some colors have their names (based on 140 HTML color names). You can use name (as keyword not string) instead of RGB values.

(let [draw (fn [canvas window frame state]
             (-> canvas
                 (set-background 255 204 0)
                 (set-color :black)
                 (line (r/drand 200)
                       (r/drand 200)
                       (r/drand 200)
                       (r/drand 200))))]
  (show-window {:draw-fn draw}))

;; https://www.funprogramming.org/4-Shades-of-gray-and-colors-frameRate.html

(let [draw (fn [canvas window frame state]
             (set-background canvas (r/irand 255) (r/irand 255) (r/irand 255)))]
  (show-window {:draw-fn draw
                :fps 4}))

;; https://www.funprogramming.org/5-Light-speed-effect-change-line-colors.html

;; Difference:
;;
;; Remember, default window is 200x200

(let [draw (fn [canvas window frame state]
             (-> canvas
                 (set-color 0 (r/irand 255) 0)
                 (line 100 100 (r/drand 200) (r/drand 200))))]
  (show-window {:draw-fn draw}))

;; https://www.funprogramming.org/6-Animate-white-lines-across-the-display.html

(let [draw (fn [canvas window frame state]
             (-> canvas
                 (set-background :black)
                 (set-color :white)
                 (line 0 (r/drand 200) 200 (r/drand 200))))]
  (show-window {:draw-fn draw}))

;; https://www.funprogramming.org/7-Animate-horizontal-lines-use-a-variable.html

(let [draw (fn [canvas window frame state]
             (let [distance-top (r/drand 200)]
               (-> canvas
                   (set-background :black)
                   (set-color :white)
                   (line 0 distance-top 199 distance-top))))]
  (show-window {:draw-fn draw}))

;; https://www.funprogramming.org/8-Animate-vertical-lines.html

(let [draw (fn [canvas window frame state]
             (let [distance-left (r/drand 200)]
               (-> canvas
                   (set-background :black)
                   (set-color :white)
                   (line distance-left 0 distance-left 199))))]
  (show-window {:draw-fn draw}))

;; https://www.funprogramming.org/9-Change-screen-size-animate-colorful-lines.html

;; Comments:
;;
;; Here I explicitely create canvas and set its size. Please note: canvas size and window size can be different.
;; In such case result will be shrinked or stretched. To set window size use `:width` and `:height` parameters.

(let [draw (fn [canvas window frame state]
             (let [distance-left (r/drand 200)]
               (-> canvas
                   ;; (set-background :black)
                   (set-color (r/irand 200 256)
                              (r/irand 200 256)
                              (r/irand 50 100))
                   (line distance-left 0 distance-left 199))))]
  (show-window {:canvas (make-canvas 200 200)
                :draw-fn draw
                :setup (fn [canvas _]
                         (set-background canvas :gray))}))

;; https://www.funprogramming.org/10-Draw-circles-and-rectangles-change-fill-color.html

;; Comments
;;
;; * Color can be represented as hex int instead of # form
;; * There is no stroke/fill here. You have to draw twice setting `stroke?` parameter to true/false.
;; * `crect` is centered version of `rect`
;; * `ellipse` is centered already
;; * `:mid` is used for quality similar to default in Processing

(let [canvas (make-canvas 400 400 :mid)
      window (show-window {:canvas canvas})]
  (with-canvas-> canvas 
    (set-background 0xC0E1EA)
    
    (set-color 0xB6FF00)
    (crect 200 200 150 150)
    (set-color 0xFFBC03)
    (crect 200 200 150 150 true)
    
    (set-color 0xB6FF00)
    (ellipse 200 200 150 150)
    (set-color 0xFFBC03)
    (ellipse 200 200 150 150 true)))

;; https://www.funprogramming.org/11-Non-random-animation-of-a-circle-crossing-the-screen.html

;; Comments:
;;
;; * I use state to keep current circle position, state initialization is done by using `:draw-state` parameter for window.

(let [draw (fn [canvas window frame ^double circle-x]
             (-> canvas
                 (set-background 0x1BB1F5)
                 (set-color 0xC1FF3E)
                 (ellipse circle-x 50 50 50))
             (inc circle-x))]
  (show-window {:canvas (make-canvas 400 400)
                :draw-fn draw
                :draw-state 0.0}))

;; https://www.funprogramming.org/12-Do-a-loop-animation-using-an-if-statement.html

(let [draw (fn [canvas window frame [^double slow-circle-x ^double fast-circle-x]]
             (-> canvas
                 (set-background 0x1BB1F5)
                 (set-color 0xC1FF3E)
                 (ellipse slow-circle-x 50 50 50)
                 (set-color 0xFF4800)
                 (ellipse fast-circle-x 50 50 50))
             [(if (> slow-circle-x 400.0) 0.0 (inc slow-circle-x))
              (if (> fast-circle-x 400.0) 0.0 (+ fast-circle-x 5.0))])]
  (show-window {:canvas (make-canvas 400 400)
                :draw-fn draw
                :draw-state [0.0 0.0]}))

;; https://www.funprogramming.org/13-Event-happening-only-sometimes.html

;; Comments:
;;
;; To get some value with random probability use `randval`

(let [draw (fn [canvas window frame [^double slow-circle-x ^double fast-circle-x]]
             (let [slow-circle-size (r/randval 0.1 60 50)]
               (-> canvas
                   (set-background 0x1BB1F5)
                   (set-color 0xC1FF3E)
                   (ellipse slow-circle-x 50 slow-circle-size slow-circle-size)
                   (set-color 0xFF4800)
                   (ellipse fast-circle-x 50 50 50)))
             [(if (> slow-circle-x 400.0) 0.0 (inc slow-circle-x))
              (if (> fast-circle-x 400.0) 0.0 (+ fast-circle-x 5.0))])]
  (show-window {:canvas (make-canvas 400 400)
                :draw-fn draw
                :draw-state [0.0 0.0]}))

;; https://www.funprogramming.org/14-New-directions-for-our-moving-circle.html

(let [draw (fn [canvas window frame [^double circle-x ^double circle-y]]
             (let [slow-circle-size (r/randval 0.1 60 50)]
               (-> canvas
                   (set-background 0x21EA73)
                   (set-color :white)
                   (ellipse circle-x circle-y 40 40)
                   (set-stroke 7.0)
                   (set-color 0xD60DFF)
                   (ellipse circle-x circle-y 40 40 true)))
             [(- circle-x 2.0)
              (+ circle-y 2.0)])]
  (show-window {:canvas (make-canvas 400 200)
                :draw-fn draw
                :draw-state [300.0 20.0]}))

;; https://www.funprogramming.org/15-Ball-bouncing-at-the-window-borders.html

(let [draw (fn [canvas window frame [^double circle-x ^double circle-y ^double move-x ^double move-y]]
             (let [slow-circle-size (r/randval 0.1 60 50)
                   circle-x (+ circle-x move-x)
                   circle-y (+ circle-y move-y)
                   [ncircle-x nmove-x] (cond
                                         (> circle-x ^int (width canvas)) (do (println "too far right")
                                                                              [(width canvas) (- move-x)])
                                         (< circle-x 0.0) (do (println "too far left")
                                                              [0.0 (- move-x)])
                                         :else [circle-x move-x])
                   [ncircle-y nmove-y] (cond
                                         (> circle-y ^int (height canvas)) (do (println "too far bottom")
                                                                               [(height canvas) (- move-y)])
                                         (< circle-y 0.0) (do (println "too far top")
                                                              [0.0 (- move-y)])
                                         :else [circle-y move-y])] 
               (-> canvas
                   (set-background 0x21EA73)
                   (set-color :white)
                   (ellipse ncircle-x ncircle-y 40 40)
                   (set-stroke 7.0)
                   (set-color 0xD60DFF)
                   (ellipse ncircle-x ncircle-y 40 40 true))
               [ncircle-x ncircle-y nmove-x nmove-y]))]
  (show-window {:canvas (make-canvas 400 200)
                :draw-fn draw
                :draw-state [300.0 20.0 2.0 -2.0]}))
