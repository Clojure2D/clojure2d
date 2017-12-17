;; Creative Coding tutorials by Abe Pazos
;; Check out accompanying video tutorials and deeper explanations on
;; https://www.funprogramming.org/

(ns examples.funprogramming
  (:require [clojure2d.core :refer :all]
            [clojure2d.math.random :as r]
            [clojure2d.math :as m]
            [clojure2d.color :as c]
            [clojure2d.math.vector :as v]
            [clojure2d.pixels :as p]
            [clojure.string :refer [join]]))

;; Below setup is not necessary. I use it to be sure everything is as fast as possible
(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

;; https://www.funprogramming.org/1-Introduction.html

;; SKIPPED

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

;; https://www.funprogramming.org/16-Create-an-animated-rainbow.html

(let [draw (fn [canvas window frame state]
             (let [rainbow-size (r/drand 200 270)]
               (-> canvas
                   (set-stroke (r/drand 3 10))
                   (set-color (r/irand 256)
                              (r/irand 256)
                              (r/irand 256))
                   (ellipse 150 350 rainbow-size rainbow-size true))))]
  (show-window {:canvas (make-canvas 300 300)
                :draw-fn draw
                :setup (fn [canvas _]
                         (set-background canvas 0x04B1CE))}))

;; https://www.funprogramming.org/17-A-better-way-to-generate-random-colors.html

;; Comments:
;;
;; * There is no colorMode here but you can use color conversion functions
;; * Color conversion functions expect color as Vec4. To make such just call `make-color`

(let [draw (fn [canvas window frame state] 
             (let [rainbow-size (r/drand 200 270)]
               (-> canvas
                   (set-stroke (r/drand 3 10))
                   (set-color (c/from-HSB (c/make-color (r/drand 256) 255 255)))
                   (ellipse 150 350 rainbow-size rainbow-size true))))]
  (show-window {:canvas (make-canvas 300 300)
                :draw-fn draw
                :setup (fn [canvas _]
                         (set-background canvas 0x04B1CE))}))

;; https://www.funprogramming.org/18-Things-that-happen-sometimes.html

;; Comments:
;;
;; Color is not kept between calls (each call is a new context) you have to pass current color via state

(let [draw (fn [canvas window frame [^double x curr-color]]
             (let [ncolor (if (r/brand 0.7)
                            curr-color
                            (if (r/brand 0.5) :black :white))
                   nx (if (>= x ^int (width canvas)) 0.0 (inc x))]
               (set-color canvas ncolor)
               (line canvas x 200 x 100)
               [nx ncolor]))]
  (show-window {:canvas (make-canvas 400 400 :mid)
                :draw-fn draw
                :setup (fn [canvas _]
                         (set-background canvas :white)
                         [0.0 :white])}))


;; https://www.funprogramming.org/19-Tweak-the-bar-code-create-your-first-function.html

(let [change-line-color (fn [canvas x]
                          (set-color canvas :red)
                          (line canvas x 100 x 200)
                          (if (r/brand 0.5) :black :white))
      draw (fn [canvas window frame [^double x curr-color]]
             (set-color canvas curr-color)
             (line canvas x 200 x 100)
             (let [ncolor (if (r/brand 0.9)
                            curr-color
                            (change-line-color canvas x))
                   nx (if (>= x ^int (width canvas)) 0.0 (inc x))]
               [nx ncolor]))]
  (show-window {:canvas (make-canvas 400 400 :mid)
                :draw-fn draw
                :setup (fn [canvas _]
                         (set-background canvas :white)
                         [0.0 :white])}))

;; https://www.funprogramming.org/20-The-smallest-drawing-program-ever.html

;; Comments:
;;
;; It's not easy to access event data like in `Processing`. Here I use state created globally for window and events and we use it to get information about mouse/keyboard state.
;; To access global window state use `get-state` function.

(do
  (def canvas (make-canvas 200 200 :mid))
  
  (let [draw (fn [canvas window _ _]
               (when (get-state window)
                 (set-color canvas :black)
                 (point canvas (mouse-x window) (mouse-y window))))]

    (with-canvas-> canvas
      (set-background :white))
    
    (show-window {:canvas canvas
                  :draw-fn draw
                  :window-name "Drawing 20"}))

  (defmethod mouse-event ["Drawing 20" :mouse-pressed] [_ _] true)
  (defmethod mouse-event ["Drawing 20" :mouse-released] [_ _] false)

  (defmethod key-pressed ["Drawing 20" \space] [_ state]
    (save canvas "my_drawing.png")
    state))

;; https://www.funprogramming.org/21-Improved-tiny-drawing-program.html

(do
  (def canvas (make-canvas 200 200))
  (def window-name "Drawing 21")
  
  (defn draw-top-line
    [clr]
    (with-canvas-> canvas
      (set-stroke 7.0)
      (set-color clr)
      (line 0 0 (width canvas) 0)))
  
  (let [draw (fn [canvas window _ _]
               (let [{:keys [pressed curr-color]} (get-state window)]
                 (when pressed
                   (set-color canvas curr-color)
                   (set-stroke canvas 2.0)
                   (point canvas (mouse-x window) (mouse-y window)))))]

    (with-canvas-> canvas
      (set-background :white))
    
    (show-window {:canvas canvas
                  :draw-fn draw
                  :window-name window-name
                  :state {:pressed false
                          :curr-color :black}}))

  (defmethod mouse-event [window-name :mouse-pressed] [_ state] (assoc state :pressed true))
  (defmethod mouse-event [window-name :mouse-released] [_ state] (assoc state :pressed false))

  (defmethod key-pressed [window-name \space] [_ state]
    (save canvas "my_drawing.png")
    state)
  
  (defmethod key-pressed [window-name \c] [_ state]
    (let [clr (c/make-color (r/drand 255)
                            (r/drand 255)
                            (r/drand 255))]
      (draw-top-line clr)
      (assoc state :curr-color clr)))

  (defmethod key-pressed [window-name \b] [_ state]
    (with-canvas-> canvas
      (set-background (r/drand 255) (r/drand 255) (r/drand 255)))
    (draw-top-line (:curr-color state))
    state))

;; https://www.funprogramming.org/22-Stars-blinking-at-night-fade-out-effect.html

;; Comments:
;;
;; Set background has the same behaviour as fill + rect on whole canvas

(let [draw (fn [canvas window frame state]
             (let [distance-left (r/drand 200)]
               (-> canvas
                   (set-background :black 30)
                   (set-color :white)
                   (ellipse (r/drand (width canvas))
                            (r/drand (height canvas))
                            30 30))))]
  (show-window {:draw-fn draw
                :canvas (make-canvas 400 300)}))

;; https://www.funprogramming.org/23-Using-the-while-loop.html

;; Comments:
;;
;; Negative ellipse size doesn't work here. I have to take absolute value

(let [canvas (make-canvas 400 400)]
  (with-canvas [c canvas]
    (set-background c (c/from-HSB (c/make-color (r/drand 256) (r/drand 50 100) (r/drand 50 100))))
    (set-color c :white 100)
    (dotimes [i 100]
      (ellipse c (+ 100 i i) (+ 100 i i) (+ 100 i) (m/abs (- 100 i i)) true)))
  (show-window {:canvas canvas}))

;; https://www.funprogramming.org/24-Circle-patterns-with-a-while-loop.html

(let [canvas (make-canvas 400 400)]
  (with-canvas [c canvas]
    (set-background c (c/from-HSB (c/make-color (r/drand 256) (r/drand 50 100) (r/drand 50 100))))
    (set-color c :white 100)
    (dotimes [i 70]
      (ellipse c
               (+ (/ ^int (width canvas) 2) i)
               (- (/ ^int (height canvas) 2) i)
               (+ 100 (* 5 i))
               (m/abs (- 100 (* 5 i))) true)))
  (println "end")
  (show-window {:canvas canvas}))

;; https://www.funprogramming.org/25-Typing-big-letters-that-fade-out.html


(do
  (def canvas (make-canvas 400 400))
  (def window-name "Letters 25")
  
  (let [draw (fn [canvas window _ _]
               (set-background canvas 0x3355cc 20))]

    (show-window {:canvas canvas
                  :draw-fn draw
                  :window-name window-name
                  :setup (fn [canvas _]
                           (set-background canvas 0x3355cc))}))

  (defmethod key-pressed :default [event _]
    (with-canvas-> canvas
      (set-color 0xffe200)
      (set-font-attributes (r/drand 20 200))
      (text (str (key-char event)) (r/drand 300) (r/drand 100 400)))))

;; https://www.funprogramming.org/26-Make-patterns-by-rotating-objects.html

(let [canvas (make-canvas 400 400)]
  (with-canvas [cnvs canvas]
    (set-background cnvs 0x6aa21e)
    (dotimes [c 100]
      (let [clr (r/drand 255)]
        (-> cnvs
            (set-color clr clr clr)
            (rect 200 10 50 5)
            (set-color :red)
            (rect 260 10 10 5)
            (rotate 0.02)))))
  (show-window {:canvas canvas}))

;; https://www.funprogramming.org/27-Animating-while-rotating.html

(let [canvas (make-canvas 400 400)
      draw (fn [canvas window frame ^double r]
             (let [circle-size (r/drand 5 15)]
               (-> canvas
                   (set-color :white)
                   (rotate r)
                   (ellipse (+ r 100) 10 circle-size circle-size)))
             
             (+ r 0.2))]

  (with-canvas-> canvas (set-background 10 10 10))
  
  (show-window {:draw-fn draw
                :canvas canvas
                :draw-state 0.0}))


;; https://www.funprogramming.org/28-Rotate-and-even-move-your-axes.html

(let [canvas (make-canvas 400 400)
      draw (fn [canvas window frame ^double r]
             (let [circle-size (r/drand 5 15)]
               (-> canvas
                   (translate (/ ^int (width canvas) 2) (/ ^int (height canvas) 2))
                   (set-color :white)
                   (rotate r)
                   (ellipse (+ r 100) 10 circle-size circle-size)))
             
             (+ r 0.2))]

  (with-canvas-> canvas (set-background 10 10 10))
  
  (show-window {:draw-fn draw
                :canvas canvas
                :draw-state 0.0}))

;; https://www.funprogramming.org/29-Rectangle-spinning-around-the-mouse-pointer.html

;; Comments:
;;
;; When you go outside window `mouse-x` and `mouse-y` return -1 value

(let [canvas (make-canvas 400 400)
      back-r (r/drand 100)
      back-g (r/drand 100)
      back-b (r/drand 100)
      draw (fn [canvas window frame ^double r]
             (-> canvas
                 (set-background back-r back-g back-b 50)
                 (set-color :white)
                 (translate (mouse-x window) (mouse-y window))
                 (rotate r)
                 (crect 0 0 100 100))
             
             (+ r 0.05))]

  (with-canvas-> canvas (set-background back-r back-g back-b))
  
  (show-window {:draw-fn draw
                :canvas canvas
                :draw-state 0.0}))

;; https://www.funprogramming.org/30-Multiple-rotating-objects-and-reset-matrix.html

(let [draw (fn [canvas window frame ^double r]
             (let [circle-size (r/drand 5 15)]
               (-> canvas
                   (set-background :white)
                   (set-color :black)
                   
                   (translate 100 100)
                   (rotate r)
                   (crect 0 0 80 80)
                   (reset-matrix)

                   (translate 300 100)
                   (rotate r)
                   (crect 0 0 80 80)
                   (reset-matrix)

                   (translate 100 300)
                   (rotate r)
                   (crect 0 0 80 80)
                   (reset-matrix)

                   (translate 300 300)
                   (rotate r)
                   (crect 0 0 80 80)
                   (reset-matrix)))
             
             (+ r 0.02))]

  (show-window {:draw-fn draw
                :canvas (make-canvas 400 400)
                :draw-state 0.0}))

;; https://www.funprogramming.org/31-Function-parameters-and-return-values.html

;; Language specific things

(let [hello #(println "Hello!")
      print-sum (fn [^double a ^double b] (println (+ a b)))
      calculate-sum (fn [^double a ^double b] (+ a b))
      my-added-numbers (calculate-sum 100 50)]
  (hello)
  (print-sum 10 10)
  (println my-added-numbers))

;; https://www.funprogramming.org/32-Use-a-function-to-simplify-a-program.html

(let [draw-rotating-rectangle (fn [canvas x y rect-size r]
                                (-> canvas
                                    (translate x y)
                                    (rotate r)
                                    (crect 0 0 rect-size rect-size)
                                    (reset-matrix)))
      draw (fn [canvas window frame ^double r]
             (let [circle-size (r/drand 5 15)]
               (-> canvas
                   (set-background :white)
                   (set-color :black)
                   (draw-rotating-rectangle 100 100 80 r)
                   (draw-rotating-rectangle 300 100 40 r)
                   (draw-rotating-rectangle 100 300 100 r)
                   (draw-rotating-rectangle 300 300 20 r)
                   (draw-rotating-rectangle 200 200 150 r)))             
             (+ r 0.02))]

  (show-window {:draw-fn draw
                :canvas (make-canvas 400 400)
                :draw-state 0.0}))

;; https://www.funprogramming.org/33-Use-multiplication-to-scale-up-or-down-speeds-and-sizes.html

(let [draw-rotating-rectangle (fn [canvas x y rect-size r]
                                (-> canvas
                                    (translate x y)
                                    (rotate r)
                                    (crect 0 0 rect-size rect-size)
                                    (reset-matrix)))
      draw (fn [canvas window frame ^double r]
             (let [circle-size (r/drand 5 15)]
               (-> canvas
                   (set-background :white)
                   (set-color :black)
                   (draw-rotating-rectangle 100 100 80 r)
                   (draw-rotating-rectangle 300 100 40 (* 0.3 r))
                   (draw-rotating-rectangle 100 300 100 (* 0.6 r))
                   (draw-rotating-rectangle 300 300 20 (* 1.2 r))
                   (draw-rotating-rectangle 200 200 150 (* 2.3 r))))             
             (+ r 0.02))]

  (show-window {:draw-fn draw
                :canvas (make-canvas 400 400)
                :draw-state 0.0}))

;; second code

(let [draw (fn [canvas window frame ^double x]
             (-> canvas
                 (set-background 0xc9ff29)
                 (translate 200 200)
                 (set-color :red)
                 (ellipse x 0 10 10)
                 (set-color :green)
                 (ellipse (* x 2.0) 40 10 10)
                 (set-color :blue)
                 (ellipse (* x 0.5) 80 10 10))
             
             (inc x))]

  (show-window {:draw-fn draw
                :canvas (make-canvas 400 400)
                :draw-state 0.0}))

;; https://www.funprogramming.org/34-Create-a-grid-of-objects-with-nested-while-loops.html

(let [canvas (make-canvas 400 400)]
  (with-canvas [c canvas]
    (set-background c 23 100 240)
    (doseq [x (range 0 (width canvas) 50)]
      (doseq [y (range 0 (height canvas) 10)]
        (set-color c (r/randval 0.02 :red (let [r (r/drand 105)] (c/make-color r r r))))
        (ellipse c (+ 20 ^long x) (+ 20 ^long y) 44 44))))

  (show-window {:canvas canvas}))

;; https://www.funprogramming.org/35-A-grid-of-rotating-objects-creates-a-wave-of-rectangles.html

(let [draw-rotating-rectangle (fn [canvas x y rect-size r]
                                (-> canvas
                                    (translate x y)
                                    (rotate r)
                                    (rect 0 0 rect-size rect-size)
                                    (reset-matrix)))
      draw (fn [canvas window frame ^double r]
             (-> canvas
                 (set-background 100 200 50)
                 (set-color :white))

             (dotimes [x 8]
               (dotimes [y 8]
                 (draw-rotating-rectangle canvas (+ 50 (* 40 x)) (+ 50 (* 30 y)) 16 (+ r x y))))
             
             (+ r 0.05))]

  (show-window {:draw-fn draw
                :canvas (make-canvas 400 400)
                :draw-state 0.0}))

;; https://www.funprogramming.org/36-Organic-random-animation-using-noise.html

;; Comments:
;;
;; Noise used here is more dense than built in Processing (more octaves). Step through noise field should be smaller (0.005 instead of 0.02).

(let [draw (fn [canvas window frame ^double my-num]
             (let [clr (* 255.0 ^double (r/noise (+ 100.0 my-num)))
                   x (* ^double (r/noise my-num) ^int (width canvas))
                   y (* ^double (r/noise (+ 40.0 my-num)) ^int (height canvas))]
               (-> canvas
                   (set-background clr clr clr)
                   (line x 0 x (height canvas))
                   (line 0 y (width canvas) y)))
             
             (+ my-num 0.005))]

  (show-window {:draw-fn draw
                :canvas (make-canvas 400 400)
                :draw-state 0.0}))

;; https://www.funprogramming.org/37-Make-a-rectangle-dance-using-noise.html

(let [draw (fn [canvas window frame ^double my-num]
             (-> canvas
                 (set-background 0x810c2f)
                 (set-color :white)
                 (translate (* ^int (width canvas) ^double (r/noise (+ 80.0 my-num)))
                            (* ^int (height canvas) ^double (r/noise (+ 100.0 my-num))))
                 (rotate (* 10.0 ^double (r/noise (+ 40.0 my-num))))
                 (crect 0 0 (* 200.0 ^double (r/noise (+ 30.0 my-num)))
                        (* 200.0 ^double (r/noise my-num))))
             
             (+ my-num 0.005))]

  (show-window {:draw-fn draw
                :canvas (make-canvas 400 400)
                :draw-state 0.0}))

;; https://www.funprogramming.org/38-Animate-the-ocean-surface-using-noise.html

(let [draw (fn [canvas window frame ^double time]
             (-> canvas
                 (set-background :white)
                 (set-color :black))

             (dotimes [x (width canvas)]
               (line canvas x (+ 200.0 (* 50.0 ^double (r/noise (/ x 100.0) time))) x (height canvas)))
             
             (+ time 0.005))]

  (show-window {:draw-fn draw
                :canvas (make-canvas 400 400 :mid)
                :draw-state 0.0}))

;; https://www.funprogramming.org/39-The-candy-space-Understanding-noise-with-1-parameter.html

(let [canvas (make-canvas 400 200)]

  (with-canvas [c canvas]
    (set-background c :gray)
    (doseq [^double x (range 0 (width canvas) 40)]
      (let [x500 (/ x 500.0) 
            co (* 255.0 ^double (r/noise x500))]
        (println x500)
        (set-color c (c/from-HSB (c/make-color co 255 255)))
        (ellipse c x 100 20 20))))
  
  (show-window {:canvas canvas}))

;; https://www.funprogramming.org/40-The-candy-space-Understanding-noise-with-2-and-3-parameters.html

(let [draw (fn [canvas window frame ^double z]
             (doseq [^double x (range 0 (width canvas) 40)]
               (doseq [^double y (range 0 (height canvas) 40)]
                 (let [co (* 255.0 ^double (r/noise (/ x 500.0) (/ y 500.0) z))]
                   (set-color canvas (c/from-HSB (c/make-color co 255 255)))
                   (ellipse canvas (+ 20.0 x) (+ 20.0 y) 60 60))))
             
             (+ z 0.005))]

  (show-window {:draw-fn draw
                :canvas (make-canvas 400 200)
                :draw-state 0.0}))

;; https://www.funprogramming.org/41-A-function-that-generates-sine-waves.html

(let [canvas (make-canvas 300 300 :mid)]

  (with-canvas [c canvas]
    (-> c
        (set-background 0x357BC4)
        (set-color 0xD66727)
        (line 0 50 (width c) 50)
        (line 0 150 (width c) 150)
        (line 0 250 (width c) 250)
        (set-color :white))
    (dotimes [x (width c)]
      (-> c
          (point x (+ 50 (r/drand -10 10)))
          (point x (+ 150 (* 20.0 ^double (r/noise (/ x 20.0)))))
          (point x (+ 250 (* 20.0 (m/sin (/ x 10.0))))))))
  
  (show-window {:canvas canvas}))

;; https://www.funprogramming.org/42-Programming-animated-effects-on-Android-phones.html

(let [draw (fn [canvas window frame state]
             (let [x (r/drand (width canvas))]
               (-> canvas
                   (set-background :black 10)
                   (set-color (c/from-HSB (c/make-color (r/irand 255) 255 255)))
                   (line x 0 x (height canvas)))))]
  (show-window {:canvas (make-canvas 200 200)
                :draw-fn draw}))

;; https://www.funprogramming.org/43-Animate-using-sin-Less-math-thanks-to-map.html

;; Comments:
;;
;; Processing `map` is `norm` here

(let [draw (fn [canvas window frame ^double a]
             (let [x (m/norm (m/sin a) -1.0 1.0 300 400)]
               (-> canvas
                   (set-background 0xBAFF0D)
                   (set-color 0x556F15)
                   (ellipse x 200 30 30))) 
             (+ a 0.03))]

  (show-window {:draw-fn draw
                :canvas (make-canvas 400 400)
                :draw-state 0.0}))

;; https://www.funprogramming.org/44-Combine-sine-functions-to-create-crazy-waveforms.html

(let [canvas (make-canvas 500 300 :mid)]

  (with-canvas [c canvas]
    (-> c
        (set-background :black)
        (set-color :white))
    (loop [x 0.0
           a 0.0]
      (let [y (m/norm (* (m/sin a) (m/sin (* a 2.0)) (m/sin (* a 1.7))) -1.0 1.0 50 250)]
        (point c x y)
        (when (< x ^int (width c))
          (recur (inc x) (+ a 0.03))))))
  
  (show-window {:canvas canvas}))

;; https://www.funprogramming.org/45-Convert-distances-into-colors-or-widths.html

;; Comments:
;;
;; You can make color converter function working on different range than 0-255 (like colorMode in Processing)

(let [canvas (make-canvas 500 300 :mid)
      color-conv (c/make-color-converter c/from-HSB 100)]

  (with-canvas [c canvas]
    (-> c
        (set-background :black))
    (loop [x 0.0
           a 0.0]
      (let [y (m/norm (* (m/sin a) (m/sin (* a 3.0)) (m/sin (* a 4.0))) -1.0 1.0 50 250)
            co (m/norm y 50 250 0 100)
            sz (m/norm y 50 250 10 1)]
        (set-stroke c sz)
        (set-color c (color-conv (c/make-color co 100 100 100)))
        (point c x y)
        (when (< x ^int (width c))
          (recur (inc x) (+ a 0.03))))))
  
  (show-window {:canvas canvas}))

;; https://www.funprogramming.org/46-Create-beautiful-curves-with-lots-of-sin-calls.html

(let [color-conv (c/make-color-converter c/from-HSB 100)
      draw (fn [canvas window frame ^double a]
             (let [x (m/norm (* (m/sin a) (m/sin (* a 0.8))) -1.0 1.0 0 (width canvas))
                   y (m/norm (* (m/sin (+ 1.5 (* a 1.1))) (m/sin (* a 3.1))) -1.0 1.0 0 (height canvas))
                   co (m/norm (m/sin (* a 0.03)) -1.0 1.0 0 100)
                   sz (m/norm (* (m/sin (* a 1.7)) (m/sin (* a 2.3))) -1.0 1.0 5 30)
                   bri (m/norm (* (m/sin (* a 1.3)) (m/sin (* a 4.1))) -1.0 1.0 10 60)]
               (-> canvas
                   (set-color (color-conv (c/make-color co 50 bri 100)))
                   (ellipse x y sz sz))) 
             (+ a 0.03))]

  (show-window {:draw-fn draw
                :canvas (make-canvas 500 300)
                :draw-state 0.0}))

;; https://www.funprogramming.org/47-Share-your-Processing-program-with-the-world.html

;; Comments:
;;
;; There is no copy function. Instead just get image and paste it with new width/height. Result is sligthly different (different method of resizing pixels)
;; You can change quality of canvas to `:high` or `:low` for different effect.

(let [draw (fn [canvas _ _ _]
             (let [r (r/drand 100)]
               (set-color canvas (r/randval 0xB1FF0A 0x315500))
               (ellipse canvas 200 200 r r true))
             (let [img (get-image canvas)]
               (image canvas img -3 -1 (+ 6 ^int (width canvas)) (+ 2 ^int (height canvas)))))]
  
  (show-window {:draw-fn draw 
                :canvas (make-canvas 400 400 :mid)
                :setup (fn [canvas _] (set-background canvas 0xB1FF0A))}))

;; https://www.funprogramming.org/48-Load-and-animate-an-image-of-Rick.html

;; Comments:
;;
;; * Image has type BufferedImage (java)
;; * More optimal drawing (stop drawing when finished)

(let [photo (load-image "results/test.jpg")
      draw (fn [canvas _ _ ^long x]
             (if-not (neg? x)
               (do
                 (set-background canvas :black)
                 (image canvas photo x 0)
                 (dec x))
               -1))]
  
  (show-window {:draw-fn draw 
                :canvas (make-canvas (width photo) (height photo) :mid)
                :draw-state (width photo)}))

;; https://www.funprogramming.org/49-Show-part-of-a-loaded-image-using-copy.html

;; Comments:
;;
;; * Used different part of the image (different source)
;; * To cut part of image use `subimage` function

(let [canvas (make-canvas 300 300 :mid)
      photo (load-image "results/test.jpg")
      draw (fn [canvas _ _ _]
             (-> canvas
                 (set-background 40 40 40)
                 (set-color :white)
                 (image (subimage photo 200 100 250 250) 0 100 (width canvas) 100)
                 (line 0 100 (width canvas) 100)
                 (line 0 200 (width canvas) 200)))]
  
  (show-window {:canvas canvas
                :draw-fn draw}))

;; https://www.funprogramming.org/50-What-are-global-and-local-variables.html

;; SKIPPED

;; https://www.funprogramming.org/51-Convert-float-into-int-and-animate-a-photo.html

(let [canvas (make-canvas 300 300 :mid)
      photo (load-image "results/test.jpg")
      draw (fn [canvas _ _ ^double a]
             (let [y (int (m/norm (r/noise a) 0.0 1.0 0 200))]
               (-> canvas
                   (set-background 40 40 40)
                   (set-color :white)
                   (image (subimage photo 200 y 250 250) 0 100 (width canvas) 100)
                   (line 0 100 (width canvas) 100)
                   (line 0 200 (width canvas) 200)))
             (+ a 0.005))]
  
  (show-window {:canvas canvas
                :draw-fn draw
                :draw-state 0.0}))

;; https://www.funprogramming.org/52-The-drunk-camera-man-effect.html

(let [canvas (make-canvas 300 300 :mid)
      photo (load-image "results/test.jpg")
      draw (fn [canvas _ _ ^double a]
             (let [wi (int (m/norm (r/noise (+ a 30)) 0.0 1.0 50 150))
                   he (int (m/norm (r/noise (+ a 40)) 0.0 1.0 50 150))
                   x (int (m/norm (r/noise (+ a 10)) 0.0 1.0 0 (- ^int (width photo) wi)))
                   y (int (m/norm (r/noise (+ a 20)) 0.0 1.0 0 (- ^int (height photo) wi)))]
               (-> canvas
                   (set-background 40 40 40)
                   (set-color :white)
                   (image (subimage photo x y wi he) 0 100 (width canvas) 100)
                   (line 0 100 (width canvas) 100)
                   (line 0 200 (width canvas) 200)))
             (+ a 0.001))]
  
  (show-window {:canvas canvas
                :draw-fn draw
                :draw-state 0.0}))

;; https://www.funprogramming.org/53-Create-a-pattern-by-drawing-150000-pixels.html

(let [canvas (make-canvas 500 300 :mid)
      color-conv (c/make-color-converter c/from-HSB 10)]

  (with-canvas [c canvas]
    (dotimes [x (width canvas)]
      (dotimes [y (height canvas)]
        (let [v (* (m/sin (+ (/ x 30.0) (/ y 20.0)))
                   (m/sin (- (/ x 13.0) (/ y 23.0))))
              h (m/norm v -1.0 1.0 0.0 10.0)]
          (set-color c (color-conv (c/make-color h 8 8 10)))
          (point c x y)))))
  
  (show-window {:canvas canvas}))

;; https://www.funprogramming.org/54-Infinite-Forest-Combine-random-words-using-an-Array.html

(let [nouns ["forest" "tree" "flower" "sky" "grass" "mountain"]
      adjectives  ["happy" "rotating" "red" "fast" "elastic" "smily" "unbelievable" "infinite"]
      window (show-window)]

  (with-canvas-> (get-canvas window)
    (text (rand-nth nouns) 10 50)
    (text (rand-nth adjectives) 10 30)))

;; https://www.funprogramming.org/55-How-many-items-in-an-Array.html
;; https://www.funprogramming.org/56-Silly-poet-writes-absurd-things.html

;; SKIPPED

;; https://www.funprogramming.org/57-A-random-sentence-generator-writes-nonsense.html

;; Comments:
;;
;; * getting random sequence is made with clojure functions
;; * random filename can be generated with `next-filename` function

(let [canvas (make-canvas 500 400)
      dicts {:art  ["the" "my" "your" "our" "that" "this" "every" "one" "the only" "his" "her"]
             :adj ["happy" "rotating" "red" "fast" "elastic" "smily" "unbelievable" "infinte" "surprising" 
                   "mysterious" "glowing" "green" "blue" "tired" "hard" "soft" "transparent" "long" "short" 
                   "excellent" "noisy" "silent" "rare" "normal" "typical" "living" "clean" "glamorous" 
                   "fancy" "handsome" "lazy" "scary" "helpless" "skinny" "melodic" "silly" 
                   "kind" "brave" "nice" "ancient" "modern" "young" "sweet" "wet" "cold" 
                   "dry" "heavy" "industrial" "complex" "accurate" "awesome" "shiny" "cool" "glittering" 
                   "fake" "unreal" "naked" "intelligent" "smart" "curious" "strange" "unique" "empty" 
                   "gray" "saturated" "blurry"]
             :nou ["forest" "tree" "flower" "sky" "grass" "mountain" "car" "computer" "man" "woman" "dog" 
                   "elephant" "ant" "road" "butterfly" "phone" "computer program" "grandma" "school" "bed" "mouse" 
                   "keyboard" "bicycle" "spaghetti" "drink" "cat" "t-shirt" "carpet" "wall" "poster" 
                   "airport" "bridge" "road" "river" "beach" "sculpture" "piano" "guitar" "fruit" 
                   "banana" "apple" "strawberry" "rubber band" "saxophone" "window" "linux computer" 
                   "skate board" "piece of paper" "photograph" "painting" "hat" "space" "fork" 
                   "mission" "goal" "project" "tax" "wind mill" "light bulb" "microphone" 
                   "cpu" "hard drive" "screwdriver"]
             :ver ["sings" "dances" "was dancing" "runs" "will run" "walks" 
                   "flies" "moves" "moved" "will move" "glows" "glowed" "spins" "promised" 
                   "hugs" "cheated" "waits" "is waiting" "is studying" "swims" 
                   "travels" "traveled" "plays" "played" "enjoys" "will enjoy" 
                   "illuminates" "arises" "eats" "drinks" "calculates" "kissed" "faded" "listens" 
                   "navigated" "responds" "smiles" "will smile" "will succeed" 
                   "is wondering" "is thinking" "is" "was" "will be" "might be" "was never"]
             :pre ["under" "in front of" "above" "behind" "near" "following" "inside" "besides" 
                   "unlike" "like" "beneath" "against" "into" "beyond" "considering" "without" 
                   "with" "towards"]} 
      sentence-conf (fn [] (map-indexed #(hash-map :x (r/drand 50 150)
                                                   :y (+ 50 (* ^long %1 30))
                                                   :text-size (r/drand 20 40)
                                                   :word %2)
                                        (map #(rand-nth (dicts %)) [:art :adj :nou :ver :pre :art :adj :nou])))

      new-sentence #(with-canvas [c canvas]
                      (set-background c :white)
                      (set-color c 0x258FBF)
                      (doseq [{:keys [x y text-size word]} (sentence-conf)]
                        (set-font-attributes c text-size)
                        (text c word x y)))
      wname "Sentence generator 57"]

  (new-sentence)
  
  (show-window {:canvas canvas
                :window-name wname})

  (defmethod mouse-event [wname :mouse-pressed] [_ _] (new-sentence))
  (defmethod key-pressed [wname \space] [_ _] (save canvas (next-filename "" ".jpg"))))


;; https://www.funprogramming.org/58-Travel-through-space-use-an-array-to-move-stars.html

(let [canvas (make-canvas 500 400 :mid)
      draw (fn [canvas _ _ points]
             
             (set-background canvas :black)
             (set-color canvas :white)
             (set-stroke canvas 5.0)
             
             (doseq [[x y _] points]
               (point canvas x y))

             (mapv #(let [[^double x y ^double speed] %1
                          nx (- x speed)]
                      [(if (neg? nx) (width canvas) nx) y speed]) points))]
  
  (show-window {:canvas canvas
                :draw-fn draw 
                :draw-state (repeatedly 100 #(vector (r/drand (width canvas))
                                                     (r/drand (height canvas))
                                                     (r/drand 1 5)))}))

;; https://www.funprogramming.org/59-A-space-triangle-flying-through-the-galaxy.html

;; Comments:
;;
;; `noCursor` is not implemented

(let [canvas (make-canvas 500 400 :mid)
      draw (fn [canvas window _ points]
             
             (set-background canvas :black)
             (set-color canvas :white)
             (set-stroke canvas 3)

             (let [^int mx (mouse-x window)
                   ^int my (mouse-y window)]
               (triangle canvas mx (- my 6) mx (+ my 6) (+ mx 30) my)
               (set-color canvas 200 200 200)
               (triangle canvas mx (- my 6) mx (+ my 6) (+ mx 30) my true))
             
             (doseq [[x y speed] points]
               (let [co (m/norm speed 1 5 100 255)]
                 (-> canvas
                     (set-color co co co)
                     (set-stroke speed)
                     (point x y))))

             (mapv #(let [[^double x y ^double speed] %1
                          nx (- x speed)]
                      [(if (neg? nx) (width canvas) nx) y speed]) points))]
  
  (show-window {:canvas canvas
                :draw-fn draw 
                :draw-state (repeatedly 100 #(vector (r/drand (width canvas))
                                                     (r/drand (height canvas))
                                                     (r/drand 1 5)))}))

;; https://www.funprogramming.org/60-Are-two-circles-touching-or-intersecting.html

(let [diam 100
      draw (fn [canvas window _ ^double a]
             (let [x (* ^int (width canvas) ^double (r/noise a 10.0))
                   y (* ^int (height canvas) ^double (r/noise a 20.0))
                   d (m/dist x y (mouse-x window) (mouse-y window))
                   stroke (if (> d diam) 1.0 (r/drand 10))]
               (-> canvas
                   (set-background :black)
                   (set-stroke stroke)
                   (set-color 255 255 0)
                   (ellipse x y diam diam true)
                   (set-color 0 255 0)
                   (ellipse (mouse-x window) (mouse-y window) diam diam true)))
             (+ a 0.003)
             )]
  (show-window {:canvas (make-canvas 500 400 :mid)
                :draw-fn draw
                :draw-state 0.0}))

;; https://www.funprogramming.org/61-Draw-shaky-points-append-items-to-an-array.html

(let [wname "Shaky point 61"
      prob (/ 17.0 20.0)
      draw (fn [canvas window _ _]             
             (let [state (get-state window)]
               (set-background canvas :white)
               (doseq [[x y] state]
                 (let [clr (r/randval prob :black :red)] 
                   (set-color canvas clr)
                   (ellipse canvas x y 20 20)))
               (set-state! window (map #(let [[^double x ^double y] %]
                                          [(+ x (r/drand -2 2))
                                           (+ y (r/drand -2 2))]) state))))]

  (defmethod mouse-event [wname :mouse-pressed] [event state]
    (conj state [(mouse-x event) (mouse-y event)]))
  
  (show-window {:canvas (make-canvas 500 400 :mid)
                :draw-fn draw
                :state [[250 200]]
                :window-name wname}))

;; https://www.funprogramming.org/62-A-screen-full-of-bouncing-circles.html

(let [wname "Bouncing circles 62"
      prob (/ 17.0 20.0)
      draw (fn [canvas window _ _]             
             (let [state (get-state window)]
               (set-background canvas :black 10)
               (set-color canvas 0xD60DFF)
               
               (doseq [[^double x ^double y] state]
                 (let [sz (r/drand 10 30)] 
                   (ellipse canvas (+ x (r/drand -3 3)) (+ y (r/drand -3 3)) sz sz true)))
               
               (set-state! window (map #(let [[^double x ^double y ^double movex ^double movey] %
                                              nx (+ x movex)
                                              ny (+ y movey)
                                              [nx nmovex] (if (neg? nx)
                                                            [0 (- movex)]
                                                            (if (> nx ^int (width canvas))
                                                              [(width canvas) (- movex)]
                                                              [nx movex]))
                                              [ny nmovey] (if (neg? ny)
                                                            [0 (- movey)]
                                                            (if (> ny ^int (height canvas))
                                                              [(height canvas) (- movey)] ; comment
                                                              ;; [0 movey] ; uncomment
                                                              [ny movey]))]
                                          [nx ny nmovex nmovey]) state))))]

  (defmethod mouse-event [wname :mouse-pressed] [event state]
    (conj state [(mouse-x event) (mouse-y event)
                 (r/drand -1 1) (r/drand 1 3)]))
  
  (show-window {:canvas (make-canvas 400 200 :mid)
                :draw-fn draw
                :state []
                :window-name wname}))

;; https://www.funprogramming.org/63-Time-for-our-first-3D-animation.html

;; SKIPPED (no 3d)

;; https://www.funprogramming.org/64-Animate-objects-that-slow-down-and-stop-using-lerp.html

(let [canvas (make-canvas 500 400)
      draw (fn [canvas window _ [x y destx desty]]
             (-> canvas
                 (set-background :white)
                 (set-color :red)
                 (ellipse x y 20 20))
             (let [nx (m/lerp x destx 0.1)
                   ny (m/lerp y desty 0.1)
                   d (m/dist x y destx desty)
                   [ndestx ndesty] (if (< d 1.0)
                                     [(r/drand (width canvas)) (r/drand (height canvas))]
                                     [destx desty])]
               (when (< d 50.0)
                 (ellipse canvas x y (- 100 d) (- 100 d) true))
               [nx ny ndestx ndesty]))]
  (show-window {:canvas canvas
                :draw-fn draw
                :draw-state [0.0 0.0 (r/drand (width canvas)) (r/drand (height canvas))]}))

;; https://www.funprogramming.org/66-How-random-is-random-randomSeed-noiseSeed.html

;; Comments
;;
;; To use seed you have to create your own RNG with `make-randomizer` function. To use own RNG you have to use `[id]random` functions family. `[id]rand` is bound to default JDK RNG.

(let [canvas (make-canvas 500 400)
      from-hsb (c/make-color-converter c/from-HSB 100)
      seed (r/irand 10000000)
      ;; seed 100
      rng (r/make-randomizer :mersenne seed)]

  (println seed)
  
  (with-canvas [c canvas]
    (loop [x (int 0)]
      (when (< x ^int (width canvas))
        (let [wi (r/irandom rng 100)]
          (set-color c (from-hsb (c/make-color (r/drandom rng 100) 80 80 100)))
          (rect c x 0 wi (height canvas))
          (recur (+ x wi))))))

  (show-window {:canvas canvas}))

;; https://www.funprogramming.org/67-Circular-motion-sine-and-her-cousin.html

(let [canvas (make-canvas 500 400)
      from-hsb (c/make-color-converter c/from-HSB 100)
      draw (fn [canvas window ^long frame [^double a ^double b]]
             (let [x0 (m/norm (m/sin a) -1.0 1.0 20.0 (- ^int (width canvas) 20))
                   y0 (m/norm (m/cos a) -1.0 1.0 20.0 (- ^int (height canvas) 20))
                   x1 (m/norm (m/sin b) -1.0 1.0 20.0 (- ^int (width canvas) 20))
                   y1 (m/norm (m/cos b) -1.0 1.0 20.0 (- ^int (height canvas) 20))]
               (-> canvas
                   (set-stroke 3.0)
                   (set-color (from-hsb (c/make-color (mod frame 100) 80 80 20)))
                   (line x0 y0 x1 y1))
               [(+ a 0.071)
                (+ b 0.07)]))]
  (with-canvas-> canvas (set-background :white))
  (show-window {:canvas canvas
                :draw-fn draw
                :draw-state [0.0 0.0]}))

;; https://www.funprogramming.org/68-Circular-motion-reviewed.html

(let [canvas (make-canvas 500 400)
      draw (fn [canvas window _ ^double a]
             (when (<= a m/TWO_PI)
               (let [r (r/drand 180 220)
                     x (+ (/ ^int (width canvas) 2) (* r (m/cos a)))
                     y (+ (/ ^int (height canvas) 2) (* r (m/sin a)))]
                 (-> canvas
                     (set-color :white) 
                     (ellipse x y 10 10)
                     (set-color :black) 
                     (ellipse x y 10 10 true))
                 (+ a 0.1))))]
  (with-canvas-> canvas (set-background :lightgrey))
  (show-window {:canvas canvas
                :draw-fn draw
                :draw-state 0.0}))

;; https://www.funprogramming.org/69-Combine-circular-and-other-motions.html

(let [canvas (make-canvas 500 400)
      x (/ ^int (width canvas) 2)
      y (/ ^int (height canvas) 2)
      draw (fn [canvas window _ [^double a ^double b]]
             (let [x2 (* (m/sin a) 50)
                   y2 (* (m/cos a) 50)
                   x3 (* (m/sin b) 200)
                   y3 (* (m/cos b) 200)]
               (set-color canvas :red)
               (ellipse canvas (+ x x2 x3) (+ y y2 y3) 10 10))
             [(+ a 0.1) (+ b 0.01)])]
  (with-canvas-> canvas (set-background :white))
  (show-window {:canvas canvas
                :draw-fn draw
                :draw-state [0.0 0.0]}))

;; https://www.funprogramming.org/70-Slowly-change-the-direction.html

(let [canvas (make-canvas 500 400)
      reset #(vector (/ ^int (width canvas) 2) (/ ^int (height canvas) 2) (r/drand m/TWO_PI))
      draw (fn [canvas window _ [^double oldx ^double oldy ^double a]]
             (let [newx (+ oldx (* 5.0 (m/cos a)))
                   newy (+ oldy (* 5.0 (m/sin a)))]
               (set-color canvas :black)
               (line canvas oldx oldy newx newy)
               [newx newy (+ a (r/drand -0.4 0.4))]))]
  (with-canvas-> canvas (set-background :white))
  (show-window {:canvas canvas
                :draw-fn draw
                :draw-state (reset)}))

;; https://www.funprogramming.org/71-Playing-with-directions.html

(let [canvas (make-canvas 500 400)
      reset #(vector (/ ^int (width canvas) 2.0) (/ ^int (height canvas) 2.0) (r/drand m/TWO_PI) 1.0)
      from-hsb (c/make-color-converter c/from-HSB 100)
      draw (fn [canvas window _ [^double oldx ^double oldy ^double a ^double w]]
             (let [newx (+ oldx (* 5.0 (m/cos a)))
                   newy (+ oldy (* 5.0 (m/sin a)))]
               
               (set-color canvas (from-hsb (c/make-color 30 100 (* 100 ^double (r/noise w a)) 100)))
               (set-stroke canvas w)
               (line canvas oldx oldy newx newy)
               
               (if (bool-or (neg? newx)
                            (neg? newy)
                            (> newx ^int (width canvas))
                            (> newy ^int (height canvas)))
                 (reset)
                 [newx newy (+ a (r/drand -0.4 0.2)) (+ w 0.1)])))]
  (with-canvas-> canvas (set-background :white))
  (show-window {:canvas canvas
                :draw-fn draw
                :draw-state (reset)}))

;; https://www.funprogramming.org/72-Create-visual-rhythms-using-modulo.html

;; Comments:
;;
;; Using framecounter here instead of variable

(let [canvas (make-canvas 500 400)
      draw (fn [canvas window ^long frame _]
             (set-background canvas :white)
             (set-color canvas 50 200 40)
             (when (zero? (mod frame 2)) (rect canvas 0 0 100 100))
             (when (== 1 (mod frame 7)) (rect canvas 100 0 100 100))
             (when (== 2 (mod frame 7)) (rect canvas 200 0 100 100)))]
  (show-window {:canvas canvas
                :fps 7
                :draw-fn draw}))

;; https://www.funprogramming.org/73-The-circlebeats-of-a-running-circle.html

(let [canvas (make-canvas 500 400)
      draw (fn [canvas window ^long frame [^int csize ^int grow]]
             (set-background canvas :white)
             (set-color canvas :red)
             (let [grow (if (zero? (mod frame 60)) 5 grow)
                   [^int csize ^int grow] (cond
                                            (> csize 80) [80 -2]
                                            (< csize 50) [50 0]
                                            :else [csize grow])]
               (ellipse canvas 250 200 csize csize)
               [(+ csize grow) grow]))]
  (show-window {:canvas canvas
                :draw-fn draw
                :draw-state [50 0]}))

;; https://www.funprogramming.org/74-for-loops-and-other-ways-of-typing-less.html

;; SKIPPED

;; https://www.funprogramming.org/75-Bezier-curves-are-so-beautiful.html

(let [canvas (make-canvas 500 400)
      ^int w (width canvas)
      ^int h (height canvas)
      midw (/ w 2)]

  (with-canvas [c canvas]
    (set-background c :white)
    (set-color c :black)
    (dotimes [i 30]
      (bezier c midw h midw (r/drand h)
              (r/drand w) (r/drand h)
              (r/drand w) (r/drand h))))
  
  (show-window {:canvas canvas}))

;; https://www.funprogramming.org/76-Slowly-morphing-bezier-curves.html

(let [canvas (make-canvas 500 400)
      ^int w (width canvas)
      ^int h (height canvas)
      midw (/ w 2)
      draw (fn [canvas _ ^long frame _]
             (let [t (/ frame 300.0)]
               (set-background canvas :white)
               (set-color canvas :black)
               (dotimes [i 30]
                 (bezier canvas
                         midw h
                         midw (* ^double (r/noise 1 i t) h)
                         (* ^double (r/noise 2 i t) w) (* ^double (r/noise 4 i t) h)
                         (* ^double (r/noise 3 i t) w) (* ^double (r/noise 5 i t) h)))))]
  
  (show-window {:canvas canvas
                :draw-fn draw}))

;; https://www.funprogramming.org/77-A-3D-rotating-cloud-of-points.html
;; https://www.funprogramming.org/78-An-array-is-like-a-book-full-of-numbers.html
;; https://www.funprogramming.org/79-A-spinning-star-becomes-a-plant.html

;; SKIPPED (no 3d)

;; https://www.funprogramming.org/80-The-color-datatype.html

;; Comments:
;;
;; Color is represented as vector (Vec4) datatype

(show-window {:setup (fn [canvas _]
                       (let [orange (c/to-color 0xFC8E05)
                             nice-blue (c/to-color 0x2A78F2)
                             white (c/make-color 255 255 255)
                             ;; white (c/to-color :white) ;; variant
                             ]
                         (println orange)
                         (println nice-blue)
                         (-> canvas
                             (set-background orange)
                             (set-stroke 4)
                             (set-color nice-blue)
                             (ellipse 100 100 120 120)
                             (set-color white)
                             (ellipse 100 100 120 120 true))))})

;; https://www.funprogramming.org/81-How-to-read-the-color-of-a-pixel.html

(let [canvas (make-canvas 500 400)
      draw (fn [canvas window _ _]
             (let [color-under-mouse (get-pixel canvas (mouse-x window) (mouse-y window))]
               (set-color canvas color-under-mouse)
               (rect canvas 0 120 (width canvas) 280)))]

  (with-canvas [c canvas]
    (dotimes [x (width c)]
      (dotimes [y 120]
        (let [r (m/norm (r/noise (/ x 80.0) (/ y 80.0) 10.0) 0.0 1.0 0.0 255.0)
              g (m/norm (r/noise (/ x 80.0) (/ y 80.0) 20.0) 0.0 1.0 0.0 255.0)
              b (m/norm (r/noise (/ x 80.0) (/ y 80.0) 30.0) 0.0 1.0 0.0 255.0)]
          (set-color c r g b)
          (point c x y)))))
  
  (show-window {:canvas canvas
                :draw-fn draw}))

;; https://www.funprogramming.org/82-Program-a-gradient-of-colors.html

(let [canvas (make-canvas 500 400)
      from-hsb (c/make-color-converter c/from-HSB 100)
      c1 (c/make-color (r/drand 100) 100 100 100)
      c2 (c/make-color (r/drand 100) 100 30 100)]
  
  (with-canvas [c canvas]
    (dotimes [y (height c)]
      (let [n (m/norm y 0 (height c))
            newc (v/interpolate c1 c2 n)]
        (set-color c (from-hsb newc))
        (line c 0 y (width c) y))))
  
  (show-window {:canvas canvas}))

;; https://www.funprogramming.org/83-Circular-gradients-can-look-like-spheres.html

(let [canvas (make-canvas 500 400)
      from-hsb (c/make-color-converter c/from-HSB 100)
      c1 (c/make-color (r/drand 100) 100 100 100)
      c2 (c/make-color (r/drand 100) 100 30 100)
      maxr 500]
  
  (with-canvas [c canvas]
    (set-stroke c 2)
    (dotimes [r maxr]
      (let [n (m/norm r 0 maxr)
            newc (v/interpolate c1 c2 n)]
        (set-color c (from-hsb newc))
        (ellipse c 100 100 r r true))))
  
  (show-window {:canvas canvas}))

;; https://www.funprogramming.org/84-Draw-gradients-review-functions-and-image-loading.html

(let [wname "Gradients 84"
      back (load-image "results/test.jpg")
      canvas (make-canvas (width back) (height back))
      from-hsb (c/make-color-converter c/from-HSB 100)
      draw-circ-grad (fn [canvas x y maxd]
                       (let [c1 (c/make-color (r/drand 100) 100 100 100)
                             c2 (c/make-color (r/drand 100) 100 30 100)]
                         (set-stroke canvas 2)
                         (dotimes [r maxd]
                           (let [n (m/norm r 0 maxd)
                                 newc (v/interpolate c1 c2 n)]
                             (set-color canvas (from-hsb newc))
                             (ellipse canvas x y r r true)))))]
  
  (with-canvas-> canvas (image back))

  (defmethod mouse-event [wname :mouse-pressed] [event _]
    (with-canvas-> canvas
      (draw-circ-grad (mouse-x event) (mouse-y event) (r/drand 50 300))))
  
  (show-window {:canvas canvas
                :window-name wname}))


;; https://www.funprogramming.org/85-Using-a-background-image-mousePressed-and-mouseReleased.html

(let [wname "Mouse pressed and released 85"
      back (load-image "results/test.jpg")
      canvas (make-canvas (width back) (height back))
      draw (fn [canvas window _ ^double d]
             (image canvas back)
             (ellipse canvas (mouse-x window) (mouse-y window) d d true)
             (if (= :pressed (get-state window)) (inc d) 20.0))]
  
  (defmethod mouse-event [wname :mouse-pressed] [_ _] :pressed)
  (defmethod mouse-event [wname :mouse-released] [_ _] :released)
  
  (show-window {:canvas canvas
                :window-name wname
                :draw-fn draw
                :draw-state 20.0
                :state :released}))

;; https://www.funprogramming.org/86-Drawing-shapes-with-your-mouse.html

;; Comments:
;;
;; Solution is highly odd. Main reasons:
;; * no access to button state inside draw function
;; * events and draw-fn are in different thread
;; * access/write to global state is asynchronous (unfortunately)

(let [wname "Mouse pressed and released 85"
      back (p/load-pixels "results/test.jpg")
      canvas (make-canvas (width back) (height back))
      draw (fn [canvas window _ [d back]]
             (let [nd (or d 20.0)]
               (p/set-canvas-pixels! canvas back)
               (ellipse canvas (mouse-x window) (mouse-y window) nd nd true)
               
               (if (= (get-state window) :pressed)
                 [(inc ^double nd) back]
                 (if (nil? d)
                   [nil back]
                   [nil (p/get-canvas-pixels canvas)]))))]
  
  (defmethod mouse-event [wname :mouse-pressed] [_ _] :pressed)
  (defmethod mouse-event [wname :mouse-released] [_ _] :released)
  
  (show-window {:canvas canvas
                :window-name wname
                :draw-fn draw
                :draw-state [nil back]
                :state :released}))

;; https://www.funprogramming.org/87-Playing-with-strings.html

;; Comments
;;
;; Skipped language related things (printing chars)

(let [get-dt (fn [] (let [dt (map #(format "%02d" %) (datetime :vector))
                          d (str "Date: " (join "." (take 3 dt)))
                          t (str "Time: " (join ":" (take 3 (drop 3 dt))))]
                      [d t]))
      draw (fn [canvas _ _ _]
             (let [[d t] (get-dt)]
               (-> canvas
                   (set-background 0xFFB81F)
                   (set-color :white)
                   (set-font-attributes 20)
                   (text d 100 100)
                   (text t 100 130))))]
  (show-window {:canvas (make-canvas 400 400)
                :draw-fn draw
                :fps 1}))

;; https://www.funprogramming.org/88-Change-pixels-using-the-pixels-array.html

;; Comments:
;;
;; Pixels in Clojure2d have different format than in Processing. It's an array with 4 separated channels in planar layout with values from 0 to 255.
;; To operate on array you can use massive and parallel filter higher order functions like `filter-colors` or `filter-channels`

(let [canvas (make-canvas 500 400)
      pixels (p/get-canvas-pixels canvas)
      make-gray (fn [_] (let [c (r/irand 256)] (c/make-color c c c)))
      draw (fn [canvas _ _ _] (p/set-canvas-pixels! canvas (p/filter-colors make-gray pixels)))]
  (show-window {:canvas canvas 
                :draw-fn draw}))

;; https://www.funprogramming.org/89-Create-your-own-photo-filters.html

(let [img (load-image "results/test.jpg")
      canvas (make-canvas (width img) (height img))
      mix-channels (fn [c] (c/make-color (c/green c) (c/blue c) (c/red c)))]

  (p/set-canvas-pixels! canvas (p/filter-colors mix-channels (p/get-image-pixels img)))
  
  (show-window {:canvas canvas}))

;; https://www.funprogramming.org/90-Change-pixel-hue-saturation-and-brightness.html

;; Comments
;;
;; Instead converting to HSB I used to-luma fn

(let [img (load-image "results/test.jpg")
      canvas (make-canvas (width img) (height img))
      mix-channels (fn [c] (if (> ^double (c/to-luma c) 100.0)
                             (c/to-color :white)
                             (c/to-color :black)))]

  (p/set-canvas-pixels! canvas (p/filter-colors mix-channels (p/get-image-pixels img)))
  
  (show-window {:canvas canvas}))

;; variant from video

(let [img (load-image "results/test.jpg")
      canvas (make-canvas (width img) (height img))
      mix-channels (fn [c] (let [hsb (c/to-HSB c)
                                 h (c/ch0 hsb)
                                 s (c/ch1 hsb)
                                 b (c/ch2 hsb)]
                             ;; (c/from-HSB (c/make-color h 0 b))
                             (c/make-color h s b) ))]

  (p/set-canvas-pixels! canvas (p/filter-colors mix-channels (p/get-image-pixels img)))
  
  (show-window {:canvas canvas}))

;; https://www.funprogramming.org/91-Timelines-tell-a-story.html

;; Comments
;;
;; * To draw filled object with stroke I use filled-with-stroke higer order function
;; * `millis` fn doesn't return time from moment of running script. You have to manage it your self.

(let [initmillis (millis)
      scene1 #(filled-with-stroke % :white :black ellipse (r/drand (width %)) (r/drand (height %)) 100 100)
      scene2 #(filled-with-stroke % :white :black rect (r/drand (width %)) (r/drand (height %)) 100 100)
      scene3 #(let [x (r/drand (width %))]
                (set-color % :black)
                (line % x 0 x (height %))) 
      draw (fn [canvas _ _ _]
             (let [m (- (millis) initmillis)]
               (set-background canvas 0x2D77EA)
               (condp clojure.core/> m
                 2000 (scene1 canvas)
                 12000 (scene2 canvas)
                 17000 (scene3 canvas)
                 nil)))]
  (show-window {:canvas (make-canvas 500 400)
                :draw-fn draw}))

;; https://www.funprogramming.org/92-Interactivity-a-draggable-circle.html
;;
;; Comments:
;;
;; Events and global state are used here to manage dragging

(let [wname "Draggable circle 92"
      sz 100
      fill (c/from-HSB (c/make-color (r/drand 255) 100 200))
      bgcolor (c/from-HSB (c/make-color (r/drand 255) 150 255))
      stroke (c/make-color :white)
      draw (fn [canvas window _ _]
             (let [{:keys [x y stroke-size]} (get-state window)]
               (set-background canvas bgcolor)
               (set-color canvas fill)
               (set-stroke canvas stroke-size)
               (ellipse canvas x y sz sz)
               (when (< (m/dist x y (mouse-x window) (mouse-y window)) (/ sz 2))
                 (set-color canvas stroke)
                 (ellipse canvas x y sz sz true))))]

  (defmethod mouse-event [wname :mouse-pressed] [_ state] (assoc state :stroke-size 5))
  (defmethod mouse-event [wname :mouse-released] [_ state] (assoc state :stroke-size 2))
  (defmethod mouse-event [wname :mouse-dragged] [event state] (assoc state :x (mouse-x event) :y (mouse-y event)))
  
  (show-window {:window-name wname
                :canvas (make-canvas 400 300)
                :draw-fn draw
                :state {:x 100
                        :y 100
                        :stroke-size 2}}))

;; https://www.funprogramming.org/93-Draggable-circle-with-tweening.html

(let [wname "Draggable circle 93"
      sz 100.0
      fill (c/from-HSB (c/make-color (r/drand 255) 100 200))
      bgcolor (c/from-HSB (c/make-color (r/drand 255) 150 255))
      stroke (c/make-color :white)
      draw (fn [canvas window _ [x y current-weight target-weight current-alpha]]
             (let [pressed? (get-state window)
                   [nx ny ntarget-weight target-alpha] (if (< (m/dist x y (mouse-x window) (mouse-y window)) (/ sz 2))
                                                         (if pressed?
                                                           [(m/lerp x (mouse-x window) 0.2)
                                                            (m/lerp y (mouse-y window) 0.2)
                                                            10 255]
                                                           [x y 5 255])
                                                         [x y target-weight 0])
                   ncurrent-weight (m/lerp current-weight ntarget-weight 0.2)
                   ncurrent-alpha (m/lerp current-alpha target-alpha 0.2)]

               (-> canvas
                   (set-background bgcolor)
                   (set-color fill)
                   (set-stroke ncurrent-weight)
                   (ellipse nx ny sz sz)
                   (set-color stroke ncurrent-alpha)
                   (ellipse nx ny sz sz true))

               [nx ny ncurrent-weight ntarget-weight ncurrent-alpha]))]

  (defmethod mouse-event [wname :mouse-pressed] [_ _] true)
  (defmethod mouse-event [wname :mouse-released] [_ _] false)
  
  (show-window {:window-name wname
                :canvas (make-canvas 400 300)
                :draw-fn draw
                :draw-state [100 100 2 2 0]}))

;; https://www.funprogramming.org/94-Boolean-true-or-false.html

;; SKIPPED

;; https://www.funprogramming.org/95-Is-the-mouse-inside-a-square.html

(let [wname "Inside square? 95"
      sz 100.0
      fill (c/from-HSB (c/make-color (r/drand 255) 100 200))
      bgcolor (c/from-HSB (c/make-color (r/drand 255) 150 255))
      stroke (c/make-color :white)
      draw (fn [canvas window _ [^double x ^double y current-weight target-weight current-alpha]]
             (let [pressed? (get-state window)
                   [nx ny ntarget-weight target-alpha] (if (bool-and (> ^int (mouse-x window) (- x (/ sz 2.0)))
                                                                     (< ^int (mouse-x window) (+ x (/ sz 2.0)))
                                                                     (> ^int (mouse-y window) (- y (/ sz 2.0)))
                                                                     (< ^int (mouse-y window) (+ y (/ sz 2.0))))
                                                         (if pressed?
                                                           [(m/lerp x (mouse-x window) 0.2)
                                                            (m/lerp y (mouse-y window) 0.2)
                                                            10.0 255.0]
                                                           [x y 5.0 255.0])
                                                         [x y target-weight 0.0])
                   ncurrent-weight (m/lerp current-weight ntarget-weight 0.2)
                   ncurrent-alpha (m/lerp current-alpha target-alpha 0.2)]

               (-> canvas
                   (set-background bgcolor)
                   (set-color fill)
                   (set-stroke ncurrent-weight)
                   (crect nx ny sz sz)
                   (set-color stroke ncurrent-alpha)
                   (crect nx ny sz sz true))

               [nx ny ncurrent-weight ntarget-weight ncurrent-alpha]))]

  (defmethod mouse-event [wname :mouse-pressed] [_ _] true)
  (defmethod mouse-event [wname :mouse-released] [_ _] false)
  
  (show-window {:window-name wname
                :canvas (make-canvas 400 300)
                :draw-fn draw
                :draw-state [100.0 100.0 2.0 2.0 0.0]}))

;; https://www.funprogramming.org/96-Easier-mouse-in-a-rectangle.html

;; Comments:
;;
;; no `CORNERS` mode for rectangle.

(let [wname "Easier mouse in rectangle 96"
      x1 100.0
      y1 50.0
      x2 300.0
      y2 100.0
      rw (- x2 x1)
      rh (- y2 y1)
      from-hsb (c/make-color-converter c/from-HSB 100)
      new-colors #(let [h (r/drand 100)
                        bgcolor (from-hsb (c/make-color h 50 30 100))
                        fgcolor (from-hsb (c/make-color h 80 100 100))]
                    [fgcolor bgcolor])
      inside? (fn [^long x ^long y]
                (bool-and (> x x1) (< x x2) (> y y1) (< y y2)))
      draw (fn [canvas window _ colors]

             (let [pressed? (get-state window)
                   ncolors (if (bool-and pressed? (inside? (mouse-x window) (mouse-y window)))
                             (new-colors)
                             colors)]
               
               (-> canvas
                   (set-background (second ncolors))
                   (set-color (first ncolors))
                   (rect x1 y1 rw rh))

               ncolors))]
  
  (defmethod mouse-event [wname :mouse-pressed] [_ _] true)
  (defmethod mouse-event [wname :mouse-released] [_ _] false)
  
  (show-window {:window-name wname
                :canvas (make-canvas 400 300)
                :draw-fn draw
                :draw-state (new-colors)
                :state false}))

;;
