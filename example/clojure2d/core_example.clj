(ns clojure2d.core-example
  (:require [clojure2d.core :refer :all]
            [metadoc.examples :refer :all]
            [fastmath.random :as r]
            [fastmath.core :as m]
            [fastmath.grid :as grid]
            [fastmath.vector :as v]
            [clojure2d.color :as c]))

(r/set-seed! r/default-rng 42)

(add-examples *jpeg-image-quality*
  (example "Value" *jpeg-image-quality*))

(add-examples file-extension
  (example-session "Usage"
    (file-extension "image.png")
    (file-extension "no_extension")
    (file-extension "with.path/file.doc")))

(add-examples img-writer-formats
  (example "Set of writable image formats" img-writer-formats))

(add-examples img-reader-formats
  (example "Set of readable image formats" img-reader-formats))

(defsnippet clojure2d.core process-image-snippet
  "Process image with function `f` and save."
  (let [img (load-image "docs/cockatoo.jpg")
        unique-name (str "images/core/" (first opts) ".jpg")]
    (binding [*jpeg-image-quality* 0.7]
      (save (f img) (str "docs/" unique-name)))
    (str "../" unique-name)))

(add-examples resize
  (example-snippet "Resize image to 300x40" process-image-snippet :image (fn [img] (resize img 300 40))))

(add-examples subimage
  (example-snippet "Get subimage and resize." process-image-snippet :image (fn [img] (-> img
                                                                                         (subimage 100 100 12 12)
                                                                                         (resize 150 150)))))


(add-examples width
  (example "Width of the image" (width (load-image "docs/cockatoo.jpg"))))

(add-examples height
  (example "Height of the image" (height (load-image "docs/cockatoo.jpg"))))

(add-examples screen-width
  (example "Example value" (screen-width)))

(add-examples screen-height
  (example "Example value" (screen-height)))

(add-examples convolution-matrices
  (example "List of kernels" (keys convolution-matrices)))

(defmacro make-conv-examples
  []
  `(add-examples convolve
     ~@(for [k (keys convolution-matrices)
             :let [n (str "Convolve image with " (name k) " kernel.")]]
         (list 'example-snippet n 'process-image-snippet :image (list 'fn '[img] (list 'convolve 'img k))))))

(make-conv-examples)

(add-examples rendering-hints
  (example "List of possible hints." (keys rendering-hints)))

(add-examples canvas
  (example "Canvas is the record." (canvas 20 30 :low))
  (example-session "Check ImageProto on canvas."
    (width (canvas 10 20))
    (height (canvas 10 20))
    (get-image (canvas 5 6))
    (width (resize (canvas 1 2) 15 15))
    (height (subimage (canvas 10 10) 5 5 2 2))))


(add-examples with-canvas->
  (example-snippet "Draw on canvas" process-image-snippet :image (fn [img]
                                                                   (with-canvas-> (canvas 100 100)
                                                                     (image img 50 50 50 50)))))

(add-examples with-canvas
  (example-snippet "Draw on canvas" process-image-snippet :image (fn [img]
                                                                   (with-canvas [c (canvas 200 200)]
                                                                     (dotimes [i 50]
                                                                       (let [x (r/irand -50 100)
                                                                             y (r/irand -50 100)
                                                                             w (r/irand (- 200 x))
                                                                             h (r/irand (- 200 y))]
                                                                         (image c img x y w h)))
                                                                     c))))

(add-examples scale
  (example-snippet "Scale canvas" process-image-snippet :image (fn [img]
                                                                 (with-canvas-> (canvas 150 150)
                                                                   (scale 0.5)
                                                                   (image img 0 0)))))

(add-examples flip-x
  (example-snippet "Flip around vertical axis" process-image-snippet :image (fn [img]
                                                                              (with-canvas-> (canvas 150 150)
                                                                                (flip-x)
                                                                                (image img -150 0)))))

(add-examples flip-y
  (example-snippet "Flip around horizontal axis" process-image-snippet :image (fn [img]
                                                                                (with-canvas-> (canvas 150 150)
                                                                                  (flip-y)
                                                                                  (image img 0 -150)))))

(add-examples translate
  (example-snippet "Translate canvas" process-image-snippet :image (fn [img]
                                                                     (with-canvas-> (canvas 150 150)
                                                                       (translate 20 20)
                                                                       (image img 0 0)))))

(add-examples rotate
  (example-snippet "Rotate canvas" process-image-snippet :image (fn [img]
                                                                  (with-canvas-> (canvas 150 150)
                                                                    (translate 75 75)
                                                                    (rotate m/QUARTER_PI)
                                                                    (image img -75 -75)))))

(add-examples shear
  (example-snippet "Shear canvas" process-image-snippet :image (fn [img]
                                                                 (with-canvas-> (canvas 150 150)
                                                                   (shear 0.2 0.4)
                                                                   (image img 0 0)))))

(def push-pop-matrix-example
  (example-snippet "Push/pop matrix canvas" process-image-snippet :image
    (fn [img]
      (with-canvas [c (canvas 250 250)]
        (translate c 125 125)
        (doseq [a (range 0 m/TWO_PI 0.3)]
          (let [x (* 80.0 (m/cos a))
                y (* 80.0 (m/sin a))]
            (-> c
                (push-matrix)
                (translate x y)
                (rotate a)
                (image img 0 0 20 20)
                (pop-matrix))))
        c))))

(add-examples push-matrix push-pop-matrix-example)
(add-examples pop-matrix push-pop-matrix-example)

(add-examples transform
  (example "Transform point using current transformation."
    (with-canvas [c (canvas 100 100)]
      (translate c 50 50)
      (rotate c m/HALF_PI)
      (transform c 10 10))))

(add-examples inv-transform
  (example "Inverse transform of point using current transformation."
    (with-canvas [c (canvas 100 100)]
      (translate c 50 50)
      (rotate c m/HALF_PI)
      (inv-transform c 40 60))))

;; orientation

(defsnippet clojure2d.core draw-on-oriented-canvas
  "Draw axes on reoriented canvas"
  (let [canvas (canvas 400 200) 
        unique-name (str "images/core/" (first opts) ".jpg")]
    (with-oriented-canvas-> f canvas
      (set-background 0x30426a)
      (set-color :white)
      (line 5 5 150 5)
      (line 5 5 5 100)
      (line 150 5 145 10)
      (line 5 100 10 95)
      (line 150 5 145 0)
      (line 5 100 0 95)
      (text "X" 150 20)
      (text "Y" 15 100)
      (text "warning: text is reoriented" 20 40))
    (binding [*jpeg-image-quality* 0.85]
      (save canvas (str "docs/" unique-name)))
    (str "../" unique-name)))

(add-examples orient-canvas
  (example-snippet "top-left-, default" draw-on-oriented-canvas :image :top-left-)
  (example-snippet "top-left+" draw-on-oriented-canvas :image :top-left+)
  (example-snippet "top-right-" draw-on-oriented-canvas :image :top-right-)
  (example-snippet "top-right+" draw-on-oriented-canvas :image :top-right+)
  (example-snippet "bottom-left-" draw-on-oriented-canvas :image :bottom-left-)
  (example-snippet "bottom-left+" draw-on-oriented-canvas :image :bottom-left+)
  (example-snippet "bottom-right-" draw-on-oriented-canvas :image :bottom-right-)
  (example-snippet "bottom-right+" draw-on-oriented-canvas :image :bottom-right+))

(add-examples orientations-list
  (example "List of orientations" orientations-list))

;;

(add-examples clip
  (example-snippet "Set clip region." process-image-snippet :image
    (fn [img]
      (with-canvas-> (canvas 150 150)
        (clip 20 20 100 100)
        (image img 0 0)))))

;;

(defsnippet clojure2d.core drawing-snippet
  "Draw something on canvas and save."
  (let [canvas (canvas 200 200) 
        unique-name (str "images/core/" (first opts) ".jpg")]
    (with-canvas-> canvas
      (set-background 0x30426a)
      (set-color :white)
      (f))
    (binding [*jpeg-image-quality* 0.85]
      (save canvas (str "docs/" unique-name)))
    (str "../" unique-name)))

(add-examples with-oriented-canvas->
  (example-snippet "Orient and draw." drawing-snippet :image
    (fn [canvas]
      (with-oriented-canvas-> :bottom-left+ canvas
        (line 0 0 100 50)
        (line 100 0 200 50)))))

(add-examples with-oriented-canvas
  (example-snippet "Orient and draw." drawing-snippet :image
    (fn [canvas]
      (with-oriented-canvas :bottom-left+ [c canvas]
        (doseq [p (range 0 100 4)]
          (set-color c (* p 2) 200 200)
          (line c p 0 (+ p 100) 50))))))

(add-examples line
  (example-snippet "Draw some lines" drawing-snippet :image
    (fn [canvas] 
      (doseq [^long x (range 10 190 10)]
        (line canvas x 10 (- 190 x) 190)))))


(add-examples stroke-joins
  (example "List of stroke join types" (keys stroke-joins)))

(add-examples stroke-caps
  (example "List of stroke cap types" (keys stroke-caps)))

(add-examples set-stroke
  (example-snippet "Various stroke settings." drawing-snippet :image
    (fn [canvas]
      (-> canvas
          (set-stroke 10 :round)
          (line 25 20 25 180)
          (set-stroke 10 :butt)
          (line 55 20 55 180)
          (set-stroke 10 :square)
          (line 85 20 85 180)
          (set-stroke 10 :round :bevel)
          (rect 120 20 60 40 true)
          (set-stroke 10 :round :miter)
          (rect 120 80 60 40 true)
          (set-stroke 10 :round :round)
          (rect 120 140 60 40 true))))
  (example-snippet "Miter limit" drawing-snippet :image
    (fn [canvas]
      (-> canvas
          (set-stroke 10 :square :miter)
          (triangle 70 50 170 50 170 70 true)
          (set-stroke 10 :square :miter 5.0)
          (triangle 70 100 170 100 170 120 true)
          (set-stroke 10 :square :miter 25.0)
          (triangle 70 150 170 150 170 170 true)))))


(add-examples set-stroke-custom
  (example-snippet "Custom strokes" drawing-snippet :image
    (fn [canvas]
      (-> canvas
          (set-stroke-custom {:size 2.0 :dash [4.0] :dash-phase 2.0})
          (line 20 20 180 20)
          (set-stroke-custom {:size 2.0 :dash [20.0] :dash-phase 10})
          (line 20 50 180 50)
          (set-stroke-custom {:size 2.0 :dash [10.0 2.0 2.0 2.0]})
          (line 20 80 180 80)
          (set-stroke-custom {:size 1.0 :dash [4.0] :dash-phase 2.0})
          (rect 20 110 160 10 true)
          (set-stroke-custom {:size 1.0 :dash [10.0 5.0] :join :miter})
          (rect 20 140 160 10 :true)
          (set-stroke-custom {:size 1.0 :dash [10.0 2.0 2.0 2.0]})
          (rect 20 170 160 10 :true)))))

(add-examples point
  (example-snippet "Sequence of points." drawing-snippet :image
    (fn [canvas]
      (doseq [^long x (range 10 190 10)]
        (set-stroke canvas (/ x 20))
        (point canvas x x))))
  (example-snippet "Magnified point can look differently when different stroke settings are used."
    drawing-snippet :image (fn [canvas]
                             (-> canvas
                                 (scale 80.0)
                                 (set-stroke 0.5)
                                 (point 0.5 0.5)
                                 (set-stroke 0.5 :square)
                                 (point 1.5 1.5)))))

(add-examples rect
  (example-snippet "Two squares, one filled and second as outline." drawing-snippet :image
    (fn [canvas]
      (-> canvas
          (rect 30 30 50 50) 
          (rect 80 80 90 90 true)))))

(add-examples crect
  (example-snippet "Two squares, regular and centered." drawing-snippet :image
    (fn [canvas]
      (-> canvas
          (set-color :white 160)
          (rect 50 50 100 100) 
          (crect 50 50 60 60)))))

(add-examples ellipse
  (example-snippet "A couple of ellipses." drawing-snippet :image
    (fn [canvas]
      (-> canvas
          (set-color :white 200)
          (ellipse 100 100 50 150)
          (ellipse 100 100 150 50 true)
          (ellipse 100 100 20 20)
          (set-color :black 200)
          (ellipse 100 100 20 20 true)))))

(add-examples arc
  (example-snippet "Arcs" drawing-snippet :image
    (fn [canvas]
      (-> canvas
          (set-color :white)
          (arc 60 60 90 90 m/PI m/HALF_PI)
          (arc 70 70 90 90 m/PI m/HALF_PI :chord)
          (arc 90 90 90 90 m/PI m/HALF_PI :pie)
          (set-color :gray)
          (arc 130 130 90 90 m/PI m/HALF_PI :open false)
          (arc 150 150 90 90 m/PI m/HALF_PI :chord false)
          (arc 170 170 90 90 m/PI m/HALF_PI :pie false)))))

(add-examples rarc
  (example-snippet "Arcs by radius" drawing-snippet :image
    (fn [canvas]
      (-> canvas
          (set-color :white)
          (rarc 60 60 45 3.5 m/HALF_PI)
          (rarc 70 70 45 3.5 m/HALF_PI :chord)
          (rarc 90 90 45 3.5 m/HALF_PI :pie)
          (set-color :gray)
          (rarc 130 130 45 m/PI 2 :open false)
          (rarc 150 150 45 m/PI 2 :chord false)
          (rarc 170 170 45 m/PI 2 :pie false)))))

(add-examples triangle
  (example-snippet "Two triangles" drawing-snippet :image
    (fn [canvas]
      (-> canvas
          (triangle 30 30 170 100 30 170)
          (set-color :black)
          (triangle 170 30 170 170 30 100 true)))))

(add-examples triangle-strip
  (example-snippet "Triangle strip" drawing-snippet :image
    (fn [canvas]
      (set-color canvas :white 190)
      (translate canvas 100 100)
      (let [s (for [a (range 0 65 3.0)]
                (v/vec2 (* 90.0 (m/cos a))
                        (* 90.0 (m/sin a))))]
        (triangle-strip canvas s true)))))

(add-examples triangle-fan
  (example-snippet "Triangle fan" drawing-snippet :image
    (fn [canvas]
      (set-color canvas :white 190)
      (translate canvas 100 100)
      (let [s (for [a (range 0 65 3.0)]
                [(* 90.0 (m/cos a))
                 (* 90.0 (m/sin a))])]
        (triangle-fan canvas s true)))))

(add-examples path
  (example-snippet "Path" drawing-snippet :image
    (fn [canvas]
      (set-color canvas :white 190)
      (translate canvas 100 100)
      (let [s (for [^double a (range 0 65 1.3)]
                (v/vec2 (* (+ a 25) (m/cos a))
                        (* (+ a 25) (m/sin a))))]
        (path canvas s))) ))

(add-examples path-bezier
  (example-snippet "Bezier path" drawing-snippet :image
    (fn [canvas]
      (set-color canvas :white 190)
      (translate canvas 100 100)
      (let [s (for [^double a (range 0 65 1.3)]
                [(* (+ a 25) (m/cos a))
                 (* (+ a 25) (m/sin a))])]
        (path-bezier canvas s)))))

(add-examples bezier
  (example-snippet "Bezier curve" drawing-snippet :image
    (fn [canvas]
      (bezier canvas 20 20 180 20 180 180 20 180 false)
      (set-color canvas :black)
      (bezier canvas 20 180 20 20 180 20 180 180))))

(add-examples curve
  (example-snippet "Quadratic curve" drawing-snippet :image
    (fn [canvas]
      (curve canvas 20 20 180 20 180 180 false)
      (set-color canvas :black)
      (curve canvas 20 180 20 20 180 20))))

(add-examples quad
  (example-snippet "Quad" drawing-snippet :image
    (fn [canvas]
      (quad canvas 20 20 180 50 50 180 70 70))))

(add-examples pointy-hex
  (example-snippet "Pointy topped hexagon" drawing-snippet :image
    (fn [canvas]
      (pointy-hex canvas 100 100 20)
      (pointy-hex canvas 100 100 90 true))))

(add-examples flat-hex
  (example-snippet "Flat topped hexagon" drawing-snippet :image
    (fn [canvas]
      (flat-hex canvas 100 100 20)
      (flat-hex canvas 100 100 90 true))))

(add-examples fonts-list
  (example "First five availabe fonts" (take 5 fonts-list)))

(add-examples set-font
  (example-snippet "Various fonts" drawing-snippet :image
    (fn [canvas]
      (-> canvas
          (set-font "Courier New")
          (text "Trying to set Courier New" 100 50 :center)
          (set-font "Arial")
          (text "Trying to set Arial" 100 100 :center)
          (set-font "Verdana")
          (text "Trying to set Verdana" 100 150 :center)))))

(add-examples set-font-attributes
  (example-snippet "Font attributes" drawing-snippet :image
    (fn [canvas]
      (-> canvas
          (set-font-attributes 30)
          (text "Size 30" 100 50 :center)
          (set-font-attributes 15 :italic)
          (text "Size 15, italic" 100 100 :center)
          (set-font-attributes 20 :bold-italic)
          (text "Size 20, bold, italic" 100 150 :center)))))

(add-examples char-width
  (example "Width of some chars."
    (with-canvas [c (canvas 10 10)] [(char-width c \W)
                                     (char-width c \a)])))

(add-examples font-height
  (example "Height of current font."
    (with-canvas-> (canvas 10 10)
      (font-height))))

(add-examples font-ascent
  (example "Ascent of current font."
    (with-canvas-> (canvas 10 10)
      (font-ascent))))

(add-examples text-width
              (example "Size of some string."
                       (with-canvas-> (canvas 10 10)
                         (text-width "Size of some string."))))

(add-examples text-bounding-box
              (example "Size of some string."
                       (with-canvas-> (canvas 10 10)
                         (text-bounding-box "Size of some string."))))


(add-examples text
              (example-snippet "Font attributes" drawing-snippet :image
                               (fn [canvas]
                                 (-> canvas
                                     (text "Align left" 100 50 :left)
                                     (text "Align center" 100 100 :center)
                                     (text "Align right" 100 150 :right)))))

(add-examples set-awt-color
  (example-snippet "Set color with `java.awt.Color`." drawing-snippet :image
    (fn [canvas]
      (-> canvas
          (set-awt-color java.awt.Color/RED)
          (rect 50 50 100 100)))))

(add-examples set-awt-background
  (example-snippet "Set background with `java.awt.Color`." drawing-snippet :image
    (fn [canvas]
      (-> canvas
          (set-awt-background java.awt.Color/BLUE)))))

(add-examples awt-xor-mode
  (example-snippet "Set Xor Mode with `java.awt.Color`." drawing-snippet :image
    (fn [canvas]
      (-> canvas
          (awt-xor-mode java.awt.Color/BLACK)
          (rect 50 50 100 100)
          (rect 70 70 60 60)))))

(add-examples set-color
  (example-snippet "Set color various ways." drawing-snippet :image
    (fn [canvas]
      (-> canvas
          (set-color 0xaabbcc)
          (rect 10 10 40 40)
          (set-color :maroon)
          (rect 60 60 40 40)
          (set-color java.awt.Color/GREEN)
          (rect 110 110 40 40)
          (set-color 0 111 200 100)
          (rect 20 20 160 160)
          (set-color (v/vec3 0 100 255))
          (rect 160 160 25 25)))))

(add-examples set-background
  (example-snippet "Set background with alpha set." drawing-snippet :image
    (fn [canvas]
      (-> canvas
          (set-background :maroon 200)))))

(add-examples xor-mode
  (example-snippet "Set XOR Painting mode" drawing-snippet :image
    (fn [canvas]
      (-> canvas
          (xor-mode :gray)
          (rect 50 50 100 100)
          (rect 70 70 60 60)))))

(add-examples filled-with-stroke
  (example-snippet "Draw two primitives" drawing-snippet :image
    (fn [canvas]
      (-> canvas
          (filled-with-stroke :maroon :black crect 100 100 180 180)
          (filled-with-stroke 0x3344ff :white ellipse 100 100 20 100)))))

(add-examples grid-cell
  (example-snippet "Draw on square grid" drawing-snippet :image
    (fn [canvas]
      (let [g (grid/grid :square 15)]
        (dotimes [_ 100]
          (let [x (r/drand 200)
                y (r/drand 200)]
            (filled-with-stroke canvas (c/color :white 100) :maroon grid-cell g x y))))))
  (example-snippet "Draw on shifted-square grid" drawing-snippet :image
    (fn [canvas]
      (let [g (grid/grid :shifted-square 15)]
        (dotimes [_ 100]
          (let [x (r/drand 200)
                y (r/drand 200)]
            (filled-with-stroke canvas (c/color :white 100) :maroon grid-cell g x y))))))
  (example-snippet "Draw on rhomboidal grid" drawing-snippet :image
    (fn [canvas]
      (let [g (grid/grid :rhombus 15)]
        (dotimes [_ 100]
          (let [x (r/drand 200)
                y (r/drand 200)]
            (filled-with-stroke canvas (c/color :white 100) :maroon grid-cell g x y))))))
  (example-snippet "Draw on triangular grid" drawing-snippet :image
    (fn [canvas]
      (let [g (grid/grid :triangle 15)]
        (dotimes [_ 100]
          (let [x (r/drand 200)
                y (r/drand 200)]
            (filled-with-stroke canvas (c/color :white 100) :maroon grid-cell g x y))))))
  (example-snippet "Draw on flat topped hexagonal grid" drawing-snippet :image
    (fn [canvas]
      (let [g (grid/grid :flat-hex 15)]
        (dotimes [_ 100]
          (let [x (r/drand 200)
                y (r/drand 200)]
            (filled-with-stroke canvas (c/color :white 100) :maroon grid-cell g x y))))))
  (example-snippet "Draw on pointy topped hexagonal grid" drawing-snippet :image
    (fn [canvas]
      (let [g (grid/grid :pointy-hex 15)]
        (dotimes [_ 100]
          (let [x (r/drand 200)
                y (r/drand 200)]
            (filled-with-stroke canvas (c/color :white 100) :maroon grid-cell g x y))))))
  (example-snippet "Draw on hexagonal grid with random size" drawing-snippet :image
    (fn [canvas]
      (let [g (grid/grid :flat-hex 15)]
        (dotimes [_ 100]
          (let [x (r/drand 200)
                y (r/drand 200)
                s (r/drand 0.1 1.0)]
            (filled-with-stroke canvas (c/color :white 100) :maroon grid-cell g x y s)))))))

(add-examples gradient-mode
  (example-snippet "Set some gradient and fill" drawing-snippet :image
    (fn [canvas]
      (-> canvas
          (gradient-mode 20 20 :maroon 180 180 :black)
          (ellipse 100 100 190 190)))))

(add-examples image
  (example-snippet "Draw image at given position." drawing-snippet :image
    (fn [canvas]
      (let [img (load-image "docs/cockatoo.jpg")]
        (doseq [^int x (range 0 80 10)]
          (image canvas img x x (- 200 x x) (- 200 x x)))))))

(add-examples load-svg
  (example "Load SVG into Batik object" (load-svg "docs/takkun.svg")))

(add-examples transcode-svg
  (example-snippet "Draw SVG onto canvas." drawing-snippet :image
    (fn [canvas]
      (let [svg (load-svg "docs/takkun.svg")]
        (-> canvas
            (image (transcode-svg svg 200 200))
            (image (transcode-svg svg 30 30) 170 170))))))

;;

(add-examples window-active?
  (example "Check if window is visible."
    (let [w (show-window)
          before-closing (window-active? w)]
      (close-window w)
      {:before-closing before-closing
       :after-closing (window-active? w)})))

(add-examples key-event-map
  (example "List of key events" (vals key-event-map)))

(add-examples mouse-event-map
  (example "List of mouse events" (vals mouse-event-map)))

(add-examples to-hex
  (example-session "Usage" (to-hex 123) (to-hex 123 8)))

;;

(add-examples year (example "Current value" (year)))
(add-examples month (example "Current value" (month)))
(add-examples day (example "Current value" (day)))
(add-examples hour (example "Current value" (hour)))
(add-examples minute (example "Current value" (minute)))
(add-examples sec (example "Current value" (sec)))
(add-examples millis (example "Current value" (millis)))
(add-examples nanos (example "Current value" (nanos)))
(add-examples datetime (example-session "Current value" (datetime) (datetime :vector)))

(add-examples load-bytes (example-session "Load image file to byte array"
                           (load-bytes "docs/cockatoo.jpg")
                           (second (load-bytes "docs/cockatoo.jpg"))))


(add-examples make-counter (example-session "Usage"
                             (let [cnt (make-counter)]
                               (repeatedly 5 cnt))
                             (let [cnt (make-counter 100)]
                               (repeatedly 5 cnt))))

(add-examples make-session (example-session "Usage"
                             (make-session)
                             (Thread/sleep 2000)
                             (make-session)))

(add-examples close-session (example-session "Usage"
                              (session-name)
                              (close-session)
                              (session-name)))

(add-examples session-name (example-session "Currenct session value"
                             (session-name)))

(add-examples next-filename (example-session "Usage"
                              (next-filename "folder/name/")
                              (next-filename "folder/name/")
                              (next-filename "folder/name/" ".jpg")
                              (close-session)
                              (next-filename "folder/name/")
                              (next-filename "folder/name/")
                              (next-filename "folder/name/" ".jpg")))

(add-examples log-name (example "Usage" (log-name)))

(add-examples log (example-session "Example usage"
                    (session-name)
                    (binding [*log-to-file* true]
                      (log "Log information to file under session"))
                    (Thread/sleep 1000)
                    (slurp (log-name))))

;;

