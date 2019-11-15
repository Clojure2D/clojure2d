(ns clojure2d.pixels-example
  (:require [metadoc.examples :refer :all]
            [clojure2d.color :as c]
            [clojure2d.core :as core]
            [clojure2d.pixels :refer :all]
            [fastmath.core :as m]
            [fastmath.random :as r]))

(r/set-seed! r/default-rng 42)

(defsnippet clojure2d.pixels saver "Save pixels to image."
  (let [n (str "images/pixels/" (first opts) ".jpg")]
    (core/save (f) (str "docs/" n))
    (str "../" n)))

(def cockatoo (load-pixels "docs/cockatoo.jpg"))

(add-examples *pixels-edge*
  (example-session "Access pixels outside image range."
    (binding [*pixels-edge* :zero] (get-color cockatoo -1 10))
    (get-color cockatoo 0 10)
    (binding [*pixels-edge* :edge] (get-color cockatoo -1 10))
    (get-color cockatoo (- (core/width cockatoo) 100) 10)
    (binding [*pixels-edge* :wrap] (get-color cockatoo -100 10))
    (binding [*pixels-edge* 123] (get-color cockatoo -1 10)))
  (example-session "Get values from second channel from outside image range."
    (binding [*pixels-edge* :zero] (get-value cockatoo 1 -1 10))
    (get-value cockatoo 1 0 10)
    (binding [*pixels-edge* :edge] (get-value cockatoo 1 -1 10))
    (get-value cockatoo 1 (- (core/width cockatoo) 100) 10)
    (binding [*pixels-edge* :wrap] (get-value cockatoo 1 -100 10))
    (binding [*pixels-edge* 123] (get-value cockatoo 1 -1 10))))

(add-examples get-value
  (example-session "Get channel value from given position."
    (get-value cockatoo 1 20 20)
    (get-value cockatoo 3 20 20)
    (get-value cockatoo 0 -10 -10)
    (get-value cockatoo 1 20)
    (get-value cockatoo 3 20)
    (get-value cockatoo 0 10000)))

(def ld-get-set (example "Low density pixels color access."
                  (let [ldp (renderer 100 100)
                        v0 (get-color ldp 50 50) 
                        v1 (get-color (set-color! ldp 50 50 :maroon) 50 50)]
                    {:before v0
                     :after v1})))


(add-examples get-color
  (example-session "Get channel value from given position."
    (get-color cockatoo 20 20)
    (get-color cockatoo -10 -10)
    (get-color cockatoo 20)
    (get-color cockatoo 10000)
    (get-color (core/canvas 100 100) 50 50)
    (get-color (core/load-image "docs/cockatoo.jpg") 50 50))
  ld-get-set)

(add-examples set-color!
  (example "Set color in pixels"
    (let [p (pixels 100 100)
          v0 (get-color p 50 50) 
          v1 (get-color (set-color! p 50 50 :maroon) 50 50)]
      {:before v0
       :after v1}))
  ld-get-set)

(add-examples set-value!
  (example "Set value in pixels"
    (let [p (pixels 100 100)
          v0 (get-value p 1 50 50) 
          v1 (get-value (set-value! p 50 50 111) 50 50)]
      {:before v0
       :after v1})))

(add-examples to-pixels
  (example-session "Convert various types to pixels"
    (to-pixels (pixels 100 100))
    (to-pixels (core/load-image "docs/cockatoo.jpg"))
    (to-pixels (core/canvas 50 50))
    (to-pixels (renderer 10 10))))

(add-examples get-channel
  (example "Extract one channel from Pixels."
    (first (get-channel cockatoo 1))))

(add-examples set-channel!
  (example "Copy channels"
    (get-color (set-channel! (clone-pixels cockatoo) 0 (get-channel cockatoo 1)) 20 20)))

(add-examples pixels
  (example "Create pixels" (pixels 100 100))
  (example "Create pixels with given array" (pixels (int-array (* 10 10 4)) 10 10)))

(add-examples clone-pixels
  (example "Usage" (let [p1 (pixels 100 100)]
                     (set-color! p1 10 10 :maroon)
                     (get-color (clone-pixels p1) 10 10))))

(add-examples set-image-pixels!
  (example-snippet "Usage" saver :image (fn [] (let [i (core/load-image "docs/cockatoo.jpg")
                                                     p2 (pixels 40 40)]
                                                 (to-pixels (set-image-pixels! i 25 15 p2))))))

(add-examples set-canvas-pixels!
  (example-snippet "Usage" saver :image (fn [] (let [i (core/canvas 100 100)
                                                     p2 (pixels 40 40)]
                                                 (core/with-canvas [c i]
                                                   (core/set-background c :maroon)
                                                   (to-pixels (set-canvas-pixels! c 25 15 p2)))))))

(add-examples load-pixels
  (example "Load file" (load-pixels "docs/cockatoo.jpg"))
  (example-snippet "Usage" saver :image (fn [] (load-pixels "docs/cockatoo.jpg"))))

(add-examples filter-colors
  (example-snippet "Convert to other colorspace." saver :image (fn [] (filter-colors c/to-LUV* cockatoo))))

(add-examples filter-colors-xy
  (example-snippet "Shift image up." saver :image (fn [] (let [filter (fn [p ^long x ^long y]
                                                                       (get-color p x (+ y 75)))] 
                                                          (binding [*pixels-edge* :wrap] (filter-colors-xy filter cockatoo))))))

(add-examples filter-channel
  (example-snippet "Operate on blue channel" saver :image (fn [] (let [target (pixels 150 150)
                                                                      filter (fn [^long v] (- 255 (/ v 2)))]
                                                                  (filter-channel filter 2 target cockatoo)
                                                                  target))))

(add-examples filter-channel-xy
  (example-snippet "Create filter and process channel."
    saver :image (fn [] (let [target (pixels 150 150)
                             filter (fn [ch p ^long x ^long y] (let [v1 (get-value p ch (dec x) y)
                                                                    v2 (get-value p ch (inc x) y)
                                                                    v3 (get-value p ch x (dec y))
                                                                    v4 (get-value p ch x (inc y))
                                                                    avg (/ (+ v1 v2 v3 v4) 4.0)]
                                                                (if (< avg 128) 0 255)))]
                         (filter-channel-xy filter 0 target cockatoo)
                         (let [c0 (get-channel target 0)]
                           (set-channel! target 1 c0)
                           (set-channel! target 2 c0))
                         target)))
  (example-snippet "Create filter and process channel. Using [[filter-channels]]."
    saver :image (fn [] (let [filter (filter-channel-xy (fn [ch p ^long x ^long y] (let [v1 (get-value p ch (dec x) y)
                                                                                       v2 (get-value p ch (inc x) y)
                                                                                       v3 (get-value p ch x (dec y))
                                                                                       v4 (get-value p ch x (inc y))
                                                                                       avg (/ (+ v1 v2 v3 v4) 4.0)]
                                                                                   (if (< avg 128) 0 255))))
                             target (filter-channels filter nil nil nil cockatoo)]
                         (let [c0 (get-channel target 0)]
                           (set-channel! target 1 c0)
                           (set-channel! target 2 c0))
                         target))))

(add-examples filter-channels
  (example-snippet "Apply equalize filter for all channels" saver :image
    (fn [] (filter-channels equalize cockatoo)))
  (example-snippet "Apply 2 filters separately for 2 channels" saver :image
    (fn [] (filter-channels negate (box-blur 10) nil nil cockatoo)))
  (example-snippet "Create gamma filter and use it to process channels" saver :image
    (fn [] (let [gamma-fn (fn [^double amt ^double v] (let [vv (/ v 255.0)
                                                          p (m/pow vv (/ amt))]
                                                      (* p 255.0)))
                my-gamma (partial gamma-fn 0.2)]
            (filter-channels (partial filter-channel my-gamma) cockatoo)))))

(add-examples blend-channel
  (example-snippet "Create own blending function and apply to second channel of image and its blurred version."
    saver :image (fn [] (let [target (clone-pixels cockatoo)
                             wrong-avg (fn [^double v1 ^double v2] (/ (+ v1 v2) 4.0))]
                         (blend-channel wrong-avg 1 target cockatoo (filter-channels box-blur-5 cockatoo))
                         target))))

(add-examples blend-channel-xy
  (example-snippet "Create flip blender and apply to second channel."
    saver :image (fn [] (let [target (clone-pixels cockatoo)
                              flip-blend (fn [ch p1 p2 x y] (let [v1 (get-value p1 ch y x)
                                                                  v2 (get-value p2 ch x y)]
                                                              (/ (+ v1 v2) 2.0)))]
                          (blend-channel-xy flip-blend 1 target cockatoo cockatoo)
                          target))))

(add-examples blend-channels
  (example-snippet "Create flip blender and apply to image."
    saver :image (fn [] (let [flip-blend (fn [ch p1 p2 x y] (let [v1 (get-value p1 ch y x)
                                                                  v2 (get-value p2 ch x y)]
                                                              (/ (+ v1 v2) 2.0)))]
                          (blend-channels (partial blend-channel-xy flip-blend) cockatoo cockatoo)))))

(add-examples compose-channels
  (example-snippet "Add two versions of the image" saver :image
    (fn [] (compose-channels :add cockatoo (filter-channels solarize cockatoo))))
  (example-snippet "XOR two versions of the image" saver :image
    (fn [] (compose-channels :xor cockatoo (filter-channels solarize cockatoo))))
  (example-snippet "Use different blending method for every channel." saver :image
    (fn [] (compose-channels :divide :multiply :mburn nil cockatoo (filter-channels solarize cockatoo))))
  (example-snippet "Use custom function" saver :image
    (fn [] (compose-channels (fn [^double v1 ^double v2] (* v1 (- 1.0 v2))) cockatoo (filter-channels solarize cockatoo)))))

(add-examples dilate
  (example-snippet "Apply filter 5 times." saver :image
    (fn [] (reduce (fn [buff _] (filter-channels dilate buff)) cockatoo (range 5)))))

(add-examples quantile-1
  (example-snippet "Apply filter 5 times." saver :image
    (fn [] (reduce (fn [buff _] (filter-channels quantile-1 buff)) cockatoo (range 5)))))

(add-examples quantile-2
  (example-snippet "Apply filter 5 times." saver :image
    (fn [] (reduce (fn [buff _] (filter-channels quantile-2 buff)) cockatoo (range 5)))))

(add-examples quantile-3
  (example-snippet "Apply filter 5 times." saver :image
    (fn [] (reduce (fn [buff _] (filter-channels quantile-3 buff)) cockatoo (range 5)))))

(add-examples median
  (example-snippet "Apply filter 5 times." saver :image
    (fn [] (reduce (fn [buff _] (filter-channels median buff)) cockatoo (range 5)))))

(add-examples quantile-5
  (example-snippet "Apply filter 5 times." saver :image
    (fn [] (reduce (fn [buff _] (filter-channels quantile-5 buff)) cockatoo (range 5)))))

(add-examples quantile-6
  (example-snippet "Apply filter 5 times." saver :image
    (fn [] (reduce (fn [buff _] (filter-channels quantile-6 buff)) cockatoo (range 5)))))

(add-examples quantile-7
  (example-snippet "Apply filter 5 times." saver :image
    (fn [] (reduce (fn [buff _] (filter-channels quantile-7 buff)) cockatoo (range 5)))))

(add-examples erode
  (example-snippet "Apply filter 5 times." saver :image
    (fn [] (reduce (fn [buff _] (filter-channels erode buff)) cockatoo (range 5)))))

;;

(add-examples dilate-cross
  (example-snippet "Apply filter 5 times." saver :image
    (fn [] (reduce (fn [buff _] (filter-channels dilate-cross buff)) cockatoo (range 5)))))

(add-examples erode-cross
  (example-snippet "Apply filter 5 times." saver :image
    (fn [] (reduce (fn [buff _] (filter-channels erode-cross buff)) cockatoo (range 5)))))

;;

(add-examples horizontal-blur
  (example-snippet "Usage" saver :image
    (fn [] (filter-channels (horizontal-blur 20) cockatoo))))

(add-examples horizontal-blur-1 (example-snippet "Usage" saver :image (fn [] (filter-channels horizontal-blur-1 cockatoo))))
(add-examples horizontal-blur-2 (example-snippet "Usage" saver :image (fn [] (filter-channels horizontal-blur-2 cockatoo))))
(add-examples horizontal-blur-3 (example-snippet "Usage" saver :image (fn [] (filter-channels horizontal-blur-3 cockatoo))))
(add-examples horizontal-blur-5 (example-snippet "Usage" saver :image (fn [] (filter-channels horizontal-blur-5 cockatoo))))

(add-examples vertical-blur
  (example-snippet "Usage" saver :image
    (fn [] (filter-channels (vertical-blur 20) cockatoo))))

(add-examples vertical-blur-1 (example-snippet "Usage" saver :image (fn [] (filter-channels vertical-blur-1 cockatoo))))
(add-examples vertical-blur-2 (example-snippet "Usage" saver :image (fn [] (filter-channels vertical-blur-2 cockatoo))))
(add-examples vertical-blur-3 (example-snippet "Usage" saver :image (fn [] (filter-channels vertical-blur-3 cockatoo))))
(add-examples vertical-blur-5 (example-snippet "Usage" saver :image (fn [] (filter-channels vertical-blur-5 cockatoo))))

;;

(add-examples box-blur
  (example-snippet "Usage" saver :image
    (fn [] (filter-channels (box-blur 20) cockatoo))))

(add-examples box-blur-1 (example-snippet "Usage" saver :image (fn [] (filter-channels box-blur-1 cockatoo))))
(add-examples box-blur-2 (example-snippet "Usage" saver :image (fn [] (filter-channels box-blur-2 cockatoo))))
(add-examples box-blur-3 (example-snippet "Usage" saver :image (fn [] (filter-channels box-blur-3 cockatoo))))
(add-examples box-blur-5 (example-snippet "Usage" saver :image (fn [] (filter-channels box-blur-5 cockatoo))))

;;

(add-examples gaussian-blur
  (example-snippet "Usage" saver :image
    (fn [] (filter-channels (gaussian-blur 20) cockatoo))))

(add-examples gaussian-blur-1 (example-snippet "Usage" saver :image (fn [] (filter-channels gaussian-blur-1 cockatoo))))
(add-examples gaussian-blur-2 (example-snippet "Usage" saver :image (fn [] (filter-channels gaussian-blur-2 cockatoo))))
(add-examples gaussian-blur-3 (example-snippet "Usage" saver :image (fn [] (filter-channels gaussian-blur-3 cockatoo))))
(add-examples gaussian-blur-5 (example-snippet "Usage" saver :image (fn [] (filter-channels gaussian-blur-5 cockatoo))))

;;

(add-examples posterize
  (example-snippet "Usage" saver :image
    (fn [] (filter-channels (posterize 2) cockatoo))))

(add-examples posterize-4 (example-snippet "Usage" saver :image (fn [] (filter-channels posterize-4 cockatoo))))
(add-examples posterize-8 (example-snippet "Usage" saver :image (fn [] (filter-channels posterize-8 cockatoo))))
(add-examples posterize-16 (example-snippet "Usage" saver :image (fn [] (filter-channels posterize-16 cockatoo))))

;;

(add-examples threshold
  (example-snippet "Usage" saver :image
    (fn [] (filter-channels (threshold 0.9) cockatoo)))
  (example-snippet "Usage (two parameters)" saver :image
    (fn [] (filter-channels (threshold 0.6 0.9) cockatoo)))
  (example-snippet "Usage on b&w" saver :image
    (fn [] (filter-channels (threshold 0.4) (filter-colors c/to-Gray* cockatoo)))))

(add-examples threshold-25 (example-snippet "Usage" saver :image (fn [] (filter-channels threshold-25 cockatoo))))
(add-examples threshold-50 (example-snippet "Usage" saver :image (fn [] (filter-channels threshold-50 cockatoo))))
(add-examples threshold-75 (example-snippet "Usage" saver :image (fn [] (filter-channels threshold-75 cockatoo))))

(add-examples normalize
  (example-snippet "Usage" saver :image
    (fn [] (filter-channels normalize cockatoo))))

(add-examples equalize
  (example-snippet "Usage" saver :image
    (fn [] (filter-channels equalize cockatoo))))

(add-examples solarize
  (example-snippet "Usage" saver :image
    (fn [] (filter-channels solarize cockatoo))))

(add-examples tint
  (example-snippet "Usage" saver :image
    (fn [] (filter-channels (tint :hotpink) cockatoo)))
  (example-snippet "Usage" saver :image
    (fn [] (filter-channels (tint :indigo :lightblue) cockatoo)))
  (example-snippet "Usage" saver :image
    (fn [] (filter-channels (tint :gray :lightblue :maroon) cockatoo))))

(add-examples modulate
  (example-snippet "Saturate" saver :image
    (fn [] (->> cockatoo
                (filter-colors c/to-HSB*)
                (filter-channels nil (modulate 2.0) nil nil)
                (filter-colors c/from-HSB*))))
  (example-snippet "Desaturate" saver :image
    (fn [] (->> cockatoo
                (filter-colors c/to-HCL*)
                (filter-channels nil (modulate 0.2) nil nil)
                (filter-colors c/from-HCL*))))
  (example-snippet "Shift hue" saver :image
    (fn [] (->> cockatoo
                (filter-colors c/to-HCL*)
                (filter-channels (modulate 0.2) nil nil nil)
                (filter-colors c/from-HCL*)))))

(add-examples brightness-contrast
  (example-snippet "Ligher" saver :image
    (fn [] (filter-channels (brightness-contrast 1.5) cockatoo)))
  (example-snippet "Darker" saver :image
    (fn [] (filter-channels (brightness-contrast 0.5) cockatoo)))
  (example-snippet "More contrast" saver :image
    (fn [] (filter-channels (brightness-contrast 1.0 1.5) cockatoo)))
  (example-snippet "Less contrast" saver :image
    (fn [] (filter-channels (brightness-contrast 1.0 0.5) cockatoo))))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def local-noise (r/fbm-noise {:seed 42}))

(add-examples renderer
  (example-snippet "Usage" saver :image
    #(let [r (renderer 300 300)]
       (dotimes [i 20000000]
         (let [x (+ 150 (r/grand 30))
               y (+ 150 (r/grand 30))
               x (+ x (* 20.0 (- ^double (local-noise (/ x 50.0) (/ y 50.0) 0.34) 0.5)))
               y (+ y (* 20.0 (- ^double (local-noise (/ y 50.0) (/ x 50.0) 2.23) 0.5)))]
           (add-pixel! r x y :white)))
       (to-pixels r {:gamma-alpha 0.6 :gamma-color 0.8})))
  (example-snippet "Compare to native Java2d rendering. You can observe oversaturation." saver :image
    #(let [r (core/black-canvas 300 300)]
       (core/with-canvas [c r]
         (core/set-color c :white 5)
         (dotimes [i 2000000]
           (let [x (+ 150 (r/grand 30))
                 y (+ 150 (r/grand 30))
                 x (+ x (* 20.0 (- ^double (local-noise (/ x 50.0) (/ y 50.0) 0.34) 0.5)))
                 y (+ y (* 20.0 (- ^double (local-noise (/ y 50.0) (/ x 50.0) 2.23) 0.5)))]
             (core/point c x y))))
       (to-pixels r))))

(add-examples gradient-renderer
  (example-snippet "Usage" saver :image
    #(let [r (gradient-renderer 300 300)]
       (dotimes [i 4000000]
         (let [x (+ 150 (r/grand 30))
               y (+ 150 (r/grand 30))
               x (+ x (* 20.0 (- ^double (local-noise (/ x 50.0) (/ y 50.0) 0.34) 0.5)))
               y (+ y (* 20.0 (- ^double (local-noise (/ y 50.0) (/ x 50.0) 2.23) 0.5)))]
           (add-pixel! r x y)))
       (to-pixels r)))
  (example-snippet "Render with different gradient" saver :image
    #(let [r (gradient-renderer 300 300)]
       (dotimes [i 4000000]
         (let [x (+ 150 (r/grand 30))
               y (+ 150 (r/grand 30))
               x (+ x (* 20.0 (- ^double (local-noise (/ x 50.0) (/ y 50.0) 0.34) 0.5)))
               y (+ y (* 20.0 (- ^double (local-noise (/ y 50.0) (/ x 50.0) 2.23) 0.5)))]
           (add-pixel! r x y)))
       (to-pixels r {:gradient (c/gradient :prl-10)})))
  (example-snippet "Render with logarithmic scale" saver :image
    #(let [r (gradient-renderer 300 300)]
       (dotimes [i 4000000]
         (let [x (+ 150 (r/grand 30))
               y (+ 150 (r/grand 30))
               x (+ x (* 20.0 (- ^double (local-noise (/ x 50.0) (/ y 50.0) 0.34) 0.5)))
               y (+ y (* 20.0 (- ^double (local-noise (/ y 50.0) (/ x 50.0) 2.23) 0.5)))]
           (add-pixel! r x y)))
       (to-pixels r {:logarithmic? true}))))
