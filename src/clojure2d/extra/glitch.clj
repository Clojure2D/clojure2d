(ns clojure2d.extra.glitch
  "Various glitching pixel filters or functions

  ### Filter

  Use following filters with [[filter-channels]] function.
  
  * Slitscan - x/y slitscan simulation based on wave functions
  * Shift-channels - just shift channels
  * Mirror - mirror image along different axes
  * Slitscan2 - slitscan simulation based on vector fields
  * Fold - apply vector field on the image
  * Pix2line - convert pixel into horizontal line

  ### Machines

  Short sketches operating on images/pixels.
  
  * Blend - compose two images in glitchy way

  All filters are equiped with random configuration generator."
  (:require [fastmath.core :as m]
            [fastmath.random :as r]
            [clojure2d.pixels :as p]
            [clojure2d.core :refer :all]
            [fastmath.vector :as v]
            [clojure2d.extra.signal :as s]
            [clojure2d.color :as c]
            [fastmath.fields :as var])
  (:import [clojure2d.pixels Pixels]
           [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

;; ## Slitscan

;; ### Simple slitscan
;;
;; Pixels are shifted by value returned by wave function. You have to provide separate wave functions for x and y axises.
;; Random setup is based on sum of oscillators defined in `signal` namespace.

(def ^:private freqs (mapv #(<< 1 ^long %) (range 8)))
(def ^:private amps (mapv #(/ ^long %) freqs))

(defn- make-random-wave 
  "Create random wave definition."
  []
  (let [r (r/randval 0.75 (r/irand 4) (r/irand (count freqs)))]
    {:wave (rand-nth s/oscillators)
     :freq (freqs r)
     :amp (amps r)
     :phase (r/drand)}))

(defn slitscan-random-config
  "Create list of random waves for each axis separately.

  Optionally you can pass number of waves to create for each axis."
  ([nx ny]
   {:x (repeatedly nx make-random-wave)
    :y (repeatedly ny make-random-wave)})
  ([]
   (slitscan-random-config (r/irand 2 6) (r/irand 2 6))))

(defn- make-slitscan-waves
  "Create function from waves definision."
  [waves]
  (apply s/sum-waves (map #(s/wave (:wave %) (:freq %) (:amp %) (:phase %)) waves)))

(defn- do-slitscan
  "Shift pixels by amount returned by functions fx and fy."
  [fx fy ch ^Pixels p x y]
  (let [wp (.w p)
        hp (.h p)
        sx (/ wp)
        sy (/ hp)
        shiftx (* 0.3 wp ^double (fx (* ^int x sx)))
        shifty (* 0.3 hp ^double (fy (* ^int y sy)))
        xx (m/wrap 0.0 wp (+ ^int x shiftx))
        yy (m/wrap 0.0 hp (+ ^int y shifty))]
    (p/get-value p ch xx yy)))

(defn slitscan
  "Create slitscan filter funtion.

  Config is a map each axis has it's own list of maps defining waves. Each map contains:

  * `:wave` - oscillator name (see [[oscillators]].
  * `:freq` - wave frequency
  * `:amp` - wave amplitude
  * `:phase` - wave phase (0-1)."
  ([]
   (slitscan (slitscan-random-config)))
  ([{:keys [x y]
     :or {x [(make-random-wave)] y [(make-random-wave)]}}]
   (partial p/filter-channel-xy (partial do-slitscan (make-slitscan-waves x) (make-slitscan-waves y)))))

;; channel shifts

(defn shift-channels-random-config
  "Random shift values along x and y axes.

  Optionally provide `spread` parameter to define maximum shift value."
  ([] (shift-channels-random-config 0.1))
  ([^double spread]
   (let [shift-x (r/randval 0.2 0.0 (r/drand (- spread) spread))
         shift-y (if (zero? shift-x)
                   (r/drand (- spread) spread)
                   (r/randval 0.2 0.0 (r/drand (- spread) spread)))]
     {:x-shift shift-x
      :y-shift shift-y})))

(defn shift-channels
  "Shift channels by given amount.

  Parameters:

  * `:x-shift` - shift amount along x axis
  * `:y-shift` - shift amount along y axis"
  ([] (shift-channels (shift-channels-random-config)))
  ([{:keys [^double x-shift ^double y-shift]
     :or {x-shift 0.05 y-shift -0.05}}]
   (slitscan {:x [{:wave :constant :amp (- x-shift)}]
              :y [{:wave :constant :amp (- y-shift)}]})))

;; Slitscan2

(defn slitscan2-random-config
  "Generate random configuration for vector fields slitscan.

  * `r` - field range (default 2.0)
  * `d` - fields configuration depth (default up to 3)"
  ([] (slitscan2-random-config 2.0 3))
  ([r] (slitscan2-random-config r 3.0))
  ([r d]
   (binding [var/*skip-random-fields* true]
     {:fields (var/random-configuration d)
      :r r})))

(defn slitscan2
  "Slitscan filter based on vector fields.

  Parameters:
  
  * `:fields` - vector fields configuration [[combine]]
  * `:r` -  range value 1.0-3.0"
  ([{:keys [fields ^double r]
     :or {r 2.0 fields (var/random-configuration 0)}}]
   (let [f (var/combine fields)
         r- (- r)]
     (fn [ch t ^Pixels p]
       (dotimes [y (.h p)]
         (let [yv (m/norm y 0.0 (.h p) r- r)]
           (dotimes [x (.w p)]
             (let [xlerp (m/norm x 0.0 (.w p))
                   v1 (f (Vec2. r- yv))
                   v2 (f (Vec2. r yv))
                   ^Vec2 vv (v/interpolate v1 v2 xlerp)
                   xx (unchecked-int (m/norm (.x vv) r- r 0.0 (.w p)))
                   yy (unchecked-int (m/norm (.y vv) r- r 0.0 (.h p)))]
               (p/set-value t ch x y (p/get-value p ch xx yy)))))))))
  ([]
   (slitscan2 (slitscan2-random-config))))

;;

(def ^{:doc   "Generate random configuration for vector fields slitscan.

  * `r` - field range (default 2.0)
  * `d` - fields configuration depth (default up to 3)"}
  fold-random-config slitscan2-random-config)

(defn fold
  "Folding filter based on vector fields.

  Parameters:
  
  * `:fields` - vector fields configuration [[combine]]
  * `:r` -  range value 1.0-3.0"
  ([{:keys [fields ^double r]}]
   (let [f (var/combine fields)
         r- (- r)]
     (fn [ch t ^Pixels p]
       (dotimes [y (.h p)]
         (let [yv (m/norm y 0.0 (.h p) r- r)]
           (dotimes [x (.w p)]
             (let [xv (m/norm x 0.0 (.w p) r- r)
                   ^Vec2 vv (f (Vec2. xv yv))
                   xx (unchecked-int (m/norm (.x vv) r- r 0.0 (.w p)))
                   yy (unchecked-int (m/norm (.y vv) r- r 0.0 (.h p)))]
               (p/set-value t ch x y (p/get-value p ch xx yy)))))))))
  ([]
   (fold (fold-random-config))))

;; mirrorimage

(defn- mi-draw-point
  ""
  ([ch target source oldx oldy newx newy sx sy]
   (p/set-value target ch (+ ^long newx ^long sx) (+ ^long newy ^long sy)
                (p/get-value source ch (+ ^long oldx ^long sx) (+ ^long oldy ^long sy))))
  ([ch target source oldx oldy newx newy]
   (p/set-value target ch newx newy (p/get-value source ch oldx oldy))))

(defn- mi-do-horizontal
  ""
  [t ch target ^Pixels source]
  (dotimes [y (/ (.h source) 2)]
    (dotimes [x (.w source)]
      (if t
        (mi-draw-point ch target source x y x (- (.h source) y 1))
        (mi-draw-point ch target source x (- (.h source) y 1) x y)))))

(defn- mi-do-vertical
  ""
  [t ch target ^Pixels source]
  (dotimes [x (/ (.w source) 2)]
    (dotimes [y (.h source)]
      (if t
        (mi-draw-point ch target source x y (- (.w source) x 1) y)
        (mi-draw-point ch target source (- (.w source) x 1) y x y)))))

(defn- mi-do-diag-ul
  ""
  [t shift? ch target ^Pixels source]
  (let [t (int t)
        size (min (.w source) (.h source))
        tx (if shift? (- (.w source) size) 0)
        ty (if shift? (- (.h source) size) 0)]
    (dotimes [y size]
      (dotimes [x (inc y)]
        (case t
          0 (mi-draw-point ch target source x y y x tx ty)
          1 (mi-draw-point ch target source y x x y tx ty)
          2 (mi-draw-point ch target source x y (- size x 1) (- size y 1) tx ty)
          3 (mi-draw-point ch target source y x (- size y 1) (- size x 1) tx ty))))))

(defn- mi-do-diag-ur
  ""
  [t shift? ch target ^Pixels source]
  (let [t (int t)
        size (min (.w source) (.h source))
        tx (if shift? (- (.w source) size) 0)
        ty (if shift? (- (.h source) size) 0)]
    (dotimes [y size]
      (loop [x (int (dec size))]
        (when (>= x (- size y 1))
          (case t
            0 (mi-draw-point ch target source x y (- size y 1) (- size x 1) tx ty)
            1 (mi-draw-point ch target source (- size y 1) (- size x 1) x y tx ty)
            2 (mi-draw-point ch target source x y (- size x 1) (- size y 1) tx ty)
            3 (mi-draw-point ch target source (- size x 1) (- size y 1) x y tx ty))
          (recur (dec x)))))))

(defn- mi-do-diag-rect
  ""
  [t l ch target ^Pixels source]
  (dotimes [y (.h source)]
    (let [d (int (if t
                   (m/norm y 0 (.h source) 0 (.w source))
                   (m/norm y 0 (.h source) (.w source) 0)))]
      (dotimes [x d]
        (if l
          (mi-draw-point ch target source (- (.w source) x 1) (- (.h source) y 1) x y)
          (mi-draw-point ch target source x y (- (.w source) x 1) (- (.h source) y 1)))))))

(def ^{:doc "Map of names and mirroring functions"}
  mirror-types {:U    (partial mi-do-horizontal true)
                :D    (partial mi-do-horizontal false)
                :L    (partial mi-do-vertical true)
                :R    (partial mi-do-vertical false)
                :DL   (partial mi-do-diag-ul 0 false)
                :UR   (partial mi-do-diag-ul 1 false)
                :DL2  (partial mi-do-diag-ul 2 false)
                :UR2  (partial mi-do-diag-ul 3 false)
                :SDL  (partial mi-do-diag-ul 0 true)
                :SUR  (partial mi-do-diag-ul 1 true)
                :SDL2 (partial mi-do-diag-ul 2 true)
                :SUR2 (partial mi-do-diag-ul 3 true)
                :DR   (partial mi-do-diag-ur 0 false)
                :UL   (partial mi-do-diag-ur 1 false)
                :DR2  (partial mi-do-diag-ur 2 false)
                :UL2  (partial mi-do-diag-ur 3 false)
                :SDR  (partial mi-do-diag-ur 0 true)
                :SUL  (partial mi-do-diag-ur 1 true)
                :SDR2 (partial mi-do-diag-ur 2 true)
                :SUL2 (partial mi-do-diag-ur 3 true)
                :RUR  (partial mi-do-diag-rect true true)
                :RDR  (partial mi-do-diag-rect false true)
                :RDL  (partial mi-do-diag-rect true false)
                :RUL  (partial mi-do-diag-rect false false)})

(defn mirror-random-config
  "Generate random mirroring functions."
  []
  (rand-nth (keys mirror-types)))

(defn mirror
  "Mirror image for given (or random) mirroring functions."
  ([t] (mirror-types t))
  ([] (mirror-types (mirror-random-config))))

;; pix2line

(defn- pix2line-grid 
  ""
  [^long grid-sx ^long grid-sy {:keys [^long nx ^long ny ^double scale nseed ^double shiftx ^double shifty]}]
  (let [nnx (m/round (inc (* nx scale)))
        nny (m/round (inc (* ny scale)))
        [bget bset] (int-array-2d grid-sx grid-sy)
        noise (r/fbm-noise {:seed (or nseed (r/irand))
                            :lacunarity 2.1})]
    (dotimes [y grid-sy]
      (bset 0 y 0) 
      (loop [currx (int 0)
             current (< ^double (noise 0 (/ y ny)) 0.5)
             x (int 1)]
        (when (< x grid-sx)
          (let [xnnx (/ x nnx)
                xx (* xnnx (m/round (* nnx (inc ^double (noise (+ 0.2 xnnx))))))
                ynny (/ y nny)
                yy (* ynny (m/round (* nny (inc ^double (noise (- 0.4 ynny))))))
                here (< ^double (noise (+ shiftx (/ (+ x xx) nx)) (+ shifty (/ (+ y yy) ny))) 0.5)
                ncurrx (if (= current here) currx x)
                ncurrent (if (= current here) current here)]
            (bset x y ncurrx)
            (recur ncurrx ncurrent (unchecked-inc x))))))
    bget))

(defn pix2line-random-config
  "Make random config for pix2line."
  []
  {:nx (inc (r/irand 100))
   :ny (inc (r/irand 100))
   :scale (r/drand 5.0)
   :tolerance (r/randval 0.9 (r/irand 5 80) (r/irand 5 250))
   :nseed (r/irand)
   :whole (r/brand 0.8)
   :shiftx (r/drand)
   :shifty (r/drand)})

(defn pix2line
  "Pix2line effect. Convert pixels to lines.

  Parametrization:

  * `:nx`, `:ny` - grid size
  * `:scale` - grid scaling factor
  * `:tolerance` - factor which regulates when start new line
  * `:nseed` - noise seed
  * `:whole` - skip lines or not
  * `:shiftx`, `:shifty` - noise shift"
  ([]
   (pix2line (pix2line-random-config)))
  ([{:keys [^long tolerance whole] :as config}] 
   (fn [ch target ^Pixels source]
     (let [grid (pix2line-grid (width source) (height source) config)]
       (dotimes [y (height source)]
         (loop [^int currentc (p/get-value source ch 0 y)
                lastx 0
                x (int 1)]
           (if (< x (.w source))
             (let [^int c (p/get-value source ch x y)
                   [^int ncurrentc ^int nlastx] (if (<= tolerance (m/abs (- currentc c)))
                                                  (let [^int gval (grid x y)
                                                        ^int myx (if (bool-and whole (< lastx gval)) lastx gval)]
                                                    (dotimes [xx (- x myx)] (p/set-value target ch (+ myx xx) y c))
                                                    [c x])
                                                  [currentc lastx])]
               (recur ncurrentc (int nlastx) (unchecked-inc x)))
             (let [x- (dec x)
                   ^int gval (grid x- y)
                   ^int myx (if (< lastx gval) lastx gval)]
               (dotimes [xx (- x- myx)] (p/set-value target ch (+ myx xx) y currentc))))))))))

;; blend machine

(defn blend-machine-random-config
  "Random configuration for blend machine."
  []
  (let [cs1 (r/randval 0.9 (rand-nth c/colorspaces-list) nil) ; let's convert to some colorspace (or leave rgb)
        cs2 (r/randval 0.2 (r/randval 0.9 (rand-nth c/colorspaces-list) nil) cs1) ; maybe different cs on second image?
        outcs (r/randval 0.2 (r/randval 0.9 (rand-nth c/colorspaces-list) nil) cs1) ; maybe some random colorspace on output?
        bl1 (r/randval 0.85 (rand-nth c/blends-list) nil)    ; ch1 blend
        bl2 (r/randval 0.85 (rand-nth c/blends-list) nil) ; ch2 blend
        bl3 (r/randval 0.85 (rand-nth c/blends-list) nil)] ; ch3 blend
    {:switch? (r/brand 0.5)
     :in1-cs cs1
     :in2-cs cs2
     :out-cs outcs
     :in1-to? (r/brand 0.5)
     :in2-to? (r/brand 0.5)
     :out-to? (r/brand 0.5)
     :blend-ch1 bl1
     :blend-ch2 bl2
     :blend-ch3 bl3}))

(defn blend-machine
  "Blend two `Pixels` based on configuration.

  The idea is to compose channels separately in different color spaces.

  Full flow does following steps:

  * convert inputs to (or from) selected color spaces
  * compose each channel separately using different method
  * convert output to (or from) selected color space
  
  Parametrization:

  * `:switch?` -  reverse input order
  * `:in1-cs` - color space for first image
  * `:in2-cs` - color space for second image
  * `:out-cs` - color space for output
  * `:in1-to?`, `:in2-to?`, `:cs-to?` - which conversion to select: to color space or from color space
  * `:blend-ch1`, `:blend-ch2`, `:blend-ch3` - blend methods for each channel"  
  ([p1 p2]
   (blend-machine (blend-machine-random-config) p1 p2))
  ([{:keys [switch? in1-cs in2-cs out-cs in1-to? in2-to? out-to? blend-ch1 blend-ch2 blend-ch3]} p1 p2]
   (let [[p1 p2] (if switch? [p2 p1] [p1 p2]) ; switch images
         in1-sel (if in1-to? first second)
         in2-sel (if in2-to? first second)
         out-sel (if out-to? first second)
         result (p/compose-channels blend-ch1 blend-ch2 blend-ch3 nil
                                    (if in1-cs (p/filter-colors (in1-sel (in1-cs c/colorspaces*)) p1) p1)
                                    (if in2-cs (p/filter-colors (in2-sel (in2-cs c/colorspaces*)) p2) p2))]
     (if out-cs
       (p/filter-colors (out-sel (out-cs c/colorspaces*)) result)
       result))))

;; find best matching pixels

(comment defn blend-images-filter
         ""
         [{:keys [names pixels mode distance cs]
           :or {names [] pixels [] distance :euclid-sq mode :color cs :RGB}} ^Pixels p]
         (let [images (concat pixels (map (comp (partial p/filter-colors (first (c/colorspaces* cs))) p/load-pixels) names))
               ^int w (width p)
               ^int h (height p)
               df (v/distances distance)]
           (if (= mode :color)
             (p/filter-colors-xy (fn [p ^long x ^long y]
                                   (let [c (p/get-color p x y)]
                                     (first (reduce (fn [curr img]
                                                      (let [nx (unchecked-int (m/norm x 0 w 0 (width img)))
                                                            ny (unchecked-int (m/norm y 0 h 0 (height img)))
                                                            [currc ^double currd] curr
                                                            nc (p/get-color img nx ny) 
                                                            ^double nd (df c nc)] 
                                                        (if (< nd currd) [nc nd] curr)))
                                                    [c Double/MAX_VALUE] images)))) p)
             (p/filter-channels (partial p/filter-channel-xy (fn [ch p ^long x ^long y]
                                                               (let [^int c (p/get-value p ch x y)]
                                                                 (first (reduce (fn [curr img]
                                                                                  (let [nx (unchecked-int (m/norm x 0 w 0 (width img)))
                                                                                        ny (unchecked-int (m/norm y 0 h 0 (height img)))
                                                                                        [currc ^double currd] curr
                                                                                        ^int nc (p/get-value img ch nx ny) 
                                                                                        nd (m/abs (- c nc))]
                                                                                    (if (< nd currd) [nc nd] curr)))
                                                                                [c Double/MAX_VALUE] images))))) p))))

