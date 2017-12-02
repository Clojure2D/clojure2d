;; # Namespace scope
;;
;; This namespace provides functions which cover: diplaying windows, events, canvases, image files and session management.
;; In brief:
;;
;; * Image file read and write, backed by Java ImageIO API. You can read and write BMP, JPG and PNG files. I didn't test WBMP and GIF. Image itself is Java `BufferedImage` in integer ARGB mode. Each pixel is represented as 32bit unsigned integer and 8 bits per channel. See `clojure2d.pixels` namespace for pixels operations.
;; * Canvas with functions to draw on it, represented as Canvas type with Graphics2d, BufferedImage, quality settings, primitive classes and size
;; * Display (JFrame) with events handlers (multimethods) + associated autorefreshing canvas, and optionally Processiing style `draw` function with context management
;; * Session management: unique identifier generation, logging (different file per session) and unique, sequenced filename creation.
;; * Some general helper functions

(ns clojure2d.core
  "JFrame, Java2D, file io and simple session management"
  (:require [clojure.java.io :refer :all]
            [clojure2d.color :as c]
            [clojure2d.math.vector :as v]
            [clojure2d.math :as m]
            [clojure.reflect :as ref])
  (:import clojure2d.math.vector.Vec2
           [java.awt BasicStroke Color Component Dimension Graphics2D GraphicsEnvironment Image RenderingHints Shape Toolkit Transparency]
           [java.awt.event InputEvent ComponentEvent KeyAdapter KeyEvent MouseAdapter MouseEvent MouseMotionAdapter WindowAdapter WindowEvent]
           [java.awt.geom Ellipse2D Ellipse2D$Double Line2D Line2D$Double Path2D Path2D$Double Rectangle2D Rectangle2D$Double Point2D Point2D$Double]
           [java.awt.image BufferedImage BufferStrategy Kernel ConvolveOp]
           [java.util Iterator]
           [javax.imageio IIOImage ImageIO ImageWriteParam ImageWriter]
           [javax.swing ImageIcon JFrame SwingUtilities]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

;; how many tasks we can run (one less than available cores)?
(def ^:const ^long available-cores (.availableProcessors (Runtime/getRuntime)))
(def ^:const ^long available-tasks (inc available-cores))

;; ## Image

;; Let's start with setting dynamic variable which defines quality of the saved jpeg file. Values are from `0.0` to `1.0`.
;; You can freely change this setting with `binding` macro. Default is 97% (0.97).
(def ^:dynamic *jpeg-image-quality* 0.97)

;; ### Load image

;; To load image from the file, just call `(load-image "filename.jpg")`. Idea is taken from Processing code. Loading is done via `ImageIcon` class and later converted to BufferedImage in ARGB mode.

(defn load-image 
  "Load image from file.

  * Input: image filename with absolute or relative path (relative to your project folder)
  * Returns BufferedImage object"
  [^String filename]
  (try
    (let [^Image img (.getImage (ImageIcon. filename))]
      (let [^BufferedImage bimg (BufferedImage. (.getWidth img nil) (.getHeight img nil) BufferedImage/TYPE_INT_ARGB)
            ^Graphics2D gr (.createGraphics bimg)]
        (.drawImage gr img 0 0 nil)
        (.dispose gr)
        (.flush img)
        bimg))
    (catch Exception e (println "Can't load image: " filename " " (.getMessage e)))))

;; ### Save image

;; Saving image is more tricky. Again, most concepts are taken from Processing.
;; 
;; For provided image object and filename process goes as follows:
;;
;; * create all necessary folders
;; * extract file extension and create image writer object
;; * call multimethod to prepare data and save in chosen format
;;
;; We have two types here. First is whether file format accepts alpha channel (jpgs, bmps don't) and second is quality settings (for jpg only). In the first case we have to properly flatten the image with `flatten-image` function. In the second case we set quality attributes.

(defn file-extension
  "Extract extension from filename.

  * Input: image filename
  * Returns extension (without dot)"
  [filename]
  (second (re-find #"\.(\w+)$" filename)))

(defn- ^ImageWriter get-image-writer
  "Returns image writer of image type based on extension."
  [filename]
  (let [ext (file-extension filename)
        ^Iterator iter (ImageIO/getImageWritersByFormatName ext)]
    (when (.hasNext iter)
      (.next iter))))

(defn- ^BufferedImage flatten-image
  "Flatten image, properly drop alpha channel.

  * Input: ARGB BufferedImage object
  * Returns RGB BufferedImage object"
  [^BufferedImage img]
  (let [w (.getWidth img)
        h (.getHeight img)
        arr (.getRGB img 0 0 w h nil 0 w)
        ^BufferedImage nimg (BufferedImage. w h BufferedImage/TYPE_INT_RGB)]
    (.setRGB nimg 0 0 w h arr 0 w)
    nimg))

(defn- do-save
  "Save image to the file via writer with parameters. Returns image."
  ([filename img ^ImageWriter writer]
   (do-save filename img writer (.getDefaultWriteParam writer)))
  ([filename ^BufferedImage img ^ImageWriter writer param]
   (with-open [os (output-stream filename)]
     (doto writer
       (.setOutput (ImageIO/createImageOutputStream os))
       (.write nil (IIOImage. img nil nil) param)
       (.dispose)))
   img))

;; Now we define multimethod which saves image. Multimethod is used here because some image types requires additional actions.
(defmulti save-file-type (fn [filename _ _] (keyword (file-extension filename))))

;; JPG requires flatten image and we must set the quality defined in `*jpeg-image-quality*` variable.
(defmethod save-file-type :jpg
  [filename img ^ImageWriter writer]
  (let [nimg (flatten-image img)
        ^ImageWriteParam param (.getDefaultWriteParam writer)]
    (doto param
      (.setCompressionMode ImageWriteParam/MODE_EXPLICIT)
      (.setCompressionQuality *jpeg-image-quality*))
    (do-save filename nimg writer param)))

;; BMP also requires image flattening
(defmethod save-file-type :bmp
  [filename img writer]
  (do-save filename (flatten-image img) writer))

;; The rest file types are saved with alpha without special treatment.
(defmethod save-file-type :default
  [filename img writer]
  (do-save filename img writer))

(defn save-image
  "Save image to the file.

  * Input: image (`BufferedImage` object) and filename
  * Side effect: saved image"
  [b filename]
  (println (str "saving: " filename "..."))
  (make-parents filename)
  (let [iwriter (get-image-writer filename)]
    (if-not (nil? iwriter)
      (do
        (save-file-type filename b iwriter)
        (println "...done!"))
      (println (str "can't save an image: " filename)))))

;; ### Additional functions
;;
;; Just an image resizer with bicubic interpolation. Native `Graphics2D` method is called.

(defn resize-image
  "Resize image

  * Input: image and target width and height
  * Returns newly created resized image"
  [img width height]
  (let [^BufferedImage target (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)
        ^Graphics2D g (.createGraphics target)]
    (doto g
      (.setRenderingHint RenderingHints/KEY_INTERPOLATION RenderingHints/VALUE_INTERPOLATION_BICUBIC)
      (.setRenderingHint RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
      (.drawImage img 0 0 width height nil)
      (.dispose))
    target))

(defn subimage
  "Get subimage of give image"
  [^BufferedImage source x y w h]
  (.getSubimage source x y w h))


;; ## Canvas
;;
;; Canvas is an object you can draw on and which can be displayed in the window. Technically it's a type cosisting of `Graphics2D` object which is internally used to draw on the image, image (`BufferedImage` object), rendering quality hints and singletons for primitives. What is important: to draw on canvas you have to wrap your operations in `with-canvas->` macro. `with-canvas->` is responsible for creating and releasing `Graphics2D` object. Initially `Graphics2D` is set to `nil`.
;; Canvas should be accelerated by Java and your video card.
;; Reminder: Drawing on canvas is single threaded.

;; Let's define protocol to equip Canvas and Window types (or any type with image inside) with `get-image` function. `get-image` extracts image. Additionally define width/height getters.
(defprotocol ImageProto 
  (get-image [i] "Return BufferedImage")
  (width [i])
  (height [i])
  (save [i n])
  (convolve [i t])
  (get-pixel [i x y] "Retruns color"))

(def convolution-matrices {:shadow (Kernel. 3 3 (float-array [0 1 2 -1 0 1 -2 -1 0]))
                           :emboss (Kernel. 3 3 (float-array [0 2 4 -2 1 2 -4 -2 0]))
                           :edges-1 (Kernel. 3 3 (float-array [1 2 1 2 -12 2 1 2 1]))
                           :edges-2 (Kernel. 3 3 (float-array [1 0 -1 0 0 0 -1 0 1]))
                           :edges-3 (Kernel. 3 3 (float-array [0 1 0 1 -4 1 0 1 0]))
                           :edges-4 (Kernel. 3 3 (float-array [-1 -1 -1 -1 8 -1 -1 -1 -1]))
                           :sharpen (Kernel. 3 3 (float-array [0 -1 0 -1 5 -1 0 -1 0]))
                           :sobel-x (Kernel. 3 3 (float-array [1 0 -1 2 0 -2 1 0 -1]))
                           :sobel-y (Kernel. 3 3 (float-array [1 2 1 0 0 0 -1 -2 -1]))
                           :gradient-x (Kernel. 3 3 (float-array [-1 0 1 -1 0 1 -1 0 1]))
                           :gradient-y (Kernel. 3 3 (float-array [-1 -1 -1 0 0 0 1 1 1]))
                           :box-blur (Kernel. 3 3 (float-array (map #(/ (int %) 9.0) [1 1 1 1 1 1 1 1 1])))
                           :gaussian-blur-3 (Kernel. 3 3 (float-array (map #(/ (int %) 16.0) [1 2 1 2 4 2 1 2 1])))
                           :gaussian-blur-5 (Kernel. 5 5 (float-array (map #(/ (int %) 256.0) [1 4 6 4 1 4 16 24 16 4 6 24 36 24 6 4 16 24 16 4 1 4 6 4 1])))
                           :unsharp (Kernel. 5 5 (float-array (map #(/ (int %) -256.0) [1 4 6 4 1 4 16 24 16 4 6 24 -476 24 6 4 16 24 16 4 1 4 6 4 1])))})

;; Add ImageProto functions to BufferedImage
(extend BufferedImage
  ImageProto
  {:get-image identity
   :width (fn [^BufferedImage i] (.getWidth i))
   :height (fn [^BufferedImage i] (.getHeight i))
   :save #(do
            (save-image %1 %2)
            %1)
   :convolve (fn [^BufferedImage i t]
               (let [kernel (if (keyword? t)
                              (t convolution-matrices)
                              (let [s (int (m/sqrt (count t)))]
                                (Kernel. s s (float-array t))))]
                 (.filter ^ConvolveOp (ConvolveOp. kernel) i nil)))
   :get-pixel (fn [^BufferedImage i ^long x ^long y]
                (if (bool-or (< x 0)
                             (< y 0)
                             (>= x (.getWidth i))
                             (>= y (.getHeight i)))
                  (c/make-color 0 0 0)
                  (let [b (int-array 1)
                        ^java.awt.image.Raster raster (.getRaster i)]
                    (.getDataElements raster x y b)
                    (let [v (aget b 0)
                          b (bit-and v 0xff)
                          g (bit-and (>> v 8) 0xff)
                          r (bit-and (>> v 16) 0xff)
                          a (bit-and (>> v 24) 0xff)]
                      (if (== (.getNumBands raster) 3)
                        (c/make-color r g b)
                        (c/make-color r g b a))))))})

;; Canvas type. Use `get-image` to extract image (`BufferedImage`).
(deftype Canvas [^Graphics2D graphics
                 ^BufferedImage buffer
                 ^Line2D line-obj
                 ^Rectangle2D rect-obj
                 ^Ellipse2D ellipse-obj
                 hints
                 ^long w
                 ^long h
                 transform-stack
                 font]
  ImageProto
  (get-image [_] buffer)
  (width [_] w)
  (height [_] h)
  (save [c n] (save-image buffer n) c)
  (convolve [_ t]
    (convolve buffer t))
  (get-pixel [_ x y] (get-pixel buffer x y)))

;; Let's define three rendering quality options: `:low`, `:mid` and `:high`. Where `:low` is fastest but has poor quality and `:high` has best quality but may be slow. Rendering options are used when you create canvas.
(def rendering-hints {:low {RenderingHints/KEY_ANTIALIASING        RenderingHints/VALUE_ANTIALIAS_OFF
                            RenderingHints/KEY_INTERPOLATION       RenderingHints/VALUE_INTERPOLATION_NEAREST_NEIGHBOR
                            RenderingHints/KEY_ALPHA_INTERPOLATION RenderingHints/VALUE_ALPHA_INTERPOLATION_SPEED
                            RenderingHints/KEY_COLOR_RENDERING     RenderingHints/VALUE_COLOR_RENDER_SPEED
                            RenderingHints/KEY_RENDERING           RenderingHints/VALUE_RENDER_SPEED
                            RenderingHints/KEY_FRACTIONALMETRICS   RenderingHints/VALUE_FRACTIONALMETRICS_OFF
                            RenderingHints/KEY_TEXT_ANTIALIASING   RenderingHints/VALUE_TEXT_ANTIALIAS_OFF}
                      :mid {RenderingHints/KEY_ANTIALIASING        RenderingHints/VALUE_ANTIALIAS_ON
                            RenderingHints/KEY_INTERPOLATION       RenderingHints/VALUE_INTERPOLATION_BILINEAR
                            RenderingHints/KEY_ALPHA_INTERPOLATION RenderingHints/VALUE_ALPHA_INTERPOLATION_SPEED
                            RenderingHints/KEY_COLOR_RENDERING     RenderingHints/VALUE_COLOR_RENDER_SPEED
                            RenderingHints/KEY_RENDERING           RenderingHints/VALUE_RENDER_SPEED
                            RenderingHints/KEY_FRACTIONALMETRICS   RenderingHints/VALUE_FRACTIONALMETRICS_OFF
                            RenderingHints/KEY_TEXT_ANTIALIASING   RenderingHints/VALUE_TEXT_ANTIALIAS_ON}
                      :high {RenderingHints/KEY_ANTIALIASING        RenderingHints/VALUE_ANTIALIAS_ON
                             RenderingHints/KEY_INTERPOLATION       RenderingHints/VALUE_INTERPOLATION_BICUBIC
                             RenderingHints/KEY_ALPHA_INTERPOLATION RenderingHints/VALUE_ALPHA_INTERPOLATION_QUALITY
                             RenderingHints/KEY_COLOR_RENDERING     RenderingHints/VALUE_COLOR_RENDER_QUALITY
                             RenderingHints/KEY_RENDERING           RenderingHints/VALUE_RENDER_QUALITY
                             RenderingHints/KEY_FRACTIONALMETRICS   RenderingHints/VALUE_FRACTIONALMETRICS_ON
                             RenderingHints/KEY_TEXT_ANTIALIASING   RenderingHints/VALUE_TEXT_ANTIALIAS_ON
                             RenderingHints/KEY_STROKE_CONTROL      RenderingHints/VALUE_STROKE_PURE}})

;; Following functions and macro are responsible for creating and releasing `Graphics2D` object for a canvas.
;; You have to use `with-canvas->` macro to draw on canvas. Internally it's a threading macro and accepts only list of methods which first parameter is canvas object.
;; Please do not use `flush-graphics` and `make-graphics` functions directly. Use `with-canvas->` macro instead. I do not check state of `Graphics2D` anywhere.
;; There is one exception: `draw` function associated with Window is already wrapped in `with-canvas->` and you can freely use canvas object inside.
;; Another note: `with-canvas->` creates it's own copy of `Canvas` object with set `Graphics2D`.
;;
;; Second alternative is `with-canvas []` with which is just wrapper (without threading). As a parameter you have to provide binding name for you canvas.
;;
;; Example `with-canvas [local-canvas canvas] (set-background local-canvas :black)`

(defn flush-graphics
  "Dispose current `Graphics2D`"
  [^Canvas canvas]
  (.dispose ^Graphics2D (.graphics canvas)))

(defn make-graphics
  "Create new `Graphics2D` object and set renedering hints"
  [^Canvas canvas]
  (let [^Graphics2D ng (.createGraphics ^BufferedImage (.buffer canvas))]
    (.setRenderingHints ng (or (.hints canvas) (rendering-hints :high)))
    (when-let [f (.font canvas)] (.setFont ng f))
    (Canvas. ng
             (.buffer canvas)
             (.line-obj canvas)
             (.rect-obj canvas)
             (.ellipse-obj canvas)
             (.hints canvas)
             (.w canvas)
             (.h canvas)
             (atom [])
             (.font canvas))))

(defmacro with-canvas->
  "Threading macro which takes care to create and destroy `Graphics2D` object for drawings on canvas. Macro returns result of last call."
  [canvas & body]  
  `(let [newcanvas# (make-graphics ~canvas)
         result# (-> newcanvas#
                     ~@body)]
     (do
       (flush-graphics newcanvas#)
       result#)))

(defmacro with-canvas
  "Macro which takes care to create and destroy `Graphics2D` object for drawings on canvas. Macro returns result of last call."
  [[c canvas] & body]
  `(let [~c (make-graphics ~canvas)
         result# (do ~@body)]
     (do
       (flush-graphics ~c)
       result#)))

;; Next functions are canvas management functions: create, save, resize and set quality.

(declare set-background)
(declare set-stroke)
(declare image)

(defn create-canvas
  "Create and return canvas with `width`, `height` and quality hint name (keyword). Default hint is `:high`."
  ([^long width ^long height hint ^String font]
   (let
       [^BufferedImage buffer (.. GraphicsEnvironment 
                                  (getLocalGraphicsEnvironment)
                                  (getDefaultScreenDevice)
                                  (getDefaultConfiguration)
                                  (createCompatibleImage width height Transparency/TRANSLUCENT))        
        result (Canvas. nil
                        buffer
                        (Line2D$Double.)
                        (Rectangle2D$Double.)
                        (Ellipse2D$Double.)
                        (rendering-hints (or (some #{hint} (keys rendering-hints)) :high))
                        width height
                        nil
                        (when font (java.awt.Font/decode font)))]
     (with-canvas-> result
       (set-background Color/black)
       (set-stroke))))
  ([width height]
   (create-canvas width height :high nil))
  ([width height hint]
   (create-canvas width height hint nil)))

;; alias for create-canvas
(def make-canvas create-canvas)

(defn resize-canvas
  "Resize canvas to new dimensions. Creates and returns new canvas."
  [^Canvas canvas width height]
  (let [ncanvas (create-canvas width height (.hints canvas))]
    (with-canvas-> ncanvas
      (image (get-image canvas)))))

;; ### Transformations
;;
;; You can transform your working area with couple of functions on canvas. They act exactly the same as in Processing. Transformation context is bound to canvas wrapped to `with-canvas->` macro. Each `with-canvas->` cleans all transformations.
;; Transformations are concatenated.

(defn scale
  "Scale canvas"
  ([^Canvas canvas ^double scalex ^double scaley]
   (.scale ^Graphics2D (.graphics canvas) scalex scaley)
   canvas)
  ([canvas s] (scale canvas s s)))

(defn flip-x
  "Flip canvas over x axis"
  [canvas]
  (scale canvas -1.0 1.0))

(defn flip-y
  "Flip canvas over y axis"
  [canvas]
  (scale canvas 1.0 -1.0))

(defn translate
  "Translate origin"
  ([^Canvas canvas ^double tx ^double ty]
   (.translate ^Graphics2D (.graphics canvas) tx ty)
   canvas)
  ([canvas ^Vec2 v]
   (translate canvas (.x v) (.y v))))

(defn rotate
  "Rotate canvas"
  [^Canvas canvas ^double angle]
  (.rotate ^Graphics2D (.graphics canvas) angle)
  canvas)

(defn shear
  "Shear canvas"
  ([^Canvas canvas ^double sx ^double sy]
   (.shear ^Graphics2D (.graphics canvas) sx sy)
   canvas)
  ([canvas s] (shear canvas s s)))

(defn push-matrix
  "Remember current transformation state"
  [^Canvas canvas]
  (swap! (.transform-stack canvas) conj (.getTransform ^Graphics2D (.graphics canvas)))
  canvas)

(defn pop-matrix
  "Restore saved transformation state"
  [^Canvas canvas]
  (when (seq @(.transform-stack canvas))
    (let [v (peek @(.transform-stack canvas))]
      (swap! (.transform-stack canvas) pop)
      (.setTransform ^Graphics2D (.graphics canvas) v)))
  canvas)

(defn transform
  "Transform given point or coordinates with current transformation."
  ([^Canvas canvas x y]
   (let [^Point2D p (.transform ^java.awt.geom.AffineTransform (.getTransform ^Graphics2D (.graphics canvas)) (Point2D$Double. x y) nil)]
     (Vec2. (.getX p) (.getY p))))
  ([canvas ^Vec2 v]
   (transform canvas (.x v) (.y v))))

(defn inv-transform
  "Inverse transform of given point or coordinates with current transformation."
  ([^Canvas canvas x y]
   (let [^Point2D p (.inverseTransform ^java.awt.geom.AffineTransform (.getTransform ^Graphics2D (.graphics canvas)) (Point2D$Double. x y) nil)]
     (Vec2. (.getX p) (.getY p))))
  ([canvas ^Vec2 v]
   (inv-transform canvas (.x v) (.y v))))

(defn reset-matrix
  "Reset transformation"
  [^Canvas canvas]
  (.setTransform ^Graphics2D (.graphics canvas) (java.awt.geom.AffineTransform.))
  canvas)

;; ### Drawing functions
;;
;; Here we have basic drawing functions. What you need to remember:
;;
;; * Color is set globally for all figures (exception: `set-background`)
;; * Filled or stroke figures are determined by last parameter `stroke?`. When set to `true` draws figure outline, filled otherwise (default). Default is `false` (filled).
;; * Always use with `with-canvas->` macro.
;; 
;; All functions return canvas object

(defn line
  "Draw line from point `(x1,y1)` to `(x2,y2)`"
  ([^Canvas canvas x1 y1 x2 y2]
   (let [^Line2D l (.line-obj canvas)]
     (.setLine l x1 y1 x2 y2)
     (.draw ^Graphics2D (.graphics canvas) l))
   canvas)
  ([canvas ^Vec2 v1 ^Vec2 v2]
   (line canvas (.x v1) (.y v1) (.x v2) (.y v2))))

(defn point
  "Draw point at `(x,y)` position"
  ([canvas ^double x ^double y]
   (line canvas x y (+ x 10.0e-6) (+ y 10.0e-6))
   canvas)
  ([canvas ^Vec2 p]
   (point canvas (.x p) (.y p))))

(defn- draw-fill-or-stroke
  "Draw filled or stroked object."
  [^Graphics2D g ^Shape obj stroke?]
  (if stroke?
    (.draw g obj)
    (.fill g obj)))

(defn rect
  "Draw rectangle with top-left corner at `(x,y)` position with width `w` and height `h`."
  ([^Canvas canvas x1 y1 w h stroke?]
   (let [^Rectangle2D r (.rect-obj canvas)] 
     (.setFrame r x1 y1 w h)
     (draw-fill-or-stroke (.graphics canvas) r stroke?))
   canvas)
  ([canvas x1 y1 w h]
   (rect canvas x1 y1 w h false)))

(defn crect
  "Centered version of rectangle"
  ([canvas x1 y1 w h stroke?]
   (let [w2 (* 0.5 ^double w)
         h2 (* 0.5 ^double h)]
     (rect canvas (- ^double x1 w2) (- ^double y1 h2) w h stroke?))
   canvas)
  ([canvas x1 y1 w h]
   (crect canvas x1 y1 w h false)))

(defn ellipse
  "Draw ellipse with middle at `(x,y)` position with width `w` and height `h`."
  ([^Canvas canvas x1 y1 w h stroke?]
   (let [^Ellipse2D e (.ellipse_obj canvas)]
     (.setFrame e (- ^double x1 (* ^double w 0.5)) (- ^double y1 (* ^double h 0.5)) w h)
     (draw-fill-or-stroke (.graphics canvas) e stroke?))
   canvas)
  ([canvas x1 y1 w h]
   (ellipse canvas x1 y1 w h false)))

(defn triangle
  "Draw triangle with corners at 3 positions."
  ([^Canvas canvas x1 y1 x2 y2 x3 y3 stroke?]
   (let [^Path2D p (Path2D$Double.)]
     (doto p
       (.moveTo x1 y1)
       (.lineTo x2 y2)
       (.lineTo x3 y3)
       (.closePath))
     (draw-fill-or-stroke (.graphics canvas) p stroke?))
   canvas)
  ([canvas x1 y1 x2 y2 x3 y3]
   (triangle canvas x1 y1 x2 y2 x3 y3 false)))

(defn triangle-strip
  "Draw triangle strip. Implementation of `Processing` `STRIP` shape.

  Input: list of vertices as Vec2 vectors"
  ([canvas vs stroke?]
   (when (> (count vs) 2)
     (loop [^Vec2 v1 (first vs)
            ^Vec2 v2 (second vs)
            vss (nnext vs)]
       (when vss
         (let [^Vec2 v3 (first vss)]
           (triangle canvas (.x v2) (.y v2) (.x v3) (.y v3) (.x v1) (.y v1) stroke?)
           (recur v2 v3 (next vss))))))
   canvas)
  ([canvas vs]
   (triangle-strip canvas vs false)))

(defn triangle-fan
  "Draw triangle fan. Implementation of `Processing` `FAN` shape.

  Input: list of vertices as Vec2 vectors"
  ([canvas vs stroke?]
   (when (> (count vs) 2)
     (let [^Vec2 v1 (first vs)]
       (loop [^Vec2 v2 (second vs)
              vss (nnext vs)]
         (when vss
           (let [^Vec2 v3 (first vss)]
             (triangle canvas (.x v1) (.y v1) (.x v2) (.y v2) (.x v3) (.y v3) stroke?)
             (recur v3 (next vss)))))))
   canvas)
  ([canvas vs]
   (triangle-strip canvas vs false)))

(defn path
  "Draw path from lines. Input: list of Vec2 points, close? - close path or not (default: false), stroke? - draw lines or filled shape (default true - lines)."
  ([^Canvas canvas vs close? stroke?]
   (when (seq vs)
     (let [^Path2D p (Path2D$Double.)
           ^Vec2 m (first vs)]
       (.moveTo p (.x m) (.y m))
       (doseq [^Vec2 v (next vs)]
         (.lineTo p (.x v) (.y v)))
       (when (or (not stroke?) close?) (.closePath p))
       (draw-fill-or-stroke (.graphics canvas) p stroke?)))
   canvas)
  ([canvas vs close?] (path canvas vs close? true))
  ([canvas vs] (path canvas vs false true)))

(defn calculate-bezier-control-points
  "Calculate bezier spline control points. http://www.antigrain.com/research/bezier_interpolation/index.html"
  [v0 v1 v2 v3]
  (let [c1 (v/mult (v/add v0 v1) 0.5)
        c2 (v/mult (v/add v1 v2) 0.5)
        c3 (v/mult (v/add v2 v3) 0.5)
        ^double len1 (v/mag c1)
        ^double len2 (v/mag c2)
        ^double len3 (v/mag c3)
        k1 (/ len1 (+ len1 len2))
        k2 (/ len2 (+ len2 len3))
        m1 (v/add c1 (v/mult (v/sub c2 c1) k1))
        m2 (v/add c2 (v/mult (v/sub c3 c2) k2))
        cp1 (-> c2
                (v/sub m1)
                (v/add m1)
                (v/add v1)
                (v/sub m1))
        cp2 (-> c2
                (v/sub m2)
                (v/add m2)
                (v/add v2)
                (v/sub m2))]
    [cp1 cp2]))

(defn path-bezier
  "Draw path from quad curves. Input: list of Vec2 points, close? - close path or not (default: false), stroke? - draw lines or filled shape (default true - lines)."
  ([^Canvas canvas vs close? stroke?]
   (when (> (count vs) 3)
     (let [cl? (or (not stroke?) close?)
           ^Path2D p (Path2D$Double.)
           ^Vec2 m0 (first vs)
           ^Vec2 m1 (second vs)
           m2 (nth vs 2) 
           f0 (if cl? m0 m0)
           ^Vec2 f1 (if cl? m1 m0)
           f2 (if cl? m2 m1)
           f3 (if cl? (nth vs 3) m2)
           vs (if cl? (next vs) vs)]
       (.moveTo p (.x f1) (.y f1))
       (loop [v0 f0
              v1 f1
              ^Vec2 v2 f2
              ^Vec2 v3 f3
              nvs (drop 3 vs)]
         (let [[^Vec2 cp1 ^Vec2 cp2] (calculate-bezier-control-points v0 v1 v2 v3)]
           (.curveTo p (.x cp1) (.y cp1) (.x cp2) (.y cp2) (.x v2) (.y v2))
           (if-not (empty? nvs)
             (recur v1 v2 v3 (first nvs) (next nvs))
             (if cl?
               (let [[^Vec2 cp1 ^Vec2 cp2] (calculate-bezier-control-points v1 v2 v3 m0)
                     [^Vec2 cp3 ^Vec2 cp4] (calculate-bezier-control-points v2 v3 m0 m1)
                     [^Vec2 cp5 ^Vec2 cp6] (calculate-bezier-control-points v3 m0 m1 m2)]
                 (.curveTo p (.x cp1) (.y cp1) (.x cp2) (.y cp2) (.x v3) (.y v3))
                 (.curveTo p (.x cp3) (.y cp3) (.x cp4) (.y cp4) (.x m0) (.y m0))
                 (.curveTo p (.x cp5) (.y cp5) (.x cp6) (.y cp6) (.x m1) (.y m1)))
               (let [[^Vec2 cp1 ^Vec2 cp2] (calculate-bezier-control-points v1 v2 v3 v3)]
                 (.curveTo p (.x cp1) (.y cp1) (.x cp2) (.y cp2) (.x v3) (.y v3)))))))
       (draw-fill-or-stroke (.graphics canvas) p stroke?)))
   canvas)
  ([canvas vs close?] (path-bezier canvas vs close? true))
  ([canvas vs] (path-bezier canvas vs false true)))

(defn quad
  "Draw quad with corners at 4 positions."
  ([^Canvas canvas x1 y1 x2 y2 x3 y3 x4 y4 stroke?]
   (let [^Path2D p (Path2D$Double.)]
     (doto p
       (.moveTo x1 y1)
       (.lineTo x2 y2)
       (.lineTo x3 y3)
       (.lineTo x4 y4)
       (.closePath))
     (draw-fill-or-stroke (.graphics canvas) p stroke?))
   canvas)
  ([canvas x1 y1 x2 y2 x3 y3 x4 y4]
   (quad canvas x1 y1 x2 y2 x3 y3 x4 y4 false)))

(defn set-font
  "Set font by name"
  [^Canvas canvas ^String fontname]
  (let [f (java.awt.Font/getFont fontname)]
    (.setFont ^Graphics2D (.graphics canvas) f)
    canvas))

(defn set-font-attributes
  "Set current font size"
  ([^Canvas canvas ^double size style]
   (let [s (or (style {:bold 1 :italic 2 :bold-italic 3}) 0)
         f (.deriveFont ^java.awt.Font (.getFont ^Graphics2D (.graphics canvas)) (int s) (float size))]
     (.setFont ^Graphics2D (.graphics canvas) f)
     canvas))
  ([^Canvas canvas ^double size]
   (let [f (.deriveFont ^java.awt.Font (.getFont ^Graphics2D (.graphics canvas)) (float size))]
     (.setFont ^Graphics2D (.graphics canvas) f)
     canvas)))

(defn text
  "Draw text with default setting"
  [^Canvas canvas ^String s ^long x ^long y]
  (.drawString ^Graphics2D (.graphics canvas) s x y)
  canvas)

(defn set-stroke
  "Set stroke (line) attributes like `cap`, `join` and size. Default `CAP_ROUND` and `JOIN_MITER` is used. Default size is `1.0`."
  ([^Canvas canvas size cap join]
   (.setStroke ^Graphics2D (.graphics canvas) (BasicStroke. size cap join))
   canvas)
  ([canvas size cap]
   (set-stroke canvas size cap BasicStroke/JOIN_MITER))
  ([canvas size]
   (set-stroke canvas size BasicStroke/CAP_ROUND BasicStroke/JOIN_MITER))
  ([canvas]
   (set-stroke canvas 1.0)))

;; ### Color

(defn- set-color-with-fn
  "Set color for primitive or background via passed function. You can use:

  * java.awt.Color object
  * clojure2d.math.vector.Vec4 or Vec3 object
  * individual r, g, b (and optional alpha) as integers from 0-255. They are converted to integer and clamped if necessary."
  ([f canvas c]
   (f canvas (c/make-awt-color c)))
  ([f canvas c a]
   (f canvas (c/make-awt-color c a)))
  ([f canvas r g b a]
   (f canvas (c/make-awt-color r g b a)))
  ([f canvas r g b]
   (f canvas (c/make-awt-color r g b))))

(defn set-awt-color
  "Set color with valid java `Color` object. Use it when you're sure you pass `java.awt.Color`."
  [^Canvas canvas ^java.awt.Color c]
  (.setColor ^Graphics2D (.graphics canvas) c)
  canvas)

(defn set-awt-background
  "Set background color. Expects valid `Color` object."
  [^Canvas canvas c]
  (let [^Graphics2D g (.graphics canvas)
        ^Color currc (.getColor g)]
    (push-matrix canvas)
    (reset-matrix canvas)
    (set-color-with-fn set-awt-color canvas c)
    (doto g
      (.fillRect 0 0 (.w canvas) (.h canvas))
      (.setColor currc))
    (pop-matrix canvas))
  canvas)

(defn set-awt-xor-mode
  "Set XOR graphics mode. If color is set to nil"
  [^Canvas canvas c]
  (let [^Graphics2D g (.graphics canvas)]
    (if c
      (.setXORMode g c)
      (.setPaintMode g)))
  canvas)

;; Set color for primitive
(def set-color (partial set-color-with-fn set-awt-color))

;; Set background color
(def set-background (partial set-color-with-fn set-awt-background))

;; Set XOR mode
(def set-xor-mode (partial set-color-with-fn set-awt-xor-mode))

;; ### Gradient

(defn set-gradient
  "Set paint mode to gradient. Call with canvas only to reset."
  ([^Canvas canvas x1 y1 color1 x2 y2 color2]
   (let [gp (java.awt.GradientPaint. x1 y1 (c/make-awt-color color1) x2 y2 (c/make-awt-color color2))
         ^Graphics2D g (.graphics canvas)]
     (.setPaint g gp)
     canvas)))

;; ### Image

(defn image
  "Draw an image. You can specify position and size of the image. Default it's placed on whole canvas."
  ([^Canvas canvas img x y w h]
   (.drawImage ^Graphics2D (.graphics canvas) (get-image img) x y w h nil)
   canvas)
  ([^Canvas canvas img]
   (image canvas img 0 0 (.w canvas) (.h canvas)))
  ([^Canvas canvas img x y]
   (image canvas img x y (width img) (height img))))

;; ## Display window
;;
;; You can find here a couple of functions which help to display your canvas and build interaction with user.
;; Display window is just a Swing `JFrame` with `java.awt.Canvas` as panel.
;; What is important, window is not a canvas (like it is in Processing) so first you need to create canvas object and then create window displaying it.
;; You can create as many windows as you want. Just name them differently. You can also create window with different size than canvas. Canvas will be rescaled.
;; Windows is not resizable and can't be set to a fullscreen mode (yet)
;; 
;; To show window you call `show-window` function and provide following parameters:
;;
;; * canvas to display
;; * window name (used to identify events)
;; * width and height
;; * canvas refresh rate as frames per second (ex. 25)
;; * optionally callback function which is called just before repainting the canvas (like `draw` in Processing)
;;
;; `show-window` returns a `Window` object containing
;;
;; * `JFrame` object
;; * `active?` atom (see below)
;; *  buffer which is canvas packed into atom (to enable easy canvas replacement)
;; *  `:panel` `java.awt.Canvas` object placed on JFrame (awt toolkit canvas)
;; *  `:fps`
;; *  `:width`
;; *  `:height`
;; *  `:window-name` window name
;;
;; `active?` atom is unique for each window and has value `true` when window is shown and set to `false` when window is closed with default close button.
;; Use this information via `window-active?` function to control (and possibly stop) all activities which refers to related window. For example you may want to cancel all updating canvas processes when user closes window.
;;
;; ### Callback function (aka `draw`)
;;
;; You can define function with three parameters which is called just before repainting canvas. You can use it to simulate Processing `draw` behaviour. Function should accept following parameters:
;;
;; * canvas - canvas to draw on, canvas bound to window will be passed here.
;; * window - window assiociated with function
;; * frame count - current number of calls, starting 0
;; * state - any state you want to pass between calls, `nil` initially
;;
;; Function should return current state, which will be passed to function when called next time.
;;
;; Note: calls to `draw` are wrapped in `with-canvas->` already.
;;
;; ### Events
;;
;; To control user activities you can use two event processing multimethods.
;;
;; * `key-pressed`
;; * `key-released`
;; * `key-typed`
;; * `mouse-event`
;;
;; Each multimethod get awt Event object and global state. Each should return new state.
;;
;; #### Key event: `key-pressed` and other `key-` multimethods
;;
;; Your dispatch value is vector with window name and pressed key (as `char`) which you want to handle.
;; This means you write different method for different key.
;; As a function parameters you get `KeyEvent` object [java.awt.KeyEvent](https://docs.oracle.com/javase/7/docs/api/java/awt/event/KeyEvent.html) and global state attached to the Window.
;;
;; `KeyEvent` is enriched by `KeyEventProto` with functions:
;;
;; * `key-code` - key code mapped to keyword. Eg. `VK_UP` -> `:up` or `VK_CONTROL` -> `:control`
;; * `key-raw` - raw key code value (integer)
;; * `key-char` - char representation (for special keys char is equal `virtual-key` or `0xffff` value
;;
;; If you want to dispatch on special keys (like arrows), dispatch on `(char 0xffff)` or `virtual-key` and read `(key-code e)` to get key pressed.
;;
;; #### Mouse event
;;
;; As as dispatch you use a vector containing window name as a String and mouse event type as a keyword
;; As a parameter you get `MouseEvent` object equipped with `MouseXYProto` protocol [java.awt.MouseEvent](https://docs.oracle.com/javase/7/docs/api/java/awt/event/MouseEvent.html) and global state attached to the Window.
;;
;; Currently implemented types are:
;;
;; * `:mouse-clicked`
;; * `:mouse-dragged`
;; * `:mouse-pressed`
;; * `:mouse-released`
;;
;; To get mouse position call `(mouse-x e)` and `(mouse-y e)` where `e` is MouseEvent object.

;; Extract all keycodes from `KeyEvent` object and pack it to the map
(def keycodes-map (->> KeyEvent
                       (ref/reflect)
                       (:members)
                       (filter #(instance? clojure.reflect.Field %))
                       (map #(str (:name %)))
                       (filter #(re-matches #"VK_.*" %))
                       (reduce #(assoc %1
                                       (clojure.lang.Reflector/getStaticField "java.awt.event.KeyEvent" ^String %2)
                                       (-> %2
                                           (subs 3)
                                           (clojure.string/lower-case)
                                           (keyword))) {})))

(defprotocol MouseXYProto
  "Access to mouse position."
  (mouse-x [m] "x position")
  (mouse-y [m] "y position")
  (mouse-pos [m] "combined position"))

(defprotocol MouseProto
  "Get mouse button status"
  (mouse-button [m] "get mouse button status :left :right :center or :none"))

(defprotocol KeyEventProto
  "Access to event data"
  (key-code [e] "keycode mapped to keyword")
  (key-char [e] "key char")
  (key-raw [e] "raw value for key (integer)"))

(defprotocol ModifiersProto
  "Get state of modifiers"
  (control-down? [e])
  (alt-down? [e])
  (meta-down? [e])
  (shift-down? [e])
  (alt-gr-down? [e]))

;; `Window` type definition, equiped with `get-image` method returning bound canvas' image.
(defrecord Window [^JFrame frame
                   active?
                   buffer
                   ^java.awt.Canvas panel
                   ^double fps
                   ^long w
                   ^long h
                   window-name]
  ImageProto
  (get-image [_] (get-image @buffer))
  (width [_] w)
  (height [_] h)
  (save [w n] (save-image (get-image @buffer) n) w)
  (convolve [w n] (convolve @buffer n))
  (get-pixel [_ x y] (get-pixel @buffer x y))
  MouseXYProto
  (mouse-pos [_]
    (let [^java.awt.Point p (.getMousePosition panel)]
      (if (nil? p)
        (Vec2. -1.0 -1.0)
        (Vec2. (.x p) (.y p)))))
  (mouse-x [_]
    (let [^java.awt.Point p (.getMousePosition panel)]
      (if (nil? p) -1 (.x p))))
  (mouse-y [_]
    (let [^java.awt.Point p (.getMousePosition panel)]
      (if (nil? p) -1 (.y p)))))

;; ### Events function
(extend MouseEvent
  MouseProto
  {:mouse-button #(condp = (.getButton ^MouseEvent %)
                    MouseEvent/BUTTON1 :left
                    MouseEvent/BUTTON2 :center
                    MouseEvent/BUTTON3 :right
                    :none)}
  MouseXYProto
  {:mouse-x #(.getX ^MouseEvent %)
   :mouse-y #(.getY ^MouseEvent %)
   :mouse-pos #(Vec2. (mouse-x %) (mouse-y %))})

(extend KeyEvent
  KeyEventProto
  {:key-code #(keycodes-map (.getKeyCode ^KeyEvent %))
   :key-char #(.getKeyChar ^KeyEvent %)
   :key-raw #(.getKeyCode ^KeyEvent %)})

(extend InputEvent
  ModifiersProto
  {:control-down? #(.isControlDown ^InputEvent %)
   :alt-down? #(.isAltDown ^InputEvent %)
   :meta-down? #(.isMetaDown ^InputEvent %)
   :shift-down? #(.isShiftDown ^InputEvent %)
   :alt-gr-down? #(.isAltGraphDown ^InputEvent %)})

;; ### Window type helper functions

(defn window-active?
  "Helper function, check if window is active"
  [^Window window]
  @(.active? window))

(defn mouse-in-window?
  "Check if mouse is inside window"
  [window]
  (bool-and (>= ^int (mouse-x window) 0.0)
            (>= ^int (mouse-y window) 0.0)))

;; ### Global state management
;;
;; Global atom is needed to keep current window state. Events don't know what window sends it. The only option is to get component name.

(defonce global-state (atom {}))

(defn get-state
  "Get state from window"
  [^Window window]
  (@global-state (.window-name window)))

(defn- change-state! 
  "Change state for Window (by name)"
  [window-name state] 
  (swap! global-state assoc window-name state)
  (@global-state window-name))

(defn- clear-state!
  "Clear state for Window (by name)"
  [window-name]
  (swap! global-state dissoc window-name))

(defn set-state!
  "Changle global state for Window."
  [^Window w state]
  (change-state! (.window-name w) state))

;; Private method which extracts the name of your window (set when `show-window` is called).

(defn event-window-name
  "Returns name of the component. Used to dispatch events."
  [^ComponentEvent e]
  (.getName ^Component (.getComponent e)))

(def ^:const virtual-key (char 0xffff))

;; Multimethod used to process pressed key
(defmulti key-pressed (fn [^KeyEvent e state] [(event-window-name e) (.getKeyChar e)]))
;; Do nothing on default
(defmethod key-pressed :default [_ s]  s)

;; Multimethod used to process released key
(defmulti key-released (fn [^KeyEvent e state] [(event-window-name e) (.getKeyChar e)]))
;; Do nothing on default
(defmethod key-released :default [_ s]  s)

;; Multimethod used to process typed key
(defmulti key-typed (fn [^KeyEvent e state] [(event-window-name e) (.getKeyChar e)]))
;; Do nothing on default
(defmethod key-typed :default [_ s]  s)

;; Map Java mouse event names onto keywords
(def mouse-event-map {MouseEvent/MOUSE_CLICKED  :mouse-clicked
                      MouseEvent/MOUSE_DRAGGED  :mouse-dragged
                      MouseEvent/MOUSE_PRESSED  :mouse-pressed
                      MouseEvent/MOUSE_RELEASED :mouse-released
                      MouseEvent/MOUSE_MOVED    :mouse-moved})

;; Multimethod used to processed mouse events
(defmulti mouse-event (fn [^MouseEvent e state] [(event-window-name e) (mouse-event-map (.getID e))]))
;; Do nothing on default
(defmethod mouse-event :default [_ s] s)

;; Event adapter objects.

;; General function which manipulates state and calls proper event multimethod

(defn- process-state-and-event 
  "For given event call provided multimethod passing state. Save new state."
  [ef e]
  (let [window-name (event-window-name e)]
    (change-state! window-name (ef e (@global-state window-name)))))

;; Key
(def key-processor (proxy [KeyAdapter] []
                     (keyPressed [e] (process-state-and-event key-pressed e))
                     (keyReleased [e] (process-state-and-event key-released e))
                     (keyTyped [e] (process-state-and-event key-typed e))))

;; Mouse
(def mouse-processor (proxy [MouseAdapter] []
                       (mouseClicked [e] (process-state-and-event mouse-event e))
                       (mousePressed [e] (process-state-and-event mouse-event e))
                       (mouseReleased [e] (process-state-and-event mouse-event e))))

;; Mouse drag and move
(def mouse-motion-processor (proxy [MouseMotionAdapter] []
                              (mouseDragged [e] (process-state-and-event mouse-event e))
                              (mouseMoved [e] (process-state-and-event mouse-event e))))

(defn close-window
  "Close window programatically"
  [window]
  (.dispatchEvent ^JFrame (:frame window) (java.awt.event.WindowEvent. (:frame window) java.awt.event.WindowEvent/WINDOW_CLOSING)))

;; ### Frame machinery functions
;;
;; Window is JFrame with panel (as java.awt.Canvas object) which is used to draw clojure2d canvas on it.

(defn- create-panel
  "Create panel which displays canvas. Attach mouse events, give a name (same as window), set size etc."
  [buffer windowname width height]
  (let [panel (java.awt.Canvas.)]
    (doto panel
      (.setName windowname)
      (.addMouseListener mouse-processor)
      (.addKeyListener key-processor)
      (.addMouseMotionListener mouse-motion-processor)
      (.setIgnoreRepaint true)
      (.setPreferredSize (Dimension. width height)))))

;; Function used to close and dispose window. As a side effect `active?` atom is set to false and global state for window is cleared.

(defn- close-window-fn
  "Close window frame"
  [^JFrame frame active? windowname]
  (reset! active? false)
  (clear-state! windowname)
  (.dispose frame))

;; Create lazy list of icons to be loaded by frame
(def window-icons (map #(.getImage (ImageIcon. (resource (str "icons/i" % ".png")))) [10 16 20 24 30 32 40 44 64 128]))

(defn- build-frame
  "Create JFrame object, create and attach panel and do what is needed to show window. Attach key events and closing event."
  [^JFrame frame ^java.awt.Canvas panel active? windowname width height]
  (let [closer (proxy [WindowAdapter] []
                 (windowClosing [^WindowEvent e] (close-window-fn frame active? windowname)))]
    (doto frame
      (.setIconImages window-icons)
      (.add panel)
      (.addKeyListener key-processor)
      (.setResizable false)
      (.pack)
      (.setDefaultCloseOperation JFrame/DO_NOTHING_ON_CLOSE)
      (.addWindowListener closer)
      (.setName windowname)
      (.setTitle windowname)
      (.setBackground Color/white)
      (.setVisible true))
    (doto panel
      (.createBufferStrategy 2))))

;; Another internal function repaints panel with frames per seconds rate. If `draw` function is passed it is called before rapaint action. Function runs infinitely until window is closed. The cycle goes like this:
;;
;; * call `draw` function if available, pass canvas, current frame number and current state (`nil` at start)
;; * repaint
;; * wait
;; * check if window is still displayed and recur incrementing frame number and pass state for another run.

(defn- repaint
  "Draw buffer on panel using `BufferStrategy` object."
  [^java.awt.Canvas panel ^Canvas canvas]
  (let [^BufferStrategy strategy (.getBufferStrategy panel)]
    (loop []
      (loop []
        (let [^Graphics2D graphics-context (.getDrawGraphics strategy)
              ^BufferedImage b (.buffer canvas)]
          (.setRenderingHints graphics-context (or (.hints canvas) (rendering-hints :high)))
          (.drawImage graphics-context b 0 0 (.getWidth panel) (.getHeight panel) nil)
          (.dispose graphics-context))
        (when (.contentsRestored strategy) (recur)))
      (.show strategy)
      (when (.contentsLost strategy) (recur)))
    (.sync (Toolkit/getDefaultToolkit))))

(defn- refresh-screen-task
  "Repaint canvas on window with set FPS.

  * Input: frame, active? atom, function to run before repaint, canvas and sleep time."
  [^Window window draw-fun draw-state]
  (let [stime (/ 1000.0 ^double (.fps window))]
    (loop [cnt (long 0)
           result draw-state]
      (let [ct (System/currentTimeMillis)
            new-result (when draw-fun 
                         (with-canvas-> @(.buffer window)
                           (draw-fun window cnt result)))] 
        (repaint (.panel window) @(.buffer window))
        (let [delay (- stime (- (System/currentTimeMillis) ct))]
          (when (pos? delay) (Thread/sleep delay)))
        (when @(.active? window) (recur (unchecked-inc cnt) new-result))))))

;; You may want to replace canvas to the other one. To make it pass result of `show-window` function and new canvas.
;; Internally it just resets buffer atom for another canvas.
;; See examples/ex01_events.clj to see how it works.

(defn replace-canvas
  "Replace canvas in window.

  * Input: window and new canvas
  * Returns canvas"
  [^Window window canvas]
  (reset! (:buffer window) canvas))

;; Finally function which creates and displays window. Function creates window's visibility status (`active?` atom), buffer as atomized canvas, creates frame, creates refreshing task (repainter) and shows window.

(declare to-hex)

(defn show-window
  "Show window with width/height, name and required fps of refresh. Optionally pass callback function.

  * Input: canvas, window name, width (defalut: canvas width), height (default: canvas height), frames per seconds (default: 60), (optional) `draw` function.
  * Returns `Window` value

  As parameters you can provide a map with folowing keys:

  * :canvas
  * :window-name
  * :w
  * :h
  * :fps
  * :draw-fn
  * :state"
  ([canvas wname width height fps draw-fun state draw-state]
   (let [active? (atom true)
         buffer (atom canvas)
         frame (JFrame.)
         panel (create-panel buffer wname width height)
         window (->Window frame
                          active?
                          buffer
                          panel
                          fps
                          width
                          height
                          wname)]
     (SwingUtilities/invokeAndWait #(build-frame frame panel active? wname width height))
     (change-state! wname state)
     (future (refresh-screen-task window draw-fun draw-state))
     window))
  ([canvas wname]
   (show-window canvas wname nil))
  ([canvas wname draw-fn]
   (show-window canvas wname 60 draw-fn))
  ([canvas wname fps draw-fn]
   (show-window canvas wname (width canvas) (height canvas) fps draw-fn nil nil))
  ([canvas wname w h fps]
   (show-window canvas wname w h fps nil nil nil))
  ([canvas wname w h fps draw-fun]
   (show-window canvas wname w h fps draw-fun nil nil))
  ([{:keys [canvas window-name w h fps draw-fn state draw-state]
     :or {canvas (make-canvas 100 100)
          window-name (str "Clojure2D - " (to-hex (rand-int (Integer/MAX_VALUE)) 8))
          w (width canvas)
          h (height canvas)
          fps 60
          draw-fn nil
          state nil
          draw-state nil}}]
   (show-window canvas window-name w h fps draw-fn state draw-state))
  ([] (show-window {})))

;; ## Utility functions
;;
;; Now we have a part with some utilities (I had no idea where to put them).

(defn to-hex
  "Return hex value of given number, padded with leading zeroes if given length"
  ([n]
   (format "%X" n))
  ([n pad]
   (format (str "%0" pad "X") n)))

(defmacro time-with-name
  "Evaluates expr and prints the time it took. Attaches comment at the beginning.
  Returns the value of expr."
  [commnt expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     (prn (str "(" ~commnt ")" " Elapsed time: " (/ (double (- (. System (nanoTime)) start#)) 1000000.0) " msecs"))
     ret#))

;; ## Session management
;;
;; Couple session management functions. Generally used to generate unique identifier, log or generate filename.
;; Use cases are:
;;
;; * Log your actions to the file. Simply writes text messages.
;; * Save your images under unique and sequenced filenames

(defn make-counter
  "Create counter function, each call returns next number."
  ([^long v]
   (let [tick (atom (dec v))]
     (fn [] (swap! tick #(inc ^long %)))))
  ([]
   (make-counter 0)))

;; Store date format in variable
(def ^java.text.SimpleDateFormat simple-date-format (java.text.SimpleDateFormat. "yyyyMMddHHmmss"))
(def ^java.text.SimpleDateFormat simple-date-format-full (java.text.SimpleDateFormat. "yyyy-MM-dd HH:mm:ss"))

;; Following block defines session related functions.
;; Session is a type of:
;;
;; * logger - file writer
;; * name - session identifiers as vector with formatted current date and hash of this date
;; * counter - used to create sequence filenames for session
;;
;; Session is encapsulated in agent and is global for library
;;
;; Example:
;;
;; * `(get-session-name) => nil`
;; * `(make-session) => ["20170123235332" "CD88D0C5"]`
;; * `(get-session-name) => ["20170123235332" "CD88D0C5"]`
;; * `(next-filename "folder/" ".txt") => "folder/CD88D0C5_000000.txt"`
;; * `(next-filename "folder/" ".txt") => "folder/CD88D0C5_000001.txt"`
;; * `(close-session) => nil`
;; * `(get-session-name) => nil`
;; * `(make-session) => ["20170123235625" "CD8B7204"]`

;; Session values are packed into the type
(defrecord SessionType [logger
                        name
                        counter])

;; Session is stored in agent
(defonce session-agent (agent (map->SessionType {})))

;; Logging to file is turned off by default.
(def ^:dynamic *log-to-file* false)

(defn- close-session-fn
  "Close current session"
  [s]
  (let [^java.io.Writer o (:logger s)]
    (when-not (nil? o)
      (.flush o)
      (.close o)))
  (map->SessionType {}))

(defn- make-logger-fn
  "Create writer for logger"
  [session-name]
  (let [fname (str "log/" (first session-name) ".log")]
    (make-parents fname) 
    (let [^java.io.Writer no (writer fname :append true)]
      (.write no (str "Session id: " (second session-name) (System/lineSeparator) (System/lineSeparator)))
      no)))

(defn- make-session-name
  "Create unique session name based on current time. Result is a vector with date and hash represented as hexadecimary number."
  []
  (let [date (java.util.Date.)]
    [(.format simple-date-format date) (to-hex (hash date) 8)]))

(defn- make-session-fn 
  "Create session"
  [^SessionType s]
  (close-session-fn s)
  (let [nname (make-session-name)
        writer (when *log-to-file* (make-logger-fn nname))]
    (->SessionType writer nname (make-counter 0))))

(defn make-session
  "Create session via agent"
  []
  (send session-agent make-session-fn)
  (await-for 1000 session-agent)
  (:name @session-agent))

(defn close-session
  "Close session via agent"
  []
  (send session-agent close-session-fn)
  (await-for 1000 session-agent))

(defn ensure-session
  "Ensure that session is active (create one if not"
  []
  (when (nil? (:name @session-agent))
    (make-session)))

(defn get-session-name
  "Get session name"
  []
  (ensure-session)
  (:name @session-agent))

(defn next-filename
  "Create next unique filename based on session"
  ([prefix]
   (ensure-session)
   (let [s @session-agent]
     (str prefix (second (:name s)) "_" (format "%06d" ((:counter s))))))
  ([prefix suffix]
   (str (next-filename prefix) suffix)))

(defn log
  "Log message to file or console"
  [message]
  (let [to-log (str (.format simple-date-format-full (java.util.Date.)) ": " message (System/lineSeparator))]
    (ensure-session)
    (if *log-to-file*
      (send session-agent (fn [s]
                            (let [^java.io.Writer o (or (:logger s) (make-logger-fn (:name s)))]
                              (.write o to-log)
                              (.flush o)
                              (->SessionType o (:name s) (:counter s)))))
      (println to-log))))

;;
;; Mutable array2d creator helper
;;

(defn make-2d-int-array
  "Create 2d int array getter and setter methods. Array is mutable!"
  [^long sizex ^long sizey]
  (let [buff (int-array (* sizex sizey))]
    [#(aget ^ints buff (+ ^long %1 (* sizex ^long %2)))
     #(aset ^ints buff (+ ^long %1 (* sizex ^long %2)) ^int %3)]))

(defn make-2d-long-array
  "Create 2d int array getter and setter methods. Array is mutable!"
  [^long sizex ^long sizey]
  (let [buff (long-array (* sizex sizey))]
    [#(aget ^longs buff (+ ^long %1 (* sizex ^long %2)))
     #(aset ^longs buff (+ ^long %1 (* sizex ^long %2)) ^long %3)]))

(defn make-2d-double-array
  "Create 2d int array getter and setter methods. Array is mutable!"
  [^long sizex ^long sizey]
  (let [buff (double-array (* sizex sizey))]
    [#(aget ^doubles buff (+ ^long %1 (* sizex ^long %2)))
     #(aset ^doubles buff (+ ^long %1 (* sizex ^long %2)) ^double %3)]))
