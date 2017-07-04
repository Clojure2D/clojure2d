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
            [clojure2d.math.random :as r])  
  (:import [clojure2d.math.vector Vec2]
           [java.awt.image BufferedImage BufferStrategy]
           [javax.swing ImageIcon]
           [javax.imageio ImageIO ImageWriter ImageWriteParam IIOImage]
           [java.awt Graphics2D Image RenderingHints GraphicsEnvironment 
            Transparency BasicStroke Color Container Dimension Component Toolkit Shape]
           [java.util Iterator]
           [java.awt.geom Line2D Line2D$Double
            Rectangle2D Rectangle2D$Double
            Path2D Path2D$Double
            Ellipse2D Ellipse2D$Double]
           [java.awt.event KeyAdapter KeyEvent MouseAdapter MouseMotionAdapter
            MouseEvent WindowAdapter WindowEvent ComponentEvent]
           [javax.swing JFrame SwingUtilities]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

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

(defn- file-extension-int
  "Extract extension from filename.

  * Input: image filename
  * Returns extension (without dot)"
  [filename]
  (second (re-find #"\.(\w+)$" filename)))

;; Memoize above function (in case you'll be saving file very often).
(def file-extension (memoize file-extension-int))

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
  (println (str "saving: " filename))
  (make-parents filename)
  (let [iwriter (get-image-writer filename)]
    (if-not (nil? iwriter)
      (save-file-type filename b iwriter)
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

;; ## Canvas
;;
;; Canvas is an object you can draw on and which can be displayed in the window. Technically it's a type cosisting of `Graphics2D` object which is internally used to draw on the image, image (`BufferedImage` object), rendering quality hints and singletons for primitives. What is important: to draw on canvas you have to wrap your operations in `with-canvas` macro. `with-canvas` is responsible for creating and releasing `Graphics2D` object. Initially `Graphics2D` is set to `nil`.
;; Canvas should be accelerated by Java and your video card.
;; Reminder: Drawing on canvas is single threaded.

;; Let's define protocol to equip Canvas and Window types with `get-image` function. `get-image` extracts image.
(defprotocol ImageProto 
  (get-image [t] "Return BufferedImage"))

;; Canvas type. Use `get-image` to extract image (`BufferedImage`).
(deftype Canvas [^Graphics2D graphics
                 ^BufferedImage buffer
                 ^Line2D line-obj
                 ^Rectangle2D rect-obj
                 ^Ellipse2D ellipse-obj
                 hints
                 ^long width
                 ^long height
                 transform-stack]
  ImageProto
  (get-image [_] buffer))

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
                             RenderingHints/KEY_TEXT_ANTIALIASING   RenderingHints/VALUE_TEXT_ANTIALIAS_ON}})

;; Following functions and macro are responsible for creating and releasing `Graphics2D` object for a canvas.
;; You have to use `with-canvas` macro to draw on canvas. Internally it's a threading macro and accepts only list of methods which first parameter is canvas object.
;; Please do not use `flush-graphics` and `make-graphics` functions directly. Use `with-canvas` macro instead. I do not check state of `Graphics2D` anywhere.
;; There is one exception: `draw` function associated with Window is already wrapped in `with-canvas` and you can freely use canvas object inside.
;; Another note: `with-canvas` creates it's own copy of `Canvas` object with set `Graphics2D`.

(defn flush-graphics
  "Dispose current `Graphics2D`"
  [^Canvas canvas]
  (.dispose ^Graphics2D (.graphics canvas)))

(defn make-graphics
  "Create new `Graphics2D` object and set renedering hints"
  [^Canvas canvas]
  (let [^Graphics2D ng (.createGraphics ^BufferedImage (.buffer canvas))]
    (.setRenderingHints ng (or (.hints canvas) (rendering-hints :high)))
    (Canvas. ng
             (.buffer canvas)
             (.line-obj canvas)
             (.rect-obj canvas)
             (.ellipse-obj canvas)
             (.hints canvas)
             (.width canvas)
             (.height canvas)
             (atom []))))

(defmacro with-canvas
  "Threading macro which takes care to create and destroy `Graphics2D` object for drawings on canvas. Macro returns result of last call."
  [canvas & body]  
  `(let [newcanvas# (make-graphics ~canvas)
         result# (-> newcanvas#
                     ~@body)]
     (do
       (flush-graphics newcanvas#)
       result#)))

;; Next functions are canvas management functions: create, save, resize and set quality.

(declare set-background)
(declare set-stroke)
(declare image)

(defn create-canvas
  "Create and return canvas with `width`, `height` and quality hint name (keyword). Default hint is `:high`."
  ([^long width ^long height hint]
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
                        nil)]
     (with-canvas result
       (set-background Color/black)
       (set-stroke))))
  ([width height]
   (create-canvas width height :high)))

;; alias for create-canvas
(def make-canvas create-canvas)

(defn resize-canvas
  "Resize canvas to new dimensions. Creates and returns new canvas."
  [^Canvas canvas width height]
  (let [ncanvas (create-canvas width height (.hints canvas))]
    (with-canvas ncanvas
      (image (get-image canvas)))))

(defn save-canvas
  "Save canvas to the file"
  [^Canvas canvas filename]
  (save-image (get-image canvas) filename))

;; ### Drawing functions
;;
;; Here we have basic drawing functions. What you need to remember:
;;
;; * Color is set globally for all figures (exception: `set-background`)
;; * Filled or stroke figures are determined by last parameter `stroke?`. When set to `true` draws figure outline, filled otherwise (default). Default is `false` (filled).
;; * Always use with `with-canvas` macro.
;; 
;; All functions return canvas object

(defn line
  "Draw line from point `(x1,y1)` to `(x2,y2)`"
  [^Canvas canvas x1 y1 x2 y2]
  (let [^Line2D l (.line-obj canvas)]
    (.setLine l x1 y1 x2 y2)
    (.draw ^Graphics2D (.graphics canvas) l))
  canvas)

(defn point
  "Draw point at `(x,y)` position"
  [canvas ^double x ^double y]
  (line canvas x y (+ x 10.0e-6) (+ y 10.0e-6))
  canvas)

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

(defn ellipse
  "Draw ellipse with middle at `(x,y)` position with width `w` and height `h`."
  ([^Canvas canvas x1 y1 w h stroke?]
   (let [^Ellipse2D e (.ellipse_obj canvas)]
     (.setFrame e (- ^double x1 (/ ^double w 2.0)) (- ^double y1 (/ ^double h 2.0)) w h)
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
   (triangle canvas x1 y2 x2 y2 x3 y3 false)))

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

(defn path
  "Draw path from lines. Input: list of Vec2 points, close? - close path or not (default: false), stroke? - draw lines or filled shape (default true - lines)."
  ([^Canvas canvas vs close? stroke?]
   (when-not (empty? vs)
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

(defn set-stroke
  "Set stroke (line) attributes like `cap`, `join` and size. Default `CAP_ROUND` and `JOIN_MITER` is used. Default size is `1.0`."
  ([^Canvas canvas size cap join]
   (.setStroke ^Graphics2D (.graphics canvas) (BasicStroke. size cap join))
   canvas)
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
    (set-color-with-fn set-awt-color canvas c)
    (doto g
      (.fillRect 0 0 (.width canvas) (.height canvas))
      (.setColor currc)))
  canvas)

;; Set color for primitive
(def set-color (partial set-color-with-fn set-awt-color))

;; Set background color
(def set-background (partial set-color-with-fn set-awt-background))

(defn image
  "Draw an image. You can specify position and size of the image. Default it's placed on whole canvas."
  ([^Canvas canvas ^BufferedImage img x y w h]
   (.drawImage ^Graphics2D (.graphics canvas) img x y w h nil)
   canvas)
  ([^Canvas canvas img]
   (image canvas img 0 0 (.width canvas) (.height canvas))))

;; ### Transformations
;;
;; You can transform your working area with couple of functions on canvas. They act exactly the same as in Processing. Transformation context is bound to canvas wrapped to `with-canvas` macro. Each `with-canvas` cleans all transformations.
;; Transformations are concatenated.

(defn scale
  "Scale canvas"
  ([^Canvas canvas ^double scalex ^double scaley]
   (.scale ^Graphics2D (.graphics canvas) scalex scaley)
   canvas)
  ([canvas s] (scale canvas s s)))

(defn translate
  "Translate origin"
  [^Canvas canvas ^double tx ^double ty]
  (.translate ^Graphics2D (.graphics canvas) tx ty)
  canvas)

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
  (when-not (empty? @(.transform-stack canvas))
    (let [v (peek @(.transform-stack canvas))]
      (swap! (.transform-stack canvas) pop)
      (.setTransform ^Graphics2D (.graphics canvas) v)))
  canvas)

(defn reset-matrix
  "Reset transformation and clean stack."
  [^Canvas canvas]
  (.setTransform ^Graphics2D (.graphics canvas) (java.awt.geom.AffineTransform.))
  (reset! (.transform-stack canvas) [])
  canvas)

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
;; * frame count - current number of calls, starting 0
;; * state - any state you want to pass between calls, `nil` initially
;;
;; Function should return current state, which will be passed to function when called next time.
;;
;; Note: calls to `draw` are wrapped in `with-canvas` already.
;;
;; ### Events
;;
;; To control user activities you can use two event processing multimethods.
;;
;; * `key-pressed`
;; * `mouse-event`
;;
;; #### Key event: `key-pressed` multimethod
;;
;; Your dispatch value is vector with window name and pressed key (as `char`) which you want to handle.
;; This means you write different method for different key.
;; As a function parameter you get `KeyEvent` object [java.awt.KeyEvent](https://docs.oracle.com/javase/7/docs/api/java/awt/event/KeyEvent.html)
;;
;; #### Mouse event
;;
;; As as dispatch you use a vector containing window name as a String and mouse event type as a keyword
;; As a parameter you get `MouseEvent` object [java.awt.MouseEvent](https://docs.oracle.com/javase/7/docs/api/java/awt/event/MouseEvent.html)
;;
;; Currently implemented types are:
;;
;; * `:mouse-clicked`
;; * `:mouse-dragged`
;; * `:mouse-pressed`
;; * `:mouse-released`
;;
;; To get mouse position call `(.getX e)` and `(.getY e)` where `e` is MouseEvent object.

;; `Window` type definition, equiped with `get-image` method returning bound canvas' image.
(defrecord Window [^JFrame frame
                   active?
                   buffer
                   ^java.awt.Canvas panel
                   ^double fps
                   ^long width
                   ^long height
                   window-name]
  ImageProto
  (get-image [_] (get-image @buffer)))

;; ### Events function

;; Private method which extracts the name of your window (set when `show-window` is called).

(defn- component-name
  "Returns name of the component. Used to dispatch events."
  [^ComponentEvent e]
  (let [^Component c (.getComponent e)]
    (.getName c)))

;; Multimethod used to process pressed key
(defmulti key-pressed (fn [^KeyEvent e] [(component-name e) (.getKeyChar e)]))
;; Do nothing on default
(defmethod key-pressed :default [e])

;; Map Java mouse event names onto keywords
(def mouse-event-map {MouseEvent/MOUSE_CLICKED  :mouse-clicked
                      MouseEvent/MOUSE_DRAGGED  :mouse-dragged
                      MouseEvent/MOUSE_PRESSED  :mouse-pressed
                      MouseEvent/MOUSE_RELEASED :mouse-released
                      MouseEvent/MOUSE_MOVED    :mouse-moved})

;; Multimethod used to processed mouse events
(defmulti mouse-event (fn [^MouseEvent e] [(component-name e) (mouse-event-map (.getID e))]))
;; Do nothing on default
(defmethod mouse-event :default [e])

;; Event adapter objects.

;; Key
(def key-processor (proxy [KeyAdapter] []
                     (keyPressed [^KeyEvent e] (key-pressed e))))

;; Mouse
(def mouse-processor (proxy [MouseAdapter] []
                       (mouseClicked [^MouseEvent e] (mouse-event e))
                       (mousePressed [^MouseEvent e] (mouse-event e))
                       (mouseReleased [^MouseEvent e] (mouse-event e))))

;; Mouse drag and move
(def mouse-motion-processor (proxy [MouseMotionAdapter] []
                              (mouseDragged [^MouseEvent e] (mouse-event e))
                              (mouseMoved [^MouseEvent e] (mouse-event e))))

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

;; Function used to close and dispose window. As a side effect `active?` atom is set to false.

(defn- close-window
  "Close window frame"
  [^JFrame frame active?]
  (reset! active? false)
  (.dispose frame))

(defn- build-frame
  "Create JFrame object, create and attach panel and do what is needed to show window. Attach key events and closing event."
  [^JFrame frame ^java.awt.Canvas panel active? windowname width height]
  (let [closer (proxy [WindowAdapter] []
                 (windowClosing [^WindowEvent e] (close-window frame active?)))]
    (.add frame panel)
    (doto frame
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
          (.setRenderingHints graphics-context (:mid rendering-hints))
          (.drawImage graphics-context b 0 0 (.getWidth panel) (.getHeight panel) nil)
          (.dispose graphics-context))
        (when (.contentsRestored strategy) (recur)))
      (.show strategy)
      (when (.contentsLost strategy) (recur)))
    (.sync (Toolkit/getDefaultToolkit))))

(defn- refresh-screen-task
  "Repaint canvas on window with set FPS.

  * Input: frame, active? atom, function to run before repaint, canvas and sleep time."
  [panel active? draw-fun buffer stime]  
  (loop [cnt (long 0)
         result nil]
    (let [new-result (when draw-fun 
                       (with-canvas @buffer
                         (draw-fun cnt result)))]
      (Thread/sleep stime)
      (repaint panel @buffer) 
      (when @active? (recur (unchecked-inc cnt) new-result)))))

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

(defn show-window
  "Show window with width/height, name and required fps of refresh. Optionally pass callback function.

  * Input: canvas, window name, width, height, frames per seconds, (optional) `draw` function.
  * Returns `Window` value"
  ([canvas wname width height fps draw-fun]
   (let [active? (atom true)
         buffer (atom canvas)
         frame (JFrame.)
         panel (create-panel buffer wname width height)]
     (SwingUtilities/invokeAndWait #(build-frame frame panel active? wname width height))
     (future (refresh-screen-task panel active? draw-fun buffer (/ 1000.0 ^double fps)))
     (->Window frame
               active?
               buffer
               panel
               fps
               width
               height
               wname)))
  ([canvas wname width height fps]
   (show-window canvas wname width height fps nil)))

(defn window-active?
  "Helper function, check if window is active"
  [^Window window]
  @(.active? window))

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
     #(swap! tick inc)))
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
(def session-agent (agent (map->SessionType {})))

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
    [(.format simple-date-format date) (to-hex (hash date))]))

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
