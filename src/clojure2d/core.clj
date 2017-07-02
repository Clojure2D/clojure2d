;; # Namespace scope
;;
;; This namespace provides functions which cover: diplaying windows with events, canvases, image files and session management.
;; In brief:
;;
;; * Image file read and write, backed by Java ImageIO API. You can read and write BMP, JPG and PNG files. I didn't test WBMP and GIF. Image itself is Java `BufferedImage` in integer ARGB mode. Each pixel is represented as 32bit unsigned integer and 8 bits per channel. See `clojure2d.pixels` namespace for pixels operations.
;; * Canvas with functions to draw on it, represented as vactor of Graphics2d, BufferedImage and quality settings wrapped in atom.
;; * Display (JFrame) with events handlers (multimethods) + associated autorefreshing canvas, and optionally Processiing style `draw` function with context management
;; * Session management: unique identifier generation, logging (different file per session) and unique, sequenced filename creation.
;; * Some general helper functions

(ns clojure2d.core
  "JFrame, Java2D, file io and simple session management"
  (:require [clojure.java.io :refer :all]
            [clojure2d.color :as c])  
  (:import [java.awt.image BufferedImage BufferStrategy]
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

;; To load image from the file, just call `(load-image "filename.jpg")`. Idea is stolen from Processing code. Loading is done via `ImageIcon` class and later converted to BufferedImage in ARGB mode.

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
;; We have two factors here. First is whether file format accepts alpha channel (jpgs, bmps don't) and second is quality settings (for jpg only). In the first case we have to properly flatten the image with `flatten-image` function. In the second case we set quality attributes.

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
  "Save image to the file via writer with parameters"
  ([filename img ^ImageWriter writer]
   (do-save filename img writer (.getDefaultWriteParam writer)))
  ([filename ^BufferedImage img ^ImageWriter writer param]
   (with-open [os (output-stream filename)]
     (doto writer
       (.setOutput (ImageIO/createImageOutputStream os))
       (.write nil (IIOImage. img nil nil) param)
       (.dispose)))))

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
  (let [nimg (flatten-image img)]
    (do-save filename nimg writer)))

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

;; ### Addition functions
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
;; Canvas is an object you can draw on and which can be displayed in the window also saved. Technically it's a type cosisting of `Graphics2D` object which is internally used to draw on the image, image (`BufferedImage` object), rendering quality hints and singletons for primitives. What is important: to draw on canvas you have to wrap your operations in `with-canvas` macro. `with-canvas` is responsible for creating and releasing `Graphics2D` object. Initially `Graphics2D` is set to `nil`.
;; Canvas should be accelerated by Java and your video card.
;; Reminder: Drawing on canvas is single threaded.

(defprotocol ImageProto
  (get-image [t]))

(deftype Canvas [^Graphics2D graphics
                 ^BufferedImage buffer
                 ^Line2D line-obj
                 ^Rectangle2D rect-obj
                 ^Ellipse2D ellipse-obj
                 hints
                 ^long width
                 ^long height]
  ImageProto
  (get-image [_] buffer))

;; First let's define three rendering quality options: `:low`, `:mid` and `:high`. Where `:low` is fastest but has poor quality and `:high` has best quality but may be slow. Rendering options are used when you create canvas.
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
;; There is one exception: your `draw` function is already wrapped in `with-canvas` and you can freely use canvas object inside.

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
             (.height canvas))))

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
  "Create and return canvas with `width`, `height` and quality hint name (keyword). Default hint is `:high`"
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
                        width height)]
     (with-canvas result
       (set-background Color/black)
       (set-stroke))))
  ([width height]
   (create-canvas width height :high)))

;; alias
(def make-canvas create-canvas)

(defn resize-canvas
  "Resize canvas to new dimensions. Creates and returns new canvas."
  [^Canvas canvas width height]
  (let [ncanvas (create-canvas width height (.hints canvas))]
    (with-canvas ncanvas
      (image (.buffer canvas)))))

(comment defn set-canvas-quality
         "Change canvas quality"
         [canvas hints]
         (let [[g b] @canvas]
           (reset! canvas [g b (rendering-hints hints)])))

(defn save-canvas
  "Save canvas to the file"
  [^Canvas canvas filename]
  (save-image (.buffer canvas) filename))

;; ### Drawing functions
;;
;; Here we have basic drawing functions. What you need to remember:
;;
;; * Color is set globally for all figures (exception: `set-background`)
;; * Filled or stroke figures are determined by last parameter `stroke?`. When set to `true` draws figure outline, filled otherwise (default).
;; * Always use with `with-canvas` macro.
;; 
;; All functions return canvas object

;; Since drawing on the canvas is single threaded we can use internal mutable objects to draw things.

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
     (draw-fill-or-stroke ^Graphics2D (.graphics canvas) r stroke?))
   canvas)
  ([canvas x1 y1 w h]
   (rect canvas x1 y1 w h false)))

(defn ellipse
  "Draw ellipse with middle at `(x,y)` position with width `w` and height `h`."
  ([^Canvas canvas x1 y1 w h stroke?]
   (let [^Ellipse2D e (.ellipse_obj canvas)]
     (.setFrame e (- ^double x1 (/ ^double w 2.0)) (- ^double y1 (/ ^double h 2.0)) w h)
     (draw-fill-or-stroke ^Graphics2D (.graphics canvas) e stroke?))
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
     (draw-fill-or-stroke ^Graphics2D (.graphics canvas) p stroke?))
   canvas)
  ([canvas x1 y1 x2 y2 x3 y3]
   (triangle canvas x1 y2 x2 y2 x3 y3 false)))

(defn triangle-strip
  "Draw triangle strip. Implementation of `Processing` `STRIP` shape.

  Input: list of vertices as vectors [x,y]"
  ([canvas vs stroke?]
   (when (> (count vs) 2)
     (loop [v1 (first vs)
            v2 (second vs)
            vss (next (next vs))]
       (when vss
         (let [v3 (first vss)]
           (triangle canvas (v2 0) (v2 1) (v3 0) (v3 1) (v1 0) (v1 1) stroke?)
           (recur v2 v3 (next vss))))))
   canvas)
  ([canvas vs]
   (triangle-strip canvas vs false)))

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
     (draw-fill-or-stroke ^Graphics2D (.graphics canvas) p stroke?))
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

(defn set-awt-color
  "Set color with valid java `Color` object. Use it when you're sure you pass `java.awt.Color`."
  [^Canvas canvas ^java.awt.Color c]
  (.setColor ^Graphics2D (.graphics canvas) c)
  canvas)

(defn set-color
  "Set global color. You can pass:

  * java.awt.Color object
  * clojure2d.math.vector.Vec4 or Vec3 object
  * individual r, g, b (and optional alpha) as integers from 0-255. They are converted to integer and clamped if necessary."
  ([canvas c]
   (set-awt-color canvas (c/make-awt-color c)))
  ([canvas r g b a]
   (set-awt-color canvas (c/make-awt-color r g b a)))
  ([canvas r g b]
   (set-awt-color canvas (c/make-awt-color r g b))))
  
(defn set-awt-background
  "Set background color. Expects valid `Color` object."
  [^Canvas canvas c]
  (let [^Graphics2D g (.graphics canvas)
        ^Color currc (.getColor g)] 
    (set-color canvas c)
    (doto g
      (.fillRect 0 0 (.width canvas) (.height canvas))
      (.setColor currc)))
  canvas)

(defn set-background
  "Set background to the specified color. Current global color is restored. You can pass:

  * java.awt.Color object
  * clojure2d.math.vector.Vec4 or Vec3 object
  * individual r, g, b (and optional alpha) as integers from 0-255. They are converted to integer and clamped if necessary."
  ([canvas c]
   (set-awt-background canvas (c/make-awt-color c)))
  ([canvas r g b a]
   (set-awt-background canvas (c/make-awt-color r g b a)))
  ([canvas r g b]
   (set-awt-background canvas (c/make-awt-color r g b))))

(defn image
  "Draw an image. You can specify position and size of the image. Default it's placed on whole canvas."
  ([^Canvas canvas ^BufferedImage img x y w h]
   (.drawImage ^Graphics2D (.graphics canvas) img x y w h nil)
   canvas)
  ([^Canvas canvas img]
   (image canvas img 0 0 (.width canvas) (.height canvas))))

;; ## Display window
;;
;; You can find here a couple of functions which help to display your canvas and build interaction with user.
;; Display window is just a Swing `JFrame` with `java.awt.Canvas` with periodically repainted bound canvas.
;; What is important, window is not a canvas (like it is in Processing) so first you need to create canvas and then create window displaying it.
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
;; `show-window` returns a vector containing
;;
;; * `JFrame` object
;; * `is-display-running?` atom (see below)
;; *  buffer which is canvas packed into atom (to enable easy canvas replacement)
;; *  map with all above + additional objects
;;    *  `:frame` JFrame object
;;    *  `:is-display-running?`
;;    *  `:buffer`
;;    *  `:panel` `java.awt.Canvas` object placed on JFrame (awt toolkit canvas)
;;    *  `:fps`
;;    *  `:width`
;;    *  `:height`
;;    *  `:name` window name
;;
;; `is-display-running?` atom is unique for each window and has value `true` when window is shown and set to `false` when window is closed with default close button.
;; You can use this atom to control (and possibly stop) all activities which refers to related window. For example you may want to cancel all updating canvas processes when user closes window.
;;
;; See: examples/ex00_display.clj
;;
;; ### Callback function (aka `draw`)
;;
;; This is function with three parameters which is called just before repainting canvas. You can use it to simulate Processing `draw` behaviour. Function should accept following parameters:
;;
;; * canvas - canvas to draw on, canvas bind to window will be passed here
;; * frame count - current number of calls, statring 0
;; * state - any state data you want to pass between calls, `nil` initially
;;
;; Function should return current state, which is subject to pass to function when called next time.
;;
;; Note: calls to `draw` is wrapped in `with-canvas` already.
;;
;; See: examples/ex02_draw.clj
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
;; See: examples/ex01_events.clj
;;
;; #### Mouse event
;;
;; As as dispatch you use a vector containing window name as a String and mouse event type as a keyword
;; As a function parameter you get `MouseEvent` object [java.awt.MouseEvent](https://docs.oracle.com/javase/7/docs/api/java/awt/event/MouseEvent.html)
;;
;; Currently implemented types are:
;;
;; * `:mouse-clicked`
;; * `:mouse-dragged`
;; * `:mouse-pressed`
;; * `:mouse-released`
;;
;; To get mouse position call `(.getX e)` and `(.getY e)` where `e` is MouseEvent object.
;; 
;; See: examples/ex01_events.clj

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
;; Window is JFrame with panel (as java.awt.Canvas object) which is used to draw canvas on it.

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

;; You may want to replace canvas to the other one on window. To make it pass result of `show-window` function and new canvas.
;; Internally it just resets buffer atom for another canvas.
;; See examples/ex01_events.clj to see how it works.

(defn replace-canvas
  "Replace canvas in window.

  * Input: window and new canvas
  * Returns canvas"
  [^Window window canvas]
  (reset! (:buffer window) canvas))

;; Finally function which displays window. Function creates window's visibility status (`active?` atom), buffer as atomized canvas, creates frame, creates refreshing task (repainter) and shows window.

(defn show-window
  "Show window with width/height, name and required fps of refresh. Optionally pass callback function.

  * Input: canvas, window name, width, height, frames per seconds, (optional) `draw` function.
  * Returns vector with: `JFrame` object, visibility status atom, buffer atom (canvas packed into the atom). For convenience last element of the vector is a map with all used objects and passed parameters."
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

(defn make-session-name
  "Create unique session name based on current time. Result is a vector with date and hash represented as hexadecimary number."
  []
  (let [date (java.util.Date.)]
    [(.format simple-date-format date) (to-hex (hash date))]))

;; Logging to file is turned off by default.
(def ^:dynamic *log-to-file* false)

;; Following block defines session related functions. I encapsulates some common variables as atoms or agents. These are:
;;
;; * session name which stores current date (formatted)
;; * file writer as agent used to log your events
;; * file counter - atom which stores current value of files saved under session
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
;;
;; It's not optimized to work parallely

(def session-name (atom nil))
(def session-file (agent nil))
(def session-cnt (atom nil))

(defn close-session
  "Close current session."
  []
  (when-not (nil? @session-file)
    (send session-file (fn [^java.io.Writer o]
                         (.flush o)
                         (.close o)
                         nil)))
  (reset! session-name nil)
  (reset! session-cnt nil))

(defn make-session
  "Create new session and log writer (if `log-to-file` set to true)."
  []
  (let [nname (make-session-name)]
    (reset! session-name nname)

    (when *log-to-file*
      (let [fname (str "log/" (first nname) ".log")]
        (make-parents fname)
        (send session-file (fn [^java.io.Writer o]
                             (do
                               (when-not (nil? o)
                                 (.flush o)
                                 (.close o))
                               (let [^java.io.Writer no (writer fname :append true)]
                                 (.write no (str "Session id: " (second nname) "\n"))
                                 no))))))

    (reset! session-cnt (make-counter 0))
    nname))

(defn log
  "Log message to the session log file."
  [message]
  (when *log-to-file*
    (if (nil? @session-file)
      (do
        (make-session)
        (log message))
      (let [to-log (str (.format simple-date-format-full (java.util.Date.)) ": " message "\n")]
        (send session-file (fn [^java.io.Writer o]
                             (.write o to-log)
                             (.flush o)
                             o)))))
  true)

(defn next-filename
  "Create sequenced filename with prefix (folder name) and optional suffix (eg. file extension)."
  ([prefix]
   (if (nil? @session-name)
     (do
       (make-session)
       (next-filename prefix))
     (str prefix (second @session-name) "_" (format "%06d" (@session-cnt)))))
  ([prefix suffix]
   (str (next-filename prefix) suffix)))

(defn get-session-name
  "Returns current session name (time and hash)"
  []
  @session-name)
