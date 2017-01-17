;; # Namespace scope
;;
;; This namespace provides basic functions to work with: display windows with events, canvases, image files and session.
;; What's here in brief:
;;
;; * Image file read and write, backed by Java ImageIO API. You can read and write BMP, JPG and PNG files. I didn't test WBMP and GIF. Image itself is Java BufferedImage in integer ARGB mode. Each pixel is represented as 32bit unsigned integer and 8 bits per channel. See `clojure2d.pixels` namespace for pixels operations.
;; * Canvas with functions to draw on it, represented as atom of BufferedImage and Graphics2D objects.
;; * Display (JFrame) with events handlers (multimethods) + assiciated autorefreshing canvas, and if you prefer Processing style calling `draw` function with context
;; * Session management to: get unique identifier, save logs (different file per session) and get unique, sequenced filename.
;; * Some general helper functions

(ns clojure2d.core
  "JFrame, Java2D, file io and simple session management"
  (:require [clojure.java.io :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.vector :as v]
            [clojure2d.color :as c])  
  (:import [java.awt.image BufferedImage]
           [javax.swing ImageIcon]
           [javax.imageio ImageIO ImageWriter ImageWriteParam IIOImage]
           [java.awt Graphics Graphics2D Image RenderingHints GraphicsEnvironment 
            Transparency BasicStroke Color Container Dimension Component]
           [java.util Iterator]
           [java.awt.geom Line2D Line2D$Double
            Rectangle2D Rectangle2D$Double
            Path2D Path2D$Double
            Ellipse2D Ellipse2D$Double]
           [java.awt.event KeyAdapter KeyEvent MouseAdapter MouseMotionAdapter
            MouseEvent WindowAdapter WindowEvent ComponentEvent]
           [javax.swing JFrame JPanel SwingUtilities]
           [clojure2d.math.vector Vec4]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; ## Image

;; Let's start with setting dynamic variable which defines quality of the saved jpeg file. Values are from `0.0` to `1.0`.
;; You can freely change this setting with `binding` macro.
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
;; * call multimethod to prepare data and save chosen format
;;
;; We have two factors here. First is whether file format accepts alpha channel (jpgs, bmps don't) and second is quality settings (for jpg only). In first case we have to properly flatten the image with `flatten-image` function. In second case we set quality attributes.

(defn- file-extension-int
  "Extract extension from filename.

  * Input: image filename
  * Returns extension (without dot)"
  [filename]
  (let [[_ ext] (re-find #"\.(\w+)$" filename)]
    ext))

;; Memoize above function (in case you'll be saving file very often).
(def file-extension (memoize file-extension-int))

(defn- ^ImageWriter get-image-writer
  "Returns image writer of type taken from extension"
  [filename]
  (let [ext (file-extension filename)
        ^Iterator iter (ImageIO/getImageWritersByFormatName ext)]
    (when (.hasNext iter)
      (.next iter))))

(defn- ^BufferedImage flatten-image
  "Flatten image, properly drop alpha channel

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
     (.setOutput writer (ImageIO/createImageOutputStream os))
     (.write writer nil (IIOImage. img nil nil) param)
     (.dispose writer))))

;; Now we define multimethod which saves image. Multimethod is used here because some image types requires additional actions.
(defmulti save-file-type (fn [filename _ _] (keyword (file-extension filename))))

;; JPG requires flatten image and we must set the quality defined in `*jpeg-image-quality*` variable.
(defmethod save-file-type :jpg
  [filename img ^ImageWriter writer]
  (let [nimg (flatten-image img)
        ^ImageWriteParam param (.getDefaultWriteParam writer)]
    (.setCompressionMode param ImageWriteParam/MODE_EXPLICIT)
    (.setCompressionQuality param *jpeg-image-quality*)
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
  * Side effect: saved image and (temporarly printed info about action made)"
  [b filename]
  (println (str "saving: " filename))
  (make-parents filename)
  (let [iwriter (get-image-writer filename)]
    (if-not (nil? iwriter)
      (save-file-type filename b iwriter)
      (println (str "can't save an image: " filename)))))

;; ### Addition functions
;;
;; Just an image resizer with bicubic interpolation. Native `Graphics2D` method is called

(defn resize-image
  "Resize image

  * Input: image and target width and height
  * Returns newly created resized image"
  [img width height]
  (let [^BufferedImage target (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)
        ^Graphics2D g (.createGraphics target)]
    (.setRenderingHint g RenderingHints/KEY_INTERPOLATION RenderingHints/VALUE_INTERPOLATION_BICUBIC)
    (.setRenderingHint g RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
    (.drawImage g img 0 0 width height nil)
    (.dispose g)
    target))

;; ## Canvas
;;
;; Canvas is an object you can draw on and which can be displayed in the window. Technically it's an atom consisting image (`BufferedImage` object), `Graphics2D` object which is internally used to draw on the image and rendering quality hints. What is important: to draw on canvas you have to wrap your operations in `with-canvas` macro. `with-canvas` is responsible for creating and releasing `Graphics2D` object. Initially `Graphics2D` is set to `nil`.
;; Canvas should be accelerated by Java and your video card.
;; Reminder: Drawing on canvas is single threaded.

;; First let's define three rendering quality options: `:low`, `:mid` and `:high`. Where `:low` is fastest but has poor quality and `:high` has best quality but may be slow. Rendering options are used when you create canvas.
(def rendering-hints { :low { RenderingHints/KEY_ANTIALIASING       RenderingHints/VALUE_ANTIALIAS_OFF
                             RenderingHints/KEY_INTERPOLATION       RenderingHints/VALUE_INTERPOLATION_NEAREST_NEIGHBOR
                             RenderingHints/KEY_ALPHA_INTERPOLATION RenderingHints/VALUE_ALPHA_INTERPOLATION_SPEED
                             RenderingHints/KEY_COLOR_RENDERING     RenderingHints/VALUE_COLOR_RENDER_SPEED
                             RenderingHints/KEY_RENDERING           RenderingHints/VALUE_RENDER_SPEED
                             RenderingHints/KEY_FRACTIONALMETRICS   RenderingHints/VALUE_FRACTIONALMETRICS_OFF
                             RenderingHints/KEY_TEXT_ANTIALIASING   RenderingHints/VALUE_TEXT_ANTIALIAS_OFF}
                      :mid  { RenderingHints/KEY_ANTIALIASING       RenderingHints/VALUE_ANTIALIAS_ON
                             RenderingHints/KEY_INTERPOLATION       RenderingHints/VALUE_INTERPOLATION_BILINEAR
                             RenderingHints/KEY_ALPHA_INTERPOLATION RenderingHints/VALUE_ALPHA_INTERPOLATION_SPEED
                             RenderingHints/KEY_COLOR_RENDERING     RenderingHints/VALUE_COLOR_RENDER_SPEED
                             RenderingHints/KEY_RENDERING           RenderingHints/VALUE_RENDER_SPEED
                             RenderingHints/KEY_FRACTIONALMETRICS   RenderingHints/VALUE_FRACTIONALMETRICS_OFF
                             RenderingHints/KEY_TEXT_ANTIALIASING   RenderingHints/VALUE_TEXT_ANTIALIAS_ON}
                      :high { RenderingHints/KEY_ANTIALIASING       RenderingHints/VALUE_ANTIALIAS_ON
                             RenderingHints/KEY_INTERPOLATION       RenderingHints/VALUE_INTERPOLATION_BICUBIC
                             RenderingHints/KEY_ALPHA_INTERPOLATION RenderingHints/VALUE_ALPHA_INTERPOLATION_QUALITY
                             RenderingHints/KEY_COLOR_RENDERING     RenderingHints/VALUE_COLOR_RENDER_QUALITY
                             RenderingHints/KEY_RENDERING           RenderingHints/VALUE_RENDER_QUALITY
                             RenderingHints/KEY_FRACTIONALMETRICS   RenderingHints/VALUE_FRACTIONALMETRICS_ON
                             RenderingHints/KEY_TEXT_ANTIALIASING   RenderingHints/VALUE_TEXT_ANTIALIAS_ON}})

;; Following functions and macro are responsible for creating and releasing `Graphics2D` object for a canvas.
;; You have to use `with-canvas` macro to draw on canvas. Internally it's a threading macro and accepts only list of methods which first parameter is canvas object.
;; Please do not use `flush-graphics` and `make-graphics` functions on your own. Use `with-canvas` macro instead. I do not check state of `Graphics2D`.

(defn flush-graphics
  "Dispose current `Graphics2D`"
  [canvas]
  (let [[^Graphics2D g b h] @canvas]
    (.dispose g)
    (swap! canvas (fn [c] [nil b h]))))

(defn make-graphics
  "Create new `Graphics2D` object and set renedering hints"
  [canvas]
  (let [[_ ^BufferedImage b ^RenderingHints h] @canvas
        ^Graphics2D ng (.createGraphics b)]
    (if h 
      (.setRenderingHints ng h)
      (.setRenderingHints ng (rendering-hints :high))  )
    (swap! canvas (fn [c] [ng b h]))))

(defmacro with-canvas
  "Make sure that your operations have freshly created `Graphics2D` object and object is released at the end."
  [canvas & body]
  `(let [canvas# ~canvas]
     (do 
       (make-graphics canvas#)
       (-> canvas#
           ~@body)
       (flush-graphics canvas#)
       canvas#)))

;; Next functions are canvas management functions: create, save, resize and set quality.

(declare set-background)
(declare set-stroke)
(declare image)

(defn create-canvas
  "Create and return canvas with `width`, `height` and hint name (keyword). If hint is not passed use `:high` variant."
  ([width height hints]
   (let
       [^BufferedImage buffer (.. GraphicsEnvironment 
                                  (getLocalGraphicsEnvironment)
                                  (getDefaultScreenDevice)
                                  (getDefaultConfiguration)
                                  (createCompatibleImage width height Transparency/TRANSLUCENT))
        h (rendering-hints hints)
        result (atom [nil buffer h])]
     (with-canvas result
       (set-background Color/black)
       (set-stroke))))
  ([width height]
   (create-canvas width height :high)))

;; Alias subject to refactor
(def make-canvas create-canvas)

(defn resize-canvas
  "Resize canvas to new dimensions. Creates and returns new canvas."
  [canvas width height]
  (let [[_ b h] @canvas
        ncanvas (create-canvas width height h)]
    (with-canvas ncanvas
      (image b))))

(defn set-canvas-quality
  "Change canvas quality"
  [canvas hints]
  (let [[g b] @canvas]
    (swap! canvas (fn [c] [g b (rendering-hints hints)]))))

(defn save-canvas
  "Save canvas to the file"
  [canvas filename]
  (let [[_ b] @canvas]
    (save-image b filename)))

;; ### Drawing functions
;;
;; Here we have basic drawing functions. What you need to remember:
;;
;; * Color is set globally for all figures (exception: `set-background`)
;; * Filled or stroke figures are determined by last parameter `stroke?`. When `true` draw figure outline, filled otherwise.
;; * Always use with `with-canvas` macro.

;; Since drawing on the canvas is single threaded we can use internal mutable objects to draw things.
(def ^Line2D line-obj (Line2D$Double.))
(def ^Rectangle2D rect-obj (Rectangle2D$Double.))
(def ^Ellipse2D ellipse-obj (Ellipse2D$Double.))

(defn line
  "Draw line from point `(x1,y1)` to `(x2,y2)`"
  [canvas x1 y1 x2 y2]
  (.setLine line-obj x1 y1 x2 y2)
  (let [[^Graphics2D g] @canvas]
    (.draw g line-obj))
  canvas)

(defn point
  "Draw point at `(x,y)` position"
  [canvas x y]
  (line canvas x y (+ x 10.0e-6) (+ y 10.0e-6))
  canvas)

(defn draw-fill-or-stroke
  "Draw filled or stroked object."
  [^Graphics2D g obj stroke?]
  (if stroke?
    (.draw g obj)
    (.fill g obj)))

(defn rect
  "Draw rectangle with top-left corner at `(x,y)` position with width `w` and height `h`."
  ([canvas x1 y1 w h stroke?]
   (.setFrame rect-obj x1 y1 w h)
   (draw-fill-or-stroke (@canvas 0) rect-obj stroke?)
   canvas)
  ([canvas x1 y1 w h]
   (rect canvas x1 y1 w h false)))

(defn ellipse
  "Draw ellipse with middle at `(x,y)` position with width `w` and height `h`."
  ([canvas x1 y1 w h stroke?]
   (.setFrame ellipse-obj (- x1 (/ w 2)) (- y1 (/ h 2)) w h)
   (draw-fill-or-stroke (@canvas 0) ellipse-obj stroke?)
   canvas)
  ([canvas x1 y1 w h]
   (ellipse canvas x1 y1 w h false)))

(defn triangle
  "Draw triangle with corners at 3 positions."
  ([canvas x1 y1 x2 y2 x3 y3 stroke?]
   (let [p (Path2D$Double.)]
     (doto p
       (.moveTo x1 y1)
       (.lineTo x2 y2)
       (.lineTo x3 y3)
       (.closePath))
     (draw-fill-or-stroke (@canvas 0) p stroke?))
   canvas)
  ([canvas x1 y1 x2 y2 x3 y3]
   (triangle canvas x1 y2 x2 y2 x3 y3 false)))

(defn triangle-strip
  "Draw triangle strip.

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
  ([canvas x1 y1 x2 y2 x3 y3 x4 y4 stroke?]
   (let [p (Path2D$Double.)]
     (doto p
       (.moveTo x1 y1)
       (.lineTo x2 y2)
       (.lineTo x3 y3)
       (.lineTo x4 y4)
       (.closePath))
     (draw-fill-or-stroke (@canvas 0) p stroke?))
   canvas)
  ([canvas x1 y1 x2 y2 x3 y3 x4 y4]
   (quad canvas x1 y1 x2 y2 x3 y3 x4 y4 false)))

(defn set-stroke
  "Set stroke (line) attributes like `cap`, `join` and size. Default `CAP_ROUND` and `JOIN_MITER` is used. Default size is `1.0`."
  ([canvas size cap join]
   (let [[^Graphics2D g] @canvas]
     (.setStroke g (BasicStroke. size cap join)))
   canvas)
  ([canvas size]
   (set-stroke canvas size BasicStroke/CAP_ROUND BasicStroke/JOIN_MITER))
  ([canvas]
   (set-stroke canvas 1.0)))

(defn set-color
  "Set global color. You can pass:

  * java.awt.Color object
  * clojure2d.math.vector.Vec4 object
  * individual r, g, b (and optional alpha) as integers from 0-255. They are converted to integer and clamped if necessary."
  ([canvas c]
   (let [[^Graphics2D g] @canvas
         cc (if (instance? Vec4 c)
              (c/to-color c)
              c)]
     (.setColor g c))
   canvas)
  ([canvas r g b a]
   (set-color canvas (Color. ^int (c/clamp255 r)
                             ^int (c/clamp255 g)
                             ^int (c/clamp255 b)
                             ^int (c/clamp255 a))))
  ([canvas r g b]
   (set-color canvas r g b 255)))
  
(defn set-background
  "Set background to the specified color. Current global color is restored."
  [canvas c]
  (let [[^Graphics2D g ^BufferedImage b] @canvas
        ^Color currc (.getColor g)] 
    (set-color canvas c)
    (.fillRect g 0 0 (.getWidth b) (.getHeight b))
    (.setColor g currc))
  canvas)

(defn image
  "Draw an image. You can specify position and size of the image. Default it's placed on whole canvas."
  ([canvas ^BufferedImage img x y w h]
   (let [[^Graphics2D g] @canvas]
     (.drawImage g img x y w h nil)
     canvas))
  ([canvas img]
   (let [[_ ^BufferedImage b] @canvas] 
     (image canvas img 0 0 (.getWidth b) (.getHeight b)))))


;;;;;;;;;;;;;;;;;;; DISPLAY

;; ## Display window
;;
;; You can find here a couple of functions which help to display your canvas and build interaction with user.
;; Display window is just a JPanel with periodically repainted external canvas.
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
;; * optionally callback function name which is called just before repainting the canvas (like `draw` in Processing)
;;
;; `show-window` returns a vector containing `JFrame` object and an atom `is-display-running?`.
;;
;; `is-display-running?` atom is unique for each window and has value `true` when window is shown and set to `false` when window is closed with default close button.
;;
;; You can use this atom to control (and possibly stop) all activities refering to related window. For example you may want to cancel all updating canvas processing when user closes window.
;;
;; See: examples/ex00_display.clj
;;
;; ### Callback function (aka `draw`)
;;
;; This is one parameter function which is called just before repainting canvas. You can use it to simulate Processing `draw` behaviour. Function gets current frame count as parameter.
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
;; #### Key event
;;
;; As a dispatch you get a vector containing `windowname` as a String and pressed key as a char.
;; As a function parameter you get `KeyEvent` object [java.awt.KeyEvent](https://docs.oracle.com/javase/7/docs/api/java/awt/event/KeyEvent.html)
;;
;; See: examples/ex01_events.clj
;;
;; #### Mouse event
;;
;; As as dispatch you get a vector containing `windowname` as a String and mouse event type as a keyword
;; As a function parameter you get `MouseEvent` object [java.awt.MouseEvent](https://docs.oracle.com/javase/7/docs/api/java/awt/event/MouseEvent.html)
;;
;; Currently implemented types are:
;;
;; * `:mouse-clicked`
;; * `:mouse-dragged`
;; * `:mouse-pressed`
;; * `:mouse-released`
;;
;; To get mouse position call `(.getX e)` and `(getY e)` where `e` is MouseEvent object.
;; 
;; See: examples/ex01_events.clj

;; Function used to close and dispose window. As a side effect `is-display-running?` atom is set to false
(defn close-window
  "Close window frame"
  [^JFrame frame is-display-running?]
  (reset! is-display-running? false)
  (.dispose frame))

(defn- component-name
  "Return name of the component. Used to dispatch events"
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
                       MouseEvent/MOUSE_RELEASED :mouse-released})

;; Multimethod used to processed mouse events
(defmulti mouse-event (fn [^MouseEvent e] [(component-name e) (mouse-event-map (.getID e))]))
;; Do nothing on default
(defmethod mouse-event :default [e])

;; Event adapter objects
;; Key
(def key-processor (proxy [KeyAdapter] []
                     (keyPressed [^KeyEvent e] (key-pressed e))))

;; Mouse
(def mouse-processor (proxy [MouseAdapter] []
                       (mouseClicked [^MouseEvent e] (mouse-event e))
                       (mousePressed [^MouseEvent e] (mouse-event e))
                       (mouseReleased [^MouseEvent e] (mouse-event e))))
;; Mouse drag
(def mouse-motion-processor (proxy [MouseMotionAdapter] []
                              (mouseDragged [^MouseEvent e] (mouse-event e))))

(defn- create-panel
  "Create panel which displays canvas. Attach mouse events, give a name (same as window)."
  [^BufferedImage buffer windowname width height]
  (let [panel (proxy [JPanel] []
                 (paint [^Graphics graphics-context]
                   (let [^JPanel this this
                         ^Graphics2D graphics-context graphics-context] ; avoid reflection hint
                     (.setRenderingHints graphics-context (rendering-hints :high))
                     (.drawImage graphics-context buffer 0 0 (.getWidth this) (.getHeight this) this))))]
    (doto panel
      (.setName windowname)
      (.addMouseListener mouse-processor)
      (.addMouseMotionListener mouse-motion-processor)
      (.setPreferredSize (Dimension. width height)))))

(defn- build-frame
  "Create JFrame object, attach JPanel and do what is needed to show desired window. Attach key events."
  [^JFrame frame is-display-running? buffer windowname width height]
  (let [^JPanel panel (create-panel buffer windowname width height)
        closer (proxy [WindowAdapter] []
                 (windowClosing [^WindowEvent e] (close-window frame is-display-running?)))]
    (.add (.getContentPane frame) panel)
    (doto frame
      (.addKeyListener key-processor)
      (.setResizable false)
      (.pack)
      (.setDefaultCloseOperation JFrame/DO_NOTHING_ON_CLOSE)
      (.addWindowListener closer)
      (.setName windowname)
      (.setTitle windowname)
      (.setBackground Color/white)
      (.setVisible true))))

(defn- refresh-screen-task
  "Task repainting canvas on window. Repaints with set FPS."
  [^JFrame frame is-display-running? draw-fun canvas stime]
  (loop [cnt 0
         result nil]
    (let [thr (future (let [curr-res (when draw-fun (draw-fun canvas cnt result))]
                        (doto frame
                          (.validate)
                          (.repaint))
                        curr-res))]
      (Thread/sleep stime)
      (when @is-display-running? (recur (inc cnt) @thr)))))

;; You may want to replace canvas to the other one on window. To make it pass `JFrame` object and new canvas.
;; See examples/ex01_events.clj to see how it works.
;; TODO: when replaceing canvas, you loose refresh-screen-task, try to recover it
(defn replace-canvas
  "Replace canvas in window"
  [[^JFrame frame _] canvas]
  (let [[_ ^BufferedImage b] @canvas
        ^Container container (.getContentPane frame)
        ^JPanel panel (create-panel b (.getName frame) (.getWidth container) (.getHeight container))] 
    (doto container
      (.removeAll)
      (.add panel)))
  (doto frame
    (.pack)
    (.invalidate)
    (.repaint)))

(defn show-window
  "Show window with width/height, name and required fps of refresh. Optionally pass callback function"
  ([canvas wname width height fps draw-fun]
   (let [[_ ^BufferedImage b] @canvas
         is-display-running? (atom true)
         ^JFrame frame (JFrame.)]
     (SwingUtilities/invokeLater #(build-frame frame is-display-running? b wname width height))
     (future (refresh-screen-task frame is-display-running? draw-fun canvas (/ 1000.0 fps)))
     [frame is-display-running?]))
  ([canvas wname width height fps]
   (show-window canvas wname width height fps nil)))


;; various utilities

(defn to-hex
  "return hex value of given number, padded with leading zeroes if given length"
  ([n]
   (format "%X" n))
  ([n pad]
   (format (str "%0" pad "X") n)))

(defmacro time-with-name
  "Evaluates expr and prints the time it took.  Returns the value of
 expr."
  [ss expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     (prn (str ~ss " Elapsed time: " (/ (double (- (. System (nanoTime)) start#)) 1000000.0) " msecs"))
     ret#))

;; array

(defn array-mutate!
  "Mutate int array value with function f"
  [f ^ints array idx]
  (let [v (aget array idx)]
    (aset array idx ^int (f v))))

(defmacro amap!
  "Mutating version of amap"
  {:added "1.0"}
  [a idx expr]
  `(let [a# ~a]
     (loop  [~idx 0]
       (if (< ~idx  (alength a#))
         (do
           (aset-int a# ~idx ~expr)
           (recur (unchecked-inc ~idx)))
         a#))))

(defn aget-2d
  "Get value from int array, treat as 2d"
  [^ints array w h x y]
  (if (or (neg? x)
          (neg? y)
          (>= x w)
          (>= y h))
    (aget-2d array w h (m/constrain x 0 (dec w)) (m/constrain y 0 (dec h)))
    (aget array (int (+ x (* y w))))))

(defn array-clone
  "Clone array using System/arraycopy"
  [^ints array]
  (let [len (int (alength array))
        res (int-array len)]
      (System/arraycopy array 0 ^ints res 0 len)
      res))

;;
(defn make-counter [v] 
  (let [tick (atom (dec v))]
    #(swap! tick inc)))

(defn make-session-name
  ""
  ([]
   (let [^java.text.SimpleDateFormat sdf (java.text.SimpleDateFormat. "yyyyMMddHHmmss")
         date (java.util.Date.)]
     [(.format sdf date) (to-hex (hash date))])))

(def ^:dynamic *log-to-file* false)

(let [session-name (atom nil) ; store session name as a current date
      session-file (agent nil) ; logger Writer, created and used when *log-to-file* is true
      session-cnt (atom nil) ; counter for next filename fn
      ^java.text.SimpleDateFormat sdf (java.text.SimpleDateFormat. "yyyy-MM-dd HH:mm:ss")
      nilfn (fn [_] nil)]

  (defn close-session
    ""
    []
    (when-not (nil? @session-file)
      (send session-file (fn [^java.io.Writer o]
                           (.flush o)
                           (.close o)
                           nil)))
    (swap! session-name nilfn)
    (swap! session-cnt nilfn))

  (defn make-session
    ""
    []
    (let [nname (make-session-name)]
      (swap! session-name (fn [_] nname))

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

      (swap! session-cnt (fn [_] (make-counter 0)))))

  (defn log
    ""
    [s]
    (when *log-to-file*
      (if (nil? @session-file)
        (do
          (make-session)
          (log s))
        (let [to-log (str (.format sdf (java.util.Date.)) ": " s "\n")]
          (send session-file (fn [^java.io.Writer o]
                               (.write o to-log)
                               (.flush o)
                               o)))))
    true)

  (defn next-filename
    ""
    ([prefix]
     (if (nil? @session-name)
       (do
         (make-session)
         (next-filename prefix))
       (str prefix (second @session-name) "_" (format "%06d" (@session-cnt)))))
    ([prefix suffix]
     (str (next-filename prefix) suffix)))

  (defn get-session-name
    ""
    []
    @session-name)


)
