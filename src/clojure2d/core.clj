(ns clojure2d.core
  ""
  (:require [clojure.java.io :refer :all]
            [clojure2d.pixels :as p])  
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
           [clojure2d.pixels Pixels]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; some constants
(def ^:const jpeg-image-quality 0.98)

;; loading image with ImageIcon

(defn load-image 
  ""
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

;; saving canvas with ImageIO

(defn- file-extension-int
  ""
  [filename]
  (let [[_ ext] (re-find #"\.(\w+)$" filename)]
    ext))

(def file-extension (memoize file-extension-int))

(defn- ^ImageWriter get-image-writer
  "return ImageIO ImageWriter based on file extension"
  [filename]
  (let [ext (file-extension filename)
        ^Iterator iter (ImageIO/getImageWritersByFormatName ext)]
    (when (.hasNext iter)
      (.next iter))))

;; necessary to save JPG and BMP
(defn- ^BufferedImage drop-alpha-in-image
  "drop alpha channel, transforming color properly - from processing code"
  [^BufferedImage img]
  (let [w (.getWidth img)
        h (.getHeight img)
        arr (.getRGB img 0 0 w h nil 0 w)
        ^BufferedImage nimg (BufferedImage. w h BufferedImage/TYPE_INT_RGB)]
    (.setRGB nimg 0 0 w h arr 0 w)
    nimg)
  )

(defn- do-save
  "physically save image"
  ([filename ^BufferedImage img ^ImageWriter writer]
   (do-save filename img writer (.getDefaultWriteParam writer)))
  ([filename ^BufferedImage img ^ImageWriter writer ^ImageWriteParam param]
   (with-open [os (output-stream filename)]
     (.setOutput writer (ImageIO/createImageOutputStream os))
     (.write writer nil (IIOImage. img nil nil) param)
     (.dispose writer))))

(defmulti save-to-image (fn [filename ^BufferedImage img ^ImageWriter writer] (keyword (file-extension filename))))

(defmethod save-to-image :jpg
  [filename ^BufferedImage img ^ImageWriter writer]
  (let [^BufferedImage nimg (drop-alpha-in-image img)
        ^ImageWriteParam param (.getDefaultWriteParam writer)]
    (.setCompressionMode param ImageWriteParam/MODE_EXPLICIT)
    (.setCompressionQuality param jpeg-image-quality)
    (do-save filename nimg writer param)))

(defmethod save-to-image :bmp
  [filename ^BufferedImage img ^ImageWriter writer]
  (let [^BufferedImage nimg (drop-alpha-in-image img)]
    (do-save filename nimg writer)))

(defmethod save-to-image :default
  [filename ^BufferedImage img ^ImageWriter writer]
  (do-save filename img writer))

(defn save-image
  ""
  [^BufferedImage b filename]
  (println (str "saving: " filename))
  (make-parents filename)
  (let [^ImageWriter iwriter (get-image-writer filename)
        ext (file-extension filename)]
    (if-not (nil? iwriter)
      (save-to-image filename b iwriter)
      (println (str "can't save an image: " filename)))))

;;
(defn resize-image
  ""
  [^BufferedImage img width height]
  (let [^BufferedImage target (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)
        ^Graphics2D g (.createGraphics target)]
    (.setRenderingHint g RenderingHints/KEY_INTERPOLATION RenderingHints/VALUE_INTERPOLATION_BICUBIC)
    (.setRenderingHint g RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
    (.drawImage g img 0 0 width height nil)
    (.dispose g)
    target))

;;

(defn load-pixels
  "Load pixels from file"
  [n]
  (p/get-image-pixels (load-image n)))

(defn save-pixels
  "Save pixels to file"
  [p n]
  (save-image (p/image-from-pixels p) n))

;;;;;;;;;;;;;;;;;;; CANVAS

;; rendering quality options
(def rendering-hints { :low { RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_OFF
                             RenderingHints/KEY_INTERPOLATION RenderingHints/VALUE_INTERPOLATION_NEAREST_NEIGHBOR
                             RenderingHints/KEY_ALPHA_INTERPOLATION RenderingHints/VALUE_ALPHA_INTERPOLATION_SPEED
                             RenderingHints/KEY_COLOR_RENDERING RenderingHints/VALUE_COLOR_RENDER_SPEED
                             RenderingHints/KEY_RENDERING RenderingHints/VALUE_RENDER_SPEED
                             RenderingHints/KEY_FRACTIONALMETRICS RenderingHints/VALUE_FRACTIONALMETRICS_OFF
                             RenderingHints/KEY_TEXT_ANTIALIASING RenderingHints/VALUE_TEXT_ANTIALIAS_OFF}
                      :mid { RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON
                             RenderingHints/KEY_INTERPOLATION RenderingHints/VALUE_INTERPOLATION_BILINEAR
                             RenderingHints/KEY_ALPHA_INTERPOLATION RenderingHints/VALUE_ALPHA_INTERPOLATION_SPEED
                             RenderingHints/KEY_COLOR_RENDERING RenderingHints/VALUE_COLOR_RENDER_SPEED
                             RenderingHints/KEY_RENDERING RenderingHints/VALUE_RENDER_SPEED
                             RenderingHints/KEY_FRACTIONALMETRICS RenderingHints/VALUE_FRACTIONALMETRICS_OFF
                             RenderingHints/KEY_TEXT_ANTIALIASING RenderingHints/VALUE_TEXT_ANTIALIAS_ON}
                      :high { RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON
                             RenderingHints/KEY_INTERPOLATION RenderingHints/VALUE_INTERPOLATION_BICUBIC
                             RenderingHints/KEY_ALPHA_INTERPOLATION RenderingHints/VALUE_ALPHA_INTERPOLATION_QUALITY
                             RenderingHints/KEY_COLOR_RENDERING RenderingHints/VALUE_COLOR_RENDER_QUALITY
                             RenderingHints/KEY_RENDERING RenderingHints/VALUE_RENDER_QUALITY
                             RenderingHints/KEY_FRACTIONALMETRICS RenderingHints/VALUE_FRACTIONALMETRICS_ON
                             RenderingHints/KEY_TEXT_ANTIALIASING RenderingHints/VALUE_TEXT_ANTIALIAS_ON}})

;;

(defn flush-graphics
  "dispose current graphics2d"
  [canvas]
  (let [[^Graphics2D g b h] @canvas]
    (.dispose g)
    (swap! canvas (fn [c] [nil b h]))))

(defn make-graphics
  ""
  [canvas]
  (let [[_ ^BufferedImage b ^RenderingHints h] @canvas
        ^Graphics2D ng (.createGraphics b)]
    (if h 
      (.setRenderingHints ng h)
      (.setRenderingHints ng (rendering-hints :high))  )
    (swap! canvas (fn [c] [ng b h]))))

(defmacro with-canvas
  [canvas & body]
  `(let [canvas# ~canvas] (do 
             (make-graphics canvas#)
             (-> canvas#
                 ~@body)
             (flush-graphics canvas#)
             canvas#)))
;;

(declare set-background)
(declare set-stroke)
(declare image)

; buffer/canvas is always vector of Graphics2D and BufferedImage
(defn create-canvas
  "Create canvas as vector of graphics2d object and BufferedImage"
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

;; resize

(defn resize-canvas
  ""
  [canvas width height]
  (let [[_ b h] @canvas
        ncanvas (create-canvas width height h)]
    (with-canvas ncanvas
      (image b))))

;; change canvas quality

(defn set-canvas-quality
  ""
  [canvas hints]
  (let [[g b] @canvas]
    (swap! canvas (fn [c] [g b (rendering-hints hints)]))))

;; taking and putting pixel array (as ints)

(defn get-canvas-pixels
  ""
  ([canvas x y w h]
   (let [[_ b] @canvas]
     (p/get-image-pixels b x y w h)))
  ([canvas]
   (let [[_ b] @canvas]
     (p/get-image-pixels b))))

(defn set-canvas-pixels
  ""
  ([canvas p x y]
   (let [[_ b] @canvas]
     (p/set-image-pixels b x y p)))
  ([canvas p]
   (set-canvas-pixels canvas p 0 0)))

(defn save-canvas
  "save image buffer"
  [canvas filename]
  (let [[_ b] @canvas]
    (save-image b filename)))

;; Drawing operations
;; use with `with-canvas` macro

;; mutable objects
(def ^Line2D line-obj (Line2D$Double.))
(def ^Rectangle2D rect-obj (Rectangle2D$Double.))
(def ^Ellipse2D ellipse-obj (Ellipse2D$Double.))

;; drawin objects
(defn line
  ""
  [canvas x1 y1 x2 y2]
  (.setLine line-obj x1 y1 x2 y2)
  (let [[^Graphics2D g] @canvas]
    (.draw g line-obj))
  canvas)

(defn point
  ""
  [canvas x y]
  (line canvas x y (+ x 10.0e-6) (+ y 10.0e-6))
  canvas)

(defn rect
  ""
  [canvas x1 y1 w h]
  (.setFrame rect-obj x1 y1 w h)
  (let [[^Graphics2D g] @canvas]
    (.fill g rect-obj))
  canvas)

(defn ellipse
  ""
  [canvas x1 y1 w h]
  (.setFrame ellipse-obj (- x1 (/ w 2)) (- y1 (/ h 2)) w h)
  (let [[^Graphics2D g _] @canvas]
    (.fill g ellipse-obj))
  canvas)

(defn triangle
  ""
  [canvas x1 y1 x2 y2 x3 y3]
  (let [p (Path2D$Double.)
        [^Graphics2D g] @canvas]
    (doto p
      (.moveTo x1 y1)
      (.lineTo x2 y2)
      (.lineTo x3 y3)
      (.closePath))
    (.fill g p))
  canvas)

(defn quad
  ""
  [canvas x1 y1 x2 y2 x3 y3 x4 y4]
  (let [p (Path2D$Double.)
        [^Graphics2D g] @canvas]
    (doto p
      (.moveTo x1 y1)
      (.lineTo x2 y2)
      (.lineTo x3 y3)
      (.lineTo x4 y4)
      (.closePath))
    (.fill g p))
  canvas)

(defn set-stroke
  ""
  ([canvas size cap join]
   (let [[^Graphics2D g] @canvas]
     (.setStroke g (BasicStroke. size cap join)))
   canvas)
  ([canvas size]
   (set-stroke canvas size BasicStroke/CAP_ROUND BasicStroke/JOIN_MITER))
  ([canvas]
   (set-stroke canvas 1.0)))

(defn set-color
  ""
  [canvas ^Color c]
  (let [[^Graphics2D g] @canvas]
    (.setColor g c))
  canvas)
  
(defn set-background
  ""
  [canvas ^Color c]
  (let [[^Graphics2D g ^BufferedImage b] @canvas
        ^Color currc (.getColor g)] 
    (.setColor g c)
    (.fillRect g 0 0 (.getWidth b) (.getHeight b))
    (.setColor g currc))
  canvas)

(defn image
  ""
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
    (let [thr (future (do
                        (let [curr-res (when draw-fun (draw-fun canvas cnt result))]
                             (doto frame
                               (.validate)
                               (.repaint))
                             curr-res)))]
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
