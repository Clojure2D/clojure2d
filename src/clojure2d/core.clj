(ns clojure2d.core
  "Main Clojure2d entry point for Canvas, Window and drawing generatively.

  Basic concepts:

  * Image - `BufferedImage` java object used to store ARGB color information.
  * Canvas - Image which contains graphical context. You draw on it. Similar to processing Graphics object.
  * Window - Window which can display canvas, process events, keeps app/script concept. Similar to Processing sketch with display.
  * Events - Mouse and keyboard events

  Protocols:

  * [[ImageProto]] - basic Image operations (Image, Canvas, Window and Pixels (see [[clojure2d.pixels]]) implement this protocol.
  * Various events protocols. Events and Window implement these:
      * [[MouseXYProto]] - mouse position related to Window.
      * [[MouseButtonProto]] - status of mouse buttons.
      * [[KeyEventProto]] - keyboard status
      * [[ModifiersProto]] - status of special keys (Ctrl, Meta, Alt, etc.)
  * Additionally Window implements [[PressedProto]] in case you want to check in draw loop if your mouse or key is pressed.

  ## Image

  Image is `BufferedImage` java object. Image can be read from file using [[load-image]] function or saved to file with [[save]]. ImageProto provides [[get-image]] function to access to Image object directly (if you need)
  There is no function which creates Image directly (use Canvas instead).

  ### SVG

  To load SVG use `load-svg` which creates internal Batik object. Object can be rendered to Image with `transcode-svg`.
  
  ## Canvas

  Canvas is an object which is used to draw on it. To create new one call [[canvas]] function. Provide width and height and optionally quality hint.

  Quality hints are as follows:

  * `:low` - no antialiasing, speed optimized rendering
  * `:mid` - antialiasing, speed optimized rendering
  * `:high` - antialiasing, quality optimized rendering (default)
  * `:highest` - as `:high` plus `PURE_STROKE` hint, which can give strange results in some cases.

  To draw on Canvas you have to create graphical context. Wrap your code into one of two functions:

  * [[with-canvas]] - binding macro `(with-canvas [local-canvas canvas-object] ...)`
  * [[with-canvas->]] - threading macro `(with-canvas-> canvas ...)`.

  Each function in this macro has to accept Canvas as first parameter and return Canvas.

  Canvas bound to Window and accessed via callback drawing function (a'ka Processing `draw()`) has graphical context created automatically.
  
  ## Events

  Java2d keyboard and mouse event handlers can be defined as custom multimethods separately for each window.
  There are following options:

  * Handlers for particular key with dispatch as a vector of window name and key character (eg. `[\"name\" \\c]`)
      * [[key-pressed]] - when key is pressed
      * [[key-released]] - when key is released
      * [[key-typed]] - when key is typed
  * Handler for given key event with dispatch as a vector of window name and key event (eg. `[\"name\" :key-pressed]`)
  * Handler for mouse event with dispatch as a vector of window name and mouse event (eg. `[\"name\" :mouse-dragged]`)

  Every event handler accepts as parameters:

  * Event object (java KeyEvent or MouseEvent) - access to the fields through defined protocols
  * Global state - state attached to window

  Event handler should return new global state.
  
  ## Window

  Window object is responsible for displaying canvas content and processing events. You can also initialize states here.

  To create window and display it call [[show-window]]. Function accepts several parameters which are described below.

  Window itself simulates workflow which is available in Processing/Quil frameworks.
  
  ### Internals

  When window is created following things are done:

  1. Check parameters and create missing values for missed ones.
  2. If `:setup` function is provided, call it and use returned value as `:draw-state`
  3. Create JFrame, java.awt.Canvas, pack them, attach event handlers and display
  4. Set global state
  5. Run separated thread which refreshes display with given `:fps`, if `:draw-fn` is available it's called before refreshment.

  Additional informations:

  * Display refreshment is done by displaying canvas on JFrame. You can set separate quality hints (same as for canvas) for this process with `:hint` parameter.
  * When you privide drawing function, it's called every refreshment. By default graphical context is created every call - which costs time but is safe in case you want to directly access pixels. The second variant which can be used is to create graphical context once at the moment of window creation. This variant can be forced by setting `:refresher` parameter to `:fast`.
  * You can replace canvas attached to window with [[replace-canvas]] function.
  * Window itself acts as event object (implements all event protocols)
  * Canvas and window can have different sizes. Display refreshing functions will scale up/down in such case.
  * Events and refreshment are not synchronized. Try to avoid drawing inside event multimethods.
  * You can create as many windows as you want.
  * You can check if window is visible or not with [[window-active?]] function.
  * If you provide both `:draw-state` and `:setup`. Value returned by `:setup` has a precedence unless is `nil` or `false`.
   
  ### Parameters

  Following parameters are used:

  * `:canvas` - canvas which is displayed on window. Default is 200x200px
  * `:window-name` - name of the window as a string. Used for event multimathods dispatch.
  * `:w` - width of the window. Default width of the canvas
  * `:h` - height of the window. Default height of the canvas
  * `:fps` - frames per second, defaults to 60
  * `:draw-fn` - drawing function, called before every display refreshment. Function should accept following four parameters:
      * canvas within graphical context (you don't need to use [[with-canvas]] or [[with-canvas->]] wrappers.
      * window object
      * current frame number as a long value
      * current state
  * `:setup` - setup function which should accept two parameters and return initial draw state.
      * canvas withing graphical context
      * window object
  * `:state` - initial global state
  * `:draw-state` - initial local (drawing) state. If `setup` is provided, value returned by it will be used instead.
  * `:hint` - display quality hint. Use it when window and canvas have different sizes
  * `:refresher` - when create graphical context for draw: `:fast` for once or `:safe` for each call (default).

  ## States

  There are two states managed by library: global state connected to window and draw state connected to callback drawing function.

  ### Global state

  Each window has its own state kept in `atom`. The main idea is to have data which flow between event calls. Every event function accepts state and should return state data.
  Initial state can be set with [[show-window]] `:state` parameter 
  To access current state from outside the flow call [[get-state]]. You can also mutate the state with [[set-state!]].

  ### Local state for drawing function

  When drawing callback is used you can keep state between calls. What is returned by callback is passed as a parameter in next call. Drawing function is not synchronized with events that's why local state is introduced. You can still access and change global state.

  You can init state from [[show-window]] with `:draw-state` or `:setup` parameters.

  ## How to draw

  There are plenty of functions which you can use to draw on canvas. They can be grouped to:

  * Primitives like [[point]], [[rect]], etc.
  * Tranformations like [[translate]], [[rotate]], etc.
  * Text rendering like [[text]], [[set-font-attributes]], etc.
  * Image manipulations like [[convolve]]
  * Color and style like [[set-color]], [[gradient-mode]], [[set-background]], [[set-stroke]],  etc.

  All operate on canvas and return canvas as a result. Obviously canvas is mutated.
  
  ## Session

  Session is a datetime with its hash kept globally in vector. To access current session names call [[session-name]].

  Following functions rely on session:

  * [[next-filename]] - generate unique filename based on session
  * [[log]] - save any information to the file under name based on session. See [[log-name]].

  Session is created automatically when needed. Session management functions are:

  * [[make-session]] - create new session
  * [[ensure-session]] - create new session when there is no one
  * [[close-session]] - close current session
  * [[session-name]] - returns current session.
  
  ## Utilities

  Additional utility functions
  
  * date and time functions
  * [[to-hex]] formatter"
  {:metadoc/categories {:image "Image functions"
                        :canvas "Canvas functions"
                        :draw "Drawing functions"
                        :display "Screen"
                        :window "Window"
                        :events "Events"
                        :dt "Date / time"
                        :session "Session"
                        :write "Text / font"
                        :transform "Transform canvas"}}
  (:require [clojure.java.io :refer :all]
            [clojure2d.color :as c]
            [fastmath.vector :as v]
            [fastmath.core :as m]
            [clojure.reflect :as ref]
            [fastmath.random :as r]
            [clojure.string :as s]
            [fastmath.grid :as grid]
            [clojure.java.io :as io])
  (:import [java.awt BasicStroke Color Component Dimension Graphics2D GraphicsEnvironment Image RenderingHints Shape Toolkit Transparency]
           [java.awt.event InputEvent ComponentEvent KeyAdapter KeyEvent MouseAdapter MouseEvent MouseMotionAdapter WindowAdapter WindowEvent]
           [java.awt.geom Ellipse2D Ellipse2D$Double Line2D Line2D$Double Path2D Path2D$Double Rectangle2D Rectangle2D$Double Point2D Point2D$Double Arc2D Arc2D$Double]
           [java.awt.image BufferedImage BufferStrategy Kernel ConvolveOp]
           [java.util Iterator Calendar]
           [javax.imageio IIOImage ImageIO ImageWriteParam ImageWriter]
           [javax.swing ImageIcon JFrame SwingUtilities]
           [org.apache.batik.transcoder.image ImageTranscoder]
           [org.apache.batik.transcoder TranscoderInput]
           [org.w3c.dom Document]
           [org.apache.batik.util SVGConstants XMLResourceDescriptor ]
           [org.apache.batik.dom.util SAXDocumentFactory]
           [org.apache.batik.anim.dom SVGDOMImplementation]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

;; how many tasks we can run (one less than available cores)?
(def ^:const ^long
  ^{:doc "How much processor cores are in system. Constant is machine dependant."}
  available-cores (.availableProcessors (Runtime/getRuntime)))
(def ^:const ^long
  ^{:doc "How much intensive tasks can we run. Which is 150% of available cores. Constant is machine dependant."}
  available-tasks (m/round (* 1.5 available-cores)))

;; ## Image

(def ^{:doc "Default quality of saved jpg (values: 0.0 (lowest) - 1.0 (highest)"
       :metadoc/categories #{:image}}
  ^:dynamic *jpeg-image-quality* 0.97)

;; ### Load image

(defn- force-argb-image
  "Create ARGB buffered image from given image."
  [^Image img]
  (let [^BufferedImage bimg (BufferedImage. (.getWidth img nil) (.getHeight img nil) BufferedImage/TYPE_INT_ARGB)
        ^Graphics2D gr (.createGraphics bimg)]
    (.drawImage gr img 0 0 nil)
    (.dispose gr)
    (.flush img)
    bimg))

(defn load-image 
  "Load Image from file.

  * Input: Image filename with absolute or relative path (relative to your project folder)
  * Returns BufferedImage object or nil when Image can't be loaded.

  For supported formats check [[img-reader-formats]]."
  {:metadoc/categories #{:image}}
  [^String filename]
  (try
    (force-argb-image (.getImage (ImageIcon. filename)))
    (catch Exception e (println "Can't load image: " filename " " (.getMessage e)))))

(defn load-url-image
  "Load image from given URL"
  [^String url]
  (try
    (force-argb-image (ImageIO/read (io/as-url url)))
    (catch Exception e (println "Can't load image from URL: " url " " (.getMessage e)))))

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
  {:metadoc/categories #{:image}}
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

(defn- formats->names
  "Convert possible formats to names' set."
  [formats]
  (->> (seq formats)
       (map s/lower-case)
       (set)))

(def
  ^{:doc "Supported writable image formats. Machine/configuration dependant."
    :metadoc/categories #{:image}}
  img-writer-formats (formats->names (ImageIO/getWriterFormatNames)))

(def
  ^{:doc "Supported readable image formats. Machine/configuration dependant."
    :metadoc/categories #{:image}}
  img-reader-formats (formats->names (ImageIO/getReaderFormatNames)))

;; Now we define multimethod which saves image. Multimethod is used here because some image types requires additional actions.
(defmulti save-file-type
  "Save Image to the file.

  Preprocess if necessary (depends on file type). For supported formats check [[img-writer-formats]].

  Dispatch is based on extension as keyword, ie. \".jpg\" -> `:jpg`."
  {:metadoc/categories #{:image}}
  (fn [filename _ _] (keyword (file-extension filename))))

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
  * Side effect: saved image

  Image is saved using [[save-file-type]] multimethod."
  {:metadoc/categories #{:image}}
  [b filename]
  (println (str "saving: " filename "..."))
  (make-parents filename)
  (let [iwriter (get-image-writer filename)]
    (if-not (nil? iwriter)
      (do
        (save-file-type filename b iwriter)
        (println "...done!"))
      (println (str "can't save an image: " filename)))))

;;

(defprotocol ImageProto
  "Image Protocol"
  (^{:metadoc/categories #{:image :canvas :window}} get-image [i] "Return BufferedImage")
  (^{:metadoc/categories #{:image :canvas :window}} width [i] "Width of the image.")
  (^{:metadoc/categories #{:image :canvas :window}} height [i] "Height of the image.")
  (^{:metadoc/categories #{:image :canvas :window}} save [i n] "Save image `i` to a file `n`.")
  (^{:metadoc/categories #{:image :canvas :window}} convolve [i t] "Convolve with Java ConvolveOp. See [[convolution-matrices]] for kernel names.")
  (^{:metadoc/categories #{:image :canvas :window}} subimage [i x y w h] "Return part of the image.")
  (^{:metadoc/categories #{:image :canvas}} resize [i w h] "Resize image."))

(declare set-rendering-hints-by-key)

(defn- resize-image
  "Resize Image.

  * Input: image and target width and height
  * Returns newly created resized image"
  {:metadoc/categories #{:image}}
  [img width height]
  (let [^BufferedImage target (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)
        ^Graphics2D g (.createGraphics target)]
    (set-rendering-hints-by-key g :high)
    (doto g
      (.drawImage img 0 0 width height nil)
      (.dispose))
    target))

(defn- get-subimage
  "Get subimage of give image"
  [^BufferedImage source x y w h]
  (.getSubimage source x y w h))

;; ## Screen info

(defn- ^Dimension screen-size
  "Screen size from java.awt.Toolkit."
  [] (.getScreenSize (Toolkit/getDefaultToolkit)))

(defn screen-width
  "Returns width of the screen."
  {:metadoc/categories #{:display}}
  ^long [] (long (.getWidth (screen-size))))

(defn screen-height 
  "Returns height of the screen." 
  {:metadoc/categories #{:display}}
  ^long [] (long (.getHeight (screen-size))))

;;

(def ^{:doc "Java ConvolveOp kernels. See [[convolve]]."
       :metadoc/categories #{:image}}
  convolution-matrices {:shadow          (Kernel. 3 3 (float-array [0 1 2 -1 0 1 -2 -1 0]))
                        :emboss          (Kernel. 3 3 (float-array [0 2 4 -2 1 2 -4 -2 0]))
                        :edges-1         (Kernel. 3 3 (float-array [1 2 1 2 -12 2 1 2 1]))
                        :edges-2         (Kernel. 3 3 (float-array [1 0 -1 0 0 0 -1 0 1]))
                        :edges-3         (Kernel. 3 3 (float-array [0 1 0 1 -4 1 0 1 0]))
                        :edges-4         (Kernel. 3 3 (float-array [-1 -1 -1 -1 8 -1 -1 -1 -1]))
                        :sharpen         (Kernel. 3 3 (float-array [0 -1 0 -1 5 -1 0 -1 0]))
                        :sobel-x         (Kernel. 3 3 (float-array [1 0 -1 2 0 -2 1 0 -1]))
                        :sobel-y         (Kernel. 3 3 (float-array [1 2 1 0 0 0 -1 -2 -1]))
                        :gradient-x      (Kernel. 3 3 (float-array [-1 0 1 -1 0 1 -1 0 1]))
                        :gradient-y      (Kernel. 3 3 (float-array [-1 -1 -1 0 0 0 1 1 1]))
                        :box-blur        (Kernel. 3 3 (float-array (map #(/ (int %) 9.0) [1 1 1 1 1 1 1 1 1])))
                        :gaussian-blur-3 (Kernel. 3 3 (float-array (map #(/ (int %) 16.0) [1 2 1 2 4 2 1 2 1])))
                        :gaussian-blur-5 (Kernel. 5 5 (float-array (map #(/ (int %) 256.0) [1 4 6 4 1 4 16 24 16 4 6 24 36 24 6 4 16 24 16 4 1 4 6 4 1])))
                        :unsharp         (Kernel. 5 5 (float-array (map #(/ (int %) -256.0) [1 4 6 4 1 4 16 24 16 4 6 24 -476 24 6 4 16 24 16 4 1 4 6 4 1])))})

;; Add ImageProto functions to BufferedImae
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
   :subimage get-subimage
   :resize resize-image})

;;

(declare resize-canvas)

(defrecord ^{:doc "Test"}
    Canvas [^Graphics2D graphics
            ^BufferedImage buffer
            ^Line2D line-obj
            ^Rectangle2D rect-obj
            ^Ellipse2D ellipse-obj
            ^Arc2D arc-obj
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
  (resize [c w h] (resize-canvas c w h))
  (subimage [_ x y w h] (get-subimage buffer x y w h)))

(def ^{:doc "Rendering hints define quality of drawing or window rendering.

The differences are in interpolation, antialiasing, speed vs quality rendering etc. See the source code for the list of each value.

The `:highest` is `:high` with `VALUE_STROKE_PURE` added. Be aware that this option can give very soft lines.

Default hint for Canvas is `:high`. You can set also hint for Window which means that when display is refreshed this hint is applied (java defaults are used otherwise)."
       :metadoc/categories #{:canvas :window}}
  rendering-hints {:low [[RenderingHints/KEY_ANTIALIASING        RenderingHints/VALUE_ANTIALIAS_OFF]
                         [RenderingHints/KEY_INTERPOLATION       RenderingHints/VALUE_INTERPOLATION_NEAREST_NEIGHBOR]
                         [RenderingHints/KEY_ALPHA_INTERPOLATION RenderingHints/VALUE_ALPHA_INTERPOLATION_SPEED]
                         [RenderingHints/KEY_COLOR_RENDERING     RenderingHints/VALUE_COLOR_RENDER_SPEED]
                         [RenderingHints/KEY_RENDERING           RenderingHints/VALUE_RENDER_SPEED]
                         [RenderingHints/KEY_FRACTIONALMETRICS   RenderingHints/VALUE_FRACTIONALMETRICS_OFF]
                         [RenderingHints/KEY_TEXT_ANTIALIASING   RenderingHints/VALUE_TEXT_ANTIALIAS_OFF]]
                   :mid [[RenderingHints/KEY_ANTIALIASING        RenderingHints/VALUE_ANTIALIAS_ON]
                         [RenderingHints/KEY_INTERPOLATION       RenderingHints/VALUE_INTERPOLATION_BILINEAR]
                         [RenderingHints/KEY_ALPHA_INTERPOLATION RenderingHints/VALUE_ALPHA_INTERPOLATION_SPEED]
                         [RenderingHints/KEY_COLOR_RENDERING     RenderingHints/VALUE_COLOR_RENDER_SPEED]
                         [RenderingHints/KEY_RENDERING           RenderingHints/VALUE_RENDER_SPEED]
                         [RenderingHints/KEY_FRACTIONALMETRICS   RenderingHints/VALUE_FRACTIONALMETRICS_OFF]
                         [RenderingHints/KEY_TEXT_ANTIALIASING   RenderingHints/VALUE_TEXT_ANTIALIAS_ON]]
                   :high [[RenderingHints/KEY_ANTIALIASING        RenderingHints/VALUE_ANTIALIAS_ON]
                          [RenderingHints/KEY_INTERPOLATION       RenderingHints/VALUE_INTERPOLATION_BICUBIC]
                          [RenderingHints/KEY_ALPHA_INTERPOLATION RenderingHints/VALUE_ALPHA_INTERPOLATION_QUALITY]
                          [RenderingHints/KEY_COLOR_RENDERING     RenderingHints/VALUE_COLOR_RENDER_QUALITY]
                          [RenderingHints/KEY_RENDERING           RenderingHints/VALUE_RENDER_QUALITY]
                          [RenderingHints/KEY_FRACTIONALMETRICS   RenderingHints/VALUE_FRACTIONALMETRICS_ON]
                          [RenderingHints/KEY_TEXT_ANTIALIASING   RenderingHints/VALUE_TEXT_ANTIALIAS_ON]]
                   :highest [[RenderingHints/KEY_ANTIALIASING        RenderingHints/VALUE_ANTIALIAS_ON]
                             [RenderingHints/KEY_INTERPOLATION       RenderingHints/VALUE_INTERPOLATION_BICUBIC]
                             [RenderingHints/KEY_ALPHA_INTERPOLATION RenderingHints/VALUE_ALPHA_INTERPOLATION_QUALITY]
                             [RenderingHints/KEY_COLOR_RENDERING     RenderingHints/VALUE_COLOR_RENDER_QUALITY]
                             [RenderingHints/KEY_RENDERING           RenderingHints/VALUE_RENDER_QUALITY]
                             [RenderingHints/KEY_FRACTIONALMETRICS   RenderingHints/VALUE_FRACTIONALMETRICS_ON]
                             [RenderingHints/KEY_TEXT_ANTIALIASING   RenderingHints/VALUE_TEXT_ANTIALIAS_ON]
                             [RenderingHints/KEY_STROKE_CONTROL      RenderingHints/VALUE_STROKE_PURE]]})

(defn- get-rendering-hints
  "Return rendering hints for a key or return default (or :high).
  This function is made to protect against user errors."
  ([hint default]
   (rendering-hints (or (some #{hint} (keys rendering-hints)) default)))
  ([hint]
   (get-rendering-hints hint :high)))

(defn- set-rendering-hints
  "Sets rendering hints for graphics context."
  [^Graphics2D g hints]
  (doseq [[key v] hints]
    (.setRenderingHint g key v))
  true)

(defn- set-rendering-hints-by-key
  "Sets rendering hints for graphics context."
  [g hints]
  (if (contains? rendering-hints hints)
    (set-rendering-hints g (hints rendering-hints))
    false))

;; 

(defn flush-graphics
  "Dispose current `Graphics2D`

  Do not use directly. Call [[with-canvas->]] or [[with-canvas]] macros."
  [^Canvas canvas]
  (.dispose ^Graphics2D (.graphics canvas)))

(defn make-graphics
  "Create new `Graphics2D` object and set rendering hints.

  Do not use directly. Call [[with-canvas->]] or [[with-canvas]] macros."
  [^Canvas canvas]
  (let [^Graphics2D ng (.createGraphics ^BufferedImage (.buffer canvas))]
    (set-rendering-hints ng (or (.hints canvas) (rendering-hints :high)))
    (when-let [f (.font canvas)] (.setFont ng f))
    (Canvas. ng
             (.buffer canvas)
             (.line-obj canvas)
             (.rect-obj canvas)
             (.ellipse-obj canvas)
             (.arc-obj canvas)
             (.hints canvas)
             (.w canvas)
             (.h canvas)
             (atom [])
             (.font canvas))))

(defmacro with-canvas->
  "Threading macro which takes care to create and destroy `Graphics2D` object for drawings on canvas. Macro returns result of last call.

  Each function have to accept canvas as second parameter and have to return canvas.

  See also [[with-canvas]]."
  {:metadoc/categories #{:canvas}}
  [canvas & body]  
  `(let [newcanvas# (make-graphics ~canvas)
         result# (-> newcanvas#
                     ~@body)]
     (do
       (flush-graphics newcanvas#)
       result#)))

(defmacro with-canvas
  "Macro which takes care to create and destroy `Graphics2D` object for drawings on canvas. Macro returns result of last call.

  See also [[with-canvas->]]." 
  {:metadoc/categories #{:canvas}}
  [[c canvas] & body]
  `(let [~c (make-graphics ~canvas)
         result# (do ~@body)]
     (do
       (flush-graphics ~c)
       result#)))

;;

(declare set-background)
(declare set-stroke)
(declare set-color)
(declare image)

(defn canvas
  "Create and return Canvas with `width`, `height` and optionally quality hint name (keyword) and font name.

  Default hint is `:high`. Default font is system one.

  Canvas is an object which keeps everything needed to draw Java2d primitives on it. As a drawing buffer `BufferedImage` is used. To draw on Canvas directly wrap your functions with [[with-canvas]] or [[with-canvas->]] macros to create graphical context.
  
  Canvas is transparent by default. See also [[black-canvas]].
  
  Be aware that drawing on Canvas is single threaded.

  Font you set while creating canvas will be default font. You can set another font in the code with [[set-font]] and [[set-font-attributes]] functions. However these set font temporary."
  {:metadoc/categories #{:canvas}}
  ([^long width ^long height hint ^String font]
   (let [^BufferedImage buffer (.. GraphicsEnvironment 
                                   (getLocalGraphicsEnvironment)
                                   (getDefaultScreenDevice)
                                   (getDefaultConfiguration)
                                   (createCompatibleImage width height Transparency/TRANSLUCENT))        
         result (Canvas. nil
                         buffer
                         (Line2D$Double.)
                         (Rectangle2D$Double.)
                         (Ellipse2D$Double.)
                         (Arc2D$Double.)
                         (get-rendering-hints hint)
                         width height
                         nil
                         (when font (java.awt.Font/decode font)))]
     (with-canvas [c result]
       (.setComposite ^Graphics2D (.graphics ^Canvas c) java.awt.AlphaComposite/Src)
       (set-background c Color/black 0))
     result))
  ([width height]
   (canvas width height :high nil))
  ([width height hint]
   (canvas width height hint nil)))

(defn black-canvas
  "Create [[canvas]] with black, opaque background."
  {:metadoc/categories #{:canvas}}
  [& r] (with-canvas-> (apply canvas r)
          (set-background :black)))

(defn- resize-canvas
  "Resize canvas to new dimensions. Creates and returns new canvas."
  [^Canvas c width height]
  (let [ncanvas (canvas width height (.hints c))]
    (with-canvas-> ncanvas
      (image (get-image c)))))

;;

(defn scale
  "Scale canvas"
  {:metadoc/categories #{:transform :canvas}} 
  ([^Canvas canvas ^double scalex ^double scaley]
   (.scale ^Graphics2D (.graphics canvas) scalex scaley)
   canvas)
  ([canvas s] (scale canvas s s)))

(defn flip-x
  "Flip canvas over x axis"
  {:metadoc/categories #{:transform :canvas}}
  [canvas]
  (scale canvas -1.0 1.0))

(defn flip-y
  "Flip canvas over y axis"
  {:metadoc/categories #{:transform :canvas}}
  [canvas]
  (scale canvas 1.0 -1.0))

(defn translate
  "Translate origin"
  {:metadoc/categories #{:transform :canvas}}
  ([^Canvas canvas ^double tx ^double ty]
   (.translate ^Graphics2D (.graphics canvas) tx ty)
   canvas)
  ([canvas [x y]]
   (translate canvas x y)))

(defn rotate
  "Rotate canvas"
  {:metadoc/categories #{:transform :canvas}}
  [^Canvas canvas ^double angle]
  (.rotate ^Graphics2D (.graphics canvas) angle)
  canvas)

(defn shear
  "Shear canvas"
  {:metadoc/categories #{:transform :canvas}}
  ([^Canvas canvas ^double sx ^double sy]
   (.shear ^Graphics2D (.graphics canvas) sx sy)
   canvas)
  ([canvas s] (shear canvas s s)))

(defn push-matrix
  "Remember current transformation state.

  See also [[pop-matrix]], [[reset-matrix]]."
  {:metadoc/categories #{:transform :canvas}}
  [^Canvas canvas]
  (swap! (.transform-stack canvas) conj (.getTransform ^Graphics2D (.graphics canvas)))
  canvas)

(defn pop-matrix
  "Restore saved transformation state.

  See also [[push-matrix]], [[reset-matrix]]."
  {:metadoc/categories #{:transform :canvas}}
  [^Canvas canvas]
  (when (seq @(.transform-stack canvas))
    (let [v (peek @(.transform-stack canvas))]
      (swap! (.transform-stack canvas) pop)
      (.setTransform ^Graphics2D (.graphics canvas) v)))
  canvas)

(defn transform
  "Transform given point or coordinates with current transformation. See [[inv-transform]]."
  {:metadoc/categories #{:transform :canvas}}
  ([^Canvas canvas x y]
   (let [^Point2D p (.transform ^java.awt.geom.AffineTransform (.getTransform ^Graphics2D (.graphics canvas)) (Point2D$Double. x y) nil)]
     [(.getX p) (.getY p)]))
  ([canvas [x y]]
   (transform canvas x y)))

(defn inv-transform
  "Inverse transform of given point or coordinates with current transformation. See [[transform]]."
  {:metadoc/categories #{:transform :canvas}}
  ([^Canvas canvas x y]
   (let [^Point2D p (.inverseTransform ^java.awt.geom.AffineTransform (.getTransform ^Graphics2D (.graphics canvas)) (Point2D$Double. x y) nil)]
     [(.getX p) (.getY p)]))
  ([canvas [x y]]
   (inv-transform canvas x y)))

(defn reset-matrix
  "Reset transformations."
  {:metadoc/categories #{:transform :canvas}}
  [^Canvas canvas]
  (.setTransform ^Graphics2D (.graphics canvas) (java.awt.geom.AffineTransform.))
  canvas)

;; canvas orientation

(defmulti orient-canvas
  "Place origin into one of four window corners and reorient axes.

  * `-` as suffix - means default orientation (y-axis reversed)
  * `+` as suffix - means usual (mathematical) orientation (y-axis not reversed)."
  {:metadoc/categories #{:transform :canvas}}
  (fn [orientation c] orientation))

(defmethod orient-canvas :default [_ c] c)
(defmethod orient-canvas :top-left- [_ c] c)

(defmethod orient-canvas :top-left+ [_ c]
  (-> c
      (flip-y)
      (rotate m/-HALF_PI)))

(defmethod orient-canvas :top-right- [_ ^Canvas c]
  (-> c
      (translate (dec (.w c)) 0)
      (rotate m/HALF_PI)))

(defmethod orient-canvas :top-right+ [_ ^Canvas c]
  (-> c
      (translate (dec (.w c)) 0)
      (flip-x)))

(defmethod orient-canvas :bottom-left+ [_ ^Canvas c]
  (-> c
      (translate 0 (dec (.h c)))
      (flip-y)))

(defmethod orient-canvas :bottom-left- [_ ^Canvas c]
  (-> c
      (translate 0 (dec (.h c)))
      (rotate m/-HALF_PI)))

(defmethod orient-canvas :bottom-right+ [_ ^Canvas c]
  (-> c
      (translate (dec (.w c)) (dec (.h c)))
      (rotate m/HALF_PI)
      (flip-x)))

(defmethod orient-canvas :bottom-right- [_ ^Canvas c]
  (-> c
      (translate (dec (.w c)) (dec (.h c)))
      (flip-x)
      (flip-y)))

(def ^{:doc "List of orientations" :metadoc/categories #{:transform :canvas}}
  orientations-list (remove #(= :default %) (sort (keys (methods orient-canvas)))))

(defmacro with-oriented-canvas 
  "Same as [[with-canvas]] but with initial orientation."
  {:metadoc/categories #{:transform :canvas}}
  [orientation [c canv] & body]
  `(with-canvas [~c ~canv]
     (orient-canvas ~orientation ~c)
     ~@body))

(defmacro with-oriented-canvas->
  "Same as [[with-canvas->]] but with initial orientation."
  {:metadoc/categories #{:transform :canvas}}
  [orientation canv & body]
  `(with-canvas [c# ~canv]
     (orient-canvas ~orientation c#)
     (-> c# ~@body)))

;; clip

(defn clip
  "Clip drawing to specified rectangle.

  See also [[reset-clip]]."
  {:metadoc/categories #{:canvas}}
  [^Canvas canvas x y w h] 
  (.setClip ^Graphics2D (.graphics canvas) x y w h)
  canvas)

(defn reset-clip
  "Resets current clipping.

  See also [[clip]]."
  {:metadoc/categories #{:canvas}}
  [^Canvas canvas] 
  (.setClip ^Graphics2D (.graphics canvas) 0 0 (width canvas) (height canvas))
  canvas)

;; Drawing functions

(defn line
  "Draw line from point `(x1,y1)` to `(x2,y2)`"
  {:metadoc/categories #{:draw}}
  ([^Canvas canvas x1 y1 x2 y2]
   (let [^Line2D l (.line-obj canvas)]
     (.setLine l x1 y1 x2 y2)
     (.draw ^Graphics2D (.graphics canvas) l))
   canvas)
  ([canvas vect1 vect2]
   (line canvas (vect1 0) (vect1 1) (vect2 0) (vect2 1))))

(def ^{:doc "Stroke join types"
       :metadoc/categories #{:draw}}
  stroke-joins {:bevel BasicStroke/JOIN_BEVEL
                :miter BasicStroke/JOIN_MITER
                :round BasicStroke/JOIN_ROUND})

(def ^{:doc "Stroke cap types"
       :metadoc/categories #{:draw}}
  stroke-caps {:round BasicStroke/CAP_ROUND
               :butt BasicStroke/CAP_BUTT
               :square BasicStroke/CAP_SQUARE})

(defn set-stroke
  "Set stroke (line) attributes like `cap`, `join`,  size and `miter-limit`.

  Default `:round` and `:bevel` are used. Default size and miter-limit are `1.0`.

  See [[stroke-joins]] and [[stroke-caps]] for names.

  See [[set-stroke-custom]]."
  {:metadoc/categories #{:draw}}
  ([^Canvas canvas size cap join miter-limit]
   (.setStroke ^Graphics2D (.graphics canvas) (BasicStroke. size
                                                            (or (stroke-caps cap) BasicStroke/CAP_ROUND)
                                                            (or (stroke-joins join) BasicStroke/JOIN_BEVEL)
                                                            (float miter-limit)))
   canvas)
  ([canvas size cap join]
   (set-stroke canvas size cap join 1.0))
  ([canvas size cap]
   (set-stroke canvas size cap :bevel 1.0))
  ([canvas size]
   (set-stroke canvas size :round :bevel 1.0))
  ([canvas]
   (set-stroke canvas 1.0)))

(defn set-stroke-custom
  "Create custom stroke.

  Provide map with following entries, see more in [JavaDoc](https://docs.oracle.com/javase/7/docs/api/java/awt/BasicStroke.html):

  * :size - size of the stroke (default: 1.0)
  * :cap - [[stroke-caps]] (default: :butt)
  * :join - [[stroke-joins]] (default: :round)
  * :miter-limit - line joins trim factor (default: 1.0)
  * :dash - array with dash pattern, (default: nil)
  * :dash-phase - offset to start pattern (default: 0.0)

  See also [[set-stroke]]."
  {:metadoc/categories #{:draw}}
  [^Canvas canvas {:keys [size cap join miter-limit dash dash-phase]
                   :or {size 1.0 cap :butt join :round miter-limit 1.0 dash nil dash-phase 0.0}}]
  (if dash
    (do
      (.setStroke ^Graphics2D (.graphics canvas) (BasicStroke. size
                                                               (or (stroke-caps cap) BasicStroke/CAP_BUTT)
                                                               (or (stroke-joins join) BasicStroke/JOIN_ROUND)
                                                               (float miter-limit)
                                                               (float-array dash)
                                                               (float dash-phase)))
      canvas) 
    (set-stroke canvas size cap join miter-limit)))

(defn point
  "Draw point at `x`,`y` or at vector position.

  It's implemented as a very short line. Consider using `(rect x y 1 1)` for speed when `x` and `y` are integers."
  {:metadoc/categories #{:draw}}  
  ([canvas ^double x ^double y]
   (line canvas x y (+ x 10.0e-6) (+ y 10.0e-6))
   canvas)
  ([canvas vect]
   (point canvas (vect 0) (vect 1))))

(defn- draw-fill-or-stroke
  "Draw filled or outlined shape."
  [^Graphics2D g ^Shape obj stroke?]
  (if stroke?
    (.draw g obj)
    (.fill g obj)))

(defn rect
  "Draw rectangle with top-left corner at `(x,y)` position with width `w` and height `h`. Optionally you can set `stroke?` (default: `false`) to `true` if you don't want to fill rectangle and draw outline only.

  See also: [[crect]]."
  {:metadoc/categories #{:draw}}
  ([^Canvas canvas x y w h stroke?]
   (let [^Rectangle2D r (.rect-obj canvas)] 
     (.setFrame r x y w h)
     (draw-fill-or-stroke (.graphics canvas) r stroke?))
   canvas)
  ([canvas x y w h]
   (rect canvas x y w h false)))

(defn crect
  "Centered version of [[rect]]."
  {:metadoc/categories #{:draw}}
  ([canvas x y w h stroke?]
   (let [w2 (* 0.5 ^double w)
         h2 (* 0.5 ^double h)]
     (rect canvas (- ^double x w2) (- ^double y h2) w h stroke?))
   canvas)
  ([canvas x y w h]
   (crect canvas x y w h false)))

(defn ellipse
  "Draw ellipse with middle at `(x,y)` position with width `w` and height `h`."
  {:metadoc/categories #{:draw}}
  ([^Canvas canvas x1 y1 w h stroke?]
   (let [^Ellipse2D e (.ellipse-obj canvas)]
     (.setFrame e (- ^double x1 (* ^double w 0.5)) (- ^double y1 (* ^double h 0.5)) w h)
     (draw-fill-or-stroke (.graphics canvas) e stroke?))
   canvas)
  ([canvas x1 y1 w h]
   (ellipse canvas x1 y1 w h false)))

(defn arc
  "Draw arc with middle at `(x,y)` position with width `w` and height `h`.

  Starting angle `start` and `extent` are in radians. Direction is clockwise.

  Type is one of the:

  * `:open`
  * `:pie`
  * `:chord`"
  {:metadoc/categories #{:draw}}
  ([^Canvas canvas x1 y1 w h start extent type stroke?]
   (let [^Arc2D e (.arc-obj canvas)]
     (.setArc e (- ^double x1 (* ^double w 0.5)) (- ^double y1 (* ^double h 0.5)) w h
              (m/degrees start) (- (m/degrees extent))
              (case type
                :chord Arc2D/CHORD
                :pie Arc2D/PIE
                Arc2D/OPEN))
     (draw-fill-or-stroke (.graphics canvas) e stroke?))
   canvas)
  ([canvas x1 y1 w h start extent type]
   (arc canvas x1 y1 w h start extent type true))
  ([canvas x1 y1 w h start extent]
   (arc canvas x1 y1 w h start extent :open true)))

(defn rarc
  "Draw arc with middle at `(x,y)` with radius `r`.

  Starting angle `start` and `extent` are in radians. Direction is clockwise.

  Type is one of the:

  * `:open`
  * `:pie`
  * `:chord`"
  {:metadoc/categories #{:draw}}
  ([^Canvas canvas x1 y1 r start extent type stroke?]
   (let [^Arc2D e (.arc-obj canvas)]
     (.setArcByCenter e x1 y1 r
                      (m/degrees start) (- (m/degrees extent))
                      (case type
                        :chord Arc2D/CHORD
                        :pie Arc2D/PIE
                        Arc2D/OPEN))
     (draw-fill-or-stroke (.graphics canvas) e stroke?))
   canvas)
  ([canvas x1 y1 r start extent type]
   (rarc canvas x1 y1 r start extent type true))
  ([canvas x1 y1 r start extent]
   (rarc canvas x1 y1 r start extent :open true)))

(defn triangle
  "Draw triangle with corners at 3 positions."
  {:metadoc/categories #{:draw}}
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

  Input: list of vertices as vectors.

  See also: [[triangle-fan]]."
  {:metadoc/categories #{:draw}}
  ([canvas vs stroke?]
   (when (> (count vs) 2)
     (loop [v1 (first vs)
            v2 (second vs)
            vss (nnext vs)]
       (when vss
         (let [v3 (first vss)]
           (triangle canvas (v2 0) (v2 1) (v3 0) (v3 1) (v1 0) (v1 1) stroke?)
           (recur v2 v3 (next vss))))))
   canvas)
  ([canvas vs]
   (triangle-strip canvas vs false)))

(defn triangle-fan
  "Draw triangle fan. Implementation of `Processing` `FAN` shape.

  First point is common vertex of all triangles.
  
  Input: list of vertices as vectors.

  See also: [[triangle-strip]]."
  {:metadoc/categories #{:draw}}
  ([canvas vs stroke?]
   (when (> (count vs) 2)
     (let [v1 (first vs)]
       (loop [v2 (second vs)
              vss (nnext vs)]
         (when vss
           (let [v3 (first vss)]
             (triangle canvas (v1 0) (v1 1) (v2 0) (v2 1) (v3 0) (v3 1) stroke?)
             (recur v3 (next vss)))))))
   canvas)
  ([canvas vs]
   (triangle-strip canvas vs false)))

(defn path
  "Draw path from lines.

  Input: list of points as vectors, close? - close path or not (default: false), stroke? - draw lines or filled shape (default true - lines).

  See also [[path-bezier]]."
  {:metadoc/categories #{:draw}}
  ([^Canvas canvas vs close? stroke?]
   (when (seq vs)
     (let [^Path2D p (Path2D$Double.)
           m (first vs)]
       (.moveTo p (m 0) (m 1))
       (doseq [v (next vs)]
         (.lineTo p (v 0) (v 1)))
       (when (or (not stroke?) close?) (.closePath p))
       (draw-fill-or-stroke (.graphics canvas) p stroke?)))
   canvas)
  ([canvas vs close?] (path canvas vs close? true))
  ([canvas vs] (path canvas vs false true)))

(defn- calculate-bezier-control-points
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
  "Draw path from quad curves.

  Input: list of points as vectors, close? - close path or not (default: false), stroke? - draw lines or filled shape (default true - lines).

  See also [[path]]."
  {:metadoc/categories #{:draw}}  
  ([^Canvas canvas vs close? stroke?]
   (when (> (count vs) 3)
     (let [cl? (or (not stroke?) close?)
           ^Path2D p (Path2D$Double.)
           m0 (first vs)
           m1 (second vs)
           m2 (nth vs 2) 
           f0 (if cl? m0 m0)
           f1 (if cl? m1 m0)
           f2 (if cl? m2 m1)
           f3 (if cl? (nth vs 3) m2)
           vs (if cl? (next vs) vs)]
       (.moveTo p (f1 0) (f1 1))
       (loop [v0 f0
              v1 f1
              v2 f2
              v3 f3
              nvs (drop 3 vs)]
         (let [[cp1 cp2] (calculate-bezier-control-points v0 v1 v2 v3)]
           (.curveTo p (cp1 0) (cp1 1) (cp2 0) (cp2 1) (v2 0) (v2 1))
           (if-not (empty? nvs)
             (recur v1 v2 v3 (first nvs) (next nvs))
             (if cl?
               (let [[cp1 cp2] (calculate-bezier-control-points v1 v2 v3 m0)
                     [cp3 cp4] (calculate-bezier-control-points v2 v3 m0 m1)
                     [cp5 cp6] (calculate-bezier-control-points v3 m0 m1 m2)]
                 (.curveTo p (cp1 0) (cp1 1) (cp2 0) (cp2 1) (v3 0) (v3 1))
                 (.curveTo p (cp3 0) (cp3 1) (cp4 0) (cp4 1) (m0 0) (m0 1))
                 (.curveTo p (cp5 0) (cp5 1) (cp6 0) (cp6 1) (m1 0) (m1 1)))
               (let [[cp1 cp2] (calculate-bezier-control-points v1 v2 v3 v3)]
                 (.curveTo p (cp1 0) (cp1 1) (cp2 0) (cp2 1) (v3 0) (v3 1)))))))
       (draw-fill-or-stroke (.graphics canvas) p stroke?)))
   canvas)
  ([canvas vs close?] (path-bezier canvas vs close? true))
  ([canvas vs] (path-bezier canvas vs false true)))

(defn bezier
  "Draw bezier curve with 4 sets of coordinates."
  {:metadoc/categories #{:draw}}  
  ([^Canvas canvas x1 y1 x2 y2 x3 y3 x4 y4 stroke?]
   (let [^Path2D p (Path2D$Double.)]
     (doto p
       (.moveTo x1 y1)
       (.curveTo x2 y2 x3 y3 x4 y4))
     (draw-fill-or-stroke (.graphics canvas) p stroke?)))
  ([canvas x1 y1 x2 y2 x3 y3 x4 y4]
   (bezier canvas x1 y1 x2 y2 x3 y3 x4 y4 true)))

(defn curve
  "Draw quadratic curve with 3 sets of coordinates."
  {:metadoc/categories #{:draw}}  
  ([^Canvas canvas x1 y1 x2 y2 x3 y3 stroke?]
   (let [^Path2D p (Path2D$Double.)]
     (doto p
       (.moveTo x1 y1)
       (.quadTo x2 y2 x3 y3))
     (draw-fill-or-stroke (.graphics canvas) p stroke?)))
  ([canvas x1 y1 x2 y2 x3 y3]
   (curve canvas x1 y1 x2 y2 x3 y3 true)))

(defn quad
  "Draw quad with corners at 4 positions."
  {:metadoc/categories #{:draw}}
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

;; hex

(defn pointy-hex
  "Draw pointy topped hex."
  {:metadoc/categories #{:draw}}
  ([canvas x y size stroke?]
   (path canvas (grid/pointy-hex-corners size x y) true stroke?))
  ([canvas x y size]
   (pointy-hex canvas x y size false)))

(defn flat-hex
  "Draw flat topped hex."
  {:metadoc/categories #{:draw}}
  ([canvas x y size stroke?]
   (path canvas (grid/flat-hex-corners size x y) true stroke?))
  ([canvas x y size]
   (flat-hex canvas x y size false)))

;; grid

(defn grid-cell
  "Draw grid cell for given grid."
  {:metadoc/categories #{:draw}}
  ([canvas grid x y stroke?]
   (path canvas (grid/corners grid [x y]) true stroke?))
  ([canvas grid x y]
   (grid-cell canvas grid x y false))
  ([canvas grid x y scale stroke?]
   (path canvas (grid/corners grid [x y] scale) true stroke?)))

;;

(def ^{:doc "List of all available font names."
       :metadoc/categories #{:write}} fonts-list (into [] (.getAvailableFontFamilyNames (java.awt.GraphicsEnvironment/getLocalGraphicsEnvironment))))

(defn set-font
  "Set font by name."
  {:metadoc/categories #{:write}}
  [^Canvas canvas ^String fontname]
  (let [f (java.awt.Font/decode fontname)]
    (.setFont ^Graphics2D (.graphics canvas) f)
    canvas))

(defn set-font-attributes
  "Set current font size and attributes.

  Attributes are: `:bold`, `:italic`, `:bold-italic`."
  {:metadoc/categories #{:write}}
  ([^Canvas canvas ^double size style]
   (let [s (or (style {:bold 1 :italic 2 :bold-italic 3}) 0)
         f (.deriveFont ^java.awt.Font (.getFont ^Graphics2D (.graphics canvas)) (int s) (float size))]
     (.setFont ^Graphics2D (.graphics canvas) f)
     canvas))
  ([^Canvas canvas ^double size]
   (let [f (.deriveFont ^java.awt.Font (.getFont ^Graphics2D (.graphics canvas)) (float size))]
     (.setFont ^Graphics2D (.graphics canvas) f)
     canvas)))

(defn char-width
  "Returns font width from metrics. Should be called within graphical context."
  {:metadoc/categories #{:write}}
  ^long [^Canvas canvas chr]
  (.charWidth (.getFontMetrics ^Graphics2D (.graphics canvas)) ^char chr))

(defn font-height
  "Returns font width from metrics. Should be called within context."
  {:metadoc/categories #{:write}}
  ^long [^Canvas canvas]
  (.getHeight (.getFontMetrics ^Graphics2D (.graphics canvas))))

(defn font-ascent
  "Returns font width from metrics. Should be called within context."
  {:metadoc/categories #{:write}}
  ^long [^Canvas canvas]
  (.getAscent (.getFontMetrics ^Graphics2D (.graphics canvas))))

(defn text-width
  "Returns width of the provided string. Should be called within context."
  {:metadoc/categories #{:write}}
  ^long [^Canvas canvas txt]
  (.stringWidth (.getFontMetrics ^Graphics2D (.graphics canvas)) (str txt)))

(defn text-bounding-box
  "Returns bounding box [x,y,w,h] for given text. `[x,y]` position is relative to base line."
  {:metadoc/categories #{:write}}
  [^Canvas canvas txt]
  (let [^Graphics2D g (.graphics canvas)
        ^Rectangle2D b (.getStringBounds (.getFontMetrics g) (str txt) g)]
    [(.getX b) (.getY b)
     (.getWidth b) (.getHeight b)]))

(defn text
  "Draw text for given position and alignment.

  Possible alignments are: `:right`, `:center`, `:left`."
  {:metadoc/categories #{:write}}
  ([^Canvas canvas s x y align]
   (let [x (float x)
         y (float y)
         s (str s)]
     (case align
       :right (let [w (.stringWidth (.getFontMetrics ^Graphics2D (.graphics canvas)) s)]
                (.drawString ^Graphics2D (.graphics canvas) s (- x w) y))
       :center (let [w (/ (.stringWidth (.getFontMetrics ^Graphics2D (.graphics canvas)) s) 2.0)]
                 (.drawString ^Graphics2D (.graphics canvas) s (float (- x w)) y))
       :left (.drawString ^Graphics2D (.graphics canvas) s x y)
       (.drawString ^Graphics2D (.graphics canvas) s x y))) 
   canvas)
  ([canvas s x y]
   (text canvas s x y :left)))

;; ### Color

(defn- set-color-with-fn
  "Set color for primitive or background via passed function. You can use:

  * java.awt.Color object
  * clojure2d.math.vector.Vec4 or Vec3 object
  * individual r, g, b (and optional alpha) as integers from 0-255. They are converted to integer and clamped if necessary."
  ([f canvas c]
   (f canvas (c/awt-color c)))
  ([f canvas c a]
   (f canvas (c/awt-color c a)))
  ([f canvas r g b a]
   (f canvas (c/awt-color r g b a)))
  ([f canvas r g b]
   (f canvas (c/awt-color r g b))))

(defn set-awt-color
  "Set color with valid java `Color` object. Use it when you're sure you pass `java.awt.Color` object."
  {:metadoc/categories #{:draw}}
  [^Canvas canvas ^java.awt.Color c]
  (.setColor ^Graphics2D (.graphics canvas) c)
  canvas)

(defn set-awt-background
  "Set background color. Expects valid `java.awt.Color` object."
  {:metadoc/categories #{:draw}}
  ([^Canvas canvas c]
   (let [^Graphics2D g (.graphics canvas)
         ^Color currc (.getColor g)]
     (push-matrix canvas)
     (reset-matrix canvas)
     (set-color-with-fn set-awt-color canvas c)
     (doto g
       (.fillRect 0 0 (.w canvas) (.h canvas))
       (.setColor currc))
     (pop-matrix canvas))
   canvas))

(defn awt-xor-mode
  "Set XOR graphics mode with `java.awt.Color`.

  To revert call [[paint-mode]]."
  {:metadoc/categories #{:draw}}
  [^Canvas canvas c]
  (let [^Graphics2D g (.graphics canvas)]
    (.setXORMode g c))
  canvas)

(defn paint-mode
  "Set normal paint mode.

  This is default mode.
  
  See [[gradient-mode]] or [[xor-mode]] for other types."
  {:metadoc/categories #{:draw}}
  [^Canvas canvas]
  (.setPaintMode ^Graphics2D (.graphics canvas))
  canvas)

(defn set-composite
  "Set composite method for blending during draw.

  See [[composite]] to create one defined by [[clojure2d.color]] blending modes.

  You can also use ones defined in `java.awt.AlphaComposite` class.

  Call witout parameters to revert to default: `java.awt.AlphaComposite/SrcOver`."
  {:metadoc/categories #{:draw}}
  ([^Canvas canvas composite]
   (.setComposite ^Graphics2D (.graphics canvas) composite)
   canvas)
  ([^Canvas canvas] (set-composite canvas java.awt.AlphaComposite/SrcOver)))

;; Set color for primitive
(def ^{:doc "Sets current color. Color can be:

* [[vec3]], [[vec4]] with rgb values.
* java.awt.Color object
* keyword with name from 140 HTML color names
* Integer
* r, g, b and optional alpha

See [[clojure2d.color]] namespace for more color functions."
       :metadoc/categories #{:draw}} 
  set-color (partial set-color-with-fn set-awt-color))

;; Set background color
(def ^{:doc "Sets background with given color.

Background can be set with alpha.

See [[set-color]]."
       :metadoc/categories #{:draw}}
  set-background (partial set-color-with-fn set-awt-background))

;; Set XOR mode
(def
  ^{:doc "Set XOR painting mode."
    :metadoc/categories #{:draw}}
  xor-mode (partial set-color-with-fn awt-xor-mode))

;;;

(defmacro filled-with-stroke
  "Draw primitive filled and with stroke.

  Provide two colors, one for fill (`color-filled`), second for stroke (`color-stroke`). `primitive-fn` is a primitive function and `attrs` are function parameters. Do not provide.

  One note: current color is replaced with `color-stroke`."
  {:metadoc/categories #{:draw}}
  [canvas color-filled color-stroke primitive-fn & attrs]
  `(-> ~canvas
       (set-color ~color-filled)
       (~primitive-fn ~@attrs false)
       (set-color ~color-stroke)
       (~primitive-fn ~@attrs true)))

;; ### Gradient

(defn gradient-mode
  "Set paint mode to gradient.

  To revert call [[paint-mode]]"
  {:metadoc/categories #{:draw}}
  ([^Canvas canvas x1 y1 color1 x2 y2 color2]
   (let [gp (java.awt.GradientPaint. x1 y1 (c/awt-color color1) x2 y2 (c/awt-color color2))
         ^Graphics2D g (.graphics canvas)]
     (.setPaint g gp)
     canvas)))

;; ### Pattern mode

(defn pattern-mode
  "Set paint mode to pattern.

  Default anchor is set to `(0,0)`. Default width and height are the same as texture dimensions.

  To revert call [[paint-mode]]"
  {:metadoc/categories #{:draw}}
  ([canvas image]
   (let [^BufferedImage image (get-image image)]
     (pattern-mode canvas image 0 0 (.getWidth image) (.getHeight image))))
  ([canvas image w h]
   (let [^BufferedImage image (get-image image)]
     (pattern-mode canvas image 0 0 w h))) 
  ([^Canvas canvas image anchor-x anchor-y w h]
   (let [image (get-image image)
         rect (Rectangle2D$Double. anchor-x anchor-y w h)
         texture (java.awt.TexturePaint. image rect)
         ^Graphics2D g (.graphics canvas)]
     (.setPaint g texture)
     canvas)))


;; ### Image

(defn image
  "Draw an image.

  You can specify position and size of the image. Default it's placed on whole canvas."
  {:metadoc/categories #{:draw}}
  ([^Canvas canvas img x y w h]
   (.drawImage ^Graphics2D (.graphics canvas) (get-image img) x y w h nil)
   canvas)
  ([^Canvas canvas img]
   (image canvas img 0 0 (.w canvas) (.h canvas)))
  ([^Canvas canvas img x y]
   (image canvas img x y (width img) (height img))))

;; SVG

(defn load-svg
  "Load image into Batik Document. See [[transcode-svg]]."
  {:metadoc/categories #{:image}}
  ^TranscoderInput  [svg-filename]
  (TranscoderInput. ^Document (.createDocument (SAXDocumentFactory.
                                                (SVGDOMImplementation/getDOMImplementation)
                                                (XMLResourceDescriptor/getXMLParserClassName))
                                               SVGConstants/SVG_NAMESPACE_URI
                                               SVGConstants/SVG_SVG_TAG
                                               (str "file:///" svg-filename)
                                               (input-stream (file svg-filename)))))

(defn transcode-svg
  "Convert transcoder input into BufferedImage. See [[load-svg]]."
  {:metadoc/categories #{:draw}}
  [^TranscoderInput input ^long w ^long h]
  (let [img (atom nil)
        transcoder (proxy [ImageTranscoder] []
                     (createImage [w h] (BufferedImage. w h BufferedImage/TYPE_INT_ARGB))
                     (writeImage [image output] (reset! img image)))]
    (.addTranscodingHint transcoder ImageTranscoder/KEY_WIDTH (float w))
    (.addTranscodingHint transcoder ImageTranscoder/KEY_HEIGHT (float h))
    (.transcode transcoder input nil)
    @img))

;; Display window

;; Extract all keycodes from `KeyEvent` object and pack it to the map
(def ^:private keycodes-map (->> KeyEvent
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
  "Mouse position."
  (^{:metadoc/categories #{:window :events}} mouse-x [m] "Mouse horizontal position within window. 0 - left side. -1 outside window.")
  (^{:metadoc/categories #{:window :events}} mouse-y [m] "Mouse vertical position. 0 - top, -1 outside window.")
  (^{:metadoc/categories #{:window :events}} mouse-pos [m] "Mouse position as [[Vec2]] type. [0,0] - top left, [-1,-1] outside window."))

(defprotocol MouseButtonProto
  "Get pressed mouse button status."
  (^{:metadoc/categories #{:window :events}} mouse-button [m] "Get mouse pressed button status: :left :right :center or :none"))

(defprotocol KeyEventProto
  "Access to key event data"
  (^{:metadoc/categories #{:window :events}} key-code [e] "Keycode mapped to keyword. See `java.awt.event.KeyEvent` documentation. Eg. `VK_LEFT` is mapped to `:left`.")
  (^{:metadoc/categories #{:window :events}} key-char [e] "Key as char.")
  (^{:metadoc/categories #{:window :events}} key-raw [e] "Raw value for pressed key (as integer)."))

(defprotocol ModifiersProto
  "Get state of keyboard modifiers."
  (^{:metadoc/categories #{:window :events}} control-down? [e] "CONTROL key state as boolean.")
  (^{:metadoc/categories #{:window :events}} alt-down? [e] "ALT key state as boolean.")
  (^{:metadoc/categories #{:window :events}} meta-down? [e] "META key state as boolean.")
  (^{:metadoc/categories #{:window :events}} shift-down? [e] "SHIFT key state as boolean.")
  (^{:metadoc/categories #{:window :events}} alt-gr-down? [e] "ALT-GR key state as boolean."))

(defprotocol PressedProto
  "Key or mouse pressed status."
  (^{:metadoc/categories #{:window :events}} key-pressed? [w] "Any key pressed? (boolean)")
  (^{:metadoc/categories #{:window :events}} mouse-pressed? [w] "Any mouse button pressed? (boolean)"))

;; `Window` type definition, equiped with `get-image` method returning bound canvas' image.
(defrecord Window [^JFrame frame
                   active?
                   buffer
                   ^java.awt.Canvas panel
                   ^double fps
                   ^long w
                   ^long h
                   window-name
                   events
                   background]
  ImageProto
  (get-image [_] (get-image @buffer))
  (width [_] w)
  (height [_] h)
  (save [w n] (save-image (get-image @buffer) n) w)
  (convolve [w n] (convolve @buffer n))
  (subimage [_ x y w h] (get-subimage @buffer x y w h))
  PressedProto
  (key-pressed? [_] (:key-pressed? @events))
  (mouse-pressed? [_] (:mouse-pressed? @events))
  ModifiersProto
  (control-down? [_] (:control-down? @events))
  (alt-down? [_] (:alt-down? @events)) 
  (meta-down? [_] (:meta-down? @events))
  (shift-down? [_] (:shift-down? @events))
  (alt-gr-down? [_] (:alt-gr-down? @events))
  KeyEventProto
  (key-code [_] (:key-code @events))
  (key-char [_] (:key-char @events))
  (key-raw [_] (:key-raw @events))
  MouseButtonProto
  (mouse-button [_] (:mouse-button @events))
  MouseXYProto
  (mouse-pos [_]
    (let [^java.awt.Point p (.getMousePosition panel)]
      (if (nil? p)
        (v/vec2 -1.0 -1.0)
        (v/vec2 (.x p) (.y p)))))
  (mouse-x [_]
    (let [^java.awt.Point p (.getMousePosition panel)]
      (if (nil? p) -1 (.x p))))
  (mouse-y [_]
    (let [^java.awt.Point p (.getMousePosition panel)]
      (if (nil? p) -1 (.y p)))))

;; ### Events function
(extend MouseEvent
  MouseButtonProto
  {:mouse-button #(condp = (.getButton ^MouseEvent %)
                    MouseEvent/BUTTON1 :left
                    MouseEvent/BUTTON2 :center
                    MouseEvent/BUTTON3 :right
                    :none)}
  MouseXYProto
  {:mouse-x #(.getX ^MouseEvent %)
   :mouse-y #(.getY ^MouseEvent %)
   :mouse-pos #(v/vec2 (mouse-x %) (mouse-y %))})

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
  "Helper function, check if window is active."
  {:metadoc/categories #{:window}}
  [^Window window]
  @(.active? window))

(defn mouse-in-window?
  "Check if mouse is inside window."
  {:metadoc/categories #{:window}}
  [window]
  (bool-and (>= ^int (mouse-x window) 0.0)
            (>= ^int (mouse-y window) 0.0)))

;; ### Global state management
;;
;; Global atom is needed to keep current window state. Events don't know what window sends it. The only option is to get component name.

(defonce ^:private global-state (atom {}))

(defn get-state
  "Get state from window"
  {:metadoc/categories #{:window}}
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
  {:metadoc/categories #{:window}}
  [^Window w state]
  (change-state! (.window-name w) state))

;; Private method which extracts the name of your window (set when `show-window` is called).

(defn- event-window-name
  "Returns name of the component. Used to dispatch events."
  [^ComponentEvent e]
  (.getName ^Component (.getComponent e)))

(def ^:const ^{:doc "Use as a dispatch in key events for keys like up/down/etc."} virtual-key (char 0xffff))

;; Multimethod used to process pressed key
(defmulti key-pressed
  "Called when key is pressed.

  * Dispatch: vector of window name and key char
  * Params: key event and current state.

  Should return state.

  ```
  (defmethod key-pressed [\"My window name\" \\c] [event state]
    ;; do something when 'c' is pressed
    (assoc state :some (calculate-something state)))
  ```"
  {:metadoc/categories #{:events}}
  (fn [^KeyEvent e state] [(event-window-name e) (.getKeyChar e)]))
;; Do nothing on default
(defmethod key-pressed :default [_ s]  s)

;; Multimethod used to process released key
(defmulti key-released
  "Called when key is released.

  * Dispatch: vector of window name and key char
  * Params: key event and current state.

  Should return state.

  ```
  (defmethod key-released [\"My window name\" \\c] [event state]
    ;; do something after 'c' is released
    (assoc state :some (calculate-something state)))
  ```"
  {:metadoc/categories #{:events}}
  (fn [^KeyEvent e state] [(event-window-name e) (.getKeyChar e)]))
;; Do nothing on default
(defmethod key-released :default [_ s]  s)

;; Multimethod used to process typed key
(defmulti key-typed
  "Called when key is typed (pressed and released).

  * Dispatch: vector of window name and key char
  * Params: key event and current state.

  Should return state.

  ```
  (defmethod key-typed [\"My window name\" \\c] [event state]
    ;; do something when 'c' is typed
    (assoc state :some (calculate-something state)))
  ```"
  {:metadoc/categories #{:events}}
  (fn [^KeyEvent e state] [(event-window-name e) (.getKeyChar e)]))
;; Do nothing on default
(defmethod key-typed :default [_ s]  s)

;; Multimethod use to processed key events (any key event)

(def ^{:doc "Map of supported key events"
       :metadoc/categories #{:events}} key-event-map {KeyEvent/KEY_PRESSED  :key-pressed
                                                      KeyEvent/KEY_RELEASED :key-released
                                                      KeyEvent/KEY_TYPED    :key-typed})

(defmulti key-event
  "Called on key event

  * Dispatch: vector of window name and key event. See [[key-event-map]].
  * Params: key event and current state.

  Should return state.

  ```
  (defmethod key-event [\"My window name\" :key-pressed] [event state]
    ;; do something when 'up' is typed
    (when (= :up (key-code event)) (do-something))
    state)
  ```"
  {:metadoc/categories #{:events}}
  (fn [^KeyEvent e state] [(event-window-name e) (key-event-map (.getID e))]))
;; Do nothing on default
(defmethod key-event :default [_ s] s)

;; Map Java mouse event names onto keywords
(def   ^{:doc "Map of supported mouse events"
         :metadoc/categories #{:events}}
  mouse-event-map {MouseEvent/MOUSE_CLICKED  :mouse-clicked
                   MouseEvent/MOUSE_DRAGGED  :mouse-dragged
                   MouseEvent/MOUSE_PRESSED  :mouse-pressed
                   MouseEvent/MOUSE_RELEASED :mouse-released
                   MouseEvent/MOUSE_MOVED    :mouse-moved})

;; Multimethod used to processed mouse events
(defmulti mouse-event
  "Called on mouse event

  * Dispatch: vector of window name and mouse event. See [[mouse-event-map]].
  * Params: key event and current state.

  Should return state.

  ```
  (defmethod mouse-event [\"My window name\" :mouse-pressed] [event state]
    ;; do something when button is pressed
    (do-something)
    state)
  ```"
  {:metadoc/categories #{:events}}
  (fn [^MouseEvent e state] [(event-window-name e) (mouse-event-map (.getID e))]))
;; Do nothing on default
(defmethod mouse-event :default [_ s] s)

;; Event adapter objects.

(defn- process-state-and-event 
  "For given event call provided multimethod passing state. Save new state."
  [ef e]
  (let [window-name (event-window-name e)]
    (change-state! window-name (ef e (@global-state window-name)))))

;; Key
(def ^:private key-char-processor (proxy [KeyAdapter] []
                                    (keyPressed [e] (process-state-and-event key-pressed e))
                                    (keyReleased [e] (process-state-and-event key-released e))
                                    (keyTyped [e] (process-state-and-event key-typed e))))

(def ^:private key-event-processor (proxy [KeyAdapter] []
                                     (keyPressed [e] (process-state-and-event key-event e))
                                     (keyReleased [e] (process-state-and-event key-event e))
                                     (keyTyped [e] (process-state-and-event key-event e))))

;; Mouse
(def ^:private mouse-processor (proxy [MouseAdapter] []
                                 (mouseClicked [e] (process-state-and-event mouse-event e))
                                 (mousePressed [e] (process-state-and-event mouse-event e))
                                 (mouseReleased [e] (process-state-and-event mouse-event e))))

;; Mouse drag and move
(def ^:private mouse-motion-processor (proxy [MouseMotionAdapter] []
                                        (mouseDragged [e] (process-state-and-event mouse-event e))
                                        (mouseMoved [e] (process-state-and-event mouse-event e))))

;;

(defn- add-events-state-processors
  "Add listeners for mouse and keyboard to store state"
  [^Window w]
  (let [mouse-events (proxy [MouseAdapter] []
                       (mousePressed [e] (swap! (.events w) assoc
                                                :mouse-pressed? true
                                                :mouse-button (mouse-button e)
                                                :control-down? (control-down? e)
                                                :alt-down? (alt-down? e)
                                                :meta-down? (meta-down? e)
                                                :shift-down? (shift-down? e)
                                                :alt-gr-down? (alt-gr-down? e)))
                       (mouseReleased [_] (swap! (.events w) assoc :mouse-pressed? false)))
        key-events (proxy [KeyAdapter] []
                     (keyPressed [e] (swap! (.events w) assoc
                                            :key-pressed? true
                                            :key-code (key-code e)
                                            :key-char (key-char e)
                                            :key-raw (key-raw e)
                                            :control-down? (control-down? e)
                                            :alt-down? (alt-down? e)
                                            :meta-down? (meta-down? e)
                                            :shift-down? (shift-down? e)
                                            :alt-gr-down? (alt-gr-down? e)))
                     (keyReleased [_] (swap! (.events w) assoc :key-pressed? false)))]
    (doto ^java.awt.Canvas (.panel w)
      (.addMouseListener mouse-events)
      (.addKeyListener key-events))))

(defn close-window
  "Close window programmatically"
  {:metadoc/categories #{:window}}
  [window]
  (.dispatchEvent ^JFrame (:frame window) (java.awt.event.WindowEvent. (:frame window) java.awt.event.WindowEvent/WINDOW_CLOSING)))

;; ### Frame machinery functions
;;
;; Window is JFrame with panel (as java.awt.Canvas object) which is used to draw clojure2d canvas on it.

(defn- create-panel
  "Create panel which displays canvas. Attach mouse events, give a name (same as window), set size etc."
  [buffer windowname width height]
  (let [panel (java.awt.Canvas.)
        d (Dimension. width height)]
    (doto panel
      (.setName windowname)
      (.addMouseListener mouse-processor)
      (.addKeyListener key-char-processor)
      (.addKeyListener key-event-processor)
      (.addMouseMotionListener mouse-motion-processor)
      (.setFocusTraversalKeysEnabled false)
      (.setIgnoreRepaint true)
      (.setPreferredSize d)
      (.setBackground Color/white))))

(defn- close-window-fn
  "Close window frame"
  [^JFrame frame active? windowname]
  (reset! active? false)
  (clear-state! windowname)
  (.dispose frame))

;; Create lazy list of icons to be loaded by frame
(def ^:private window-icons (map #(.getImage (ImageIcon. (resource (str "icons/i" % ".png")))) [10 16 20 24 30 32 40 44 64 128]))

(defn- build-frame
  "Create JFrame object, create and attach panel and do what is needed to show window. Attach key events and closing event."
  [^JFrame frame ^java.awt.Canvas panel active? on-top? windowname width height]
  (let [closer (proxy [WindowAdapter] []
                 (windowClosing [^WindowEvent e] (close-window-fn frame active? windowname)))]
    (doto frame
      (.setLayout (java.awt.BorderLayout.))
      (.setIconImages window-icons)
      (.add panel)
      (.setSize (Dimension. width height))
      (.invalidate)
      (.setResizable false)
      (.pack)
      (.setDefaultCloseOperation JFrame/DO_NOTHING_ON_CLOSE)
      (.addWindowListener closer)
      (.setName windowname)
      (.setTitle windowname)
      (.setBackground Color/white)
      (.setLocationRelativeTo nil)
      (.setVisible true)
      (.toFront)
      (.setAlwaysOnTop on-top?))
    (doto panel
      (.requestFocus)
      (.createBufferStrategy 2))))

(defn- repaint
  "Draw buffer on panel using `BufferStrategy` object."
  [^java.awt.Canvas panel ^Canvas canvas ^BufferedImage background hints]
  (let [^BufferStrategy strategy (.getBufferStrategy panel)
        ^BufferedImage b (.buffer canvas)]
    (when strategy
      (loop []
        (loop []
          (let [^Graphics2D graphics-context (.getDrawGraphics strategy)]
            (.drawImage graphics-context background 0 0 (.getWidth panel) (.getHeight panel) nil)
            (when hints (set-rendering-hints graphics-context hints))
            (.drawImage graphics-context b 0 0 (.getWidth panel) (.getHeight panel) nil) ;; sizes of panel???
            (.dispose graphics-context))
          (when (.contentsRestored strategy) (recur)))
        (.show strategy)
        (when (.contentsLost strategy) (recur))))
    (.sync (Toolkit/getDefaultToolkit))))

(deftype WithExceptionT [exception? value])

(defn- refresh-screen-task-safety
  "Repaint canvas on window with set FPS.

  * Input: frame, active? atom, function to run before repaint, canvas and sleep time."
  [^Window window draw-fun draw-state hints]
  (let [stime (/ 1000.0 ^double (.fps window))]
    (loop [cnt (long 0)
           result draw-state
           t (System/nanoTime)
           overt 0.0]
      (let [^WithExceptionT new-result (try
                                         (WithExceptionT. false (when (and draw-fun @(.active? window)) ; call draw only when window is active and draw-fun is defined
                                                                  (with-canvas-> @(.buffer window)
                                                                    (draw-fun window cnt result))))
                                         (catch Throwable e
                                           (.printStackTrace e)
                                           (WithExceptionT. true e)))] 
        (let [at (System/nanoTime)
              diff (/ (- at t) 1.0e6)
              delay (- stime diff overt)]
          (when (pos? delay)
            (Thread/sleep (long delay) (int (* 1000000.0 (m/frac delay)))))
          (repaint (.panel window) @(.buffer window) (.background window) hints)
          (when (bool-and @(.active? window) (not (.exception? new-result)))
            (recur (inc cnt)
                   (.value new-result)
                   (System/nanoTime)
                   (if (pos? delay) (- (/ (- (System/nanoTime) at) 1.0e6) delay) 0.0))))))))


(defn- refresh-screen-task-speed
  "Repaint canvas on window with set FPS.

  * Input: frame, active? atom, function to run before repaint, canvas and sleep time."
  [^Window window draw-fun draw-state hints]
  (let [stime (/ 1000.0 ^double (.fps window))]
    (with-canvas [canvas @(.buffer window)]
      (loop [cnt (long 0)
             result draw-state
             t (System/nanoTime)
             overt 0.0]
        (let [^WithExceptionT new-result (try
                                           (WithExceptionT. false (when (and draw-fun @(.active? window)) ; call draw only when window is active and draw-fun is defined
                                                                    (draw-fun canvas window cnt result)))
                                           (catch Throwable e
                                             (when @(.active? window) (.printStackTrace e))
                                             (WithExceptionT. true e)))] 
          (let [at (System/nanoTime)
                diff (/ (- at t) 1.0e6)
                delay (- stime diff overt)]
            (when (pos? delay)
              (Thread/sleep (long delay) (int (* 1000000.0 (m/frac delay)))))
            (repaint (.panel window) @(.buffer window) (.background window) hints)
            (when (bool-and @(.active? window) (not (.exception? new-result)))
              (recur (inc cnt)
                     (.value new-result)
                     (System/nanoTime)
                     (if (pos? delay) (- (/ (- (System/nanoTime) at) 1.0e6) delay) 0.0)))))))))

;;

(defn replace-canvas
  "Replace canvas in window.

  * Input: window and new canvas
  * Returns canvas"
  {:metadoc/categories #{:canvas :window}}
  [^Window window canvas]
  (reset! (.buffer window) canvas))

;; You may want to extract canvas bound to window

(defn get-canvas
  "Returns canvas bound to `window`."
  {:metadoc/categories #{:canvas :window}}
  [^Window window]
  @(.buffer window))

;;

(declare to-hex)

(defn show-window
  "Show window for given canvas

  * Returns `Window` type value

  As parameters you can provide a map with folowing keys:

  * `:canvas` - canvas attached to window (default is canvas 200x200 px)
  * `:window-name` - window name, used also for events dispatch and global state
  * `:w` - width of the window (default as canvas width)
  * `:h` - height of the window (default as canvas heiht)
  * `:fps` - refresh rate
  * `:draw-fn` - drawing callback (fn [canvas window frame loop-state] ... new-loop-state)
  * `:state` - initial global state data
  * `:draw-state` - initial drawing state
  * `:setup` - inital callback function, returns drawing state (fn [canvas window] ... initial-loop-state)
  * `:hint` - rendering hint for display: `:low`, `:mid`, `:high` or `:highest`
  * `:refresher` - `:safe` (default) or `:fast`
  * `:background` - background color for window panel, default white."
  {:metadoc/categories #{:window}}
  ([canvas window-name]
   (show-window {:canvas canvas
                 :window-name window-name}))
  ([canvas window-name draw-fn]
   (show-window {:canvas canvas
                 :window-name window-name
                 :draw-fn draw-fn}))
  ([canvas window-name fps draw-fn]
   (show-window {:canvas canvas
                 :window-name window-name
                 :fps fps
                 :draw-fn draw-fn}))
  ([canvas window-name w h fps]
   (show-window {:canvas canvas
                 :window-name window-name
                 :fps fps
                 :w w
                 :h h}))
  ([canvas window-name w h fps draw-fn]
   (show-window {:canvas canvas
                 :window-name window-name
                 :fps fps
                 :w w
                 :h h
                 :draw-fn draw-fn}))
  ([{:keys [canvas window-name w h fps draw-fn state draw-state setup hint refresher always-on-top? background]
     :or {canvas (canvas 200 200)
          window-name (str "Clojure2D - " (to-hex (rand-int (Integer/MAX_VALUE)) 8))
          fps 60
          draw-fn nil
          state nil
          draw-state nil
          setup nil
          hint nil
          refresher nil
          always-on-top? false
          background :white}}]
   (let [w (or w (width canvas))
         h (or h (height canvas))
         active? (atom true)
         buffer (atom canvas)
         frame (JFrame.)
         panel (create-panel buffer window-name w h)
         window (->Window frame
                          active?
                          buffer
                          panel
                          fps
                          w
                          h
                          window-name
                          (atom {})
                          (get-image (with-canvas-> (clojure2d.core/canvas w h)
                                       (set-background background))))
         setup-state (when setup (with-canvas-> canvas
                                   (setup window))) 
         refresh-screen-task (if (= refresher :fast)
                               refresh-screen-task-speed
                               refresh-screen-task-safety)]
     (SwingUtilities/invokeAndWait #(build-frame frame panel active? always-on-top? window-name w h))
     (add-events-state-processors window)
     (change-state! window-name state)
     (future (refresh-screen-task window draw-fn (or setup-state draw-state) (when hint (get-rendering-hints hint :mid))))
     window))
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

;; ## Date/Time functions

(defn year "Current year" {:metadoc/categories #{:dt}} ^long [] (.get ^Calendar (Calendar/getInstance) Calendar/YEAR))
(defn month "Current month" {:metadoc/categories #{:dt}} ^long [] (inc ^int (.get ^Calendar (Calendar/getInstance) Calendar/MONTH)))
(defn day "Current day" {:metadoc/categories #{:dt}} ^long [] (.get ^Calendar (Calendar/getInstance) Calendar/DAY_OF_MONTH))
(defn hour "Current hour" {:metadoc/categories #{:dt}} ^long [] (.get ^Calendar (Calendar/getInstance) Calendar/HOUR_OF_DAY))
(defn minute "Current minute" {:metadoc/categories #{:dt}} ^long [] (.get ^Calendar (Calendar/getInstance) Calendar/MINUTE))
(defn sec "Current second" {:metadoc/categories #{:dt}} ^long [] (.get ^Calendar (Calendar/getInstance) Calendar/SECOND))
(defn millis "Milliseconds from epoch" {:metadoc/categories #{:dt}} ^long [] (System/currentTimeMillis))
(defn nanos "JVM running time in nanoseconds" {:metadoc/categories #{:dt}} ^long [] (System/nanoTime))
(defn datetime
  "Date time values in the array. Optional parameter :vector or :hashmap (default) to indicate what to return."
  {:metadoc/categories #{:dt}}
  ([type-of-array]
   (let [y (year) m (month) d (day)
         h (hour) mi (minute) s (sec) mil (millis)
         n (nanos)]
     (if (= type-of-array :vector)
       [y m d h mi s mil n]
       {:year y :month m :day d :hour h :minute mi :second s :millis mil :nanos n :sec s})))
  ([] (datetime :hashmap)))

;; ## Load bytes

(defn load-bytes
  "Load file and return byte array."
  [file]
  (with-open [in (clojure.java.io/input-stream file)
              out (java.io.ByteArrayOutputStream.)]
    (clojure.java.io/copy in out)
    (.toByteArray out)))

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
     (fn ^long [] (swap! tick #(inc ^long %)))))
  ([]
   (make-counter 0)))

;; Store date format in variable
(def ^:private ^java.text.SimpleDateFormat simple-date-format (java.text.SimpleDateFormat. "yyyyMMddHHmmss"))
(def ^:private ^java.text.SimpleDateFormat simple-date-format-full (java.text.SimpleDateFormat. "yyyy-MM-dd HH:mm:ss"))

;; Following block defines session related functions.

;; Session values are packed into the type
(defrecord SessionType [logger
                        name
                        counter])

;; Session is stored in agent
(defonce ^:private session-agent (agent (map->SessionType {})))

;; Logging to file is turned off by default.
(def ^:dynamic ^{:doc "Should [[log]] save to file under [[log-name]]. Print to terminal if `false` (default)."
                 :metadoc/categories #{:session}} *log-to-file* false)

(defn- close-session-fn
  "Close current session"
  [s]
  (let [^java.io.Writer o (:logger s)]
    (when-not (nil? o)
      (.flush o)
      (.close o)))
  (map->SessionType {}))

(defn- make-session-name
  "Create unique session name based on current time. Result is a vector with date and hash represented as hexadecimary number."
  []
  (let [date (java.util.Date.)]
    [(.format simple-date-format date) (to-hex (hash date) 8)]))

(declare session-name)

(defn log-name
  "Returns log file name with path."
  {:metadoc/categories #{:session}}
  []
  (str "log/" (first (session-name)) ".log"))

(defn- make-logger-fn
  "Create writer for logger"
  [session-name]
  (let [fname (log-name)]
    (make-parents fname) 
    (let [^java.io.Writer no (writer fname :append true)]
      (.write no (str "Session id: " (second session-name) (System/lineSeparator) (System/lineSeparator)))
      no)))

(defn- make-session-fn 
  "Create session"
  [^SessionType s]
  (close-session-fn s)
  (let [nname (make-session-name)
        writer (when *log-to-file* (make-logger-fn nname))]
    (->SessionType writer nname (make-counter 0))))

(defn make-session
  "Create session via agent"
  {:metadoc/categories #{:session}}
  []
  (send session-agent make-session-fn)
  (await-for 1000 session-agent)
  (:name @session-agent))

(defn close-session
  "Close session via agent"
  {:metadoc/categories #{:session}}
  []
  (send session-agent close-session-fn)
  (await-for 1000 session-agent))

(defn ensure-session
  "Ensure that session is active (create one if not)"
  {:metadoc/categories #{:session}}
  []
  (when (nil? (:name @session-agent))
    (make-session)))

(defn session-name
  "Get session name"
  {:metadoc/categories #{:session}}
  []
  (ensure-session)
  (:name @session-agent))

(defn next-filename
  "Create next unique filename based on session"
  {:metadoc/categories #{:session}}
  ([prefix]
   (ensure-session)
   (let [s @session-agent]
     (str prefix (second (:name s)) "_" (format "%06d" ((:counter s))))))
  ([prefix suffix]
   (str (next-filename prefix) suffix)))

(defn log
  "Log message to file or console"
  {:metadoc/categories #{:session}}
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

(defn int-array-2d
  "Create 2d int array getter and setter methods. Array is mutable!"
  [^long sizex ^long sizey]
  (let [buff (int-array (* sizex sizey))]
    [#(aget ^ints buff (+ ^long %1 (* sizex ^long %2)))
     #(aset ^ints buff (+ ^long %1 (* sizex ^long %2)) ^int %3)]))

(defn long-array-2d
  "Create 2d int array getter and setter methods. Array is mutable!"
  [^long sizex ^long sizey]
  (let [buff (long-array (* sizex sizey))]
    [#(aget ^longs buff (+ ^long %1 (* sizex ^long %2)))
     #(aset ^longs buff (+ ^long %1 (* sizex ^long %2)) ^long %3)]))

(defn double-array-2d
  "Create 2d int array getter and setter methods. Array is mutable!"
  [^long sizex ^long sizey]
  (let [buff (double-array (* sizex sizey))]
    [#(aget ^doubles buff (+ ^long %1 (* sizex ^long %2)))
     #(aset ^doubles buff (+ ^long %1 (* sizex ^long %2)) ^double %3)]))

;;

