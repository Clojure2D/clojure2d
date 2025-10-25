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
  * `:retina` - retina display with `:high` quality

  Hints can be provided as a set in case if you want to combine `:retina` with different quality than `:high`

  To draw on Canvas you have to create graphical context. Wrap your code into one of two functions:

  * [[with-canvas]] - binding macro `(with-canvas [local-canvas canvas-object] ...)`
  * [[with-canvas->]] - threading macro `(with-canvas-> canvas ...)`.

  Each function in this macro has to accept Canvas as first parameter and return Canvas.

  Canvas bound to Window and accessed via callback drawing function (a'ka Processing `draw()`) has graphical context created automatically.

  ### Retina / High density displays

  When quality hint for canvas is set to `:retina` (to allow proper window / canvas scaling) internal ImageBuffer is doubled in size and drawing is done with `scale` set to 2.0 by default.
  When image is retrieved (via `get-image`, `save` or during window display), resulting image is an internal ImageBuffer scaled down.
  
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
  * When you privide drawing function, it's called every refreshment. By default graphical context is created every call - which costs time but is safe in case you want to directly access pixels. The second variant which can be used is to create graphical context once at the moment of window creation. This variant can be forced by setting `:refresher` parameter to `:fast`
  * In case you don't want to refresh window content automatically, set `:refresher` to `:onrepaint`. This will lower CPU consumption.
  * You can replace canvas attached to window with [[replace-canvas]] function.
  * Window itself acts as event object (implements all event protocols)
  * Canvas and window can have different sizes. Display refreshing functions will scale up/down in such case.
  * Events and refreshment are not synchronized. Synchonization can be done explicitely by wrapping functions with `locking` macro.
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
  (:require [clojure2d.color :as c]
            [fastmath.vector :as v]
            [fastmath.core :as m]
            [clojure.reflect :as ref]
            [clojure.string :as s]
            [fastmath.grid :as grid]
            [clojure.java.io :as io]
            [clojure2d.protocols :as pr]
            [clojure2d.core.shape :as sh])
  (:import [java.awt Font Shape BasicStroke Color Component Dimension Graphics2D GraphicsEnvironment Image RenderingHints Toolkit Transparency]
           [java.awt.event InputEvent KeyAdapter KeyEvent MouseAdapter MouseEvent MouseMotionAdapter WindowAdapter WindowEvent]
           [java.awt.geom PathIterator Path2D Path2D$Double Rectangle2D$Double Point2D Point2D$Double]
           [java.awt.image BufferedImage BufferStrategy Kernel ConvolveOp]
           [java.util Iterator Calendar]
           [javax.imageio IIOImage ImageIO ImageWriteParam ImageWriter]
           [javax.swing ImageIcon JFrame SwingUtilities]
           [org.apache.batik.transcoder.image ImageTranscoder]
           [org.apache.batik.transcoder TranscoderInput]
           #_[org.apache.batik.svggen SVGGraphics2D]
           [fastmath.vector Vec2]))

(set! *unchecked-math* :warn-on-boxed)
(set! *warn-on-reflection* true)
(m/use-primitive-operators)

;; how many tasks we can run (one less than available cores)?
(def ^{:const true :tag 'long :doc "How much processor cores are in system. Constant is machine dependant."}
  available-cores (.availableProcessors (Runtime/getRuntime)))

(def ^{:const true :tag 'long :doc "How much intensive tasks can we run. Which is 150% of available cores. Constant is machine dependant."}
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

  * Input: Image filename with absolute or relative path (relative to your project folder) or url.
  * Returns BufferedImage object or nil when Image can't be loaded.

  For supported formats check [[img-reader-formats]]."
  {:metadoc/categories #{:image}}
  [^String filename-or-url]
  (try
    (force-argb-image (ImageIO/read (io/input-stream filename-or-url)))
    (catch Exception e (println "Can't load image: " filename-or-url " " (.getMessage e)))))

(defn load-url-image
  "Load image from given URL, see [[load-image]]."
  [^String url]
  {:metadoc/categories #{:image}
   :deprecated "Use `load-image`."}
  (load-image url))


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

(defn- get-image-writer
  "Returns image writer of image type based on extension."
  ^ImageWriter [filename]
  (let [ext (file-extension filename)
        ^Iterator iter (ImageIO/getImageWritersByFormatName ext)]
    (when (.hasNext iter)
      (.next iter))))

(defn- flatten-image
  "Flatten image, properly drop alpha channel.

  * Input: ARGB BufferedImage object
  * Returns RGB BufferedImage object"
  ^BufferedImage [^BufferedImage img]
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
   (with-open [os (io/output-stream filename)]
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

(defn- save-jpg-file
  [filename img ^ImageWriter writer]
  (let [nimg (flatten-image img)
        ^ImageWriteParam param (.getDefaultWriteParam writer)]
    (doto param
      (.setCompressionMode ImageWriteParam/MODE_EXPLICIT)
      (.setCompressionQuality *jpeg-image-quality*))
    (do-save filename nimg writer param)))

(defmethod save-file-type :jpg
  [filename img writer]
  (save-jpg-file filename img writer))

(defmethod save-file-type :jpeg
  [filename img writer]
  (save-jpg-file filename img writer))

;; BMP also requires image flattening
(defmethod save-file-type :bmp
  [filename img writer]
  (do-save filename (flatten-image img) writer))

(defmethod save-file-type :tiff
  [filename img writer]
  (do-save filename (flatten-image img) writer))

(defmethod save-file-type :tif
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
  (io/make-parents filename)
  (let [iwriter (get-image-writer filename)]
    (if-not (nil? iwriter)
      (do
        (save-file-type filename b iwriter)
        (println "...done!"))
      (println (str "can't save an image: " filename)))))

;;

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

(defn- screen-size
  "Screen size from java.awt.Toolkit."
  ^Dimension [] (.getScreenSize (Toolkit/getDefaultToolkit)))

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
  pr/ImageProto
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
                 (.filter (ConvolveOp. kernel) i nil)))
   :subimage get-subimage
   :resize resize-image})

;;

(declare resize-canvas)

(defrecord Canvas [^Graphics2D graphics
                   ^BufferedImage buffer
                   hints
                   ^long w
                   ^long h
                   transform-stack
                   font
                   retina?]
  pr/CanvasProto
  (graphics2d [_] graphics)
  (transform-stack-atom [_] transform-stack)
  pr/ImageProto
  (get-image [_] (if retina? (pr/resize buffer w h) buffer))
  (width [_] w)
  (height [_] h)
  (save [c n] (save-image buffer n) c)
  (convolve [_ t]  (pr/convolve buffer t))
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
  ([hint-pred default]
   (rendering-hints (or (some hint-pred (keys rendering-hints)) default)))
  ([hint-pred]
   (get-rendering-hints hint-pred :high)))

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
    (when (.retina? canvas) (.scale ng 2.0 2.0))
    (Canvas. ng
             (.buffer canvas)
             (.hints canvas)
             (.w canvas)
             (.h canvas)
             (atom [])
             (.font canvas)
             (.retina? canvas))))

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

(defn- resolve-canvas-options
  [options]
  (cond
    (keyword? options) [(get-rendering-hints #{options}) (= :retina options)]
    (set? options) [(get-rendering-hints options) (options :retina)]
    (map? options) ((juxt (comp get-rendering-hints set :hint) :retina?) options)
    :else options))

(defn canvas
  "Create and return Canvas with `width`, `height` and optionally quality hints and font (name or Font object).

  Default hint is `:high`.
  Possible options are: `:low`, `:mid`, `:high`, `:highest` and `:retina`. When `:retina` is selected, rendering hint is set to `:high`. To select other rendering hint you can provide a set, eg. `#{:low :retina}`.

  When `:retina` is set, created canvas is doubled and transform matrix is set for scaling 2.0
  
  Default font is system one.

  Canvas is an object which keeps everything needed to draw Java2d primitives on it. As a drawing buffer `BufferedImage` is used. To draw on Canvas directly wrap your functions with [[with-canvas]] or [[with-canvas->]] macros to create graphical context.
  
  Canvas is transparent by default. See also [[black-canvas]].
  
  Be aware that drawing on Canvas is single threaded.

  Font you set while creating canvas will be default font. You can set another font in the code with [[set-font]] and [[set-font-attributes]] functions. However these set font temporary."
  {:metadoc/categories #{:canvas}}
  ([{:keys [^long width ^long height hints font]
     :or {hints :high width 600 height 600}}]
   (let [[hint retina?] (resolve-canvas-options hints)
         w (if retina? (* 2 width) width)
         h (if retina? (* 2 height) height)
         ^BufferedImage buffer (try (.. GraphicsEnvironment 
                                        (getLocalGraphicsEnvironment)
                                        (getDefaultScreenDevice)
                                        (getDefaultConfiguration)
                                        (createCompatibleImage w h Transparency/TRANSLUCENT))
                                    (catch java.awt.HeadlessException _
                                      (BufferedImage. w h BufferedImage/TYPE_INT_ARGB)))
         result (Canvas. nil
                         buffer
                         hint
                         width height
                         nil
                         (cond
                           (string? font) (Font/decode font)
                           (instance? Font font) font)
                         retina?)]
     (with-canvas [c result]
       (.setComposite ^Graphics2D (.graphics ^Canvas c) java.awt.AlphaComposite/Src)
       (set-background c Color/black 0))
     result))
  ([width height hints font]
   (canvas {:width width :height height :hints hints :font font}))
  ([width height]
   (canvas {:width width :height height}))
  ([width height hints]
   (canvas {:width width :height height :hints hints})))

(defn black-canvas
  "Create [[canvas]] with black, opaque background."
  {:metadoc/categories #{:canvas}}
  [& r] (with-canvas-> (apply canvas r)
          (set-background :black)))

(defn- resize-canvas
  "Resize canvas to new dimensions. Creates and returns new canvas."
  [^Canvas c width height]
  (let [ncanvas (canvas width height [(.hints c) (.retina? c)] (.font c))]
    (with-canvas-> ncanvas
      (image (pr/get-image c)))))

;;

(defn scale
  "Scale canvas"
  ([canvas ^double scalex ^double scaley]
   (.scale ^Graphics2D (pr/graphics2d canvas) scalex scaley)
   canvas)
  ([canvas ^double s] (scale canvas s s)))

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
  ([canvas ^double tx ^double ty]
   (.translate ^Graphics2D (pr/graphics2d canvas) tx ty)
   canvas)
  ([canvas [^double x ^double y]] (translate canvas x y)))

(defn rotate
  "Rotate canvas"
  [canvas ^double angle]
  (.rotate ^Graphics2D (pr/graphics2d canvas) angle)
  canvas)

(defn shear
  "Shear canvas"
  ([canvas ^double sx ^double sy]
   (.shear ^Graphics2D (pr/graphics2d canvas) sx sy)
   canvas)
  ([canvas ^double s] (shear canvas s s)))

(defn push-matrix
  "Remember current transformation state.

  See also [[pop-matrix]], [[reset-matrix]]."
  [canvas]
  (swap! (pr/transform-stack-atom canvas) conj (.getTransform ^Graphics2D (pr/graphics2d canvas)))
  canvas)

(defn pop-matrix
  "Restore saved transformation state.

  See also [[push-matrix]], [[reset-matrix]]."
  [canvas]
  (when (seq @(pr/transform-stack-atom canvas))
    (let [v (peek @(pr/transform-stack-atom canvas))]
      (swap! (pr/transform-stack-atom canvas) pop)
      (.setTransform ^Graphics2D (pr/graphics2d canvas) v)))
  canvas)

(defn transform
  "Transform given point or coordinates with current transformation. See [[inv-transform]]."
  ([canvas ^double x ^double y]
   (let [^Point2D p (.transform ^java.awt.geom.AffineTransform (.getTransform ^Graphics2D (pr/graphics2d canvas)) (Point2D$Double. x y) nil)]
     (Vec2. (.getX p) (.getY p))))
  ([canvas [^double x ^double y]]
   (transform canvas x y)))

(defn inv-transform
  "Inverse transform of given point or coordinates with current transformation. See [[transform]]."
  ([canvas ^double x ^double y]
   (let [^Point2D p (.inverseTransform ^java.awt.geom.AffineTransform (.getTransform ^Graphics2D (pr/graphics2d canvas)) (Point2D$Double. x y) nil)]
     (Vec2. (.getX p) (.getY p))))
  ([canvas [^double x ^double y]]
   (inv-transform canvas x y)))

(defn reset-matrix
  "Reset transformations."
  [canvas]
  (.setTransform ^Graphics2D (pr/graphics2d canvas) (java.awt.geom.AffineTransform.))
  (when (:retina? canvas) (.scale ^Graphics2D (pr/graphics2d canvas) 2.0 2.0))
  canvas)

;; canvas orientation

(defmulti orient-canvas
  "Place origin into one of four window corners and reorient axes.

  * `-` as suffix - means default orientation (y-axis reversed)
  * `+` as suffix - means usual (mathematical) orientation (y-axis not reversed)."
  {:metadoc/categories #{:transform :canvas}}
  (fn [orientation _] orientation))

(defmethod orient-canvas :default [_ c] c)
(defmethod orient-canvas :top-left- [_ c] c)

(defmethod orient-canvas :top-left+ [_ c]
  (-> c
      (flip-y)
      (rotate m/-HALF_PI)))

(defmethod orient-canvas :top-right- [_ ^Canvas c]
  (-> c
      (translate (dec (.w c)) 0.0)
      (rotate m/HALF_PI)))

(defmethod orient-canvas :top-right+ [_ ^Canvas c]
  (-> c
      (translate (dec (.w c)) 0.0)
      (flip-x)))

(defmethod orient-canvas :bottom-left+ [_ ^Canvas c]
  (-> c
      (translate 0.0 (dec (.h c)))
      (flip-y)))

(defmethod orient-canvas :bottom-left- [_ ^Canvas c]
  (-> c
      (translate 0.0 (dec (.h c)))
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

;; shape

(defn bounding-box
  "Returns `[x,y,width,height]` of shape's bounding box."
  [shape] (pr/bounding-box shape))

(defn contains-point?
  "Returns `true` if given point is inside a shape."
  ([shape ^double x ^double y] (pr/contains-point? shape x y))
  ([shape point] (pr/contains-point? shape (first point) (second point))))

(defn contains-rectangle?
  "Returns `true` if given rectangle is inside a shape."
  ([shape x y w h] (pr/contains-rectangle? shape x y w h))
  ([shape [^double x ^double y ^double w ^double h]] (pr/contains-rectangle? shape x y w h)))

(defn intersects-rectangle?
  "Returns `true` if given rectangle is inside a shape."
  ([shape x y w h] (pr/intersects-rectangle? shape x y w h))
  ([shape [^double x ^double y ^double w ^double h]] (pr/intersects-rectangle? shape x y w h)))

;; shape - path

(def ^:private segment-types->keywords {PathIterator/SEG_CLOSE :close
                                      PathIterator/SEG_LINETO :line
                                      PathIterator/SEG_MOVETO :move
                                      PathIterator/SEG_QUADTO :quad
                                      PathIterator/SEG_CUBICTO :cubic})

(def ^:private winding-rules->keywords {PathIterator/WIND_EVEN_ODD :even-odd
                                      PathIterator/WIND_NON_ZERO :non-zero})

(defn- shape->path-seq
  ([iterator] (shape->path-seq iterator (double-array 6)))
  ([^PathIterator iterator ^doubles buff]
   (lazy-seq
    (when-not (.isDone iterator)
      (let [t (segment-types->keywords (.currentSegment iterator buff) :unknown)
            w (winding-rules->keywords (.getWindingRule iterator) :unknown)
            triplet [t (case t
                         :close nil
                         :line (vec (take 2 buff))
                         :move (vec (take 2 buff))
                         :quad (vec (take 4 buff))
                         :cubic (vec buff)
                         :unknown) w]]
        (.next iterator)
        (cons triplet (shape->path-seq iterator buff)))))))

(defn shape->path-def
  "Create path definition from a shape
  
  Returns sequence of triplets of [command coords winding-rule]:

  * `[:move [x y]]` - move to a position
  * `[:line [x y]]` - line to a position
  * `[:quad [x1 y1 x2 y2]` - curved line to a position
  * `[:cubic [x1 y1 x2 y2 x3 y3]]` - bezier line to a position
  * `[:close]` - close path
  
  Winding rule is one of `:even-odd` and `:non-zero` (default)  

  See [[path-def->shape]]"
  [^Shape shape]
  (shape->path-seq (.getPathIterator shape nil)))

(defn- seg-moveto [^Path2D p [x1 y1]] (.moveTo p x1 y1))
(defn- seg-lineto [^Path2D p [x1 y1]] (.lineTo p x1 y1))
(defn- seg-quadto [^Path2D p [x1 y1 x2 y2]] (.quadTo p x1 y1 x2 y2))
(defn- seg-cubicto [^Path2D p [x1 y1 x2 y2 x3 y3]] (.curveTo p x1 y1 x2 y2 x3 y3))

(defn path-def->shape
  "Create a shape from path definition, see [[shape->path-def]].
  
  Path entry is a tripplet [command arguments [winding-rule]]  

  * `[:move [x y]]` - move to a position
  * `[:line [x y]]` - line to a position
  * `[:quad [x1 y1 x2 y2]` - curved line to a position
  * `[:cubic [x1 y1 x2 y2 x3 y3]]` - bezier line to a position
  * `[:close]` - close path
  * `[:shape [shape-object connect?]]` - append given shape
  
  Winding rule can be one of `:even-odd` and `:non-zero` (default) and is optional. "
  [path-def]
  (let [^Path2D p (Path2D$Double.)]
    (doseq [[t v w] path-def]
      (when w (.setWindingRule p (case t
                                   :even-odd Path2D/WIND_EVEN_ODD
                                   :non-zero Path2D/WIND_NON_ZERO)))
      (case t
        :close (.closePath p)
        :line (seg-lineto p v)
        :quad (seg-quadto p v)
        :cubic (seg-cubicto p v)
        :move (seg-moveto p v)
        :shape (.append p ^Shape (first v) (if (second v) true false))))
    p))

;; clip

(defn clip
  "Clip drawing to specified rectangle or shape.

  See also [[reset-clip]]."
  ([canvas x y w h] 
   (.setClip ^Graphics2D (pr/graphics2d canvas) x y w h)
   canvas)
  ([canvas ^Shape shape] 
   (.setClip ^Graphics2D (pr/graphics2d canvas) shape)
   canvas))

(defn reset-clip
  "Resets current clipping.

  See also [[clip]]."
  [canvas] 
  (.setClip ^Graphics2D (pr/graphics2d canvas) 0 0 (pr/width canvas) (pr/height canvas))
  canvas)

;; Drawing functions

(defn shape
  "Draw Java2D shape object"
  ([canvas sh stroke?]
   (if stroke?
     (.draw ^Graphics2D (pr/graphics2d canvas) sh)
     (.fill ^Graphics2D (pr/graphics2d canvas) sh))
   canvas)
  ([canvas sh] (shape canvas sh false)))

(defn line
  "Draw line from point `(x1,y1)` to `(x2,y2)`"
  ([canvas x1 y1 x2 y2]
   (shape canvas (sh/line x1 y1 x2 y2) true))
  ([canvas [x1 y1] [x2 y2]] (line canvas x1 y1 x2 y2)))

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
  ([canvas size cap join miter-limit]
   (.setStroke ^Graphics2D (pr/graphics2d canvas)
               (BasicStroke. size
                             (or (stroke-caps cap) BasicStroke/CAP_ROUND)
                             (or (stroke-joins join) BasicStroke/JOIN_BEVEL)
                             (float miter-limit)))
   canvas)
  ([canvas size cap join] (set-stroke canvas size cap join 1.0))
  ([canvas size cap] (set-stroke canvas size cap :bevel 1.0))
  ([canvas size] (set-stroke canvas size :round :bevel 1.0))
  ([canvas] (set-stroke canvas 1.0)))

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
  [canvas {:keys [size cap join miter-limit dash dash-phase]
           :or {size 1.0 cap :butt join :round miter-limit 1.0 dash nil dash-phase 0.0}}]
  (if dash
    (do
      (.setStroke ^Graphics2D (pr/graphics2d canvas)
                  (BasicStroke. size
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
  ([canvas ^double x ^double y]
   (line canvas x y (+ x 10.0e-6) (+ y 10.0e-6)))
  ([canvas [^double x ^double y]] (point canvas x y)))

(defn rect
  "Draw rectangle with top-left corner at `(x,y)` position with width `w` and height `h`. Optionally you can set `stroke?` (default: `false`) to `true` if you don't want to fill rectangle and draw outline only.

  See also: [[crect]] and [[prect]]."
  ([canvas x y w h stroke?]
   (shape canvas (sh/rect x y w h) stroke?))
  ([canvas x y w h] (rect canvas x y w h false))
  ([canvas [x y] w h] (rect canvas x y w h)))

(defn crect
  "Centered version of [[rect]]."
  ([canvas x y w h stroke?]
   (shape canvas (sh/crect x y w h) stroke?))
  ([canvas x y w h] (crect canvas x y w h false))
  ([canvas [x y] w h] (crect canvas x y w h)))

(defn prect
  "Draw rectangle with top-left corner at `(x1,y1)` and bottom-right corner at `(x2,y2)`. Optionally you can set `stroke?` (default: `false`) to `true` if you don't want to fill rectangle and draw outline only.

  See also: [[rect]]."
  ([canvas x1 y1 x2 y2 stroke?]
   (shape canvas (sh/prect x1 y1 x2 y2) stroke?))
  ([canvas x1 y1 x2 y2] (prect canvas x1 y1 x2 y2 false))
  ([canvas [x1 y1] [x2 y2]] (prect canvas x1 y1 x2 y2)))

(defn ellipse
  "Draw ellipse with middle at `(x,y)` position with width `w` and height `h`."
  ([canvas x y w h stroke?]
   (shape canvas (sh/ellipse x y w h) stroke?))
  ([canvas x y w h] (ellipse canvas x y w h false))
  ([canvas [x y] w h] (ellipse canvas x y w h)))

(defn arc
  "Draw arc with middle at `(x,y)` position with width `w` and height `h`.

  Starting angle `start` and `extent` are in radians. Direction is clockwise.

  Type is one of the:

  * `:open` - default
  * `:pie`
  * `:chord`"
  ([canvas x y w h start extent type stroke?]
   (shape canvas (sh/arc x y w h start extent type) stroke?))
  ([canvas x y w h start extent type] (arc canvas x y w h start extent type true))
  ([canvas x y w h start extent] (arc canvas x y w h start extent :open))
  ([canvas [x y] w h start extent] (arc canvas x y w h start extent)))

(defn rarc
  "Draw arc with middle at `(x,y)` with radius `r`.

  Starting angle `start` and `extent` are in radians. Direction is clockwise.

  Type is one of the:

  * `:open`
  * `:pie`
  * `:chord`"
  ([canvas x y r start extent type stroke?]
   (shape canvas (sh/rarc x y r start extent type) stroke?))
  ([canvas x y r start extent type] (rarc canvas x y r start extent type true))
  ([canvas x y r start extent] (rarc canvas x y r start extent :open))
  ([canvas [x y] r start extent] (rarc canvas x y r start extent)))

(defn triangle
  "Draw triangle with corners at 3 positions."
  ([canvas x1 y1 x2 y2 x3 y3 stroke?]
   (shape canvas (sh/triangle x1 y1 x2 y2 x3 y3) stroke?))
  ([canvas x1 y1 x2 y2 x3 y3] (triangle canvas x1 y1 x2 y2 x3 y3 false))
  ([canvas [x1 y1] [x2 y2] [x3 y3]] (triangle canvas x1 y1 x2 y2 x3 y3)))

(defn triangle-strip
  "Draw triangle strip. Implementation of `Processing` `STRIP` shape.

  Input: list of vertices as vectors.

  See also: [[triangle-fan]]."
  ([canvas vs stroke?]
   (when (> (count vs) 2)
     (loop [v1 (first vs)
            v2 (second vs)
            vss (nnext vs)]
       (when vss
         (let [v3 (first vss)]
           (triangle canvas (first v2) (second v2) (first v3) (second v3) (first v1) (second v1) stroke?)
           (recur v2 v3 (next vss))))))
   canvas)
  ([canvas vs] (triangle-strip canvas vs false)))

(defn triangle-fan
  "Draw triangle fan. Implementation of `Processing` `FAN` shape.

  First point is common vertex of all triangles.
  
  Input: list of vertices as vectors.

  See also: [[triangle-strip]]."
  ([canvas vs stroke?]
   (when (> (count vs) 2)
     (let [v1 (first vs)]
       (loop [v2 (second vs)
              vss (nnext vs)]
         (when vss
           (let [v3 (first vss)]
             (triangle canvas (first v1) (second v1) (first v2) (second v2) (first v3) (second v3) stroke?)
             (recur v3 (next vss)))))))
   canvas)
  ([canvas vs] (triangle-strip canvas vs false)))

(defn path
  "Draw path from lines.

  Input: list of points as vectors, close? - close path or not (default: false), stroke? - draw lines or filled shape (default true - lines).

  See also [[path-bezier]]."
  ([canvas vs close? stroke?]
   (shape canvas (sh/path vs (or (not stroke?) close?)) stroke?))
  ([canvas vs close?] (path canvas vs close? true))
  ([canvas vs] (path canvas vs false true)))

(defn path-bezier
  "Draw path from quad curves.

  Input: list of points as vectors, close? - close path or not (default: false), stroke? - draw lines or filled shape (default true - lines).

  See also [[path]]."
  ([canvas vs close? stroke?]
   (shape canvas (sh/path-bezier vs (or (not stroke?) close?)) stroke?))
  ([canvas vs close?] (path-bezier canvas vs close? true))
  ([canvas vs] (path-bezier canvas vs false true)))

(defn bezier
  "Draw bezier curve with 4 sets of coordinates."
  ([canvas x1 y1 x2 y2 x3 y3 x4 y4 stroke?]
   (shape canvas (sh/bezier x1 y1 x2 y2 x3 y3 x4 y4) stroke?))
  ([canvas x1 y1 x2 y2 x3 y3 x4 y4] (bezier canvas x1 y1 x2 y2 x3 y3 x4 y4 true))
  ([canvas [x1 y1] [x2 y2] [x3 y3] [x4 y4]] (bezier canvas x1 y1 x2 y2 x3 y3 x4 y4)))

(defn curve
  "Draw quadratic curve with 3 sets of coordinates."
  ([^Canvas canvas x1 y1 x2 y2 x3 y3 stroke?]
   (shape canvas (sh/curve x1 y1 x2 y2 x3 y3) stroke?))
  ([canvas x1 y1 x2 y2 x3 y3] (curve canvas x1 y1 x2 y2 x3 y3 true))
  ([canvas [x1 y1] [x2 y2] [x3 y3]] (curve canvas x1 y1 x2 y2 x3 y3)))

(defn quad
  "Draw quad with corners at 4 positions."
  {:metadoc/categories #{:draw}}
  ([^Canvas canvas x1 y1 x2 y2 x3 y3 x4 y4 stroke?]
   (shape canvas (sh/quad x1 y1 x2 y2 x3 y3 x4 y4) stroke?))
  ([canvas x1 y1 x2 y2 x3 y3 x4 y4] (quad canvas x1 y1 x2 y2 x3 y3 x4 y4 false))
  ([canvas [x1 y1] [x2 y2] [x3 y3] [x4 y4]] (quad canvas x1 y1 x2 y2 x3 y3 x4 y4)))

(defn quad-strip
  "Draw quad strip. Implementation of `Processing` shape.
  
  List of vertices as vectors.

  0-2-4-...
  | | |
  1-3-5-..."
  ([canvas vs stroke?]
   (doseq [[[x0 y0] [x1 y1] [x2 y2] [x3 y3]] (partition 4 2 vs)]
     (quad canvas x0 y0 x1 y1 x3 y3 x2 y2 stroke?))
   canvas)
  ([canvas vs] (quad-strip canvas vs false)))

;; hex

(defn pointy-hex
  "Draw pointy topped hex."
  ([canvas x y size stroke?]
   (shape canvas (sh/pointy-hex x y size) stroke?))
  ([canvas x y size] (pointy-hex canvas x y size false))
  ([canvas [x y] size] (pointy-hex canvas x y size)))

(defn flat-hex
  "Draw flat topped hex."
  {:metadoc/categories #{:draw}}
  ([canvas x y size stroke?]
   (shape canvas (sh/flat-hex x y size) stroke?))
  ([canvas x y size] (flat-hex canvas x y size false))
  ([canvas [x y] size] (flat-hex canvas x y size)))

;; grid

(defn grid-cell
  "Draw grid cell for given grid in screen (x,y) coordinates. For cell coordinates, see [[grid-qr-cell]]."
  ([canvas grid x y stroke?]
   (shape canvas (sh/grid-cell grid x y) stroke?))
  ([canvas grid x y] (grid-cell canvas grid x y false))
  ([canvas grid x y scale stroke?]
   (shape canvas (sh/grid-cell grid x y scale) stroke?)))

(defn grid-qr-cell
  "Draw grid cell for given grid in cell (q,r) coordinates. For screen coordinates, see [[grid-cell]]."
  ([canvas grid q r stroke?]
   (let [[x y] (grid/cell->anchor grid q r)]
     (shape canvas (sh/grid-cell grid x y) stroke?)))
  ([canvas grid q r] (grid-qr-cell canvas grid q r false))
  ([canvas grid q r scale stroke?]
   (let [[x y] (grid/cell->anchor grid q r)]
     (shape canvas (grid-cell grid x y scale) stroke?))))

;;

(def ^{:doc "List of all available font names."
     :metadoc/categories #{:write}} fonts-list
  (try
    (into [] (.getAvailableFontFamilyNames (java.awt.GraphicsEnvironment/getLocalGraphicsEnvironment)))
    (catch Exception _ []))) ;; in headless mode function call fails

(defn load-font
  "Load font from file or url. Only TrueType and OpenTrueType fonts are supported."
  [font-file-or-url]
  (Font/createFont Font/TRUETYPE_FONT (io/input-stream font-file-or-url)))

(defn set-font
  "Set font by name or actual font."
  [canvas font-or-fontname]
  (let [f (if (string? font-or-fontname) (Font/decode font-or-fontname) font-or-fontname)]
    (.setFont ^Graphics2D (pr/graphics2d canvas) f)
    canvas))

(defn set-font-attributes
  "Set current font size and attributes.

  Attributes are: `:bold`, `:italic`, `:bold-italic`."
  ([canvas ^double size style]
   (let [s (case style :bold 1 :italic 2 :bold-italic 3 0)
         f (.deriveFont ^Font (.getFont ^Graphics2D (pr/graphics2d canvas)) (int s) (float size))]
     (.setFont ^Graphics2D (pr/graphics2d canvas) f)
     canvas))
  ([canvas ^double size]
   (let [f (.deriveFont ^Font (.getFont ^Graphics2D (pr/graphics2d canvas)) (float size))]
     (.setFont ^Graphics2D (pr/graphics2d canvas) f)
     canvas)))

(defn char-width
  "Returns font width from metrics. Should be called within graphical context."
  ^long [canvas chr]
  (.charWidth (.getFontMetrics ^Graphics2D (pr/graphics2d canvas)) (char chr)))

(defn font-height
  "Returns font width from metrics. Should be called within context."
  ^long [canvas]
  (.getHeight (.getFontMetrics ^Graphics2D (pr/graphics2d canvas))))

(defn font-ascent
  "Returns font width from metrics. Should be called within context."
  ^long [canvas]
  (.getAscent (.getFontMetrics ^Graphics2D (pr/graphics2d canvas))))

(defn text-width
  "Returns width of the provided string. Should be called within context."
  ^long [canvas txt]
  (.stringWidth (.getFontMetrics ^Graphics2D (pr/graphics2d canvas)) (str txt)))

(defn text-bounding-box
  "Returns bounding box [x,y,w,h] for given text. `[x,y]` position is relative to base line."
  {:metadoc/categories #{:write}}
  [canvas txt]
  (let [^Graphics2D g (pr/graphics2d canvas)]
    (sh/rectangle2d->box (.getStringBounds (.getFontMetrics g) (str txt) g))))

(defn text
  "Draw text for given position and alignment.

  Possible alignments are: `:right`, `:center`, `:left`."
  {:metadoc/categories #{:write}}
  ([canvas s x y align]
   (let [x (float x)
         y (float y)
         s (str s)]
     (case align
       :right (let [w (.stringWidth (.getFontMetrics ^Graphics2D (pr/graphics2d canvas)) s)]
                (.drawString ^Graphics2D (pr/graphics2d canvas) s (- x w) y))
       :center (let [w (/ (.stringWidth (.getFontMetrics ^Graphics2D (pr/graphics2d canvas)) s) 2.0)]
                 (.drawString ^Graphics2D (pr/graphics2d canvas) s (float (- x w)) y))
       :left (.drawString ^Graphics2D (pr/graphics2d canvas) s x y)
       (.drawString ^Graphics2D (pr/graphics2d canvas) s x y))) 
   canvas)
  ([canvas s x y] (text canvas s x y :left)))

;; ### Color

(defmacro ^:private make-set-color-fn
  [doc n f]
  `(defn ~n
     ~doc
     {:metadoc/categories #{:draw}}
     ([~'canvas ~'c]
      (~f ~'canvas (c/awt-color ~'c)))
     ([~'canvas ~'c ~'a]
      (~f ~'canvas (c/awt-color ~'c ~'a)))
     ([~'canvas ~'r ~'g ~'b ~'a]
      (~f ~'canvas (c/awt-color ~'r ~'g ~'b ~'a)))
     ([~'canvas ~'r ~'g ~'b]
      (~f ~'canvas (c/awt-color ~'r ~'g ~'b)))))

(defn set-awt-color
  "Set color with valid java `Color` object. Use it when you're sure you pass `java.awt.Color` object."
  [canvas ^java.awt.Color c]
  (.setColor ^Graphics2D (pr/graphics2d canvas) c)
  canvas)

(defn set-awt-background
  "Set background color. Expects valid `java.awt.Color` object."
  ([canvas c]
   (let [^Graphics2D g (pr/graphics2d canvas)
         ^Color currc (.getColor g)]
     (push-matrix canvas)
     (reset-matrix canvas)
     (doto g
       (.setColor(c/awt-color c))
       (.fillRect 0 0 (pr/width canvas) (pr/height canvas))
       (.setColor currc))
     (pop-matrix canvas))
   canvas))

(defn awt-xor-mode
  "Set XOR graphics mode with `java.awt.Color`.

  To revert call [[paint-mode]]."
  {:metadoc/categories #{:draw}}
  [canvas c]
  (.setXORMode ^Graphics2D (pr/graphics2d canvas) c)
  canvas)

;; Set color for primitive
(make-set-color-fn
 "Sets current color. Color can be:

* [[vec3]], [[vec4]] with rgb values.
* java.awt.Color object
* keyword with name from 140 HTML color names
* Integer
* r, g, b and optional alpha

See [[clojure2d.color]] namespace for more color functions."
 set-color set-awt-color)

;; Set background color
(make-set-color-fn
 "Sets background with given color.

Background can be set with alpha.

See [[set-color]]."
 set-background set-awt-background)

;; Set XOR mode
(make-set-color-fn
 "Set XOR painting mode with given color."
 xor-mode awt-xor-mode)

(defn paint-mode
  "Set normal paint mode.

  This is default mode.
  
  See [[gradient-mode]] or [[xor-mode]] for other types."
  {:metadoc/categories #{:draw}}
  [canvas]
  (.setPaintMode ^Graphics2D (pr/graphics2d canvas))
  canvas)

(defn set-composite
  "Set composite method for blending during draw.

  See [[composite]] to create one defined by [[clojure2d.color]] blending modes.

  You can also use ones defined in `java.awt.AlphaComposite` class.

  Call witout parameters to revert to default: `java.awt.AlphaComposite/SrcOver`."
  {:metadoc/categories #{:draw}}
  ([canvas composite]
   (.setComposite ^Graphics2D (pr/graphics2d canvas) composite)
   canvas)
  ([canvas] (set-composite canvas java.awt.AlphaComposite/SrcOver)))

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
  ([canvas x1 y1 color1 x2 y2 color2]
   (gradient-mode canvas x1 y1 color1 x2 y2 color2 false))
  ([canvas x1 y1 color1 x2 y2 color2 cyclic?]
   (let [gp (java.awt.GradientPaint. x1 y1 (c/awt-color color1) x2 y2 (c/awt-color color2)
                                     (boolean cyclic?))
         ^Graphics2D g (pr/graphics2d canvas)]
     (.setPaint g gp)
     canvas)))

(defn- infer-fractions-colors
  [fractions-or-cnt colors-or-gradient]
  (let [[f c] (if (or (number? fractions-or-cnt) (fn? colors-or-gradient))
                (let [fractions (if (number? fractions-or-cnt)
                                  (m/slice-range fractions-or-cnt)
                                  fractions-or-cnt)
                      cnt (if (number? fractions-or-cnt) fractions-or-cnt (count fractions-or-cnt))
                      colors (if (fn? colors-or-gradient)
                               (map colors-or-gradient fractions)
                               (c/palette colors-or-gradient cnt))]
                  [fractions colors])
                [fractions-or-cnt colors-or-gradient])]
    [(into-array Float/TYPE f)
     (into-array (map c/awt-color c))]))

(defn- multiple-gradient-method
  ^java.awt.MultipleGradientPaint$CycleMethod [cycle-method]
  (case cycle-method
    :reflect java.awt.MultipleGradientPaint$CycleMethod/REFLECT
    :repeat java.awt.MultipleGradientPaint$CycleMethod/REPEAT
    java.awt.MultipleGradientPaint$CycleMethod/NO_CYCLE))

(defn radial-gradient-mode
  "Set paint to radial gradient.

  To revert call [[paint-mode]]"
  ([canvas cx cy radius fractions-or-cnt colors-or-gradient]
   (radial-gradient-mode canvas cx cy radius cx cy fractions-or-cnt colors-or-gradient :no-cycle))
  ([canvas cx cy radius fractions-or-cnt colors-or-gradient cycle-method]
   (radial-gradient-mode canvas cx cy radius cx cy fractions-or-cnt colors-or-gradient cycle-method))
  ([canvas cx cy radius fx fy fractions-or-cnt colors-or-gradient cycle-method]
   (let [[fractions colors] (infer-fractions-colors fractions-or-cnt colors-or-gradient)
         gp (java.awt.RadialGradientPaint. (float cx) (float cy)
                                           (float radius)
                                           (float fx) (float fy)
                                           ^floats fractions
                                           ^"[Ljava.awt.Color;" colors
                                           (multiple-gradient-method cycle-method))
         ^Graphics2D g (pr/graphics2d canvas)]
     (.setPaint g gp)
     canvas)))

(defn linear-gradient-mode
  "Set paint to linear multiple gradient.

  To revert call [[paint-mode]]"
  ([canvas sx sy ex ey fractions-or-cnt colors-or-gradient]
   (linear-gradient-mode canvas sx sy ex ey fractions-or-cnt colors-or-gradient :no-cycle))
  ([canvas sx sy ex ey fractions-or-cnt colors-or-gradient cycle-method]
   (let [[fractions colors] (infer-fractions-colors fractions-or-cnt colors-or-gradient)
         gp (java.awt.LinearGradientPaint. (float sx) (float sy)
                                           (float ex) (float ey)
                                           ^floats fractions
                                           ^"[Ljava.awt.Color;" colors
                                           (multiple-gradient-method cycle-method))
         ^Graphics2D g (pr/graphics2d canvas)]
     (.setPaint g gp)
     canvas)))

;; ### Pattern mode

(defn pattern-mode
  "Set paint mode to pattern.

  Default anchor is set to `(0,0)`. Default width and height are the same as texture dimensions.

  To revert call [[paint-mode]]"
  ([canvas image]
   (let [^BufferedImage image (pr/get-image image)]
     (pattern-mode canvas image 0 0 (.getWidth image) (.getHeight image))))
  ([canvas image w h]
   (let [^BufferedImage image (pr/get-image image)]
     (pattern-mode canvas image 0 0 w h))) 
  ([canvas image anchor-x anchor-y w h]
   (let [image (pr/get-image image)
         rect (Rectangle2D$Double. anchor-x anchor-y w h)
         texture (java.awt.TexturePaint. image rect)
         ^Graphics2D g (pr/graphics2d canvas)]
     (.setPaint g texture)
     canvas)))

;; ### Image

(defn image
  "Draw an image.

  You can specify position and size of the image. Default it's placed on whole canvas."
  {:metadoc/categories #{:draw}}
  ([canvas img x y w h]
   (.drawImage ^Graphics2D (pr/graphics2d canvas) (pr/get-image img) x y w h nil)
   canvas)
  ([canvas img]
   (image canvas img 0 0 (pr/width canvas) (pr/height canvas)))
  ([canvas img x y]
   (image canvas img x y (pr/width img) (pr/height img))))

;; SVG

(defn load-svg
  "Load image into Batik Document. See [[transcode-svg]]."
  {:metadoc/categories #{:image}}
  ^TranscoderInput  [^String filename-or-url]
  (TranscoderInput. filename-or-url))

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

#_(defrecord SVGCanvas [^SVGGraphics2D graphics
                        transform-stack
                        font]
    pr/CanvasProto
    (graphics2d [_] graphics)
    (transform-stack-atom [_] transform-stack)
    pr/ImageProto
    (get-image [_ w h] (transcode-svg (TranscoderInput. (.getDOMFactory graphics)) w h))
    (save [c n] (if (str/ends-with? n ".svg")
                  (let [^SVGGraphics2D g2c-copy (.create graphics)
                        out (java.io.StringWriter.)]
                    (.stream g2c-copy out)
                    (spit n out))
                  (pr/save (pr/get-image c) n))
      c))

#_(defn svg-canvas []
    (let [di (org.apache.batik.dom.GenericDOMImplementation/getDOMImplementation)
          doc (.createDocument di org.apache.batik.util.SVGConstants/SVG_NAMESPACE_URI "svg" nil)
          g2d (org.apache.batik.svggen.SVGGraphics2D. doc)]
      (SVGCanvas. g2d (atom []) nil)))

#_(-> (svg-canvas)
      (line 10 10 30 300)
      (line 20 20 100 30)                     
      (save "some.svg")
      (line 20 20 110 30)
      #_  (pr/get-image 200 200)
      (save "some2.svg"))

#_(let [di (org.apache.batik.dom.GenericDOMImplementation/getDOMImplementation)
        doc (.createDocument di org.apache.batik.util.SVGConstants/SVG_NAMESPACE_URI "svg" nil)
        g2d (org.apache.batik.svggen.SVGGraphics2D. doc)
        out (java.io.StringWriter.)]
    (.setPaint g2d Color/red)
    (.fill g2d (ellipse-shape 10 40 20 20))
    (.stream g2d out)
    (str out))

;; Display window

;; Extract all keycodes from `KeyEvent` object and pack it to the map
(defonce ^:private keycodes-map (->> KeyEvent
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

(defn mouse-x
  "Mouse horizontal position within window. 0 - left side. -1 outside window."
  {:metadoc/categories #{:window :events}}
  ^long [m] (pr/mouse-x m))

(defn mouse-y
  "Mouse vertical position within window. 0 - left side. -1 outside window."
  {:metadoc/categories #{:window :events}}
  ^long [m] (pr/mouse-y m))

(defn mouse-pos
  "Mouse position as [[Vec2]] type. [0,0] - top left, [-1,-1] outside window."
  {:metadoc/categories #{:window :events}}
  ^Vec2 [m] (pr/mouse-pos m))

(defn mouse-button
  "Get mouse pressed button status: :left :right :center or :none"
  {:metadoc/categories #{:window :events}}
  [m] (pr/mouse-button m))

(defn key-code
  "Keycode mapped to keyword. See `java.awt.event.KeyEvent` documentation. Eg. `VK_LEFT` is mapped to `:left`."
  {:metadoc/categories #{:window :events}}
  [e] (pr/key-code e))

(defn key-char
  "Key as char."
  {:metadoc/categories #{:window :events}}
  [e] (pr/key-char e))

(defn key-raw
  "Raw value for pressed key (as integer)."
  {:metadoc/categories #{:window :events}}
  ^long [e] (pr/key-raw e))

(defn control-down?
  "CONTROL key state as boolean."
  {:metadoc/categories #{:window :events}}
  [e] (pr/control-down? e))

(defn alt-down?
  "ALT key state as boolean."
  {:metadoc/categories #{:window :events}}
  [e] (pr/alt-down? e))

(defn meta-down?
  "META key state as boolean."
  {:metadoc/categories #{:window :events}}
  [e] (pr/meta-down? e))

(defn shift-down?
  "SHIFT key state as boolean."
  {:metadoc/categories #{:window :events}}
  [e] (pr/shift-down? e))

(defn alt-gr-down?
  "ALT-GR key state as boolean."
  {:metadoc/categories #{:window :events}}
  [e] (pr/alt-gr-down? e))

(defn key-pressed?
  "Any key pressed? (boolean)"
  {:metadoc/categories #{:window :events}}
  [w] (pr/key-pressed? w))

(defn mouse-pressed?
  "Any mouse button pressed? (boolean)"
  {:metadoc/categories #{:window :events}}
  [w] (pr/mouse-pressed? w))

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
  pr/ImageProto
  (get-image [_] (pr/get-image @buffer))
  (width [_] w)
  (height [_] h)
  (save [w n] (save-image (pr/get-image @buffer) n) w)
  (convolve [_ n] (pr/convolve @buffer n))
  (subimage [_ x y w h] (get-subimage @buffer x y w h))
  pr/PressedProto
  (key-pressed? [_] (:key-pressed? @events))
  (mouse-pressed? [_] (:mouse-pressed? @events))
  pr/ModifiersProto
  (control-down? [_] (:control-down? @events))
  (alt-down? [_] (:alt-down? @events)) 
  (meta-down? [_] (:meta-down? @events))
  (shift-down? [_] (:shift-down? @events))
  (alt-gr-down? [_] (:alt-gr-down? @events))
  pr/KeyEventProto
  (key-code [_] (:key-code @events))
  (key-char [_] (:key-char @events))
  (key-raw [_] (:key-raw @events))
  pr/MouseButtonProto
  (mouse-button [_] (:mouse-button @events))
  pr/MouseXYProto
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
  pr/MouseButtonProto
  {:mouse-button (fn [^MouseEvent e]
                   (condp = (.getButton e)
                     MouseEvent/BUTTON1 :left
                     MouseEvent/BUTTON2 :center
                     MouseEvent/BUTTON3 :right
                     :none))}
  pr/MouseXYProto
  {:mouse-x (fn ^long [^MouseEvent e] (.getX e))
   :mouse-y (fn ^long [^MouseEvent e] (.getY e))
   :mouse-pos (fn [^MouseEvent e] (v/vec2 (.getX e) (.getY e)))})

(extend KeyEvent
  pr/KeyEventProto
  {:key-code (fn [^KeyEvent e] (keycodes-map (.getKeyCode e)))
   :key-char (fn [^KeyEvent e] (.getKeyChar e))
   :key-raw (fn ^long [^KeyEvent e] (long (.getKeyCode e)))})

(extend InputEvent
  pr/ModifiersProto
  {:control-down? (fn [^InputEvent e] (.isControlDown e))
   :alt-down? (fn [^InputEvent e] (.isAltDown e))
   :meta-down? (fn [^InputEvent e] (.isMetaDown e))
   :shift-down? (fn [^InputEvent e] (.isShiftDown e))
   :alt-gr-down? (fn [^InputEvent e] (.isAltGraphDown e))})

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
  (and (>= (mouse-x window) 0)
       (>= (mouse-y window) 0)))

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
  [^java.awt.AWTEvent e]
  (.getName ^Component (.getSource e)))

(defn- process-state-and-event 
  "For given event call provided multimethod passing state. Save new state."
  ([ef e]
   (let [window-name (event-window-name e)]
     (change-state! window-name (ef e (@global-state window-name)))))
  ([ef e d]
   (let [window-name (event-window-name e)]
     (change-state! window-name (ef e (@global-state window-name) d)))))

;; Custom event section

(defmulti custom-event
  "Called when [[fire-custom-event]] is invoked. Dispatch is a window name.

  Parameters are: event, state and additional data passed to [[fire-custom-event]]."
  (fn [e _ _] (event-window-name e)))

(defmethod custom-event :default [_ s _] s)

;; Custom event interface
(definterface ICustomEvent (getData []))

(defn fire-custom-event
  "Add an event to AWT Event Queue, helps to process window state. Can carry additional data."
  ([window] (fire-custom-event window nil))
  ([^Window window data]
   (.postEvent (.getSystemEventQueue (java.awt.Toolkit/getDefaultToolkit))
               (proxy [java.awt.event.ActionEvent ICustomEvent]
                   [(.panel window) java.awt.event.ActionEvent/ACTION_FIRST "clojure2d custom event"]
                 (getData [] data)))))

;; add custom event listener, it should be added only once, space for possbile refactoring
#_{:clj-kondo/ignore [:unused-private-var]}
(defonce ^:private _custom-event-listener
  (let [listener (reify
                   java.awt.event.AWTEventListener
                   (eventDispatched [_ event]
                     (when (instance? ICustomEvent event)
                       (let [^ICustomEvent event event]
                         (process-state-and-event custom-event event (.getData event))))))]
    (.addAWTEventListener (java.awt.Toolkit/getDefaultToolkit)
                          listener
                          java.awt.AWTEvent/ACTION_EVENT_MASK)
    listener))

#_(run! #(.removeAWTEventListener (java.awt.Toolkit/getDefaultToolkit) %)
        (.getAWTEventListeners (java.awt.Toolkit/getDefaultToolkit)))

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
  (fn [^KeyEvent e _] [(event-window-name e) (.getKeyChar e)]))
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
  (fn [^KeyEvent e _] [(event-window-name e) (.getKeyChar e)]))
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
  (fn [^KeyEvent e _] [(event-window-name e) (.getKeyChar e)]))
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
  (fn [^KeyEvent e _] [(event-window-name e) (key-event-map (.getID e))]))
;; Do nothing on default
(defmethod key-event :default [_ s] s)

;; Map Java mouse event names onto keywords
(def ^{:doc "Map of supported mouse events"
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
  (fn [^MouseEvent e _] [(event-window-name e) (mouse-event-map (.getID e))]))
;; Do nothing on default
(defmethod mouse-event :default [_ s] s)

;; Event adapter objects.

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
  [windowname width height]
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

(defn- create-panel-auto-repaint
  "Create panel which displays canvas. Attach mouse events, give a name (same as window), set size etc.
  Buffer is repainted automatically without FPS refreshing."
  [windowname width height buffer ^BufferedImage background hints]
  (let [panel (proxy [java.awt.Canvas] []
                (paint [^java.awt.Graphics g]
                  (let [^java.awt.Canvas this this
                        ^Canvas canvas @buffer]
                    (proxy-super paint g)
                    (when background (.drawImage g background 0 0 (.getWidth this) (.getHeight this) nil))
                    (when hints (set-rendering-hints g hints))
                    (.drawImage g (.buffer canvas) 0 0 (.getWidth this) (.getHeight this) nil))))
        d (Dimension. width height)]
    (doto panel
      (.setName windowname)
      (.addMouseListener mouse-processor)
      (.addKeyListener key-char-processor)
      (.addKeyListener key-event-processor)
      (.addMouseMotionListener mouse-motion-processor)
      (.setFocusTraversalKeysEnabled false)
      (.setPreferredSize d)
      (.setBackground Color/white))))

(defn- close-window-fn
  "Close window frame"
  [^JFrame frame active?]
  (reset! active? false)
  (.dispose frame))

;; Create lazy list of icons to be loaded by frame
(def ^:private window-icons (map #(.getImage (ImageIcon. (io/resource (str "icons/i" % ".png")))) [10 16 20 24 30 32 40 44 64 128]))

(defn- build-frame
  "Create JFrame object, create and attach panel and do what is needed to show window. Attach key events and closing event."
  [^JFrame frame ^java.awt.Canvas panel active? on-top? windowname width height init-fn close-fn]
  (let [closer (proxy [WindowAdapter] []
                 (windowClosing [^WindowEvent e]
                   (do (when close-fn (close-fn))
                       (close-window-fn frame active?)
                       (clear-state! windowname))))]
    (when init-fn (init-fn))
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
      ;; (.setLocation 100 100)
      (.setLocationRelativeTo nil)
      (.setVisible true)
      (.toFront)
      (.setAlwaysOnTop (if on-top? true false)))
    (doto panel
      (.requestFocus)
      (.createBufferStrategy 2))))

(defn- refresh-repaint
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
  (refresh-repaint (.panel window) @(.buffer window) (.background window) hints) ;; initial repaint
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
                                           (WithExceptionT. true e))) 
            at (System/nanoTime)
            diff (/ (- at t) 1.0e6)
            delay (- stime diff overt)]
        (when (pos? delay)
          (Thread/sleep (long delay) (int (* 1000000.0 (m/frac delay)))))
        (refresh-repaint (.panel window) @(.buffer window) (.background window) hints)
        (if (and @(.active? window) (not (.exception? new-result)))
          (recur (inc cnt)
                 (.value new-result)
                 (System/nanoTime)
                 (if (pos? delay) (- (/ (- (System/nanoTime) at) 1.0e6) delay) 0.0))
          (clear-state! (.window-name window)))))))


(defn- refresh-screen-task-speed
  "Repaint canvas on window with set FPS.

  * Input: frame, active? atom, function to run before repaint, canvas and sleep time."
  [^Window window draw-fun draw-state hints]
  (refresh-repaint (.panel window) @(.buffer window) (.background window) hints) ;; initial repaint
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
                                             (WithExceptionT. true e))) 
              at (System/nanoTime)
              diff (/ (- at t) 1.0e6)
              delay (- stime diff overt)]
          (when (pos? delay)
            (Thread/sleep (long delay) (int (* 1000000.0 (m/frac delay)))))
          (refresh-repaint (.panel window) @(.buffer window) (.background window) hints)
          (if (and @(.active? window) (not (.exception? new-result)))
            (recur (inc cnt)
                   (.value new-result)
                   (System/nanoTime)
                   (if (pos? delay) (- (/ (- (System/nanoTime) at) 1.0e6) delay) 0.0))
            (clear-state! (.window-name window))))))))

(defn replace-canvas
  "Replace canvas in window.

  * Input: window and new canvas
  * Returns canvas"
  {:metadoc/categories #{:canvas :window}}
  [^Window window canvas]
  (reset! (.buffer window) canvas))

(defn repaint
  "Repaints window, call only in `:onrepaint` mode"
  [^Window window]
  (let [^java.awt.Canvas panel (.panel window)]
    (.repaint panel)))

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
  * `:init-fn` - called when window is created
  * `:close-fn` - called when window is closed
  * `:state` - initial global state data
  * `:draw-state` - initial drawing state
  * `:setup` - inital callback function, returns drawing state (fn [canvas window] ... initial-loop-state)
  * `:hint` - rendering hint for display: `:low`, `:mid`, `:high` or `:highest`
  * `:refresher` - `:safe` (default), `:fast`, `:onrepaint`
  * `:background` - background color for window panel, default white.
  * `:position` - window position as a pair of [x y], `nil` (default) for center.

  `:refresher` controls what to do with repainting a window. `:safe` and `:fast` repaint a window autmatically, calling `draw-fn` (if provided) and repaint is done `:fps` times per second. `:onrepaint` leaves repainting to the AWT or user by calling `repaint`, `draw-fn` is ignored."
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
  ([{:keys [canvas window-name w h fps draw-fn init-fn close-fn state draw-state setup hint refresher always-on-top? background position]
     :or {canvas (canvas 200 200)
          window-name (str "Clojure2D - " (to-hex (rand-int Integer/MAX_VALUE) 8))
          fps 60
          background :white
          refresher :safe}}]
   (assert (#{:safe :fast :onrepaint} refresher) "refresher should be one of :safe, :fast or :onrepaint")
   (let [w (or w (pr/width canvas))
         h (or h (pr/height canvas))
         active? (atom true)
         buffer (atom canvas)
         frame (JFrame.)
         background-img (when background (pr/get-image (with-canvas-> (clojure2d.core/canvas w h)
                                                         (set-background background))))
         panel (if (= refresher :onrepaint)
                 (create-panel-auto-repaint window-name w h buffer background-img hint)
                 (create-panel window-name w h))
         window (->Window frame
                          active?
                          buffer
                          panel
                          fps
                          w
                          h
                          window-name
                          (atom {})
                          background-img)
         setup-state (when setup (with-canvas-> canvas
                                   (setup window))) 
         refresh-screen-task (case refresher
                               :fast refresh-screen-task-speed
                               :safe refresh-screen-task-safety
                               :onrepaint nil)
         draw-fn (when draw-fn #(draw-fn %1 %2 %3 %4))]
     (SwingUtilities/invokeAndWait #(build-frame frame panel active? always-on-top? window-name w h init-fn close-fn))
     (add-events-state-processors window)
     (change-state! window-name state)
     (when refresh-screen-task (future (refresh-screen-task window draw-fn (or setup-state draw-state) (when hint (get-rendering-hints #{hint} :mid)))))
     (when-let [[x y] position]
       (.setLocation frame (int x) (int y)))
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

(defn calendar
  "Current calendar"
  {:metadoc/categories #{:dt}}
  ^Calendar [] (Calendar/getInstance))

(defn year
  "Current year"
  {:metadoc/categories #{:dt}}
  (^long [] (year (calendar)))
  (^long [^Calendar c] (.get c Calendar/YEAR)))

(defn month
  "Current month"
  {:metadoc/categories #{:dt}}
  (^long [] (month (calendar)))
  (^long [^Calendar c] (inc ^int (.get c Calendar/MONTH))))

(defn day
  "Current day"
  {:metadoc/categories #{:dt}}
  (^long [] (day (calendar)))
  (^long [^Calendar c] (.get c Calendar/DAY_OF_MONTH)))

(defn hour
  "Current hour"
  {:metadoc/categories #{:dt}}
  (^long [] (hour (calendar)))
  (^long [^Calendar c] (.get c  Calendar/HOUR_OF_DAY)))

(defn minute
  "Current minute"
  {:metadoc/categories #{:dt}}
  (^long [] (minute (calendar)))
  (^long [^Calendar c] (.get c Calendar/MINUTE)))

(defn sec
  "Current second"
  {:metadoc/categories #{:dt}}
  (^long [] (sec (calendar)))
  (^long [^Calendar c] (.get c Calendar/SECOND)))

(defn millis
  "Milliseconds from epoch"
  {:metadoc/categories #{:dt}}
  (^long [] (System/currentTimeMillis))
  (^long [^Calendar c] (.getTimeInMillis c)))

(defn nanos
  "JVM running time in nanoseconds"
  {:metadoc/categories #{:dt}}
  ^long [] (System/nanoTime))

(defn datetime
  "Date time values in the array. Optional parameter :vector or :hashmap (default) to indicate what to return."
  {:metadoc/categories #{:dt}}
  ([type-of-array]
   (let [c (calendar)
         y (year c) m (month c) d (day c)
         h (hour c) mi (minute c) s (sec c) mil (millis c)
         n (nanos)]
     (if (= type-of-array :vector)
       [y m d h mi s mil n]
       {:year y :month m :day d :hour h :minute mi :second s :millis mil :nanos n :sec s})))
  ([] (datetime :hashmap)))

;; ## Load bytes

(defn load-bytes
  "Load file or http and return byte array."
  [file-or-url]
  (with-open [in (io/input-stream file-or-url)
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
(defonce ^:private session-atom (atom nil))

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
  nil)

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
    (io/make-parents fname) 
    (let [^java.io.Writer no (io/writer fname :append true)]
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
  (swap! session-atom make-session-fn)
  (:name @session-atom))

(defn close-session
  "Close session via agent"
  {:metadoc/categories #{:session}}
  []
  (swap! session-atom close-session-fn))

(defn ensure-session
  "Ensure that session is active (create one if not)"
  {:metadoc/categories #{:session}}
  []
  (when (nil? @session-atom)
    (make-session)))

(defn session-name
  "Get session name"
  {:metadoc/categories #{:session}}
  []
  (ensure-session)
  (:name @session-atom))

(defn next-filename
  "Create next unique filename based on session"
  {:metadoc/categories #{:session}}
  ([prefix]
   (ensure-session)
   (let [s @session-atom]
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
      (swap! session-atom (fn [s]
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

(defn get-image
  "Return BufferedImage"
  {:metadoc/categories #{:image :canvas :window}}
  [i] (pr/get-image i))

(defn to-image
  "Return BufferedImage, alias for `get-image`"
  {:metadoc/categories #{:image :canvas :window}}
  [i] (pr/get-image i))

(defn width
  "Width of the image."
  {:metadoc/categories #{:image :canvas :window}}
  ^long [i] (pr/width i))

(defn height
  "height of the image."
  {:metadoc/categories #{:image :canvas :window}}
  ^long [i] (pr/height i))

(defn save
  "Save image `i` to a file `n`."
  {:metadoc/categories #{:image :canvas :window}}
  [i n] (pr/save i n))

(defn convolve
  "Convolve with Java ConvolveOp.

  Kernel can be predefined as keywords or sequence of numbers (row-wise).
  
  See [[convolution-matrices]] for kernel names."
  {:metadoc/categories #{:image :canvas :window}}
  [i kernel] (pr/convolve i kernel))

(defn subimage
  "Return rectangular subimage"
  {:metadoc/categories #{:image :canvas :window}}
  [i x y w h] (pr/subimage i x y w h))

(defn resize
  "Resize image."
  {:metadoc/categories #{:image :canvas}}
  [i w h] (pr/resize i w h))

(m/unuse-primitive-operators)
