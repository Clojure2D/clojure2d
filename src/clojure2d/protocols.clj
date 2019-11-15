(ns clojure2d.protocols)

(defprotocol ImageProto
  "Image Protocol"
  (get-image [i] "Return BufferedImage")
  (width [i] "Width of the image.")
  (height [i] "Height of the image.")
  (save [i n] "Save image `i` to a file `n`.")
  (convolve [i t] "Convolve with Java ConvolveOp. See [[convolution-matrices]] for kernel names.")
  (subimage [i x y w h] "Return part of the image.")
  (resize [i w h] "Resize image."))

(defprotocol MouseXYProto
  "Mouse position."
  (mouse-x [m] "Mouse horizontal position within window. 0 - left side. -1 outside window.")
  (mouse-y [m] "Mouse vertical position. 0 - top, -1 outside window.")
  (mouse-pos [m] "Mouse position as [[Vec2]] type. [0,0] - top left, [-1,-1] outside window."))

(defprotocol MouseButtonProto
  "Get pressed mouse button status."
  (mouse-button [m] "Get mouse pressed button status: :left :right :center or :none"))

(defprotocol KeyEventProto
  "Access to key event data"
  (key-code [e] "Keycode mapped to keyword. See `java.awt.event.KeyEvent` documentation. Eg. `VK_LEFT` is mapped to `:left`.")
  (key-char [e] "Key as char.")
  (key-raw [e] "Raw value for pressed key (as integer)."))

(defprotocol ModifiersProto
  "Get state of keyboard modifiers."
  (control-down? [e] "CONTROL key state as boolean.")
  (alt-down? [e] "ALT key state as boolean.")
  (meta-down? [e] "META key state as boolean.")
  (shift-down? [e] "SHIFT key state as boolean.")
  (alt-gr-down? [e] "ALT-GR key state as boolean."))

(defprotocol PressedProto
  "Key or mouse pressed status."
  (^{:metadoc/categories #{:window :events}} key-pressed? [w] "Any key pressed? (boolean)")
  (^{:metadoc/categories #{:window :events}} mouse-pressed? [w] "Any mouse button pressed? (boolean)"))

;; Define `ColorProto` for representation conversions.
(defprotocol ColorProto
  "Basic color operations"
  (to-color [c] "Convert any color representation to `Vec4` vector.")
  (to-awt-color [c] "Convert any color representation to `java.awt.Color`.") 
  (luma [c] "Returns luma")
  (red [c] "Returns red (first channel) value. See also [[ch0]].")
  (green [c] "Returns green (second channel) value. See also [[ch1]].") 
  (blue [c] "Returns blue (third channel) value. See also [[ch2]].")
  (alpha [c] "Returns alpha value."))

(defprotocol PixelsProto
  "Functions for accessing and setting channel values or colors. PixelsProto is used in following types:

  * `Pixels` - all functions
  * `Image`, `Canvas`, `Window` - Only [[get-value]] and [[get-color]] for given position and conversion to Pixels. Accessing color or channel value is slow.
  * `Log density renderer` - Only [[set-color!]], [[get-color]] and conversion to Pixels.  "
  (get-value [pixels ch x y] [pixels ch idx] "Get channel value by index or position.")
  (get-color [pixels x y] [pixels idx] "Get color by index or position. In case of low density rendering returns current average color without alpha value.")
  (set-value! [pixels ch x y v] [pixels ch idx v] "Set channel value by index or position")
  (set-color! [pixels x y v] [pixels idx v] "Set color value by index or position.")
  (get-channel [pixels ch] "Return whole `ints` array with chosen channel")
  (set-channel! [pixels ch v] "Set whole channel (as `ints` array)")
  (to-pixels [pixels] [pixels cfg] "Convert to Pixels. For low density rendering provide configuration. Works with Image/Canvas/Window and low density renderer."))

(defprotocol RendererProto
  (add-pixel! [r x y] [r x y c])
  (get-pixel [r x y]))
