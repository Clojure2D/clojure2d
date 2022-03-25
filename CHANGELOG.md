# Change Log
All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

## 1.4.4

* Clojure 1.11.0 

### Added

* `shape` introduced (all primitives have additiona shape version) + functions: `bounding-box`, `intersects-rectangle?`, `contains-point?` and `contains-rectangle?`
* `shape->path-def` and `path-def->shape` to construct path from a definition and back
* `text-shape` returns shape of the text
* `load-font` loads fonts from a file
* `load-image`, `load-svg` and `load-bytes` can load from http URLs now
* PalettonHSV, RYB, XYB and Oklab color spaces
* Dictionary of Colour Combinations - colors and palettes (prefix `:docc`) - https://github.com/mattdesl/dictionary-of-colour-combinations
* Color Vision Deficiency `cvd-lens` function (a filter)
* `mixsub` subtactive mixing
* `adjust` adds a value to color channel
* `complementary` to find complementary color
* `random-color` can select from various presets, also selects colors from palettes and known colors
* `wavelength` returns color from light wavelength in nm
* `clojure2d.color.cssgram` set of instagram-like filters + `custom-filter` function
* many more color difference functions
* `:retina` hint added to support high density displays (first attempt, things might not work well)

### Fixed

* LDRednerer is aware of alpha channel now
* `palette` should return a vector in every case
* `path` points should be any seq (not vector only)
* `possible-color?` should recognize String as a valid color
* `HWB` colorspaces fixed to ensure reversibility
* canvas orientation shift

### Changed

* color difference functions revisited and reorganized

### Deprecated

* `load-url-image` - use `load-image`

## 1.4.3

### Changed

* fastmath deps updated

## 1.4.2

### Changed

* deps updated

## 1.4.1

### Changed

* deps updated

## 1.4.0

### Added

* Introduced cpt-city palettes and gradients
* Introduced paletteer palettes and gradients

### Changed

* Updated fastmath and batik deps
* Refactor to load gradients and palettes dynamically
* [breaking] due to the number of palettes/gradients, `find-palette` and `find-gradient` should be used to find names.

### Fixed

* `compose-channels` applies alpha blending by default when alpha channel processing is turned off.

## 1.3.1

### Changed

* internal core optimizations (fn -> macros)

## 1.3.0

### Changed

* deps updated

## 1.3.0-alpha2

### Added

* `lerp-` color interpolator

### Fixed

* `possible-color?` catches gradient
* `iq-gradient` wrong interpolation between two colors

## 1.3.0-alpha1

### Added

* mathematica gradients (thanks to [Chris N.](https://github.com/techascent/tech.datatype/blob/master/src/tech/v2/tensor/color_gradients.clj))
* `palette` helper function
* `temperature` to convert black-body emissions to color
* `get-channel`, `set-channel`, `modulate` - to manipulate channels in selected color space
* `relative-luma` (WCAG version)
* more String as color (single letter, triplets, #rgba)
* correct-lightness for gradient and palette

### Changed

* deps bumped
* pixels mutating functions are now with `!`
* [breaking] all blending functions moved to separate namespace and reweritten from the scratch
* [breaking] changed API for certain color functions. Especially for [[palette]] and [[gradient]]

### Removed

* Signal processing - moved to fastmath
* palette/gradient presets as vars (use [[palette]] or [[gradient]] to access them)

### Fixed

* Closing window during `draw` can cause NPE when accessing global state.
* `HCL` is now proper HCL by Sarifuddin and Missaou (to use CIELab version use LCH)
* `brighten`/`darken` and `saturate`/`desaturate` corresponds to chroma.js now
* `mix` corresponds to chroma.js now
* `contrast-ratio` reflects WCAG definition

## 1.2.0-SNAPSHOT

### Added

* `:always-on-top?` parameter to set window to be on top
* `orient-canvas` - axis orientation functions
* `filter-channel`, `filter-channel-xy`, `blend-channel` and `blend-channel-xy` can accept function as a parameter single parameter (acts a partial)..
* `set-composite` and `composite` to compose drawing with canvas by Java2d, see java.awt.Composite
* new window parameter, `:background` set panel background color
* bounding box for given text
* `load-url-image` to handle loading from URL
* `pattern-mode` to set textured fill for shapes
* Gradient density rendering

### Changed

* BREAKING: by default canvas is fully transparent. Window background is set to white. This is made to match html canvas behaviour.
* BREAKING: `set-background` uses SrcOver (default) composite. Previously was Src.

### Removed

* thi.ng/color dependency

### Fixed

* Bring window to top // (still doesn't work well)
* Headless mode fix
* Blur algorithm rounding (ghosting effect)
* Native color conversion `from-` was clumping an input

## 1.1.0

### Added
* grid cells
* hex primitive

### Changed

* quadtree segmentation now uses low-discrepancy sequence generator

## 1.0.3

### Added
* reconstruction filters spread parameter
* Hann reconstruction filter

### Changed
* reconstruction filters default values
* refactored renderer, breaking change: intensity is now vibrancy and acts opposite way

### Fixed
* reconstruction filters errors

## 1.0.2

### Added

* `pack` color into 32 bit integer
* `resize` Pixels
* 4 new color spaces: IPT, LMS, JAB, JCH
* new palettes and gradient
* new color distance based on JAB
* list of all predefined colors, palettes and gradients have separate docs

### Changed

* `reduce-colors` - selects color from cluster using `mode` rather than centroid
* html-color* -> named-color*

## 1.0.1

### Added

* `Pixels` can be treated as sequence now (sequence of colors)
* `reduce-colors` using x-means clustering algorithm

## 1.0.0

### Added

- `iq-gradient` accepts also two colors (https://github.com/thi-ng/color/blob/master/src/gradients.org#gradient-coefficient-calculation)
- `load-svg` and `transcode-svg` added

### Changed

- `merge-renderers` can merge any number of renderers (previously only two).
- `iq-random-gradient` also uses `iq-gradient` for two colors generated by paletton generator
- `random-gradient` uses also iq-random-gradient function
- `filled-with-stroke` is macro now
- removed dependency to `Vec2` in some functions in `clojure2d.core`

### Fixed

- `random-gradient` will use only two interpolators (:cubic and :linear) to avoid exceed of the range

## 1.0.0-RC2

### Added

- arc and rarc (radius arc) functions

### Changed

- spots overlay - number of spots heuristic changed

## 1.0.0-RC1 (0.1.0-SNAPSHOT)

Please note - breaking changes!

### Added

- key-pressed? mouse-pressed? (and other event values) functions available in `draw-fn` (via Window protocols)
- lerp in color (to interpolate colors)
- set-stroke-custom - to set custom stoke pattern
- to-XXX* / from-XXX* are now normalized (0-255) colorspace conversion functions.
- color distances
- color interpolators
- color preset palettes (Brewer, Tableaus, Microsoft)
- color gradients
- color tweaking functions
- font-ascent
- thi.ng rgba interoperability

### Changed

- project dependencies to external `generateme/fastmath` library
- to-luma -> luma, get-hue -> hue
- hue returns angle (0 - 360)
- make-color, make-awt-color, make-canvas, make-2d-...-array changed to names without `make-` prefix
- make-pixels -> pixels
- make-effect* -> effect
	- almost all make- in raymarching are changed to version without prefix
- `resize-image` and `resize-canvas` are now moved to ImageProto as `resize`
- xor-mode, gradient-mode are reset with paint-mode
- to-XXX/from-XXX in colorspaces are raw conversions now
- YXY renamed to Yxy
- make-color-converter -> color-converter
- paletton-palette -> paletton
- signal processing refactored, api changed
- overlays refactored
- glitch scripts refectored, api changed

Prefix `make-` is reserved when function is created.

### Fixed

- set-font works now

### Removed

- math namespaces moved to `generateme/fastmath` project
- examples moved to separate repository

## 0.0.7

### Added

- `with-canvas` is no longer threading macro (use `with-canvas->` for that)
- `:setup` parameter added to `show-window`. It's run before displaying window. Parameters are: canvas (within context) and window. When returned value is not `nil` it's used as initial state. In other case `:draw-state` parameter is used.
- color can be represented as int (eg. in hex form like: 0x234344)
- `clamp255` returns double, for long version call `lclamp255`
- bezier and curve functions added
- date/time functions (year, month, day, hour, minute, sec, millis, nanos)
- new function `filled-with-stroke` - to draw primitive with stroke and filled
- `load-bytes` added
- general key-event added (like mouse-event). You can now dispatch also on event type not only key char or code
- background supports transparency
- screen-width and screen-height functions

### Fixed

- threshold filter wrong values
- `draw` now displays exception when something is wrong
- window canvas was in the bad size causing visual artifacts
- more accurate frame delay calculations

### Changed

- `with-canvas` is renamed to `with-canvas->`
- `:highest` hint added (it's `:high` with Stroke Pure)
- :right, :left or :center alignments for text (last parameter, default :left)
- `rendering-hints` is map of vectors now, it gave 25% speed gain during recreating graphics context

## 0.0.5-0.0.6 - 2017-11-21

### Added

- Vcf303, Echo and BassTreble signal processing effects + example 16 updated
- key-released and key-typed events added
- fractal bestiary explorer - example 37
- path-bezier - bezier spline
- text and font functions
- Java2D convolution filters (via (convolve ...)), returning image
- Expectations Tests
- ArrayVec type - doubles array vector representation (for speed gain)

### Changed

- Overlays API changed to image + configuration map
- Switched from clojure math to primitive math library
- FastMath bindings changed to macros for several methods
- Saving picture moved to image protocol, replaced save-canvas and save-pixels removed (call (save canvas name)).
- KeyEvent protocols
- vector scale -> set-mag
- fixes after tests

### Fixed

- rgb scanlines bad tint values
- subpixel accuracy for strokes included in :high canvas profile

## 0.0.4 - 2017-08-07

### Added

- New overlay - crt scanlines

### Changed

- Events and Window carry global state, `show-window` parameters reorganized.

## 0.0.3 - 2017-07-02
### Added

- Canvas transformations (transpose, rotate, scale, push/pop matrix)
- Image protocol (get-image, width and height fns) for Window, Canvas, image, Pixels
- Mouse position protocol to Window and MouseEvent
- Nature of Code examples
- html 140 colors defined in color namespace (access by (c/to-color :name) or (set-color canvas :name))
- point and line drawing function accept Vec2 coordinates as well
- more variations
- clojure2d temporary icon

### Changed

- Canvas as a type
- Window as a type (record)
- Helper functions to check if window is visible
- Helper function to get image from canvas and window
- Color manimulations refactored (awt vs vec4)
- Complex type is Vec2 now
- Session management refactored
- Draw function accepts also Window (to access mouse position)
- Examples revisited
- Signal processing optimisations - speed gain 2x

## 0.0.2 - 2017-04-14
### Added
- Core:
	- session management functions
- Pixels:
	- accumulation bins rendering support
- Random:
	- random and low discrepancy sequence generators (binding to apache commons math)
- Color:
	- color converters
	- palette generators (paletton style, colourlovers 200 palettes, iq generator)
- Extra:
	- glitch functions - slitscan, mirror, color redutions, bleding
	- raymarching - basic raymarching
	- signal - signal processing for sonification (pixels to raw, raw to pixels converters, wave generators)
- Examples: currently 32 examples

### Changed

- A lot of changes and huge number of code optimizations (for speed)
- Draw function behaviour
- Canvas wrapper (Graphics2d allocation and release)
- Mouse moved event
- JPanel -> java.awt.Canvas and custom paint function
- Vectors as Seq + PersistentVector enhanced (VectorProto)
- filters changed to signal ns

### Fixed

Bugs, bugs, bugs + optimization for speed

## 0.0.1 - 2016-11-01
### Added
- Initial release
- Core:
	- image load/save
	- display window
	- basic canvas operations
- Pixels:
	- operate on pixel and channel level
	- basic filter
	- blending functions
- Math:
	- jafama FastMath lib
	- random functions based on Apache Commons Math
	- noise functions based on flow-noise and joise
	- statistic functions
	- Complex type
- Color:
	- various blending methods
- Extra, place for common functions used in glitch/generative process
	- filters.clj - analog filters (for sonification)
	- overlays.clj - rgb scanlines, noise, spots
	- segmentation.clj - divide image into squares
	- variations.clj - fractal flames variation libarary (vector fields)
- Examples: 13 examples
