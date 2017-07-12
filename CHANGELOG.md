# Change Log
All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

## 0.0.3 - 2017-07-02
### Added

- Canvas transformations (transpose, rotate, scale, push/pop matrix)
- Image protocol (get-image, width and height fns) for Window, Canvas, image, Pixels
- Mouse position protocol to Window and MouseEvent
- Nature of Code examples
- html 140 colors defined in color namespace (access by (c/to-color :name) or (set-color canvas c/:name)

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
