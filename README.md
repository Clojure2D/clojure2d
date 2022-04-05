# clojure2d [![Build Status](https://travis-ci.org/Clojure2D/clojure2d.svg?branch=master)](https://travis-ci.org/Clojure2D/clojure2d) [![Clojars Project](https://img.shields.io/clojars/v/clojure2d.svg)](https://clojars.org/clojure2d)

Clojure2D is a library supporting generative coding or glitching. It's based on Java2D directly. It's Clojure only, no ClojureScript version.

![ex10](results/small/ex10.jpg "Example 10")
![ex21](results/small/ex21.jpg "Example 21")
![ex5](results/small/ex5.jpg "Example 5")

![ex11](results/small/ex11.jpg "Example 11")
![ex16](results/small/ex16.jpg "Example 16")
![ex19](results/small/ex19.jpg "Example 19")

![ex22](results/small/ex22.jpg "Example 22")
![ex23](results/small/ex23.jpg "Example 23")

## Documentation

* [Documentation with examples](https://clojure2d.github.io/clojure2d/docs/codox/)
* [clojure.color detailed documentation](https://clojure2d.github.io/clojure2d/docs/notebooks/index.html#/notebooks/color.clj) - WIP

### Color/Palettes/Gradients

Library contains plenty named colors, palettes and gradients.

There are various sources: [colourlovers](http://www.colourlovers.com/), [thi.ng](https://github.com/thi-ng/color/blob/master/src/presets.org), [paletteer](https://github.com/EmilHvitfeldt/paletteer), [inigo quilez](https://www.iquilezles.org/www/articles/palettes/palettes.htm), [cpt-city](http://soliton.vm.bytemark.co.uk/pub/cpt-city/)

Lists:

* [color names](https://clojure2d.github.io/clojure2d/docs/static/colors.html)
* [named palettes](https://clojure2d.github.io/clojure2d/docs/static/palettes/index.html)
* [named gradients](https://clojure2d.github.io/clojure2d/docs/static/gradients/index.html)

## Motivation

This project is the answer to personal needs to optimize my own workflow for generative or glitch creations. I've been producing a lot of Processing code and started to suffer from limitations of working in 'write sketch and run' mode. Too much copy&paste between sketches, zillions of folders, zillions of processed images. And one day I fell in love with FP. This code is the answer.

### Is it replacement for...

... quil, thi.ng, clisk, possibly other?

No, rather no. The closest is quil, which is a really great library (both Clojure and Clojurescript, great documentation and webpage, functional mode, 3d support etc.).
`Clojure2d` emphasises math, color and pixel operations and adds a lot of ready to use effects/filters (see `extra` namespaces`).

### What's special in this library then?

In points:

* Almost decoupled display and canvas (decoupled drawing and refreshing) - you can have as many windows as you want, you can have as many canvases as you want. Display repaints selected canvas automaticaly in separate thread. This way you can operate on the canvas in your own pace.
* It is still possible to code the Processing way (e.g. you can attach a draw() function to your Display), but draw function keeps state between invocations. This allows you to avoid global state (atoms etc.) and write more functional code. The same is true for events, each Window has an assigned state which is passed through event calls.
* Easy live coding possible (Emacs/Cider/REPL)
* FastMath as main math library
* Main focus on higher level generative/glitch concepts (like sonification support, vector field functions, colorspace operations, things like slitscan etc. See my [Processing sketches](https://github.com/tsulej/GenerateMe))

### What's odd?

It's kind of a personal library which supports my (probably not optimal, not convenient for others) way of creating stuff.
There are still plenty of bugs and not idiomatic code. It may be slower than Processing. Eats a lot of memory (Pixels code is generally immutable).

## Installation

Add the following line as a dependency to your `project.clj`

![](https://clojars.org/clojure2d/latest-version.svg)

## Usage

Since there are still no tutorials are available, see [documentation](https://clojure2d.github.io/clojure2d/docs/codox/). All namespaces are described below:

### clojure2d.core [docs](https://clojure2d.github.io/clojure2d/docs/codox/clojure2d.core.html)

This is the main namespace with functions in four groups:

* image file oparations (load/save); jpg, png and bmp
* canvas operations (wrapper for Java2D BufferedImage and Graphics2D)
* window and event operations (JFrame wrapper)
* session handling (logger + unique, sequential filename generator)

### clojure2d.pixels [docs](https://clojure2d.github.io/clojure2d/docs/codox/clojure2d.pixels.html)

Pixels type as a representation of channel values of the image plus operations on pixels.
Defines also:

* pixel filters (blur, erode, dilate, median, threshold, posterize, tint)
* composing Pixels
* higher order functions which operate on Pixels parallelly (filter-channels, blend-channels)
* log density renderer

### clojure2d.color [docs](https://clojure2d.github.io/clojure2d/docs/codox/clojure2d.color.html)

* color space converters
* huge collection of palettes and gradients

#### clojure2d.color.blend [docs](https://clojure2d.github.io/clojure2d/docs/codox/clojure2d.color.blend.html)

* collection of blending basic functions (like add, subtract, divide, difference, etc.)

### clojure2d.extra

This is the namespace for common generative/glitch specific libraries:

* signal - signal processing, wave generators, effects and filters (for sonification process) [docs](https://clojure2d.github.io/clojure2d/docs/codox/clojure2d.extra.signal.html)
* overlays - postprocessing filters (like rgb/crt scanlines, etc.) [docs](https://clojure2d.github.io/clojure2d/docs/codox/clojure2d.extra.overlays.html)
* segmentation - image segmentation [docs](https://clojure2d.github.io/clojure2d/docs/codox/clojure2d.extra.segmentation.html)
* glitch - glitching filters [docs](https://clojure2d.github.io/clojure2d/docs/codox/clojure2d.extra.glitch.html)
* utils - visualize objects: palette, gradient, image, vector/scalar field... [docs](https://clojure2d.github.io/clojure2d/docs/codox/clojure2d.extra.glitch.html)
* raymarching - simple ray marching 3d scene renderer (abandoned)

### generateme/fastmath

All math functions are in [fastmath](https://github.com/generateme/fastmath) repository.

## Examples

200+ examples are in separate repository

[EXAMPLES](https://github.com/Clojure2D/clojure2d-examples)

## Community

[Clojurians](https://clojurians.slack.com/archives/clojure2d)

### How to help

Yes! You can help with this project:

* Beginners:
  - create pixel filters
  - create color spaces
* Advanced:
  - speed optimizations
  - idiomatic clojure fixes

Discuss about it with me on Slack.
Or just Pull Request.

## Projects / links

* [Processing glitch/generative projects](https://github.com/tsulej/GenerateMe)
* [Visual log](http://generateme.tumblr.com)
* [Folds project](http://folds2d.tumblr.com)
* [Articles](https://generateme.wordpress.com)
* [Facebook 1](https://www.facebook.com/generateme)
* [Facebook 2](https://www.facebook.com/folds2d)
* [Twitter](https://twitter.com/generateme_blog)

## License

Copyright © 2016-2022 GenerateMe
Distributed under the MIT Licence
