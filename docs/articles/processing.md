# Comparison to Processing

Below you can find comparison Processing functions with Clojure2d.

Assuming that following variables are defined:

* `window` - display created with [[show-window]]
* `cnvs` - canvas created with [[canvas]] inside drawing context (see [[with-canvas->]] or [[with-canvas]])

And following namespaces are loaded:

```clojure
(:require [clojure2d.core :refer :all] ;; generaral Clojure2d functions
          [clojure2d.pixels :as p] ;; pixels operations
          [clojure2d.color :as c] ;; color operations
          [fastmath.core :as m] ;; general math functions
          [fastmath.random :as r]) ;; random and noise
```

## Structure

Processing | Clojure2d | Comments
---: | --- | ---
[draw()](https://processing.org/reference/draw_.html) | Any function with following signature `(fn [canvas window frame local-state] ...)` attached to window created with [[show-window]] (`:draw-fn` key). | Such function is called before refresh of the display. Result is passed as `local-state` in the next call.
[exit()](https://processing.org/reference/exit_.html) | [[close-window]] | Closes window programmatically.
[loop()](https://processing.org/reference/loop_.html) | not implemented | possible in future versions
[noLoop()](https://processing.org/reference/noLoop_.html) | not implemented | possible in future versions
[popStyle()](https://processing.org/reference/popStyle_.html) | not implemented |
[pushStyle()](https://processing.org/reference/pushStyle_.html) | not implemented |
[redraw()](https://processing.org/reference/redraw_.html) | not implemented | not necessary
[setup()](https://processing.org/reference/setup_.html) | Any function with following signature `(fn [canvas window] ...)` attached to window created with [[show-window]] (`:setup` key). | Returned value is treated as local-state for first call of drawing function.
[thread()](https://processing.org/reference/thread_.html) | | use `future`
all the rest | use Clojure |

## Environment

Processing | Clojure2d | Comments
---: | --- | ---
[cursor()](https://processing.org/reference/cursor_.html) | not implemented | possible in future versions
[delay()](https://processing.org/reference/delay_.html) | not implemented | use `Thread/sleep`
[displayDensity()](https://processing.org/reference/displayDensity_.html) | not implemented | 
[focused](https://processing.org/reference/focused.html) | not implemented | possible in future versions
[frameCount](https://processing.org/reference/frameCount.html) | implemented | frame number is passed to the drawing function as third parameter |
[frameRate()](https://processing.org/reference/frameRate_.html) | implemented | set `:fps` key when [[show-window]] is called
[frameRate](https://processing.org/reference/frameRate.html) | not implemented |
[fullScreen()](https://processing.org/reference/fullScreen_.html) | not implemented | possible in future versions
[height](https://processing.org/reference/height.html) | [[height]] | it's a function working on canvas/window/image
[noCursor()](https://processing.org/reference/noCursor_.html) | not implemented | possible in future versions
[noSmooth()](https://processing.org/reference/noSmooth_.html) | implemented | use `:low` quality key in [[canvas]] call
[pixelDensity()](https://processing.org/reference/pixelDensity_.html) | not implemented | for Retina use two times bigger canvas/window and call `(scale canvas 2)`.
[pixelHeight](https://processing.org/reference/pixelHeight.html) | not implemented |
[pixelWidth](https://processing.org/reference/pixelWidth.html) | not implemented |
[settings()](https://processing.org/reference/settings_.html) | not necessary |
[size()](https://processing.org/reference/size_.html) | implemented | set size in [[canvas]] or [[show-window]] calls
[smooth()](https://processing.org/reference/smooth_.html) | implemented | use one of the keys defined in [[rendering-hints]] and pass to [[canvas]] or [[show-window]]
[width](https://processing.org/reference/width.html) | [[width]] | it's a function working on canvas/window/image

## Data

Use Clojure2d data types and functions

## Control

Use Clojure2d functions

