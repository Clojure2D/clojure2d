# Change Log
All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

## 1.5.0-alpha3

### Added

* Munsell colours: `clojure2d.color/munsell-to-Yxy` (D65 by default) and `clojure2d.color/munsell-to-sRGB` (not clamped)
* `clojure2d.color.munsell` namespace (port of colour-science, BSD-3-Clause): `munsell-spec`, `munsell-to-Yxy` (Illuminant C by default), `munsell-value` and `luminance` with `:astm`, `:newhall`, `:osa`, `:priest`, `:munsell`, `:moon`, `:saunderson`, `:ladd`, `:mccamy` value scales
* `clojure2d.color.whitepoints/chromatic-adaptation-transform`: cone response matrices of an adaptation method, throws for an unknown one
* `clojure2d.color.whitepoints/cmf`: color matching functions of `:CIE-2`, `:CIE-10`, `:CIE-2015-2`, `:CIE-2015-10` observers
* `clojure2d.color.whitepoints`: `whitepoint` lookup by observer and illuminant; correlated color temperature `CCT->xy` (`:cie-d`, `:kang2002`), `xy->CCT` (`:cie-d`, `:mccamy`, `:hernandez`), `CCT->uv` / `uv->CCT` (`:krystek`), `CCT->mired`, `mired->CCT`, `daylight-locus`; light source spectra `planck-law`, `blackbody-spectrum`, `illuminant-A-spectrum`, `illuminant-D-spectrum` (ported from colour-science, checked against it)
* Docstrings for every public var of `clojure2d.color.whitepoints` (including the 359 white point vars) and for `->XYZ-to-LMS`, `->LMS-to-XYZ`, `->RGB-to-RGB`, `->spectrum-to-XYZ`, `wavelength-to-XYZ`
* Docstrings for `->RGB-to-XYZ`, `->XYZ-to-RGB`, `->RGB-to-linear-RGB`, `->linear-RGB-to-RGB`, `RGB-to-RGB1`, `RGB1-to-RGB`, `XYZ-to-XYZ1`, `XYZ1-to-XYZ`, `to-sRGB` (including the `:sRGB` colorspace semantics), `from-sRGB`
* `clojure2d.color.rgb`: transfer functions `to-`/`from-BT709`, `-ROMM` (ProPhoto), `-SMPTE240M`, `-BT1886` (optional display white/black luminance), `-PQ` (SMPTE ST 2084, 1.0 = 10000 cd/m²), `-HLG` (ITU-R BT.2100 OETF); RGB spaces `:ITU-R-BT709`, `:Display-P3`, `:P3-D65`, `:DCI-P3`, `:SMPTE-240M`, `:ProPhoto-RGB` (alias `:ROMM-RGB`)
* `clojure2d.color.rgb`: `rgb-space` (space lookup, throws `ex-info` for an unknown name), `rgb-spaces-list`; docstrings for the namespace, transfer functions, `rgb-data->matrix`, `rgb-data->map`, `rgbs`
* Data generator `utils/whitepoints/gen_resources.py` (replaces `utils/colors/cie.clj`) and CIE D-series basis data `resources/color/illuminant_d_series_basis.edn`
* `clojure2d.color.blend`: docstrings for the namespace, every per-channel mode (formula, source, argument roles), `blends`, `blends-list`, `alpha-blending`, `blend-colors`, `blend-palettes`, `blend-gradients`, `hue`, `saturation`, `luminosity`, `color`, `ch0`-`ch2`
* `clojure2d.color.blend`: per-channel modes `w3c-softlight` (W3C/CSS soft light), `grainextract`, `grainmerge` (GIMP), `phoenix`, `interpolation` (pegtop.net), registered in `blends` (`blends-list` changes, seeded `glitch` results may differ)
* `clojure2d.color.blend`: W3C non-separable modes `w3c-hue`, `w3c-saturation`, `w3c-color`, `w3c-luminosity` (luminance-preserving, as in CSS) and whole-color modes `darker-color`, `lighter-color`
* `clojure2d.color.blend/blend-fn` (mode lookup by name, throws `ex-info` for an unknown name) and `opacity` (mode mixed with the base)
* `clojure2d.color.blend/color-blends`: whole-color modes by name (HSB, W3C non-separable, darker/lighter color, channel copies); `cssgram/custom-filter` resolves whole-color modes through it
* `clojure2d.color.cssgram`: filters `brannan` and `perpetua` (gradient approximated); `filter-definitions` (filters as data: overlays, filter steps, approximation notes); docstrings for the namespace, every filter, `filters`, `filters-list`, `custom-filter`; `custom-filter` accepts the W3C whole-color modes and `:darker-color`, `:lighter-color`
* `clojure2d.color`: docstrings for the representation layer (named colors, accessors, setters, AWT colors, clamping, scaling, `hue`, `hue-polar`, `format-hex`, `pack`, `black?`) and the `ColorProto` protocol; namespace docstring corrected for packed integers and hex strings
* `clojure2d.color/to-color` (and every color function) accepts color names as strings, case-insensitive, for names without a namespace (`"red"`, `"Dark-Blue"`), and `float` arrays
* `clojure2d.color`: docstrings for the colorspace conversions (model, source, conventions, ranges; `to-`, `from-`, normalized `*` variants) and a convention table in `colorspaces`, `colorspaces*`, `colorspaces-list`; `to-OSA`/`from-OSA` docstrings no longer swapped; docstrings for the XYZ helpers (`XYZ-to-LAB`, `LAB-to-XYZ`, `XYZ-to-LUV`, `LUV-to-XYZ`, `XYZ-to-HunterLAB`, `HunterLAB-to-XYZ`, `XYZ-to-LCH`, `LCH-to-XYZ`, `XYZ-to-LCHuv`, `LCHuv-to-XYZ`, `XYZ-to-Yxy`, `Yxy-to-XYZ`, `XYZ-to-UVW`, `UVW-to-XYZ`, `XYZ-to-UCS`, `UCS-to-XYZ`), `to-luma-color-hue`, `from-luma-color-hue`
* `clojure2d.color`: docstrings for the color differences and contrast (`delta-E*`, `delta-C*`, `delta-H*`, `delta-E-HyAB`, `delta-E*-94`, `delta-E*-euclidean`, `delta-C-RGB`, `delta-D-HCL`, `delta-E*-CMC`, `delta-E-z`, `delta-E*-2000`, `contrast-ratio`, `noticeably-different?`): formula, source, options and defaults, reference argument; `delta-E*-euclidean` had none
* `clojure2d.color/contrast-ratio` option `:method`: `:wcag` (default), `:lstar` (L* difference), `:weber` (signed, target on background), `:michelson`; Weber and Michelson on the luminance Y of `to-XYZ`
* `clojure2d.color`: docstrings for `temperature`, `temperature-names` (the named temperatures), `adjust-temperature` and `wavelength`
* `clojure2d.color`: docstrings for the adjustments and filters (`whiten`, `blacken`, `brighten`, `darken`, `saturate`, `desaturate`, `adjust`, `modulate`, `tinter`, `correct-luma`, `apply-theme`, `color-themes`, `fe-color-matrix`, `sepia`, `contrast`, `exposure`, `brightness`, `saturation`, `hue-rotate`, `grayscale`, the color vision deficiency filters, `complementary`, `make-LCH`): sources (W3C Filter Effects, chroma.js, Machado et al. 2009, Brettel et al. 1997), formulas, ranges
* `clojure2d.color`: premultiplied alpha as an option of `lerp`, `lerp-`, `lerp+`, `mix`, `average`, `weighted-average` (`{:premultiplied? true}` after the colorspace, `:RGB` for sRGB; as CSS `color-mix()` and coloraide; the hue channel is not premultiplied): transparent red and blue give blue at alpha 0.5; straight alpha stays the default; `weighted-average` throws `{:alpha-weights-sum 0.0}` when negative weights times alphas cancel out
* `clojure2d.color`: docstrings for palettes and gradients (`gradient` with the list of built-in gradients and their sources, `palette`, `merge-gradients`, `iq-gradient`, `gradient-cubehelix`, `find-gradient`, `find-palette`, `random-color`, `random-palette`, `random-gradient`, `paletton`, `hue-paletton`, `paletton-presets-list`); paletton verified against paletton.com's own color model
* `clojure2d.color`: docstrings for mixing and reduction (`nearest-color`, `average`, `weighted-average`, `lerp`, `lerp-`, `lerp+`, `mix`, `mixbox`, `mixsub`, `negate`, `reduce-colors`, `resample`): formulas, sources (chroma.js, Mixbox, ProfJski ArtColors), ranges
* `clojure2d.color`: CSS filters `invert` and `opacity` (W3C Filter Effects Level 1); `cvd-filter` simulating protan / deutan (Machado et al. 2009), tritan (Brettel et al. 1997) and achromat color vision deficiency at any severity in [0, 1] (`:protanomaly`, `:deuteranomaly`, `:tritanomaly`, `:achromatomaly`), or complete loss by name (`:protanopia`, `:deuteranopia`, `:tritanopia`, `:achromatopsia`)
* `clojure2d.color/delta-E-HyCH`: HyCH color difference (Abasi, Amani Tehran, Fairchild 2020), CIEDE2000 terms with a city-block lightness term; option `:textiles?`

### Changed

* `spectrum` and `->spectrum-to-XYZ1` moved to `clojure2d.color.whitepoints`; `clojure2d.color` keeps aliases
* `->spectrum-to-XYZ1` / `->spectrum-to-XYZ`: `:extrapolation` accepts `:constant` (new default) or `:zero`; `:trim` and `nil` are removed (they threw under fastmath 3.0.0-alpha10) and now throw `ex-info`; unknown observer or illuminant throws `ex-info`
* `->spectrum-to-XYZ1` / `->spectrum-to-XYZ`: `:interpolation` now applies to the spectrum only, default `:auto` (Sprague for uniform data, not-a-knot cubic spline otherwise, as colour-science); new `:illuminant-interpolation` (default `:linear`); color matching functions always use `:auto`
* [breaking] `clojure2d.color.blend/luminocity` renamed to `luminosity` (no alias); `cssgram` `custom-filter` operator `:luminocity` renamed to `:luminosity`
* `clojure2d.color.cssgram`: soft light and hue overlays use the W3C (CSS) modes `w3c-softlight` (gingham, moon, reyes, slumber, stinson) and `w3c-hue` (maven), as in browsers; output changes (gingham up to 2.7, maven up to 34 on the 0-255 scale)
* [breaking] `clojure2d.color/color` (every arity) clamps channels and alpha to `[0,255]` (before: only the 3- and 4-argument forms); `to-color` of 1- and 2-element sequences and arrays no longer clamps, like longer ones
* [breaking] `clojure2d.color/luma` and `relative-luma` use the Y row of the `to-XYZ` sRGB matrix (0.2126740, 0.7151511, 0.0721750) instead of 0.212671, 0.715160, 0.072169: `relative-luma` equals the Y of `to-XYZ` times 2.55; values change by up to 0.0023 (0-255), also `to-Gray` (`contrast-ratio` uses the WCAG weights, see below)
* [breaking] `clojure2d.color/to-XYZ`, `from-XYZ` (and every XYZ-based colorspace: LAB, LUV, LCH, HunterLAB, Yxy, UVW, UCS, IPT, IgPgTg, JAB, OSA, DIN99, LMS) use the matrices of `clojure2d.color.rgb/rgbs :sRGB`, derived from the sRGB primaries and `CIE-2-D65`, instead of fixed Lindbloom matrices: white maps exactly to the white point (LAB a*, b* of white 0 instead of -2.7e-4); XYZ changes by up to 2.1e-4 (0-100), LAB and delta E by up to 3.1e-4
* [breaking] `clojure2d.color`: nil and unknown color names are not colors: `to-color`, `color`, the channel accessors, `to-awt-color` (and every function converting colors) throw `ex-info` with `{:color c}` instead of returning nil or failing later (e.g. `core/set-color` with nil was silently ignored); `valid-color?` returns false
* [breaking] `clojure2d.color` DIN99o applies the 26° hue rotation of DIN 6176 (as culori, coloraide): a, b change by up to about 20
* [breaking] `clojure2d.color` YCbCr, YUV, YIQ use BT.601 luma 0.299, 0.587, 0.114 (was 0.298839, 0.586811, 0.114350 with BT.601 chroma rows); YCbCr chroma rows exact `(B - Y) / 1.772`, `(R - Y) / 1.402`; inverses are the exact inverse matrices (round trips exact)
* [breaking] `clojure2d.color` YPbPr uses the standard BT.709 scaling `Pb = (B - Y) / 1.8556`, `Pr = (R - Y) / 1.5748` (Pb, Pr within 127.5; were unscaled differences)
* [breaking] `clojure2d.color` Oklab family (Oklab, Oklch, Okhsl, Okhsv, Okhwb) uses coloraide's high-precision matrices and gamut coefficients: grays map to a = b = 0, round trips are exact; Okhsl, Okhsv saturation changes by up to 1e-3
* [breaking] `clojure2d.color/from-HCL` solves the Sarifuddin-Missaoui Q exactly (the published inverse estimates it and round-trips within about 0.06); `from-XYB` and `from-OSA` use exact inverse matrices
* [breaking] `clojure2d.color` DIN99c and DIN99d apply Kuehni's correction `X' = c0 X - (c0 - 1) Z` (c0 1.1 and 1.12) to the color and the white before CIELAB (Cui et al. 2002, as colour-science); the docstrings described it, the code skipped it: a, b change by up to about 21
* `clojure2d.color` normalized `to-X*`/`from-X*` ranges recomputed from sampled extrema (`utils/color/gen_normalized_ranges.clj`) for the DIN99 family, IPT, IgPgTg, JAB, JCH, LAB, LCH, LCHuv, LUV, HunterLAB, LMS, OSA, the Oklab family, UCS, UVW, YPbPr: normalized values stay within `[0,255]` and reach 0 and 255
* [breaking] `clojure2d.color/contrast-ratio` computes the relative luminance as WCAG defines it (weights 0.2126, 0.7152, 0.0722) instead of `relative-luma` (Y row of the `to-XYZ` matrix, unchanged): values change by up to 4.9e-4 relative (blue / red 2.1500 → 2.1489, red / black 5.2535 → 5.2520), grays below 1e-7; pass/fail at 3, 4.5, 7 flips for about 1 in 20000-100000 random pairs
* [breaking] `clojure2d.color/temperature` computes Mitchell Charity's blackbody colors (Planck spectrum, CIE 1964 10°, sRGB without adaptation) from a table uniform in mired, 500-40000 K, generated with colour-science (`resources/color/blackbody_srgb.edn`, `utils/color/gen_temperature.py`), instead of the author's curve fit of the same data: within 0.1 of the exact model (the fit: 1.2 at 2000-6500 K, about 1.8 at 1000 K, 3 at 1900-2000 K); 1000 K (255, 58.2, 0) → (255, 56.4, 0), 2000 K (255, 137.6, 16.9) → (255, 136.6, 18.1); below 1000 K the model instead of an extrapolation, 500 K (255, 0, 0) → (255, 0, 68.6); outside 500-40000 K the ends of the table (50000 K was (152.6, 186.3, 255), now the 40000 K color (155.0, 187.9, 255)); `adjust-temperature` and the `:black-body` gradient follow
* [breaking] `clojure2d.color/wavelength` returns the hue at full brightness: negative linear sRGB channels clipped to 0, the largest channel scaled to 1 (was the clamped sRGB of the XYZ, dark at the ends of the spectrum): 380 nm (3.8, 0, 19.8) → (113.9, 0, 255), 700 nm (48.8, 0, 0) → (255, 0, 0); outside the observer range black as before
* [breaking] `clojure2d.color/adjust-temperature` keeps the CIELAB lightness of the color (`lerp-`; was `lerp+`, which gave every color the lightness of the temperature color) and clamps channels to `[0,255]` (were not clamped): (10, 10, 10) toward 3000 K (224, 190, 160) → (34, 2, 0), (30, 60, 200) (198, 187, 260) → (75, 71, 133)
* [breaking] `clojure2d.color/temperature` (and `adjust-temperature`) throws `ex-info` with `{:temperature t}` for an unknown name (was a `ClassCastException`), a value that is neither a number nor a keyword, a temperature not above 0 K and NaN (0 K and negative temperatures silently gave pure red)
* [breaking] `clojure2d.color` CSS filters follow W3C Filter Effects Level 1: `hue-rotate` blue-row coefficient 0.072 (was 0.071, grays drifted: gray 128 at 90° (128, 128, 127.9) → (128, 128, 128)); `saturation` weights 0.213, 0.715, 0.072 of the saturate matrix (were 0.2126, 0.7152, 0.0722; (200, 50, 80) at 1.5: (255, 33.0, 78.0) → (255, 32.9, 77.9)); `grayscale` its own matrix with 0.2126, 0.7152, 0.0722; `sepia`, `grayscale` amounts above 1 clamped to 1 (`(sepia 2)` (100, 150, 200) (255, 192.8, 67.0) → (192.5, 171.4, 133.5))
* [breaking] `clojure2d.color/whiten`, `blacken` move W (B) linearly `amt` of the way to 1, `W + amt (1 - W)`, or to 0 for negative `amt`, `W + amt W`; `amt` clamped to [-1, 1] (was `W + amt` clamped to [0, 1]): (200, 50, 80) `whiten` 0.2 (200, 101.0, 120.8) → (200, 91.0, 112.8), (200, 150, 160) `whiten` -0.5 (200, 22.5, 58.0) → (200, 75.0, 100.0)
* [breaking] `clojure2d.color` color vision deficiency filters from physiological models in linear sRGB (were matrices of colorjack's ColorMatrix via chromelens, applied to encoded sRGB, without a scientific source): `protanopia`, `deuteranopia` Machado et al. (2009) severity 1, `protanomaly`, `deuteranomaly` severity 0.6, `tritanopia` Brettel et al. (1997) as DaltonLens, `tritanomaly` 60 % toward it, `achromatopsia` the gray of the luminance Y, `achromatomaly` halfway to it; (200, 50, 80) `protanopia` (135.0, 133.7, 72.7) → (91.2, 88.7, 80.2), `achromatopsia` (98.3, 98.3, 98.3) → (108.5, 108.5, 108.5); data `resources/color/cvd_machado2009.edn`, `cvd_brettel1997.edn` (`utils/color/gen_filters.py`)
* [breaking] `clojure2d.color/palette` and `gradient` throw `ex-info` with `{:palette k}` / `{:gradient k}` for an unknown name (`palette` returned nil, `gradient` nil or `IllegalArgumentException` for a namespaced name); `gradient` throws `{:domain d}` for a domain that is not strictly increasing or not one value per color, `{:interpolation k :colors n}` for `:iq` or an easing with other than 2 colors (an easing ignored the rest, `:iq` threw `ArityException` or a protocol error), `{:options o}` for options that are not a map and `{:colorspace cs}` for an unknown colorspace; `merge-gradients` throws `{:midpoint m}` outside (0, 1) (1 gave NaN at the end); `palette` with a number of colors below 1 throws `{:number-of-colors n}` (returned `[]`); `paletton` throws `{:preset p}` for an unknown preset (returned `[]`) and `{:type t}` for an unknown scheme (`IllegalArgumentException`)
* [breaking] `clojure2d.color/lerp` (and `lerp-`, `lerp+`) in colorspaces with a hue (HSx, HWB, GLHS, HCL, LCH, LCHuv, Oklch, JCH, Okhsl, Okhsv, Okhwb, Cubehelix) interpolates the hue along the shorter arc and takes the hue of the other color for an achromatic color (CSS, chroma.js, coloraide; was the hue number interpolated linearly, gray hue 0): `(lerp :red :blue :HSL 0.5)` (0, 255, 0) → (255, 0, 255), `(lerp :gray :blue :HSL 0.5)` (64.1, 191.4, 64.1) → (64.1, 64.1, 191.4), `(lerp [255 0 0] [255 0 200] :Oklch 0.5)` (−1091.9, 188.6, 166.2) → (263.5, −49.7, 119.7)
* [breaking] `clojure2d.color/average`, `weighted-average` in colorspaces with a hue use the circular mean of the hues of the chromatic colors; cancelling hues give an achromatic color (coloraide): `(average [:red :magenta] :HSL)` (0, 255, 127.5) → (255, 0, 127.5), `(average [:red :cyan] :HSL)` (127.5, 255, 0) → (127.5, 127.5, 127.5)
* [breaking] `clojure2d.color/mix` with a colorspace other than `:RGB` interpolates linearly in it, as chroma.js `mix` in that mode (was square-root mixing of the normalized channels, out of gamut): `(mix :red :blue :LAB 0.5)` (217.3, −71.8, 82.2) → (201.5, −59.0, 136.5) (chroma.js #ca0088)
* [breaking] `clojure2d.color/mixsub` follows ProfJski ArtColors `ColorMixSub`: RGB distance divided by 255 √3 (was the RGBA distance divided by 510), alpha 255: `(mixsub :red :white)` (255, 37.3, 37.3) → (255, 23.4, 23.4), `(mixsub :blue :yellow)` (17.1, 17.1, 17.1) → (0, 0, 0); with a colorspace the formula works on the normalized channels (`colorspaces*`, 0-255, was the raw channels): `(mixsub :red :blue :LAB 0.5)` (117.2, 11.9, 80.4) → (89.4, −53.4, 150.1)
* [breaking] `clojure2d.color/reduce-colors` uses k-means++ by default (`:fuzzy?` false, about 20 times faster); accepts any color type; `:rng` nil lets fastmath create the generator; returns the distinct colors when there are not more of them than requested (was an error) and drops empty clusters; throws `ex-info` with `{:options o}` for options that are not a map (was `ClassCastException`), `{:number-of-colors n}` below 1 and `{:colors 0}` for no colors: viridis to 3 colors (65.0, 54.8, 124.0), (35, 138, 141), (181.3, 220.5, 48.7) → (64.7, 56.0, 124.4), (35, 138, 141), (180.0, 220.2, 49.4)
* [breaking] `clojure2d.color` CSS filters `sepia`, `grayscale`, `saturation`, `contrast`, `exposure`, `invert`, `opacity` throw `ex-info` with `{:amount a}` for a negative amount (W3C Filter Effects: not allowed) or `NaN`; `brightness` (offset) and `hue-rotate` accept any value; `whiten`, `blacken` throw `{:amount a}` for `NaN`
* [breaking] `clojure2d.color/complementary` throws `ex-info` with `{:colorspace cs}` for a colorspace other than `:PalettonHSV`, `:HSB`, `:HSV`, `:HSL`, `:HWB`, `:GLHS` (was a `NullPointerException` for an unknown name, a wrong channel turned in other spaces: `:LCH` gave (812, 608, 618)); `make-LCH` throws `{:colorspace cs}` for an unknown colorspace when created (was a `NullPointerException` when called)
* `clojure2d.color/apply-theme` throws `ex-info` with `{:color-theme t}` for an unknown theme name (was an interpolation error)
* [breaking] `clojure2d.color/adjust`, `modulate` throw `ex-info` with `{:colorspace cs}` for an unknown colorspace (was a `NullPointerException`) and `{:channel ch}` for a channel outside 0-3 (was silently ignored)
* [breaking] `clojure2d.color/noticable-different?` renamed to `noticeably-different?` (no alias); it throws `ex-info` with `{:s s :p p}` for a size `:s` not positive or a share `:p` outside (0, 1] (`:p` 0 made identical colors different)
* [breaking] `clojure2d.color`: deprecated aliases `delta-c`, `delta-h`, `delta-e-cie`, `delta-e-cmc`, `delta-e-jab` removed; use `delta-C*`, `delta-H*`, `delta-E*`, `delta-E*-CMC`, `delta-E-z`
* [breaking] `clojure2d.color` color differences (`delta-E*`, `delta-C*`, `delta-H*`, `delta-E-HyAB`, `delta-E*-94`, `delta-E*-euclidean`, `delta-D-HCL`, `delta-E*-CMC`, `delta-E-z`, `delta-E*-2000`) and `noticeably-different?` throw `ex-info` with `{:colorspace cs}` for an unknown `:colorspace` (was a `NullPointerException`) and `{:options x}` when the options are not a map (a positional colorspace or weight was silently ignored, e.g. `(delta-E*-euclidean c1 c2 :YUV)` measured in Oklab)
* [breaking] `clojure2d.color/to-Yxy*`, `from-Yxy*` map x from 0.15-0.64 and y from 0.06-0.60 (the sRGB range) instead of from 0: normalized x, y change by up to 60 and 26

### Fixed

* `->linear-RGB-to-RGB` applied the decoding transfer function (`:from`) instead of the encoding one; linear -> RGB conversions were wrong for every space
* `->RGB-to-XYZ`, `->XYZ-to-RGB`, `->RGB-to-RGB`, `->RGB-to-linear-RGB`, `->linear-RGB-to-RGB` with an unknown space name, or a space map missing required keys, failed with a `NullPointerException` (some only when called); now throw `ex-info` with `{:space k}` or `{:missing-keys ks}` when the converter is built
* `clojure2d.color.rgb/rgbs` `:NTSC` (and `:ITU-R-BT470-525`) used gamma 2.8; now 2.2, the assumed display gamma of ITU-R BT.470 System M; the former curve (as in colour-science) is available as `:NTSC-colour`
* White point values (`resources/color/whitepoints.edn` regenerated):
* `clojure2d.color.cssgram/lark` overlay gray 248 instead of CSSgram's 242; `willow` applied the invalid CSS `::before` overlay (ignored by browsers), lacked the `::after` color overlay rgb(216, 205, 203) and used contrast 0.85 instead of 0.95
* `clojure2d.color.cssgram/custom-filter`: a function step replaced all previous steps (now composed); an unknown operator or blend mode was silently ignored or replaced by `normal` (now throws `ex-info` with `{:operator op}` or `{:blend mode}`)
* `clojure2d.color.blend/blend-colors`, `blend-palettes`, `blend-gradients` with a mode keyword (e.g. `:multiply`) silently acted as `normal` (the keyword was called as a lookup); keywords now resolve through `blend-fn`, an unknown one throws `ex-info` with `{:blend mode}`
* `clojure2d.color.blend/ch0`, `ch1`, `ch2` with an unknown colorspace threw a `NullPointerException`; now `ex-info` with `{:colorspace colorspace}`
* `clojure2d.color/to-color` read the CSS 4-digit hex `#rgba` as a packed number (`#f00a` gave [0 240 10 255]); now red, green, blue, alpha with doubled digits
* `clojure2d.color/alpha` of a packed integer whose top byte is 0 returned 0 while `to-color` treats it as opaque; now 255 (so `pack` of such an integer keeps it opaque)
* named color `:sandy-brown` was `#faa460` instead of the CSS `#f4a460` (`:sandybrown`)
* `clojure2d.color/set-channel`, `get-channel`, `color-converter` with an unknown colorspace threw a `NullPointerException` (or returned nil); now `ex-info` with `{:colorspace cs}`; a channel outside 0-3 was ignored by `set-channel` and gave NaN in `get-channel`, now `ex-info` with `{:channel ch}`
* `clojure2d.color/to-color` accepted hex strings of 5, 7 or more digits as plain numbers and threw `NumberFormatException` for non-hex text; now 1, 2, 3, 4, 6 and 8 digits only, otherwise `ex-info` with `{:color s}`
* `clojure2d.color/possible-color?` treated a vector of packed RGB integers (e.g. `[0xff0000 0x00ff00]`) as one color and accepted any seqable (sets, Java collections); a sequential collection or a supported array is a color only with 1-4 numbers within `[-255,255]`. `possible-palette?` returned true for nil, maps and sets; a palette is now a non-empty sequential collection of possible colors
* `clojure2d.color/to-Okhsl`, `to-Okhsv` gave white a saturation of 0.558 (no achromatic guard, rounded matrices); black, white and grays now have saturation 0 and hue 0; `get-ST-mid` coefficients had truncated digits
* `clojure2d.color/from-HCL` returned NaN for black
* `clojure2d.color/from-OSA` stopped its Newton iteration on a negative residual (signed test); now the absolute residual
* `clojure2d.color/from-JAB` returned NaN for colors within rounding noise of black
* `clojure2d.color/from-UVW` (`UVW-to-XYZ`) returned NaN for W* = 0; the white chromaticity is used
* `clojure2d.color/gradient`, `palette`, `resample` with a one-color palette threw a fastmath error (25 palettes of the files have one color; `random-gradient` failed now and then); now a constant gradient and n copies of the color; no colors throw `ex-info` with `{:colors 0}`
* `clojure2d.color/find-palette` with a group threw `ClassCastException` (colourlovers palettes have integer names); `find-gradient`, `find-palette` accept a string as the regex
* `clojure2d.color/lerp+` with two colors kept the lightness of the first color (it called `lerp-`); now of the second: `(lerp+ :red :blue)` (194.0, 82.0, 190.8) → (134.8, 15.3, 134.4)
* `clojure2d.color/nearest-color` threw `ClassCastException` for palettes of keywords, strings or vectors with the default distance and returned the input color for an empty palette; entries are now converted with `to-color` for the distance (and returned as given), an empty palette throws `ex-info` with `{:palette-size 0}`; the 1-arity converts the palette once; NaN distances are skipped (every distance NaN throws `{:distance :nan}`, was the first entry)
* `clojure2d.color/mixbox` and `reduce-colors` returned fully transparent results as opaque (the ARGB int with alpha 0 was read as opaque): `(mixbox [0 0 255 0] [255 255 0 0])` alpha 255 → 0; `reduce-colors` finds the most frequent color without packing it
* `clojure2d.color` `average`, `weighted-average`, `lerp`, `lerp-`, `lerp+`, `mix`, `mixsub`, `reduce-colors` threw `NullPointerException` for an unknown colorspace; now `ex-info` with `{:colorspace cs}`. `average` of no colors throws `{:colors 0}` (was a protocol error); `weighted-average` throws `{:colors n :weights m}` for different counts (extra colors were dropped) and `{:weights-sum s}` for a sum of weights that is 0 or not finite (gave (0, 0, 0, 0) or ±9.2e15); `resample` throws `{:number-of-colors n}` below 1 (returned `[]`)
* `clojure2d.color/complementary` added 180 to the saturation channel (to the lightness for `:GLHS`) instead of the hue: `(complementary [200 50 80] :HSV)` gave (200, -35950, -28720), now (50, 200, 170); the default `:PalettonHSV` gave (200, 0, 40), now (92.5, 186.2, 46.6)
* `clojure2d.color/correct-luma` threw for gradients and palettes with decreasing lightness and for small lightness reversals (fastmath 3 interpolation needs increasing samples); now both directions work, reversals up to 5 % of the total lightness change are smoothed out, larger ones throw `ex-info` with `{:lightness :non-monotonic}`; after a reversal at the end the corrected gradient still ends at the last color (`[:black :white [245 245 245]]` at 1: (254.9, 254.9, 254.9) → (245, 245, 245)); a palette of fewer than 2 colors throws `{:lightness :non-monotonic}` (was an interpolation error)
  - D-series `CIE-2-D50`, `CIE-2-D55`, `CIE-2-D60`, `CIE-2-D75`, `CIE-10-D50`, `CIE-10-D55`, `CIE-10-D60`, `CIE-10-D65`, `CIE-10-D75` are now full precision 1 nm integrations of the CIE spectra (xy change 1e-5 to 3e-5; the former values were the CIE 15:2004 Table T.3 values, 5 nm sums over 380-780 nm, which the new values match to 4 decimals); `CIE-2-D65` is unchanged; RGB spaces based on `CIE-2-D50` (Adobe Wide Gamut, Best, Beta, ColorMatch, Don, Ekta Space PS5, ECI v2, Xtreme) change by less than 0.01 on 0-255
  - computed (`*`) white points recomputed with the fixed integration: `*-ISO-SP*` were off by about 0.24 in xy, `CIE-2015-*-E*` were not 1/3, other ISO 7589 entries off by about 8e-4, the rest by about 2e-7
* `clojure2d.color.whitepoints/chromatic-adaptation-matrix` (and `->XYZ-to-XYZ`) failed with a `ClassCastException` mixing `RealMatrix` and `Mat3x3`
* `:rainbow` gradient returned channel values above 255 (now clamped like other gradients)
* `->spectrum-to-XYZ1` / `->spectrum-to-XYZ` with default options (`:trim` extrapolation) or `nil` extrapolation threw an exception
* `->spectrum-to-XYZ1` / `->spectrum-to-XYZ` integrated over the wrong range (the intersection or the union of the illuminant, sample and observer ranges); now over the observer's range with the illuminant and sample extended, as CIE 15 (`:ISO-SP`, 2015 observers with `:E` were wrong)
* `->spectrum-to-XYZ1` docstring named the spectrum key `:values` (the key is `:value`) and the converter was hinted as `Vec4` (returns `Vec3`)
* `wavelength-to-XYZ` (and `wavelength`) used CIE 1964 10° for every observer other than `:CIE-2`; now supports all four observers and throws for an unknown one
* `:cat97` chromatic adaptation used a transposed Bradford matrix with a sign error (copied misprint, also in colour-science); now the Bradford matrix as defined for CMCCAT97 / CIECAM97s; the former matrix stays available as `:cat97-colour` (colour-science compatibility)
* An unknown chromatic adaptation method in `chromatic-adaptation-matrix`, `->XYZ-to-XYZ`, `->XYZ-to-LMS`, `->LMS-to-XYZ` and `->RGB-to-RGB` (white points differ) failed with an unrelated `IllegalArgumentException` (LMS builders only when called); now throws `ex-info` with `{:method k}` when the converter is built
* `clojure2d.color.blend/softburn` and `msoftburn`: the branch for `a + b > 255` used `0.5 - (1 - a) / b` instead of the pegtop formula `1 - (1 - a) / (2 * b)`, so values jumped at `a + b = 255`
* `clojure2d.color.blend/linearlight` and `mlinearlight` had the layers swapped (`2 * a + b - 1`); now Photoshop's `a + 2 * b - 1` (same values as `stamp`, `mstamp`)
* `clojure2d.color.blend/pinlight` had the layers swapped (clamped the blend into `[2a - 1, 2a]`); now clamps the base into `[2b - 1, 2b]`
* `clojure2d.color.blend` modulus modes (`madd`, `mdodge`, ...) returned values in `(255, 256]`; now wrapped with mod 256 and capped at 255 (integer-valued results unchanged)
* `clojure2d.color.blend/nor`, `nand`, `xnor` were missing from `blends` (and `blends-list`); registered, so `blends-list` changes and seeded `glitch` results may differ
* `clojure2d.color.blend/alpha-blending` (and `blend-colors`, `hue`, `saturation`, `luminosity`, `color`, `ch0`-`ch2`) returned `NaN` channels when both colors were fully transparent; now the blended channels with alpha 0

## 1.5.0-alpha1

### Changed

* [breaking] all shapes creating functions moved to separate namespace: `clojure2d.core.shape`

## 1.5.0-SNAPSHOT

Moved to `fastmath 3.x`

### Added

* White point manipulation namespace `clojure2d.color.whitepoints`
* `delta-` accepts input without conversion
* various conversions related to XYZ and white points
* spectrum <-> XYZ converter
* `init-fn` and `close-fn` callbacks for window creation and closing
* `radial-gradient-mode` and `linear-gradient-mode` added

### Changed

* [breaking] `reduce-colors` uses (fuzzy)-kmeans++ and arguments are changed
* api for `delta-` functions
* `wavelength` conversion uses interpolated color matching function

### Fixed

* `delta-E*-94` - error of calculating `C*` var

## 1.4.6-SNAPSHOT

### Added

* Refreshed palettes and gradients
* `quad-strip`

### Fixed

* `OHTA` conversion should coerce input to color
* `IgPgTg` normalized conversion fixed
* `HCL` conversion fixed (uncached case)
* eastwood related fixes
* `custom-event` and `fire-custom-event` to allow firing an event from the code

## 1.4.5

### Added

* `apply-theme` to generate random color from a given color and theme definition
* `whiten` and `blacken`
* `to-XYZ1`, `from-XYZ1` not scaled version (range from 0 to around 1)
* `scale` 3 or 4 channels by a value (to easier normalize rgb), also `scale-up`, `scale-down`
* new colorspaces from [colour](https://colour.readthedocs.io/en/v0.4.1/index.html):
    - [`:IgPgTg`](https://www.ingentaconnect.com/content/ist/jpi/2020/00000003/00000002/art00002#)
	- [`:UCS`](https://en.wikipedia.org/wiki/CIE_1960_color_space#Relation_to_CIE_XYZ)
	- [`:UVW`](https://en.wikipedia.org/wiki/CIE_1964_color_space)
* more from `Oklab` family: `Okhsv`, `Okhsl` and `Okhwb`
* `DIN99` color space family: `:DIN99`, `:DIN99b`, `:DIN99c`, `:DIN99d` and `:DIN99o`
* `reduce-colors` can accept a distance (as function or `fastmath.distance` object), `clarans` algorithm is used then (instead of `xmeans`). `clarans` path is very slow though.
* path definition accepts winding rule and shape appending
* certain shapes can accept coordinates as vectors
* `:onrepaint` refresher type for a window, leaves repainting a window for AWT. Good when you want to diplay a static image.
* `:position` argument for window creation, sets location of the window in desired position.
* `mixbox` pigment-based color mixing by [Secret Weapons](https://github.com/scrtwpns/mixbox)
* `grid-qr-cell` to allow draw grid cells in `q,r` coordinate system
* `clj-kondo` configs

### Fixed

* `Oklab` should use linear RGB not on sRGB
* `UCS-OSA` color space should have different order of `g` and `j` channels. 
* `IPT` should operate on not scaled XYZ
* missing palettes from `:nord`
* `LDRenderer` with filtering should draw on borders
* [breaking] `to-lch` and `from-lch` renenamed to `to-luma-color-hue` and `from-luma-color-hue` [#21](https://github.com/Clojure2D/clojure2d/issues/21)

### Changed

* `LDRenderer` revisited
    - added new `to-pixel` modes:
	    * `linear?` (default: `false`), when `true` turns off `log` scaling
		* `splats?` (default: `false`), when `true` averages pixel's values only
	- `vibrancy` is `1.0` by default (was `0.5`)
	- contrast/saturation/brightness don't affect background now
	- changed internal splat representation (as a number from `0.0` to `1.0`)
* `show-image` (and other functions in `clojure2d.extra.utils`) uses `:onrepaint` refreshing strategy by default.

### Removed

* Dependency to `smile-mkl` is removed. `openblas` seems to be enough for current functionality. This makes a package lighter (1GB -> 250MB).

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
