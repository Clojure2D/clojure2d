# Color generators

Oracle generators for `clojure2d.color` (color audit). Run from the repository root; reruns reproduce the output byte for byte.

| Generator | Environment | Output |
|---|---|---|
| `gen_representation.py` | matplotlib 3.11.1, numpy 2.5.2 in `/home/ts/penv` | `test/resources/color/representation_reference.edn` |
| `gen_colorspaces.py` | colour-science 0.4.7, coloraide 8.14.1, numpy 2.5.2 in `/home/ts/penv` | `test/resources/color/colorspaces_reference.edn` |
| `gen_deltas.py` | colour-science 0.4.7, coloraide 8.14.1, numpy 2.5.2 in `/home/ts/penv` | `test/resources/color/deltas_reference.edn` |
| `gen_temperature.py` | colour-science 0.4.7, numpy 2.5.2 in `/home/ts/penv` | `resources/color/blackbody_srgb.edn` (runtime table), `test/resources/color/temperature_reference.edn` |
| `gen_filters.py` | colour-science 0.4.7, DaltonLens 0.1.5, numpy 2.5.2 in `/home/ts/penv`; Node with chroma-js 2.6.0 (`npm install` in `utils/color`, `package.json`, `package-lock.json`, `chroma_oracle.mjs`) | `test/resources/color/filters_reference.edn`, `resources/color/cvd_machado2009.edn`, `resources/color/cvd_brettel1997.edn` (runtime tables) |
| `gen_normalized_ranges.clj` | clojure2d (`lein run -m clojure.main utils/color/gen_normalized_ranges.clj`) | printed table of sampled extrema; the normalization literals of the `to-X*`/`from-X*` functions are set from it |

```
/home/ts/penv/.venv/bin/python -I utils/color/gen_representation.py
/home/ts/penv/.venv/bin/python -I utils/color/gen_colorspaces.py
/home/ts/penv/.venv/bin/python -I utils/color/gen_deltas.py
/home/ts/penv/.venv/bin/python -I utils/color/gen_temperature.py
(cd utils/color && npm install) && /home/ts/penv/.venv/bin/python -I utils/color/gen_filters.py
```

`gen_representation.py` (area A, representation): CSS Color 4 named colors and hex parsing from matplotlib, hexagonal hue from Python `colorsys`, polar hue from its formula. See the generator docstring.

`gen_colorspaces.py` (area B, colorspaces): colour-science on clojure2d's XYZ matrix and CIE-2-D65 white, coloraide on sRGB, standard matrices, a port of Leonard's RGB to RYB. See the generator docstring.

`gen_deltas.py` (area E, color differences and contrast): colour-science and coloraide differences on clojure2d's CIELAB, Safdar ΔEz, redmean, WCAG, L*, Weber and Michelson contrast from their formulas, d3-jnd intervals, Sharma et al. (2005) CIEDE2000 test data. See the generator docstring.

`gen_temperature.py` (area D, blackbody / temperature): Mitchell Charity's blackbody colors (Planck, CIE 1964 10°, sRGB without adaptation, add white, max channel 1) on a grid uniform in mired, as the runtime table of `temperature` and as test oracle; normalized `wavelength` colors. See the generator docstring.

`gen_filters.py` (area G, adjustments and filters): W3C Filter Effects Level 1 matrices written from the specification, chroma.js `brighten`/`darken`/`saturate`/`desaturate` via Node, Machado et al. (2009) color vision deficiency matrices from colour-science and Brettel et al. (1997) tritanopia from DaltonLens (also the runtime tables of `cvd-filter`). See the generator docstring.
