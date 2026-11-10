# Color generators

Oracle generators for `clojure2d.color` (color audit). Run from the repository root; reruns reproduce the output byte for byte.

| Generator | Environment | Output |
|---|---|---|
| `gen_representation.py` | matplotlib 3.11.1, numpy 2.5.2 in `/home/ts/penv` | `test/resources/color/representation_reference.edn` |
| `gen_colorspaces.py` | colour-science 0.4.7, coloraide 8.14.1, numpy 2.5.2 in `/home/ts/penv` | `test/resources/color/colorspaces_reference.edn` |
| `gen_deltas.py` | colour-science 0.4.7, coloraide 8.14.1, numpy 2.5.2 in `/home/ts/penv` | `test/resources/color/deltas_reference.edn` |
| `gen_normalized_ranges.clj` | clojure2d (`lein run -m clojure.main utils/color/gen_normalized_ranges.clj`) | printed table of sampled extrema; the normalization literals of the `to-X*`/`from-X*` functions are set from it |

```
/home/ts/penv/.venv/bin/python -I utils/color/gen_representation.py
/home/ts/penv/.venv/bin/python -I utils/color/gen_colorspaces.py
/home/ts/penv/.venv/bin/python -I utils/color/gen_deltas.py
```

`gen_representation.py` (area A, representation): CSS Color 4 named colors and hex parsing from matplotlib, hexagonal hue from Python `colorsys`, polar hue from its formula. See the generator docstring.

`gen_colorspaces.py` (area B, colorspaces): colour-science on clojure2d's XYZ matrix and CIE-2-D65 white, coloraide on sRGB, standard matrices, a port of Leonard's RGB to RYB. See the generator docstring.

`gen_deltas.py` (area E, color differences and contrast): colour-science and coloraide differences on clojure2d's CIELAB, Safdar ΔEz, redmean, WCAG, L*, Weber and Michelson contrast from their formulas, d3-jnd intervals, Sharma et al. (2005) CIEDE2000 test data. See the generator docstring.
