# Color generators

Oracle generators for `clojure2d.color` (color audit). Run from the repository root; reruns reproduce the output byte for byte.

| Generator | Environment | Output |
|---|---|---|
| `gen_representation.py` | matplotlib 3.11.1, numpy 2.5.2 in `/home/ts/penv` | `test/resources/color/representation_reference.edn` |

```
/home/ts/penv/.venv/bin/python -I utils/color/gen_representation.py
```

`gen_representation.py` (area A, representation): CSS Color 4 named colors and hex parsing from matplotlib, hexagonal hue from Python `colorsys`, polar hue from its formula. See the generator docstring.
