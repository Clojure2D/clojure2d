# Blend mode generators

Oracle generator for `clojure2d.color.blend`. Run from the repository root; reruns reproduce the output byte for byte.

| Generator | Environment | Output |
|---|---|---|
| `gen_reference.py` | numpy 2.5.2, Pillow 12.3.0 in `/home/ts/penv` | `test/resources/blend/blend_reference.edn` |

```
/home/ts/penv/.venv/bin/python -I utils/blend/gen_reference.py
```

Clamped modes come from their source formulas (W3C Compositing Level 1, Photoshop, pegtop.net), written independently of the clojure2d code; the modulus family from the author's definitions; Pillow `ImageChops` serves as a second implementation on integer inputs. See the generator docstring for the point sets and sections.
