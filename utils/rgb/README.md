# RGB space generators

Oracle generators for `clojure2d.color.rgb` and the RGB conversions of `clojure2d.color`. Run from the repository root; reruns reproduce the output byte for byte.

| Generator | Environment | Output |
|---|---|---|
| `gen_reference.py` | colour-science 0.4.7 in `/home/ts/penv` | `test/resources/rgb/colour_reference.edn` |

```
/home/ts/penv/.venv/bin/python -I utils/rgb/gen_reference.py
```

Where colour deviates from a standard, the generator uses the standard (BT.2020 double precision constants, ACEScct decoding capped at 65504); see the generator docstring.
