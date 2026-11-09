# White point data generators

Generators for the test oracles of `clojure2d.color.whitepoints`. Run all commands from the repository root. Each generator is deterministic: re-running reproduces its output byte for byte.

| Generator | Environment | Output |
|---|---|---|
| `gen_reference.py` | colour-science 0.4.7 in `/home/ts/penv` | `test/resources/whitepoints/colour_reference.edn` |
| `gen_reference.R` | R with `spacesXYZ` (1.6.0) | `test/resources/whitepoints/spacesxyz_reference.edn` |

```
/home/ts/penv/.venv/bin/python -I utils/whitepoints/gen_reference.py
Rscript utils/whitepoints/gen_reference.R
```

Both read the white points from `resources/color/whitepoints.edn`, so the adaptation oracles use exactly clojure2d's values.
