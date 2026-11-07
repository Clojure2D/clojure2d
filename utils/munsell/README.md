# Munsell data generators

Generators for the Munsell Renotation data used by `clojure2d.color.munsell` and for the test oracles. Run all commands from the repository root. Each generator is deterministic: re-running reproduces its output byte for byte.

| Generator | Environment | Output |
|---|---|---|
| `gen_resources.py` | colour-science 0.4.7 in `/home/ts/penv` | `resources/color/munsell_renotation.edn` (library data) |
| `gen_reference.py` | colour-science 0.4.7 in `/home/ts/penv` | `test/resources/munsell/colour_reference.edn` |
| `gen_reference.R` | R with `munsellinterpol` (3.6.0) | `test/resources/munsell/munsellinterpol_reference.edn` |
| `gen_reference.m` | Octave with Centore's toolbox (Sept 2017) | `test/resources/munsell/centore_reference.edn` |

```
/home/ts/penv/.venv/bin/python -I utils/munsell/gen_resources.py
/home/ts/penv/.venv/bin/python -I utils/munsell/gen_reference.py
Rscript utils/munsell/gen_reference.R
octave -q --no-gui --eval "toolbox='/path/to/MunsellAndKubelkaMunkToolboxSept2017'; source('utils/munsell/gen_reference.m')"
```

`gen_reference.m` reads `colour_reference.edn`, so run it after `gen_reference.py`. The toolbox is GPL-3 and is not part of this repository; download it from https://www.munsellcolorscienceforpainters.com/MunsellAndKubelkaMunkToolbox/MunsellAndKubelkaMunkToolboxSept2017.zip and pass the unzipped folder as `toolbox`.

`gen_resources.py` also checks that the linear/radial hue interpolation is constant inside every ASTM hue segment (20000 random hues) and that renotation chromas are contiguous; it fails when either does not hold.
