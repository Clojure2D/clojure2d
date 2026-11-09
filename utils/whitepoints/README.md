# White point data generators

Generators for the data of `clojure2d.color.whitepoints` and its test oracles. Run all commands from the repository root. Each generator is deterministic: re-running reproduces its output byte for byte.

| Generator | Environment | Output |
|---|---|---|
| `export_cie_spectra.R` | R with `colorSpec` (1.8.0) | `utils/whitepoints/data/cie_spectra.csv` (official CIE D65 1 nm and D50 5 nm spectra, generator input) |
| `gen_resources.py` | colour-science 0.4.7 in `/home/ts/penv` | `resources/color/whitepoints.edn`, `illuminants.edn`, `cmfs_standard_observer.edn`, `illuminant_d_series_basis.edn` (library data) |
| `gen_reference.py` | colour-science 0.4.7 in `/home/ts/penv` | `test/resources/whitepoints/colour_reference.edn` |
| `gen_reference.R` | R with `spacesXYZ` (1.6.0) | `test/resources/whitepoints/spacesxyz_reference.edn` |

```
Rscript utils/whitepoints/export_cie_spectra.R
/home/ts/penv/.venv/bin/python -I utils/whitepoints/gen_resources.py
/home/ts/penv/.venv/bin/python -I utils/whitepoints/gen_reference.py
Rscript utils/whitepoints/gen_reference.R
```

`gen_resources.py` takes an optional output directory (default `resources/color`), useful to compare new data with the current files before replacing them. The white point rules (tabulated, D-series integration, computed) are described in its docstring.

The reference generators read the white points from `resources/color/whitepoints.edn`, so run them after `gen_resources.py`.

`gen_resources.py` replaces the former `utils/colors/cie.clj`, which needed colour datasets exported to JSON by hand.
