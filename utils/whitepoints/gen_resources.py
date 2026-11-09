"""Generate the spectral and white point data of clojure2d.color.whitepoints from colour-science.

Run from the repository root:

    /home/ts/penv/.venv/bin/python -I utils/whitepoints/gen_resources.py [output-dir]

Output directory defaults to resources/color. Files:

* cmfs_standard_observer.edn  colour MSDS_CMFS, four standard observers
* illuminants.edn  colour SDS_ILLUMINANTS (ISO 7589 names as :ISO-* keys)
* illuminant_d_series_basis.edn  colour SDS_BASIS_FUNCTIONS_CIE_ILLUMINANT_D_SERIES (S0, S1, S2)
* whitepoints.edn  {:<observer>-<illuminant>[*] [X Z x y]}, Y = 1:
    - tabulated (no star): colour CCS_ILLUMINANTS for the 1931 2 and 1964 10 degree observers,
      X and Z from xy as x * (1/y), (1 - x - y) * (1/y) (as clojure2d xy->XZ);
      except the D-series entries below and CIE-2-D65
    - D-series D50, D55, D60, D65, D75 (2 and 10 degree, CIE-2-D65 excepted): 1 nm integration of the
      CIE spectrum over 300-830 nm, linearly interpolated, with the 1 nm observer (360-830 nm);
      D65 and D50 spectra are the official CIE tables (data/cie_spectra.csv), D55 and D75 are the
      colour tables (to 780 nm) completed with the CIE daylight basis, D60 is colour's basis spectrum
    - CIE-2-D65 (0.312727, 0.329023): the CIE D65 1 nm integration rounded to 6 decimals (kept)
    - computed (star): colour sd_to_XYZ Integration of a perfect reflector for every illuminant and
      observer (illuminant interpolated linearly, extended with constants to the observer range)
"""

import csv
import sys
import warnings

import numpy as np
import colour
from colour.colorimetry import (SDS_BASIS_FUNCTIONS_CIE_ILLUMINANT_D_SERIES,
                                sd_CIE_illuminant_D_series)

warnings.filterwarnings("ignore")

OUT = sys.argv[1] if len(sys.argv) > 1 else "resources/color"
CIE_SPECTRA = "utils/whitepoints/data/cie_spectra.csv"

OBSERVERS = {"CIE-2": "CIE 1931 2 Degree Standard Observer",
             "CIE-10": "CIE 1964 10 Degree Standard Observer",
             "CIE-2015-2": "CIE 2015 2 Degree Standard Observer",
             "CIE-2015-10": "CIE 2015 10 Degree Standard Observer"}
ISO = {"PF": "Photoflood", "PD": "Photographic Daylight", "SD": "Sensitometric Daylight",
       "SPF": "Sensitometric Photoflood", "SP": "Sensitometric Printer",
       "SST": "Sensitometric Studio Tungsten", "ST": "Studio Tungsten"}
SPECIAL = {"Blackmagic Wide Gamut": "BLACKMAGIC-WG", "ICC D50": "ICC-D50",
           "PLASA ANSI E1.54": "PLASA"}
D_SERIES = {"D50": 5000, "D55": 5500, "D60": 6000, "D65": 6500, "D75": 7500}
CIE_2_D65 = (0.312727, 0.329023)


def key(name):
    if name.startswith("ISO 7589 "):
        return "ISO-" + next(a for a, b in ISO.items() if "ISO 7589 " + b == name)
    return SPECIAL.get(name, name.replace(".", "-"))


def num(x):
    return repr(float(x))


def nums(xs):
    return "[" + " ".join(num(x) for x in xs) + "]"


def longs(xs):
    return "[" + " ".join(str(int(x)) for x in xs) + "]"


def write(name, entries):
    with open(f"{OUT}/{name}", "w") as f:
        f.write("{" + "\n ".join(entries) + "}\n")


def cmfs():
    for k, n in OBSERVERS.items():
        c = colour.MSDS_CMFS[n]
        w = c.wavelengths
        yield (f":{k} {{:lambda {longs(w)} :x {nums(c.values[:, 0])} :y {nums(c.values[:, 1])}"
               f" :z {nums(c.values[:, 2])} :range [{int(w[0])} {int(w[-1])}]}}")


def illuminants():
    for k, n in sorted((key(n), n) for n in colour.SDS_ILLUMINANTS):
        sd = colour.SDS_ILLUMINANTS[n]
        w = sd.wavelengths
        yield f":{k} {{:lambda {longs(w)} :value {nums(sd.values)} :range [{int(w[0])} {int(w[-1])}]}}"


def basis():
    B = SDS_BASIS_FUNCTIONS_CIE_ILLUMINANT_D_SERIES
    w = B["S0"].wavelengths
    return [f":lambda {longs(w)}"] + [f":{k} {nums(B[k].values)}" for k in ["S0", "S1", "S2"]] + \
        [f":range [{int(w[0])} {int(w[-1])}]"]


def cie_spectra():
    rows = [r for r in csv.reader(l for l in open(CIE_SPECTRA) if not l.startswith("#"))][1:]
    out = {}
    for ill, l, v in rows:
        out.setdefault(ill, ([], []))
        out[ill][0].append(float(l))
        out[ill][1].append(float(v))
    return {k: (np.array(w), np.array(v)) for k, (w, v) in out.items()}


def d_series_spectrum(name, cie):
    """(wavelengths, values) over 300-830 nm."""
    if name in cie:
        w, v = cie[name]
        tab = colour.SDS_ILLUMINANTS[name]
        on = np.isin(w, tab.wavelengths)
        ratio = v[on] / tab[w[on]]
        # same spectrum as colour up to colorSpec's normalisation (and table rounding)
        assert np.max(np.abs(ratio / np.median(ratio) - 1)) < 1e-3, name
        return w, v
    tab = colour.SDS_ILLUMINANTS[name]
    full = sd_CIE_illuminant_D_series(
        colour.temperature.CCT_to_xy_CIE_D(D_SERIES[name] * 1.4388 / 1.4380))
    w = full.wavelengths
    v = np.where(np.isin(w, tab.wavelengths), tab[w], full.values)
    return w, v


def integrate(w, v, observer):
    """1 nm integration with linear interpolation of the spectrum; [X Y Z] with Y = 1."""
    c = colour.MSDS_CMFS[OBSERVERS[observer]]
    S = np.interp(c.wavelengths, w, v)
    XYZ = (S[:, None] * c.values).sum(0)
    return XYZ / XYZ[1]


def from_xy(x, y):
    r = 1.0 / y
    return [x * r, (1.0 - x - y) * r, x, y]


def from_XYZ(XYZ):
    x, y = colour.XYZ_to_xy(XYZ)
    return [XYZ[0] / XYZ[1], XYZ[2] / XYZ[1], x, y]


def whitepoints():
    wps = {}
    cie = cie_spectra()
    for k in ["CIE-2", "CIE-10"]:
        for n, xy in colour.CCS_ILLUMINANTS[OBSERVERS[k]].items():
            wps[f"{k}-{key(n)}"] = from_xy(*xy)
        for d in D_SERIES:
            wps[f"{k}-{d}"] = from_XYZ(integrate(*d_series_spectrum(d, cie), k))
    wps["CIE-2-D65"] = from_xy(*CIE_2_D65)
    for k, n in OBSERVERS.items():
        cm = colour.MSDS_CMFS[n]
        ones = colour.sd_ones(cm.shape)
        for i in colour.SDS_ILLUMINANTS:
            XYZ = colour.sd_to_XYZ(ones, cm, colour.SDS_ILLUMINANTS[i], method="Integration")
            wps[f"{k}-{key(i)}*"] = from_XYZ(XYZ / 100)
    return [f":{k} {nums(v)}" for k, v in sorted(wps.items())]


def main():
    write("cmfs_standard_observer.edn", cmfs())
    write("illuminants.edn", illuminants())
    write("illuminant_d_series_basis.edn", basis())
    write("whitepoints.edn", whitepoints())


if __name__ == "__main__":
    main()
