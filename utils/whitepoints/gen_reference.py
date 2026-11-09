"""Generate test/resources/whitepoints/colour_reference.edn (oracle: colour-science).

Run from the repository root:

    /home/ts/penv/.venv/bin/python -I utils/whitepoints/gen_reference.py

Sections (clojure2d conventions: white point layout [X Z x y], Y = 1):

* :xy->XYZ  rows [x y X Y Z], colour xy_to_XYZ
* :XYZ->xy  rows [X Z x y], colour XYZ_to_xy of [X 1 Z]
* :cat-matrices  {method [9 entries, row major]}, colour CHROMATIC_ADAPTATION_TRANSFORMS
* :adaptation  {[method source destination] [9 entries]}, colour matrix_chromatic_adaptation_VonKries
  for clojure2d white points (X, Z read from resources/color/whitepoints.edn)
* :lms  {method [[X Y Z L M S] ...]}, colour vecmul(M, XYZ)
* :rgb-to-rgb  {[source target method] [[r g b r' g' b'] ...]} (0-255), colour RGB_to_RGB with
  colourspaces built from clojure2d primaries, white points and transfer functions
* :cmfs, :illuminants  colour MSDS_CMFS / SDS_ILLUMINANTS in the format of resources/color/*.edn
* :wavelength  {observer [[lambda X Y Z] ...]}, colour wavelength_to_XYZ (Y of the CMF peak = 1)
* :spectrum-white  {[observer illuminant] [X Y Z]}, colour sd_to_XYZ Integration of a perfect
  reflector (Y = 1), illuminant aligned to the CMF range (colour's illuminant default: linear
  interpolation, constant extrapolation)
* :colorchecker  ColorChecker N Ohta spectra [{:lambda [...] :value [...]} ...]
* :spectrum-samples  {[observer illuminant extrapolation interpolation illuminant-interpolation]
  [[X Y Z] ...]} for the ColorChecker spectra (Y in 0-1): colour defaults (:auto sample = Sprague,
  :linear illuminant), both linear, both Sprague; :zero extrapolates sample and illuminant with zeros
* :spectrum-nonuniform  {:spectra [...] :XYZ [...]}, ColorChecker at non-uniform wavelengths, colour
  CubicSpline (default rule), D65, CIE-2
"""

import re
import warnings

import numpy as np
import colour
from colour.algebra import LinearInterpolator, SpragueInterpolator, vecmul

warnings.filterwarnings("ignore")

SEED = 20261109
OUT = "test/resources/whitepoints/colour_reference.edn"

# fixed chromaticities: E, D65 (clojure2d), C, A, and extremes of the domain
FIXED_XY = [(1 / 3, 1 / 3), (0.312727, 0.329023), (0.31006, 0.31616), (0.44757, 0.40745),
            (0.05, 0.9), (0.7, 0.05), (0.05, 0.05)]


def num(x):
    return repr(float(x))


def records(rows):
    return "[" + "\n  ".join("[" + " ".join(num(x) for x in r) + "]" for r in rows) + "]"


def xy_rows(rng):
    xs = list(FIXED_XY)
    while len(xs) < 200:
        x, y = rng.uniform(0.05, 0.75, 2)
        if x + y < 0.95:
            xs.append((x, y))
    return [[x, y, *colour.xy_to_XYZ(np.array([x, y]))] for x, y in xs]


def XZ_rows(rng):
    XZs = [(1.0, 1.0), (0.95047, 1.08883)] + [tuple(rng.uniform(0.05, 3.0, 2)) for _ in range(198)]
    return [[X, Z, *colour.XYZ_to_xy(np.array([X, 1.0, Z]))] for X, Z in XZs]


# clojure2d method -> colour transform; :cat97s is the Fairchild (2001 revised CMCCAT97) matrix
CAT = {"xyz-scaling": "XYZ Scaling", "bradford": "Bradford", "von-kries": "Von Kries",
       "sharp": "Sharp", "fairchild": "Fairchild", "cat97": "CMCCAT97", "cat97s": "Fairchild",
       "cat2000": "CMCCAT2000", "cat02": "CAT02", "cat02brill2008": "CAT02 Brill 2008",
       "cat16": "CAT16", "bianco2010": "Bianco 2010", "bianco2010-pc": "Bianco PC 2010"}
PAIRS = [("CIE-2-C", "CIE-2-D65"), ("CIE-2-A", "CIE-2-D50"), ("CIE-2-D65", "CIE-2-E")]


def whitepoints():
    with open("resources/color/whitepoints.edn") as f:
        return {k: [float(v) for v in vs.split()]
                for k, vs in re.findall(r":(\S+) \[([^\]]+)\]", f.read())}


def XYZ_w(wp):
    X, Z, _, _ = wp
    return np.array([X, 1.0, Z])


def flat(m):
    return [float(v) for v in np.asarray(m).ravel()]


def kv_map(entries):
    return "{" + "\n  ".join(f"{k} {v}" for k, v in entries) + "}"


def cat_matrices():
    return kv_map((f":{k}", records([flat(colour.CHROMATIC_ADAPTATION_TRANSFORMS[v])])[1:-1])
                  for k, v in CAT.items())


def adaptation(wps):
    return kv_map((f"[:{k} :{s} :{d}]",
                   records([flat(colour.adaptation.matrix_chromatic_adaptation_VonKries(
                       XYZ_w(wps[s]), XYZ_w(wps[d]), v))])[1:-1])
                  for k, v in CAT.items() for s, d in PAIRS)


def lms(rng):
    XYZs = [[0.0, 0.0, 0.0], [95.047, 100.0, 108.883]] + rng.uniform(0.0, 110.0, (18, 3)).tolist()
    return kv_map((f":{k}", records([[*XYZ, *vecmul(colour.CHROMATIC_ADAPTATION_TRANSFORMS[v], XYZ)]
                                     for XYZ in XYZs]))
                  for k, v in CAT.items())


def gamma(e):
    return lambda x: colour.gamma_function(x, e, "Mirror")


def rgb_space(wps, name, primaries, wp, encode, decode):
    return colour.RGB_Colourspace(name, np.array(primaries), np.array(wps[wp][2:]), wp,
                                  cctf_encoding=encode, cctf_decoding=decode,
                                  use_derived_matrix_RGB_to_XYZ=True,
                                  use_derived_matrix_XYZ_to_RGB=True)


def rgb_to_rgb(rng, wps):
    spaces = {
        "sRGB": rgb_space(wps, "sRGB", [[0.64, 0.33], [0.30, 0.60], [0.15, 0.06]], "CIE-2-D65",
                          colour.models.eotf_inverse_sRGB, colour.models.eotf_sRGB),
        "Adobe-RGB-1998": rgb_space(wps, "Adobe RGB", [[0.64, 0.33], [0.21, 0.71], [0.15, 0.06]],
                                    "CIE-2-D65", gamma(0.4547069271758437), gamma(2.19921875)),
        "Adobe-Wide-Gamut-RGB": rgb_space(wps, "Adobe Wide Gamut",
                                          [[0.7347, 0.2653], [0.1152, 0.8264], [0.1566, 0.0177]],
                                          "CIE-2-D50", gamma(0.4547069271758437),
                                          gamma(2.19921875))}
    RGBs = [[0.0, 0.0, 0.0], [255.0, 255.0, 255.0], [255.0, 0.0, 0.0], [0.0, 255.0, 0.0],
            [0.0, 0.0, 255.0]] + rng.uniform(0.0, 255.0, (45, 3)).tolist()
    cases = [("sRGB", "Adobe-RGB-1998", "bradford"), ("sRGB", "Adobe-Wide-Gamut-RGB", "bradford")]
    return kv_map((f"[:{s} :{t} :{m}]",
                   records([[*RGB, *(255.0 * colour.RGB_to_RGB(
                       np.array(RGB) / 255.0, spaces[s], spaces[t], CAT[m],
                       apply_cctf_decoding=True, apply_cctf_encoding=True))] for RGB in RGBs]))
                  for s, t, m in cases)


OBSERVERS = {"CIE-2": "CIE 1931 2 Degree Standard Observer",
             "CIE-10": "CIE 1964 10 Degree Standard Observer",
             "CIE-2015-2": "CIE 2015 2 Degree Standard Observer",
             "CIE-2015-10": "CIE 2015 10 Degree Standard Observer"}
ISO = {"PF": "Photoflood", "PD": "Photographic Daylight", "SD": "Sensitometric Daylight",
       "SPF": "Sensitometric Photoflood", "SP": "Sensitometric Printer",
       "SST": "Sensitometric Studio Tungsten", "ST": "Studio Tungsten"}


def illuminant_names():
    """clojure2d key -> colour SDS_ILLUMINANTS name."""
    names = {}
    for n in colour.SDS_ILLUMINANTS:
        if n.startswith("ISO 7589 "):
            k = "ISO-" + next(a for a, b in ISO.items() if "ISO 7589 " + b == n)
        else:
            k = n.replace(".", "-")
        names[k] = n
    return names


def nums(xs):
    return "[" + " ".join(num(x) for x in xs) + "]"


def longs(xs):
    return "[" + " ".join(str(int(x)) for x in xs) + "]"


def cmfs_data():
    def entry(k, n):
        c = colour.MSDS_CMFS[n]
        w = c.wavelengths
        return (f":{k} {{:lambda {longs(w)} :x {nums(c.values[:, 0])} :y {nums(c.values[:, 1])}"
                f" :z {nums(c.values[:, 2])} :range [{int(w[0])} {int(w[-1])}]}}")
    return "{" + "\n  ".join(entry(k, n) for k, n in OBSERVERS.items()) + "}"


def illuminants_data():
    def entry(k, n):
        sd = colour.SDS_ILLUMINANTS[n]
        w = sd.wavelengths
        return f":{k} {{:lambda {longs(w)} :value {nums(sd.values)} :range [{int(w[0])} {int(w[-1])}]}}"
    return "{" + "\n  ".join(entry(k, n) for k, n in sorted(illuminant_names().items())) + "}"


def wavelength(rng):
    out = []
    for k, n in OBSERVERS.items():
        c = colour.MSDS_CMFS[n]
        lo, hi = c.shape.start, c.shape.end
        ls = list(np.arange(lo, hi + 1, 7.0)) + [hi] + list(np.round(rng.uniform(lo, hi, 60), 3))
        out.append((f":{k}", records([[l, *colour.wavelength_to_XYZ(l, c)] for l in ls])))
    return kv_map(out)


def spectrum_white():
    out = []
    for k, n in OBSERVERS.items():
        cmfs = colour.MSDS_CMFS[n]
        ones = colour.sd_ones(cmfs.shape)
        for i, ni in sorted(illuminant_names().items()):
            XYZ = colour.sd_to_XYZ(ones, cmfs, colour.SDS_ILLUMINANTS[ni], method="Integration") / 100
            out.append((f"[:{k} :{i}]", nums(XYZ)))
    return kv_map(out)


def colorchecker():
    sds = list(colour.SDS_COLOURCHECKERS["ColorChecker N Ohta"].values())
    return sds, "[" + "\n  ".join(f"{{:lambda {longs(sd.wavelengths)} :value {nums(sd.values)}}}"
                                  for sd in sds) + "]"


ZERO = {"method": "Constant", "left": 0, "right": 0}


def aligned(sd, shape, interpolator, kw):
    sd = sd.copy()
    if interpolator is not None:
        sd.interpolator = interpolator
    return sd.align(shape, interpolator=interpolator, **kw)


# [clojure2d :interpolation, :illuminant-interpolation] -> colour interpolators (None: colour's
# default rule, Sprague for uniform and CubicSpline for non-uniform data)
INTERPOLATIONS = [("auto", "linear", None, LinearInterpolator),
                  ("linear", "linear", LinearInterpolator, LinearInterpolator),
                  ("sprague", "sprague", SpragueInterpolator, SpragueInterpolator)]


def spectrum_samples(sds):
    out = []
    for interp, ill_interp, cls, ill_cls in INTERPOLATIONS:
        for k in ["CIE-2", "CIE-2015-10"]:
            cmfs = colour.MSDS_CMFS[OBSERVERS[k]]
            for i in ["D65", "A", "FL2"]:
                for e in ["constant", "zero"]:
                    kw = {} if e == "constant" else {"extrapolator_kwargs": ZERO}
                    ill = aligned(colour.SDS_ILLUMINANTS[i], cmfs.shape, ill_cls, kw)
                    rows = [colour.sd_to_XYZ(aligned(sd, cmfs.shape, cls, kw), cmfs, ill,
                                             method="Integration") / 100 for sd in sds]
                    out.append((f"[:{k} :{i} :{e} :{interp} :{ill_interp}]", records(rows)))
    return kv_map(out)


def nonuniform(sds):
    """ColorChecker spectra at non-uniform wavelengths (every third 5 nm sample dropped);
    colour default rule picks CubicSpline (scipy not-a-knot), D65 linear, CIE-2, constant."""
    cmfs = colour.MSDS_CMFS[OBSERVERS["CIE-2"]]
    ill = colour.SDS_ILLUMINANTS["D65"].copy().align(cmfs.shape)
    spectra, rows = [], []
    for sd in sds:
        keep = [j for j in range(len(sd.wavelengths)) if j % 3 != 1]
        nu = colour.SpectralDistribution(sd.values[keep], sd.wavelengths[keep])
        assert not nu.is_uniform()
        spectra.append(f"{{:lambda {longs(nu.wavelengths)} :value {nums(nu.values)}}}")
        rows.append(colour.sd_to_XYZ(nu.copy().align(cmfs.shape), cmfs, ill, method="Integration") / 100)
    return "{:spectra [" + "\n  ".join(spectra) + "]\n :XYZ " + records(rows) + "}"


def main():
    rng = np.random.default_rng(SEED)
    wps = whitepoints()
    sections = [(":xy->XYZ", records(xy_rows(rng))), (":XYZ->xy", records(XZ_rows(rng))),
                (":cat-matrices", cat_matrices()), (":adaptation", adaptation(wps)),
                (":lms", lms(rng)), (":rgb-to-rgb", rgb_to_rgb(rng, wps)),
                (":cmfs", cmfs_data()), (":illuminants", illuminants_data()),
                (":wavelength", wavelength(rng)), (":spectrum-white", spectrum_white())]
    sds, cc = colorchecker()
    sections += [(":colorchecker", cc), (":spectrum-samples", spectrum_samples(sds)),
                 (":spectrum-nonuniform", nonuniform(sds))]
    body = "\n ".join(f"{k}\n {v}" for k, v in sections)
    with open(OUT, "w") as f:
        f.write(f";; generated by utils/whitepoints/gen_reference.py, colour {colour.__version__}\n")
        f.write("{" + body + "}\n")


if __name__ == "__main__":
    main()
