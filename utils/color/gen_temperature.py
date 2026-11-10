"""Generate the blackbody color table and the temperature / wavelength oracle (color audit, area D).

Run from the repository root:

    /home/ts/penv/.venv/bin/python -I utils/color/gen_temperature.py

Outputs:

* resources/color/blackbody_srgb.edn - table used by clojure2d.color/temperature: normalized linear sRGB of a
  blackbody on a grid uniform in mired (1e6 / T), 25 to 2000 mired (40000 to 500 K) every 0.5 mired.
* test/resources/color/temperature_reference.edn - oracle for the tests.

Blackbody color, Mitchell Charity's method (vendian.org bbr_color; with colour's sRGB matrix the values kept in the
author's gist https://gist.github.com/genmeblog/b6f55912ae02406bba329b2860fb11e6 are reproduced to 2.8e-4, with
clojure2d's matrix used here to 4.5e-4): Planck spectrum
(colour planck_law) 360-830 nm every 1 nm, CIE 1964 10 degree color matching functions (summed, the scale is
irrelevant), XYZ to linear sRGB with clojure2d's matrix (sRGB primaries, CIE-2-D65 white from
resources/color/whitepoints.edn, no chromatic adaptation), a negative channel lifted by adding white (the minimum
is subtracted), the largest channel scaled to 1.

Oracle (:temperature-*, sRGB 0-255, colour cctf_encoding sRGB on the normalized values):

* :temperature-10k - [T r g b] every 10 K from 500 to 40000 K
* :temperature-fractional - [T r g b] for 60 temperatures uniform in mired (numpy default_rng, seed 20261110)
* :temperature-nodes - [T r g b] at every 50th table node (exact node temperatures)
* :gist - linear 0-1 values [T r g b] (nil where the gist has no value) of Charity's table as kept in the gist,
  18 temperatures

Normalized wavelength (:wavelength {observer [[lambda r g b] ...]}, sRGB 0-255): colour wavelength_to_XYZ (Sprague
on the 1 nm data, as clojure2d wavelength-to-XYZ), clojure2d's sRGB matrix, negative channels clipped to 0 (not
lifted by adding white: that turns deep reds pink, user decision), the largest channel scaled to 1; zero XYZ
(outside the observer range: 360-830 nm, 390-830 nm for the 2015 observers) gives black. Wavelengths 350-850 nm
every 1 nm and 30 fractional ones (seed 20261111), observers :CIE-2, :CIE-10, :CIE-2015-2 and :CIE-2015-10.
"""

import re
import warnings

import numpy as np
import colour

warnings.filterwarnings("ignore")

TABLE = "resources/color/blackbody_srgb.edn"
OUT = "test/resources/color/temperature_reference.edn"

MIRED_MIN, MIRED_MAX, MIRED_STEP = 25.0, 2000.0, 0.5
LAMBDAS = np.arange(360.0, 831.0, 1.0)
CMF_10 = colour.MSDS_CMFS["CIE 1964 10 Degree Standard Observer"].copy().align(colour.SpectralShape(360, 830, 1)).values
OBSERVERS = {"CIE-2": "CIE 1931 2 Degree Standard Observer", "CIE-10": "CIE 1964 10 Degree Standard Observer",
             "CIE-2015-2": "CIE 2015 2 Degree Standard Observer", "CIE-2015-10": "CIE 2015 10 Degree Standard Observer"}

# Charity's table values kept in the gist (linear, 0-1): [T r g b], None where the gist has no entry
GIST = [(1000, None, 0.0401, None), (1500, None, 0.1515, None), (1900, None, 0.2272, None),
        (2000, None, 0.2484, 0.0061), (2500, None, 0.3577, 0.0640), (3000, None, 0.4589, 0.1483),
        (4000, None, 0.6354, 0.3684), (5000, None, 0.7792, 0.6180), (6000, None, 0.8952, 0.8666),
        (6500, None, 0.9445, 0.9853), (6600, 0.9917, 0.9458, None), (7000, 0.9102, 0.9000, None),
        (8000, 0.7644, 0.8139, None), (10000, 0.6033, 0.7106, None), (15000, 0.4493, 0.6007, None),
        (20000, 0.3928, 0.5565, None), (30000, 0.3471, 0.5188, None), (40000, 0.3277, 0.5022, None)]


def xyz_to_srgb_matrix():
    txt = open("resources/color/whitepoints.edn").read()
    xn, zn, x, y = (float(v) for v in re.search(r":CIE-2-D65 \[([^\]]+)\]", txt).group(1).split())
    m = colour.normalised_primary_matrix(np.array([0.64, 0.33, 0.30, 0.60, 0.15, 0.06]), np.array([x, y]))
    return np.linalg.inv(m)


XYZ_TO_SRGB = xyz_to_srgb_matrix()


def lift_normalize(rgb):
    """Lift a negative channel by adding white, scale the largest channel to 1; zero light gives black."""
    m = rgb.min()
    if m < 0.0:
        rgb = rgb - m
    mx = rgb.max()
    return rgb / mx if mx > 0.0 else np.zeros(3)


def clip_normalize(rgb):
    """Clip negative channels to 0, scale the largest channel to 1; zero light gives black."""
    rgb = np.maximum(rgb, 0.0)
    mx = rgb.max()
    return rgb / mx if mx > 0.0 else np.zeros(3)


def blackbody_linear(t):
    xyz = colour.colorimetry.planck_law(LAMBDAS * 1e-9, t) @ CMF_10
    return lift_normalize(XYZ_TO_SRGB @ xyz)


def encode(v):
    return 255.0 * colour.cctf_encoding(v, function="sRGB")


def wavelength_srgb(lam, observer):
    # outside the observer range clojure2d wavelength-to-XYZ gives zero light (colour raises)
    cmfs = colour.MSDS_CMFS[OBSERVERS[observer]]
    if not cmfs.shape.start <= lam <= cmfs.shape.end:
        return np.zeros(3)
    xyz = colour.wavelength_to_XYZ(lam, cmfs)
    return encode(clip_normalize(XYZ_TO_SRGB @ np.asarray(xyz)))


def num(x):
    if x is None:
        return "nil"
    x = float(x)
    return f"{int(x)}.0" if x == int(x) and abs(x) < 1e15 else repr(x)


def row(*vs):
    return "[" + " ".join(num(v) for v in vs) + "]"


def main():
    mireds = MIRED_MIN + MIRED_STEP * np.arange(int(round((MIRED_MAX - MIRED_MIN) / MIRED_STEP)) + 1)
    table = [blackbody_linear(1.0e6 / m) for m in mireds]
    with open(TABLE, "w") as f:
        f.write(f";; generated by utils/color/gen_temperature.py, colour {colour.__version__}, numpy {np.__version__}\n")
        f.write(";; normalized linear sRGB of a blackbody (Mitchell Charity's method, CIE 1964 10 degree, no adaptation)\n")
        f.write(f"{{:mired-min {num(MIRED_MIN)} :mired-step {num(MIRED_STEP)}\n :rgb [")
        f.write("\n       ".join(row(*v) for v in table))
        f.write("]}\n")

    rng = np.random.default_rng(20261110)
    t10 = np.arange(500.0, 40001.0, 10.0)
    tfrac = 1.0e6 / rng.uniform(MIRED_MIN, MIRED_MAX, 60)
    nodes = 1.0e6 / mireds[::50]
    lrng = np.random.default_rng(20261111)
    lams = np.concatenate([np.arange(350.0, 851.0, 1.0), lrng.uniform(360.0, 830.0, 30)])
    out = [f";; generated by utils/color/gen_temperature.py, colour {colour.__version__}, numpy {np.__version__}",
           "{:temperature-10k [" + "\n                   ".join(row(t, *encode(blackbody_linear(t))) for t in t10) + "]",
           " :temperature-fractional [" + "\n                          ".join(row(t, *encode(blackbody_linear(t))) for t in tfrac) + "]",
           " :temperature-nodes [" + "\n                     ".join(row(t, *encode(blackbody_linear(t))) for t in nodes) + "]",
           " :gist [" + " ".join(row(*g) for g in GIST) + "]",
           " :wavelength {" + "\n              ".join(
               f":{o} [" + " ".join(row(l, *wavelength_srgb(l, o)) for l in lams) + "]" for o in OBSERVERS) + "}}"]
    with open(OUT, "w") as f:
        f.write("\n".join(out) + "\n")


if __name__ == "__main__":
    main()
