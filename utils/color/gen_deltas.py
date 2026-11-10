"""Generate test/resources/color/deltas_reference.edn (oracle for color differences and contrast).

Run from the repository root:

    /home/ts/penv/.venv/bin/python -I utils/color/gen_deltas.py

Pairs (:pairs, sRGB 0-255): fixed pairs (black, white, primaries, grays, identical colors) and 300 random
pairs (numpy default_rng, seed 20261110): 200 independent, 100 close (second color = first + N(0, 12),
clipped).

Every Lab-based oracle gets clojure2d's CIELAB: sRGB decoding, the sRGB primaries with the CIE-2-D65 white
read from resources/color/whitepoints.edn (colour normalised_primary_matrix), colour XYZ_to_Lab. So the
oracles test the difference formulas, not the colorspace conversion (tested in gen_colorspaces.py).

Oracles ({key [value ...]} in :oracle, one value per pair):

* colour-science: :E76 (CIE 1976), :E94, :E94-textiles (CIE 1994), :E2000, :E2000-textiles (CIE 2000, kL 1
  and 2), :CMC11, :CMC21
  (CMC l:c 1:1 and 2:1), :HyAB, :HyCH, :HyCH-textiles, :DIN99 (Euclidean in DIN99, ASTM D2244-07)
* CIE 1976 definitions: :C* (C1 - C2) and :H* (sqrt(da^2 + db^2 - dC^2))
* coloraide 8.14: :OK (Euclidean in Oklab), :lstar-coloraide (L* contrast, coloraide's own D65 Lab)
* formulas: :Ez (Safdar et al. 2017 on colour Jzazbz of XYZ x 100 cd/m2, as clojure2d), :redmean
  (compuphase), :wcag (WCAG 2 contrast ratio: sRGB threshold 0.04045, weights 0.2126 0.7152 0.0722),
  :lstar (|L1 - L2| of the Lab above), :weber ((Y1 - Y2) / Y2) and :michelson
  ((Ymax - Ymin) / (Ymax + Ymin)) on Y of clojure2d XYZ (0-1)
* d3-jnd (connorgr/d3-jnd src/jnd.js): :jnd {[p s] [interval-L interval-a interval-b]} for ND(p, s) =
  p (A + B / s), A = (10.16, 10.68, 10.70), B = (1.50, 3.08, 5.74)

:sharma: CIEDE2000 test data of Sharma, Wu and Dalal (2005), The CIEDE2000 color-difference formula:
implementation notes, supplementary test data, and mathematical observations, Color Research & Application
30(1), 21-30 (34 pairs [Lab1 Lab2 dE], dE to 4 decimals; transcribed from colour's test suite, pair 14
commented out there for numpy platform differences); :sharma-colour: colour CIE 2000 on the same pairs.
"""

import re
import warnings

import numpy as np
import colour
import coloraide
from coloraide.everything import ColorAll as Color

warnings.filterwarnings("ignore")

OUT = "test/resources/color/deltas_reference.edn"

SHARMA = [
    ([50.0, 2.6772, -79.7751], [50.0, 0.0, -82.7485], 2.0425),
    ([50.0, 3.1571, -77.2803], [50.0, 0.0, -82.7485], 2.8615),
    ([50.0, 2.8361, -74.02], [50.0, 0.0, -82.7485], 3.4412),
    ([50.0, -1.3802, -84.2814], [50.0, 0.0, -82.7485], 1.0),
    ([50.0, -1.1848, -84.8006], [50.0, 0.0, -82.7485], 1.0),
    ([50.0, -0.9009, -85.5211], [50.0, 0.0, -82.7485], 1.0),
    ([50.0, 0.0, 0.0], [50.0, -1.0, 2.0], 2.3669),
    ([50.0, -1.0, 2.0], [50.0, 0.0, 0.0], 2.3669),
    ([50.0, 2.49, -0.001], [50.0, -2.49, 0.0009], 7.1792),
    ([50.0, 2.49, -0.001], [50.0, -2.49, 0.001], 7.1792),
    ([50.0, 2.49, -0.001], [50.0, -2.49, 0.0011], 7.2195),
    ([50.0, 2.49, -0.001], [50.0, -2.49, 0.0012], 7.2195),
    ([50.0, -0.001, 2.49], [50.0, 0.0009, -2.49], 4.8045),
    ([50.0, -0.001, 2.49], [50.0, 0.001, -2.49], 4.8045),
    ([50.0, -0.001, 2.49], [50.0, 0.0011, -2.49], 4.7461),
    ([50.0, 2.5, 0.0], [50.0, 0.0, -2.5], 4.3065),
    ([50.0, 2.5, 0.0], [73.0, 25.0, -18.0], 27.1492),
    ([50.0, 2.5, 0.0], [61.0, -5.0, 29.0], 22.8977),
    ([50.0, 2.5, 0.0], [56.0, -27.0, -3.0], 31.903),
    ([50.0, 2.5, 0.0], [58.0, 24.0, 15.0], 19.4535),
    ([50.0, 2.5, 0.0], [50.0, 3.1736, 0.5854], 1.0),
    ([50.0, 2.5, 0.0], [50.0, 3.2972, 0.0], 1.0),
    ([50.0, 2.5, 0.0], [50.0, 1.8634, 0.5757], 1.0),
    ([50.0, 2.5, 0.0], [50.0, 3.2592, 0.335], 1.0),
    ([60.2574, -34.0099, 36.2677], [60.4626, -34.1751, 39.4387], 1.2644),
    ([63.0109, -31.0961, -5.8663], [62.8187, -29.7946, -4.0864], 1.263),
    ([61.2901, 3.7196, -5.3901], [61.4292, 2.248, -4.962], 1.8731),
    ([35.0831, -44.1164, 3.7933], [35.0232, -40.0716, 1.5901], 1.8645),
    ([22.7233, 20.0904, -46.694], [23.0331, 14.973, -42.5619], 2.0373),
    ([36.4612, 47.858, 18.3852], [36.2715, 50.5065, 21.2231], 1.4146),
    ([90.8027, -2.0831, 1.441], [91.1528, -1.6435, 0.0447], 1.4441),
    ([90.9257, -0.5406, -0.9208], [88.6381, -0.8985, -0.7239], 1.5381),
    ([6.7747, -0.2908, -2.4247], [5.8714, -0.0985, -2.2286], 0.6377),
    ([2.0776, 0.0795, -1.135], [0.9033, -0.0636, -0.5514], 0.9082),
]

JND_A = np.array([10.16, 10.68, 10.70])
JND_B = np.array([1.50, 3.08, 5.74])
JND_PS = [(0.5, 0.1), (0.8, 0.5), (0.5, 1.0), (0.3, 2.0)]


def whitepoint():
    txt = open("resources/color/whitepoints.edn").read()
    xn, zn, x, y = (float(v) for v in re.search(r":CIE-2-D65 \[([^\]]+)\]", txt).group(1).split())
    return np.array([x, y])


WP_XY = whitepoint()
SRGB_TO_XYZ = colour.normalised_primary_matrix(np.array([0.64, 0.33, 0.30, 0.60, 0.15, 0.06]), WP_XY)


def pairs():
    fixed = [([0, 0, 0], [255, 255, 255]), ([255, 255, 255], [0, 0, 0]), ([0, 0, 0], [0, 0, 0]),
             ([255, 0, 0], [0, 255, 0]), ([255, 0, 0], [255, 165, 0]), ([255, 165, 0], [255, 0, 0]),
             ([0, 0, 255], [0, 0, 0]), ([255, 255, 0], [0, 0, 255]), ([128, 128, 128], [128, 128, 128]),
             ([128, 128, 128], [255, 0, 0]), ([1, 1, 1], [0, 0, 0]), ([200, 30, 90], [200, 30, 90])]
    rng = np.random.default_rng(20261110)
    a = rng.uniform(0.0, 255.0, (300, 3))
    b = rng.uniform(0.0, 255.0, (300, 3))
    b[200:] = np.clip(a[200:] + rng.normal(0.0, 12.0, (100, 3)), 0.0, 255.0)
    A = np.array([p for p, _ in fixed] + list(a), dtype=float)
    B = np.array([q for _, q in fixed] + list(b), dtype=float)
    return A, B


def xyz(RGB):
    return colour.cctf_decoding(RGB / 255.0, function="sRGB") @ SRGB_TO_XYZ.T


def lab(RGB):
    return colour.XYZ_to_Lab(xyz(RGB), WP_XY)


def delta_ez(p, q):
    c1, c2 = np.hypot(p[1], p[2]), np.hypot(q[1], q[2])
    h1, h2 = np.arctan2(p[2], p[1]), np.arctan2(q[2], q[1])
    dh = 2.0 * np.sqrt(c1 * c2) * np.sin(0.5 * (h2 - h1))
    return float(np.sqrt((q[0] - p[0]) ** 2 + (c2 - c1) ** 2 + dh ** 2))


def redmean(p, q):
    r = 0.5 * (p[0] + q[0])
    d = q - p
    return float(np.sqrt((2.0 + r / 256.0) * d[0] ** 2 + 4.0 * d[1] ** 2 + (2.0 + (255.0 - r) / 256.0) * d[2] ** 2))


def wcag_luminance(RGB):
    v = RGB / 255.0
    lin = np.where(v <= 0.04045, v / 12.92, ((v + 0.055) / 1.055) ** 2.4)
    return lin @ np.array([0.2126, 0.7152, 0.0722])


def oracle(A, B):
    la, lb = lab(A), lab(B)
    ya, yb = xyz(A)[:, 1], xyz(B)[:, 1]
    ja, jb = colour.XYZ_to_Jzazbz(xyz(A) * 100.0), colour.XYZ_to_Jzazbz(xyz(B) * 100.0)
    oa = np.array([Color("srgb", list(c / 255.0)).convert("oklab").coords() for c in A])
    ob = np.array([Color("srgb", list(c / 255.0)).convert("oklab").coords() for c in B])
    ca, cb = np.hypot(la[:, 1], la[:, 2]), np.hypot(lb[:, 1], lb[:, 2])
    dc = ca - cb
    wa, wb = wcag_luminance(A), wcag_luminance(B)
    with np.errstate(divide="ignore", invalid="ignore"):
        weber = (ya - yb) / yb
        michelson = (np.maximum(ya, yb) - np.minimum(ya, yb)) / (ya + yb)
    E = colour.delta_E
    return {
        "E76": E(la, lb, method="CIE 1976"),
        "E94": E(la, lb, method="CIE 1994"),
        "E94-textiles": E(la, lb, method="CIE 1994", textiles=True),
        "E2000": E(la, lb, method="CIE 2000"),
        "E2000-textiles": E(la, lb, method="CIE 2000", textiles=True),
        "CMC11": E(la, lb, method="CMC", l=1, c=1),
        "CMC21": E(la, lb, method="CMC", l=2, c=1),
        "HyAB": E(la, lb, method="HyAB"),
        "HyCH": E(la, lb, method="HyCH"),
        "HyCH-textiles": E(la, lb, method="HyCH", textiles=True),
        "DIN99": E(la, lb, method="DIN99"),
        "C*": dc,
        "H*": np.sqrt(np.maximum(0.0, (la[:, 1] - lb[:, 1]) ** 2 + (la[:, 2] - lb[:, 2]) ** 2 - dc ** 2)),
        "OK": np.linalg.norm(oa - ob, axis=1),
        "Ez": np.array([delta_ez(p, q) for p, q in zip(ja, jb)]),
        "redmean": np.array([redmean(p, q) for p, q in zip(A, B)]),
        "wcag": (np.maximum(wa, wb) + 0.05) / (np.minimum(wa, wb) + 0.05),
        "lstar": np.abs(la[:, 0] - lb[:, 0]),
        "lstar-coloraide": np.array([Color("srgb", list(p / 255.0)).contrast(Color("srgb", list(q / 255.0)), method="lstar")
                                     for p, q in zip(A, B)]),
        "weber": weber,
        "michelson": michelson,
    }


def num(x):
    x = float(x)
    if np.isnan(x):
        return "##NaN"
    if np.isinf(x):
        return "##Inf" if x > 0 else "##-Inf"
    return f"{int(x)}.0" if x == int(x) and abs(x) < 1e15 else repr(x)


def vec(v):
    return "[" + " ".join(num(x) for x in v) + "]"


def main():
    A, B = pairs()
    O = oracle(A, B)
    L1 = np.array([p for p, _, _ in SHARMA])
    L2 = np.array([q for _, q, _ in SHARMA])
    jnd = {ps: ps[0] * (JND_A + JND_B / ps[1]) for ps in JND_PS}
    out = [f";; generated by utils/color/gen_deltas.py, colour {colour.__version__}, coloraide {coloraide.__version__}, numpy {np.__version__}",
           "{:pairs [" + "\n         ".join("[" + vec(p) + " " + vec(q) + "]" for p, q in zip(A, B)) + "]",
           " :oracle {" + "\n          ".join(f":{k} {vec(v)}" for k, v in O.items()) + "}",
           " :jnd {" + " ".join(f"[{num(p)} {num(s)}] {vec(v)}" for (p, s), v in jnd.items()) + "}",
           " :sharma [" + "\n          ".join(f"[{vec(p)} {vec(q)} {num(e)}]" for p, q, e in SHARMA) + "]",
           " :sharma-colour " + vec(colour.delta_E(L1, L2, method="CIE 2000")) + "}"]
    with open(OUT, "w") as f:
        f.write("\n".join(out) + "\n")


if __name__ == "__main__":
    main()
