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
"""

import re

import numpy as np
import colour
from colour.algebra import vecmul

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


def main():
    rng = np.random.default_rng(SEED)
    wps = whitepoints()
    sections = [(":xy->XYZ", records(xy_rows(rng))), (":XYZ->xy", records(XZ_rows(rng))),
                (":cat-matrices", cat_matrices()), (":adaptation", adaptation(wps)),
                (":lms", lms(rng)), (":rgb-to-rgb", rgb_to_rgb(rng, wps))]
    body = "\n ".join(f"{k}\n {v}" for k, v in sections)
    with open(OUT, "w") as f:
        f.write(f";; generated by utils/whitepoints/gen_reference.py, colour {colour.__version__}\n")
        f.write("{" + body + "}\n")


if __name__ == "__main__":
    main()
