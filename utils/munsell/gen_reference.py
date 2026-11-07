"""Generate test/resources/munsell/colour_reference.edn (oracle: colour-science).

Run from the repository root:

    /home/ts/penv/.venv/bin/python -I utils/munsell/gen_reference.py

Conventions of the output (clojure2d):

* specs are [hue value chroma code], greys [NaN value NaN NaN]
* Y is in 0-100
* :xyY records hold the requested (unclamped) chroma; the expected values are computed by colour
  for the chroma clamped to the renotation maximum (clojure2d clamps, colour throws)
"""

import math
import warnings

import numpy as np
import colour
from colour.notation import MUNSELL_COLOURS_ALL
from colour.notation import munsell as M

warnings.filterwarnings("ignore")

SEED = 20261107
LETTERS = {1: "B", 2: "BG", 3: "G", 4: "GY", 5: "Y", 6: "YR", 7: "R", 8: "RP", 9: "P", 10: "PB"}
# white points used by clojure2d (wp/CIE-2-C, wp/CIE-2-D65)
XY_C = np.array([0.31006, 0.31616])
XY_D65 = np.array([0.312727, 0.329023])


def num(x):
    x = float(x)
    return "##NaN" if math.isnan(x) else repr(x)


def vec(xs):
    return "[" + " ".join(num(x) if not isinstance(x, str) else '"' + x + '"' for x in xs) + "]"


def records(rows):
    return "[" + "\n  ".join(vec(r) for r in rows) + "]"


def fix_zero_hue(spec):
    """Hue within colour's snap threshold of 0 becomes 10 of the next letter, code 0 -> 10.

    colour snaps such hues to 0 and normalises 0P to code 0, which is not in the renotation data;
    Centore's toolbox (and clojure2d) use code 10 (PB).
    """
    h, v, c, code = spec
    if not math.isnan(h) and abs(h) < 0.001:
        code = (code + 1) % 10
        return [10.0, v, c, 10 if code == 0 else code]
    return spec


def xyY(spec):
    """colour xyY for a normalised spec, Y in 0-100."""
    x, y, Y = M.munsell_specification_to_xyY(np.array(fix_zero_hue(spec), dtype=float))
    return [x, y, 100 * Y]


def clamp_spec(spec):
    h, v, c, code = spec
    if math.isnan(c):
        return spec
    mx = M.maximum_chroma_from_renotation([h, v, code])
    c = min(c, mx)
    return [math.nan, v, math.nan, math.nan] if c == 0 else [h, v, c, code]


def parse_cases():
    strings = ["4.2YR 8.1/5.3", "5r 5/2", ".5R 5/2", "N.5", "N5", "N0", "N10", "n7.25",
               "5R 5/0", "5R 5/+2", "1e1R 5/2", "2.5BG 1e0/4", "0YR 2.3/2.2", "10PB 3.5/12.25",
               "7.5 GY 6 / 8"]
    rows = [[s, *M.munsell_colour_to_munsell_specification(s)] for s in strings]
    # colour's normalisation yields code 0 for 0P; the correct code (Centore) is 10 (PB)
    rows += [["0P 5/4", 10.0, 5.0, 4.0, 10.0], ["0P 2/2", 10.0, 2.0, 2.0, 10.0]]
    return rows


def grid_cases():
    rows = []
    for (hue_s, value, chroma), _ in MUNSELL_COLOURS_ALL:
        if 1 <= value <= 9:
            spec = list(M.munsell_colour_to_munsell_specification(f"{hue_s} {value}/{chroma}"))
            rows.append([*spec, *xyY(spec)])
    return rows


def random_cases(rng, n):
    rows = []
    while len(rows) < n:
        h = float(rng.uniform(0.001, 10))
        code = int(rng.integers(1, 11))
        v = float(rng.uniform(1, 10))
        c = float(rng.uniform(0.01, 40))
        spec = [h, v, c, code]
        rows.append([*spec, *xyY(clamp_spec(spec))])
    return rows


def edge_cases():
    specs = [[5, 1, 2, 7], [5, 9, 4, 7], [5, 9.5, 2, 7], [5, 9.99, 1, 7], [5, 10, 4, 7],
             [5, 9.9995, 2, 7], [5, 1.0005, 2, 7],
             [5, 5, 0.5, 7], [5, 5, 1, 7], [5, 5, 1.9, 7], [3.3, 2.7, 1.2, 3],
             [2.4995, 3, 6, 4], [2.5004, 3, 6, 4], [9.9996, 6, 8, 9], [0.0004, 6, 8, 9],
             [7.5, 4, 40, 10], [1.3, 3.3, 30, 5]]
    rows = [[*s, *xyY(clamp_spec(s))] for s in specs]
    # chromatic colours below value 1 are greys of the same value (max chroma 0)
    for s in [[5, 0.5, 4, 7], [2.5, 0, 2, 1], [7.1, 0.99, 6, 5]]:
        rows.append([*s, *xyY([math.nan, s[1], math.nan, math.nan])])
    for v in [0, 0.5, 1, 4.25, 9.5, 10]:
        rows.append([math.nan, v, math.nan, math.nan, *xyY([math.nan, v, math.nan, math.nan])])
    return rows


def string_cases():
    strings = ["4.2YR 8.1/5.3", "N5", "2.5PB 3/6", "7.3GY 4.4/7.2", "5R 9.7/3"]
    rows = [[s, *xyY(clamp_spec(list(M.munsell_colour_to_munsell_specification(s))))] for s in strings]
    rows.append(["0P 5/4", *xyY(list(M.munsell_colour_to_munsell_specification("10PB 5/4")))])
    rows.append(["0P 2/2", *xyY(list(M.munsell_colour_to_munsell_specification("10PB 2/2")))])
    return rows


def max_chroma_cases(rng, n):
    rows = []
    for _ in range(n):
        h, code = float(rng.uniform(0.001, 10)), int(rng.integers(1, 11))
        v = float(rng.choice([rng.uniform(1, 9), rng.uniform(9, 10), float(rng.integers(1, 10))]))
        rows.append([h, v, code, M.maximum_chroma_from_renotation([h, v, code])])
    for h, v, code in [[2.5, 9.5, 5], [5, 10, 7], [5, 9.999, 7], [10, 3, 10], [0.5, 1, 1]]:
        rows.append([h, v, code, M.maximum_chroma_from_renotation([h, v, code])])
    return rows


METHODS = {"priest": "Priest 1920", "munsell": "Munsell 1933", "moon": "Moon 1943",
           "saunderson": "Saunderson 1944", "ladd": "Ladd 1955", "mccamy": "McCamy 1987",
           "astm": "ASTM D1535"}


def value_cases():
    out = {}
    Ys = np.round(np.arange(0, 100.0001, 0.5), 10)
    for k, method in METHODS.items():
        V = np.array([float(colour.munsell_value(Y, method=method)) for Y in Ys])
        keep = (V >= 0) & (V <= 10)
        out[k] = [[Y, v] for Y, v in zip(Ys[keep], V[keep])]
    return out


def luminance_cases():
    Vs = np.round(np.arange(0, 10.0001, 0.1), 10)
    return {"astm": [[v, float(colour.colorimetry.luminance_ASTMD1535(v))] for v in Vs],
            "newhall": [[v, float(colour.colorimetry.luminance_Newhall1943(v))] for v in Vs]}


def adapted_cases(rows, transform="Bradford"):
    """C -> D65 adaptation of colour's xyY: [spec... Y x y (D65) X Y Z (D65, 0-100)]."""
    XYZ_c = colour.xy_to_XYZ(XY_C)
    XYZ_d65 = colour.xy_to_XYZ(XY_D65)
    Mcat = colour.adaptation.matrix_chromatic_adaptation_VonKries(XYZ_c, XYZ_d65, transform)
    out = []
    for r in rows:
        spec, (x, y, Y) = r[:4], r[4:]
        XYZ = colour.xyY_to_XYZ([x, y, Y / 100]) if Y > 0 else np.zeros(3)
        XYZa = Mcat @ XYZ
        if Y > 0:
            xa, ya, _ = colour.XYZ_to_xyY(XYZa)
        else:
            xa, ya = XY_D65
        out.append([*spec, Y, xa, ya, *(100 * XYZa)])
    return out


def main():
    rng = np.random.default_rng(SEED)
    grid = grid_cases()
    rand = random_cases(rng, 2000)
    edges = edge_cases()
    adapted = adapted_cases([*grid[::10], *rand[:300], *edges])
    adapted_cat02 = adapted_cases([*grid[::40], *rand[:100]], "CAT02")
    values = value_cases()
    lum = luminance_cases()
    out = "test/resources/munsell/colour_reference.edn"
    with open(out, "w") as f:
        f.write(";; Oracle values from colour-science, generated by utils/munsell/gen_reference.py, do not edit.\n")
        f.write(f'{{:generated "colour-science {colour.__version__}, numpy {np.__version__}"\n')
        f.write(f" :seed {SEED}\n")
        f.write(" :parse " + records(parse_cases()) + "\n")
        f.write(" :xyY-grid " + records(grid) + "\n")
        f.write(" :xyY-random " + records(rand) + "\n")
        f.write(" :xyY-edges " + records(edges) + "\n")
        f.write(" :xyY-strings " + records(string_cases()) + "\n")
        f.write(" :max-chroma " + records(max_chroma_cases(rng, 500)) + "\n")
        f.write(" :munsell-value {" + "\n  ".join(f":{k} {records(v)}" for k, v in values.items()) + "}\n")
        f.write(" :luminance {" + "\n  ".join(f":{k} {records(v)}" for k, v in lum.items()) + "}\n")
        f.write(" :adapted-d65 " + records(adapted) + "\n")
        f.write(" :adapted-d65-cat02 " + records(adapted_cat02) + "}\n")
    print(f"grid {len(grid)}, random {len(rand)}, edges {len(edges)}, adapted {len(adapted)} -> {out}")


if __name__ == "__main__":
    main()
