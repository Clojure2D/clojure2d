"""Generate the oracle for color mixing, averaging and nearest color (color audit, area F).

Run from the repository root (needs `npm install` in utils/color for the chroma.js oracle):

    /home/ts/penv/.venv/bin/python -I utils/color/gen_mixing.py

Output: test/resources/color/mixing_reference.edn, sRGB 0-255 (alpha 0-255) unless stated.

Colors (:colors, [r g b]): primaries, secondaries, black, white, grays, a few named colors and 40 random integer colors
(numpy default_rng, seed 20261113). Pairs (:pairs, indices into :colors): fixed pairs (red-blue, gray-blue,
white-red, gray-white, red-cyan, yellow-navy, black-white) and 20 random ones. Random colors are integers: chroma.js
average in rgb mode reads colors with get('rgb'), which rounds the channels. Interpolation weights :ts. Sets
(:sets, indices) for averages: chroma.js documentation example (#ddd, yellow, red, teal), red-magenta, red-cyan,
gray-blue, white-black and 10 random sets of 3-5 colors; :weights, positive random weights per set.

* :chroma - chroma-js 2.6.0 (utils/color/chroma_oracle.mjs, Node), unrounded, clipped:
  :mix {mode [[[r g b] per t] per pair]} for lrgb (chroma.mix default, square-root mixing), rgb, lab, hsl;
  :average {mode [[r g b] per set]} for rgb, lrgb (chroma.average default), lab, hsl; :weighted [[r g b] per set]
  (rgb with :weights).
* :coloraide - coloraide 8.14.1, not clipped (out of gamut values kept), alpha 255 unless stated:
  :mix {space [[[r g b a] per t] per pair]} - Color.mix, hue shorter, straight alpha, spaces HSL, HSV, HWB, LCH
  (coloraide lch-d65), Oklch; powerless hues (achromatic colors) are "none" and take the other hue;
  :average {space [[r g b a] per set]} and :weighted-average (with :weights) - Color.average (circular hue mean);
  :alpha - premultiplied vs straight: :alpha-pairs [[[r g b a] [r g b a]] ...], :alpha-mix {space {flag [[[r g b a]
  per t] per pair]}} for spaces RGB (coloraide srgb), HSL, LCH, Oklch and flags :premultiplied / :straight;
  :alpha-sets [[[r g b a] ...] ...], :alpha-average {space {flag [[r g b a] per set]}}; :alpha-weights (positive,
  random) and :alpha-weighted-average {space {flag [[r g b a] per set]}} - Color.average with the weights.
* :mixsub [[[r g b a] per t] per pair] - ProfJski ArtColors ColorMixSub (RYB.cpp) in floating point: f = max(0,
  a + b - 255) per channel, cd = 4 t (1 - t) |a - b| / (sqrt(3) 255) (RGB distance), lerp(lerp(a, b, t), f, cd),
  alpha 255.
* :nearest {:palette [[r g b a] ...] :index [i per color]} - argmin of the Euclidean RGBA distance from each color of
  :colors (alpha 255) to the palette (16 random colors, random alpha for half of them, seed 20261114).
"""

import json
import subprocess

import numpy as np
from coloraide import Color
import coloraide

OUT = "test/resources/color/mixing_reference.edn"

TS = [0.0, 0.25, 0.5, 0.75, 1.0]
SPACES = {"HSL": "hsl", "HSV": "hsv", "HWB": "hwb", "LCH": "lch-d65", "Oklch": "oklch"}
ALPHA_SPACES = {"RGB": "srgb", "HSL": "hsl", "LCH": "lch-d65", "Oklch": "oklch"}

FIXED = [[255, 0, 0], [0, 255, 0], [0, 0, 255], [255, 255, 0], [0, 255, 255], [255, 0, 255],
         [0, 0, 0], [255, 255, 255], [128, 128, 128], [221, 221, 221], [0, 128, 128], [0, 0, 128],
         [200, 50, 80], [30, 60, 120]]
RED, GREEN, BLUE, YELLOW, CYAN, MAGENTA, BLACK, WHITE, GRAY, DDD, TEAL, NAVY = range(12)


def colors():
    rng = np.random.default_rng(20261113)
    return np.array(FIXED + [list(c) for c in rng.integers(0, 256, (40, 3))], dtype=float)


def pairs_sets_weights(n):
    rng = np.random.default_rng(20261113 + 1)
    fixed_pairs = [[RED, BLUE], [GRAY, BLUE], [WHITE, RED], [GRAY, WHITE], [RED, CYAN], [YELLOW, NAVY],
                   [BLACK, WHITE]]
    pairs = fixed_pairs + [list(map(int, rng.choice(np.arange(12, n), 2, replace=False))) for _ in range(20)]
    fixed_sets = [[DDD, YELLOW, RED, TEAL], [RED, MAGENTA], [RED, CYAN], [GRAY, BLUE], [WHITE, BLACK]]
    sets = fixed_sets + [list(map(int, rng.choice(np.arange(12, n), int(rng.integers(3, 6)), replace=False)))
                         for _ in range(10)]
    weights = [[float(w) for w in rng.uniform(0.1, 3.0, len(s))] for s in sets]
    return pairs, sets, weights


def chroma(C, pairs, sets, weights):
    payload = json.dumps({"mixing": {"colors": C.tolist(), "pairs": pairs, "ts": TS, "sets": sets,
                                     "weights": weights}})
    res = subprocess.run(["node", "utils/color/chroma_oracle.mjs"], input=payload, capture_output=True, text=True,
                         check=True)
    return json.loads(res.stdout)


def ca(rgba):
    """coloraide sRGB color from [r g b] or [r g b a] on 0-255."""
    a = rgba[3] / 255.0 if len(rgba) > 3 else 1.0
    return Color("srgb", [v / 255.0 for v in rgba[:3]], a)


def out_rgba(c):
    s = c.convert("srgb")
    return [255.0 * v for v in s[:-1]] + [255.0 * s[-1]]


def coloraide_oracle(C, pairs, sets, weights):
    mix = {k: [[out_rgba(ca(C[i]).mix(ca(C[j]), t, space=s, premultiplied=False)) for t in TS] for i, j in pairs]
           for k, s in SPACES.items()}
    average = {k: [out_rgba(Color.average([ca(C[i]) for i in st], space=s, premultiplied=False)) for st in sets]
               for k, s in SPACES.items()}
    weighted = {k: [out_rgba(Color.average([ca(C[i]) for i in st], w, space=s, premultiplied=False))
                    for st, w in zip(sets, weights)]
                for k, s in SPACES.items()}
    rng = np.random.default_rng(20261113 + 2)
    alpha_pairs = [[[255, 0, 0, 51], [0, 0, 255, 178.5]], [[255, 0, 0, 0], [0, 0, 255, 255]],
                   [[255, 0, 0, 0], [0, 0, 255, 0]], [[0, 255, 0, 127.5], [255, 0, 255, 127.5]]]
    alpha_pairs += [[list(C[int(i)]) + [float(rng.uniform(0, 255))], list(C[int(j)]) + [float(rng.uniform(0, 255))]]
                    for i, j in (rng.choice(np.arange(12, len(C)), 2, replace=False) for _ in range(6))]
    flags = {"premultiplied": True, "straight": False}
    alpha_mix = {k: {f: [[out_rgba(ca(a).mix(ca(b), t, space=s, premultiplied=p)) for t in TS] for a, b in alpha_pairs]
                     for f, p in flags.items()}
                 for k, s in ALPHA_SPACES.items()}
    alpha_sets = [[[255, 0, 0, 0], [0, 0, 255, 255]], [[255, 0, 0, 51], [0, 255, 0, 255], [0, 0, 255, 127.5]]]
    alpha_sets += [[list(C[int(i)]) + [float(rng.uniform(0, 255))] for i in rng.choice(np.arange(12, len(C)), 3,
                                                                                       replace=False)]
                   for _ in range(4)]
    alpha_average = {k: {f: [out_rgba(Color.average([ca(c) for c in st], space=s, premultiplied=p)) for st in alpha_sets]
                         for f, p in flags.items()}
                     for k, s in ALPHA_SPACES.items()}
    alpha_weights = [[float(w) for w in rng.uniform(0.1, 3.0, len(st))] for st in alpha_sets]
    alpha_weighted = {k: {f: [out_rgba(Color.average([ca(c) for c in st], w, space=s, premultiplied=p))
                              for st, w in zip(alpha_sets, alpha_weights)]
                          for f, p in flags.items()}
                      for k, s in ALPHA_SPACES.items()}
    return {"mix": mix, "average": average, "weighted-average": weighted, "alpha-pairs": alpha_pairs,
            "alpha-mix": alpha_mix, "alpha-sets": alpha_sets, "alpha-average": alpha_average,
            "alpha-weights": alpha_weights, "alpha-weighted-average": alpha_weighted}


def mixsub(a, b, t):
    """ProfJski ArtColors ColorMixSub in floating point, alpha 255."""
    a, b = np.asarray(a, dtype=float), np.asarray(b, dtype=float)
    f = np.maximum(0.0, a + b - 255.0)
    cd = 4.0 * t * (1.0 - t) * np.sqrt(np.sum((a - b) ** 2)) / (np.sqrt(3.0) * 255.0)
    mixed = (1.0 - t) * a + t * b
    return list((1.0 - cd) * mixed + cd * f) + [255.0]


def nearest(C):
    rng = np.random.default_rng(20261114)
    rgb = rng.uniform(0.0, 255.0, (16, 3))
    alpha = np.concatenate([np.full(8, 255.0), rng.uniform(0.0, 255.0, 8)])
    palette = np.column_stack([rgb, alpha])
    queries = np.column_stack([C, np.full(len(C), 255.0)])
    index = [int(np.argmin(np.sqrt(np.sum((palette - q) ** 2, axis=1)))) for q in queries]
    return palette, index


def num(x):
    x = float(x)
    return f"{int(x)}.0" if x == int(x) and abs(x) < 1e15 else repr(x)


def vec(v):
    return "[" + " ".join(num(x) for x in v) + "]"


def rows(rs):
    return "[" + " ".join(vec(r) for r in rs) + "]"


def nested(rss):
    return "[" + "\n   ".join(rows(rs) for rs in rss) + "]"


def keyed(d, f, indent):
    return "{" + ("\n" + " " * indent).join(f":{k} {f(v)}" for k, v in d.items()) + "}"


def main():
    C = colors()
    pairs, sets, weights = pairs_sets_weights(len(C))
    ch = chroma(C, pairs, sets, weights)
    co = coloraide_oracle(C, pairs, sets, weights)
    palette, index = nearest(C)
    out = [f";; generated by utils/color/gen_mixing.py, chroma-js 2.6.0, coloraide {coloraide.__version__}, "
           f"numpy {np.__version__}",
           "{:colors " + rows(C),
           " :ts " + vec(TS),
           " :pairs [" + " ".join("[" + " ".join(str(i) for i in p) + "]" for p in pairs) + "]",
           " :sets [" + " ".join("[" + " ".join(str(i) for i in s) + "]" for s in sets) + "]",
           " :weights " + rows(weights),
           " :chroma {:mix " + keyed(ch["mix"], nested, 15),
           "          :average " + keyed(ch["average"], rows, 19),
           "          :weighted " + rows(ch["weighted"]) + "}",
           " :coloraide {:mix " + keyed(co["mix"], nested, 18),
           "             :average " + keyed(co["average"], rows, 22),
           "             :weighted-average " + keyed(co["weighted-average"], rows, 31),
           "             :alpha-pairs [" + " ".join(rows(p) for p in co["alpha-pairs"]) + "]",
           "             :alpha-mix " + keyed(co["alpha-mix"], lambda d: keyed(d, nested, 24), 24),
           "             :alpha-sets [" + " ".join(rows(s) for s in co["alpha-sets"]) + "]",
           "             :alpha-average " + keyed(co["alpha-average"], lambda d: keyed(d, rows, 28), 28),
           "             :alpha-weights " + rows(co["alpha-weights"]),
           "             :alpha-weighted-average " + keyed(co["alpha-weighted-average"], lambda d: keyed(d, rows, 37), 37) + "}",
           " :mixsub " + nested([[mixsub(C[i], C[j], t) for t in TS] for i, j in pairs]),
           " :nearest {:palette " + rows(palette) + "\n           :index [" + " ".join(map(str, index)) + "]}}"]
    with open(OUT, "w") as f:
        f.write("\n".join(out) + "\n")


if __name__ == "__main__":
    main()
