"""Generate the oracle for gradients and paletton palettes (color audit, area C).

Run from the repository root (needs `npm install` in utils/color, and curl with network access for paletton.com):

    /home/ts/penv/.venv/bin/python -I utils/color/gen_gradients.py

Output: test/resources/color/gradients_reference.edn, sRGB 0-255 (alpha 0-255) unless stated.

Stop lists (:stops, [[r g b] ...]): fixed lists (red-blue, red-blue-lime, yellow-navy-white, red-gray-blue,
orange-teal-purple-gold) and 8 random lists of 2-5 integer colors (numpy default_rng, seed 20261115); :ts 11 points
0, 0.1, ..., 1. :chromatic - indices of the stop lists without achromatic, black or white colors.

* :chroma {mode [[[r g b] per t] per stop list]} - chroma-js 2.6.0 chroma.scale(stops).mode(mode), modes rgb, lab
  (all lists) and hsl (only :chromatic lists, others nil: chroma.js rounds HSL to integers and special-cases grays,
  white and black), unrounded, clipped (utils/color/chroma_oracle.mjs, `scale` job).
* :coloraide {space {hue [[[r g b a] per t] per stop list]}} - coloraide 8.14.1 Color.interpolate(stops, space,
  hue=..., premultiplied=False), linear, spaces HSL, HWB, LCH (coloraide lch-d65), Oklch, hue modes shorter, longer,
  increasing, decreasing; achromatic stops have an undefined (powerless) hue, taken from the neighbour per segment;
  not clipped. :domain-stops [[stops positions] ...] and :coloraide-domain {space {hue [[[r g b a] per t] per
  list]}} - the same with uneven stop positions (coloraide stop()), spaces HSL, Oklch, hue shorter and longer.
  :alpha-stops [[r g b a] ...] lists and :coloraide-alpha {space {flag [[[r g b a] per t] per list]}}
  (hue shorter, flags :premultiplied / :straight), spaces RGB (srgb), HSL, LCH, Oklch.
* :d3 {:api {name [[r g b] per t]} :unrounded {name [[r g b] per t]}} - d3-scale-chromatic 3.1.0
  interpolateCubehelixDefault, interpolateWarm, interpolateCool, interpolateRainbow at 101 points (:d3-ts): the API
  output (rounded to integers) and the same cubehelix parameters converted by d3-color without rounding, not clamped
  (utils/color/d3_oracle.mjs).
* :cosine {name [[r g b] per t]} - cosine gradients a + b cos(2 pi (c t + d)), clamped to [0, 1], of the thi.ng/color
  presets (cosine-schemes in thi.ng/color src/gradients.org) and of the iq presets (iquilezles.org palettes article,
  values as in clojure2d: the article renders them in images).
* :paletton - paletton.com color model: js/app.compiled.js is downloaded to a temporary directory, its sha256 checked
  against PALETTON_SHA256 (a mismatch stops the generator: the site changed, check and update the pin), and run by
  utils/color/paletton_oracle.cjs; only the output is stored. :presets (sorted names, spaces as hyphens: almost black), :hues (every 7 degrees and the
  segment edges), :shades {preset [[[r g b] x 5] per hue]} (paletton rounds the hue to integer degrees and RGB to
  integers), :angles and :schemes {model [[complement sec1 sec2] per angle]} for hue 0 (models analog, triad,
  tetrad; paletton limits the angle to [5, 175]).
"""

import colorsys
import hashlib
import json
import os
import subprocess
import tempfile

import numpy as np
from coloraide import Color
from coloraide.interpolate import stop
import coloraide

OUT = "test/resources/color/gradients_reference.edn"
PALETTON_URL = "https://paletton.com/js/app.compiled.js"
PALETTON_SHA256 = "8b6180587d1395975d025ce5f537f02c419d5b021136f3fbb0b1a99720c82881"

TS = [i / 10.0 for i in range(11)]
D3_TS = [i / 100.0 for i in range(101)]
SPACES = {"HSL": "hsl", "HWB": "hwb", "LCH": "lch-d65", "Oklch": "oklch"}
ALPHA_SPACES = {"RGB": "srgb", "HSL": "hsl", "LCH": "lch-d65", "Oklch": "oklch"}
HUES = ["shorter", "longer", "increasing", "decreasing"]

FIXED_STOPS = [[[255, 0, 0], [0, 0, 255]],
               [[255, 0, 0], [0, 0, 255], [0, 255, 0]],
               [[255, 255, 0], [0, 0, 128], [255, 255, 255]],
               [[255, 0, 0], [128, 128, 128], [0, 0, 255]],
               [[255, 165, 0], [0, 128, 128], [128, 0, 128], [255, 215, 0]]]
# stop lists with uneven positions (:domain of clojure2d gradients)
DOMAIN_STOPS = [([[255, 0, 0], [128, 128, 128], [0, 0, 255]], [0.0, 0.2, 1.0]),
                ([[255, 0, 0], [0, 0, 255], [0, 255, 0]], [0.0, 0.7, 1.0]),
                ([[255, 165, 0], [0, 128, 128], [128, 0, 128], [255, 215, 0]], [0.0, 0.1, 0.5, 1.0])]
DOMAIN_SPACES = {"HSL": "hsl", "Oklch": "oklch"}
ALPHA_STOPS = [[[255, 0, 0, 0], [0, 0, 255, 255]],
               [[255, 0, 0, 51], [0, 0, 255, 204], [0, 255, 0, 127.5]],
               [[255, 255, 0, 255], [0, 0, 128, 25.5], [255, 255, 255, 178.5]]]

# thi.ng/color cosine-schemes (src/gradients.org)
THING = {"rainbow1": [[0.5, 0.5, 0.5], [0.5, 0.5, 0.5], [1.0, 1.0, 1.0], [0, 0.3333, 0.6666]],
         "rainbow2": [[0.5, 0.5, 0.5], [0.666, 0.666, 0.666], [1.0, 1.0, 1.0], [0, 0.3333, 0.6666]],
         "rainbow3": [[0.5, 0.5, 0.5], [0.75, 0.75, 0.75], [1.0, 1.0, 1.0], [0, 0.3333, 0.6666]],
         "rainbow4": [[0.5, 0.5, 0.5], [1, 1, 1], [1.0, 1.0, 1.0], [0, 0.3333, 0.6666]],
         "yellow-magenta-cyan": [[1, 0.5, 0.5], [0.5, 0.5, 0.5], [0.75, 1.0, 0.6666], [0.8, 1.0, 0.3333]],
         "orange-blue": [[0.5, 0.5, 0.5], [0.5, 0.5, 0.5], [0.8, 0.8, 0.5], [0, 0.2, 0.5]],
         "green-magenta": [[0.6666, 0.5, 0.5], [0.5, 0.6666, 0.5], [0.6666, 0.666, 0.5], [0.2, 0.0, 0.5]],
         "green-red": [[0.5, 0.5, 0], [0.5, 0.5, 0], [0.5, 0.5, 0], [0.5, 0.0, 0]],
         "green-cyan": [[0.0, 0.5, 0.5], [0, 0.5, 0.5], [0.0, 0.3333, 0.5], [0.0, 0.6666, 0.5]],
         "yellow-red": [[0.5, 0.5, 0], [0.5, 0.5, 0], [0.1, 0.5, 0], [0.0, 0.0, 0]],
         "blue-cyan": [[0.0, 0.5, 0.5], [0, 0.5, 0.5], [0.0, 0.5, 0.3333], [0.0, 0.5, 0.6666]],
         "red-blue": [[0.5, 0, 0.5], [0.5, 0, 0.5], [0.5, 0, 0.5], [0, 0, 0.5]],
         "yellow-green-blue": [[0.650, 0.5, 0.310], [-0.650, 0.5, 0.6], [0.333, 0.278, 0.278], [0.660, 0.0, 0.667]],
         "blue-white-red": [[0.660, 0.56, 0.680], [0.718, 0.438, 0.720], [0.520, 0.8, 0.520], [-0.430, -0.397, -0.083]],
         "cyan-magenta": [[0.610, 0.498, 0.650], [0.388, 0.498, 0.350], [0.530, 0.498, 0.620], [3.438, 3.012, 4.025]],
         "yellow-purple-magenta": [[0.731, 1.098, 0.192], [0.358, 1.090, 0.657], [1.077, 0.360, 0.328],
                                   [0.965, 2.265, 0.837]],
         "green-blue-orange": [[0.892, 0.725, 0.000], [0.878, 0.278, 0.725], [0.332, 0.518, 0.545],
                               [2.440, 5.043, 0.732]],
         "orange-magenta-blue": [[0.821, 0.328, 0.242], [0.659, 0.481, 0.896], [0.612, 0.340, 0.296],
                                 [2.820, 3.026, -0.273]],
         "blue-magenta-orange": [[0.938, 0.328, 0.718], [0.659, 0.438, 0.328], [0.388, 0.388, 0.296],
                                 [2.538, 2.478, 0.168]],
         "magenta-green": [[0.590, 0.811, 0.120], [0.410, 0.392, 0.590], [0.940, 0.548, 0.278],
                           [-4.242, -6.611, -4.045]]}
IQ = {"iq-1": [[0.5] * 3, [0.5] * 3, [1.0] * 3, [0.0, 0.33, 0.67]],
      "iq-2": [[0.5] * 3, [0.5] * 3, [1.0] * 3, [0.0, 0.1, 0.2]],
      "iq-3": [[0.5] * 3, [0.5] * 3, [1.0] * 3, [0.3, 0.2, 0.2]],
      "iq-4": [[0.5] * 3, [0.5] * 3, [1.0, 1.0, 0.5], [0.8, 0.9, 0.3]],
      "iq-5": [[0.5] * 3, [0.5] * 3, [1.0, 0.7, 0.4], [0.0, 0.15, 0.2]],
      "iq-6": [[0.5] * 3, [0.5] * 3, [2.0, 1.0, 0.0], [0.5, 0.2, 0.25]],
      "iq-7": [[0.8, 0.5, 0.4], [0.2, 0.4, 0.2], [2.0, 1.0, 1.0], [0.0, 0.25, 0.25]]}

PALETTON_HUES = sorted(set(range(0, 360, 7)) | {119, 120, 121, 179, 180, 209, 210, 254, 255, 314, 315})
PALETTON_ANGLES = [2, 5, 30, 60, 90, 120]


def stop_lists():
    rng = np.random.default_rng(20261115)
    rnd = [[list(map(int, c)) for c in rng.integers(0, 256, (int(rng.integers(2, 6)), 3))] for _ in range(8)]
    return FIXED_STOPS + rnd


def chromatic(stops):
    for c in stops:
        _, l, s = colorsys.rgb_to_hls(*[v / 255.0 for v in c[:3]])
        if s == 0.0 or l in (0.0, 1.0):
            return False
    return True


def node(script, payload, *args):
    res = subprocess.run(["node", script, *args], input=json.dumps(payload), capture_output=True, text=True,
                         check=True)
    return json.loads(res.stdout)


def ca(rgba):
    a = rgba[3] / 255.0 if len(rgba) > 3 else 1.0
    return Color("srgb", [v / 255.0 for v in rgba[:3]], a)


def out_rgba(c):
    s = c.convert("srgb")
    return [255.0 * v for v in s[:-1]] + [255.0 * s[-1]]


def coloraide_oracle(stops):
    hue = {k: {h: [[out_rgba(f(t)) for t in TS]
                   for f in (Color.interpolate([ca(c) for c in st], space=s, hue=h, premultiplied=False)
                             for st in stops)]
               for h in HUES}
           for k, s in SPACES.items()}
    flags = {"premultiplied": True, "straight": False}
    alpha = {k: {f: [[out_rgba(g(t)) for t in TS]
                     for g in (Color.interpolate([ca(c) for c in st], space=s, premultiplied=p) for st in ALPHA_STOPS)]
                 for f, p in flags.items()}
             for k, s in ALPHA_SPACES.items()}
    domain = {k: {h: [[out_rgba(f(t)) for t in TS]
                      for f in (Color.interpolate([stop(ca(c), x) for c, x in zip(st, xs)], space=s, hue=h,
                                                  premultiplied=False)
                                for st, xs in DOMAIN_STOPS)]
                  for h in ["shorter", "longer"]}
              for k, s in DOMAIN_SPACES.items()}
    return hue, alpha, domain


def cosine(a, b, c, d, t):
    a, b, c, d = map(np.asarray, (a, b, c, d))
    return list(255.0 * np.clip(a + b * np.cos(2.0 * np.pi * (c * t + d)), 0.0, 1.0))


def paletton():
    with tempfile.TemporaryDirectory() as tmp:
        path = os.path.join(tmp, "app.compiled.js")
        # downloaded with curl: the server answers 406 to Python's urllib
        subprocess.run(["curl", "-sSfL", "--max-time", "60", "-o", path, PALETTON_URL], check=True)
        with open(path, "rb") as f:
            data = f.read()
        digest = hashlib.sha256(data).hexdigest()
        if digest != PALETTON_SHA256:
            raise SystemExit(f"paletton.com bundle changed: sha256 {digest}, expected {PALETTON_SHA256}; "
                             "check the color model and update PALETTON_SHA256")
        return node("utils/color/paletton_oracle.cjs", {"hues": PALETTON_HUES, "angles": PALETTON_ANGLES}, path)


def kw(name):
    """Paletton preset name as a keyword name (paletton.com writes almost black with a space)."""
    return name.replace(" ", "-")


def num(x):
    if x is None:
        return "nil"
    x = float(x)
    return f"{int(x)}.0" if x == int(x) and abs(x) < 1e15 else repr(x)


def vec(v):
    return "nil" if v is None else "[" + " ".join(num(x) for x in v) + "]"


def rows(rs):
    return "nil" if rs is None else "[" + " ".join(vec(r) for r in rs) + "]"


def nested(rss):
    return "[" + "\n   ".join(rows(rs) for rs in rss) + "]"


def keyed(d, f, indent):
    return "{" + ("\n" + " " * indent).join(f":{k} {f(v)}" for k, v in d.items()) + "}"


def main():
    stops = stop_lists()
    chrom = [i for i, st in enumerate(stops) if chromatic(st)]
    ch = node("utils/color/chroma_oracle.mjs", {"scale": {"stops": stops, "modes": ["rgb", "lab", "hsl"], "ts": TS}})
    ch = ch["scale"]
    ch["hsl"] = [rs if i in chrom else None for i, rs in enumerate(ch["hsl"])]
    hue, alpha, domain = coloraide_oracle(stops)
    d3 = node("utils/color/d3_oracle.mjs", {"ts": D3_TS})
    cos = {k: [cosine(*v, t) for t in TS] for k, v in {**THING, **IQ}.items()}
    pal = paletton()
    out = [f";; generated by utils/color/gen_gradients.py, chroma-js 2.6.0, coloraide {coloraide.__version__}, "
           f"d3-scale-chromatic 3.1.0, paletton.com bundle sha256 {PALETTON_SHA256[:16]}..., numpy {np.__version__}",
           "{:stops [" + "\n         ".join(rows(st) for st in stops) + "]",
           " :ts " + vec(TS),
           " :chromatic [" + " ".join(map(str, chrom)) + "]",
           " :chroma " + keyed(ch, nested, 10),
           " :coloraide " + keyed(hue, lambda d: keyed(d, nested, 21), 13),
           " :domain-stops [" + " ".join("[" + rows(st) + " " + vec(xs) + "]" for st, xs in DOMAIN_STOPS) + "]",
           " :coloraide-domain " + keyed(domain, lambda d: keyed(d, nested, 28), 20),
           " :alpha-stops [" + " ".join(rows(st) for st in ALPHA_STOPS) + "]",
           " :coloraide-alpha " + keyed(alpha, lambda d: keyed(d, nested, 27), 19),
           " :d3-ts " + vec(D3_TS),
           " :d3 {:api " + keyed(d3["api"], rows, 11) + "\n      :unrounded " + keyed(d3["unrounded"], rows, 17) + "}",
           " :cosine " + keyed(cos, rows, 10),
           " :paletton {:presets [" + " ".join(f":{kw(p)}" for p in pal["presets"]) + "]",
           "            :hues " + vec(PALETTON_HUES),
           "            :shades " + keyed({kw(k): v for k, v in pal["shades"].items()}, nested, 21),
           "            :angles " + vec(PALETTON_ANGLES),
           "            :schemes " + keyed(pal["schemes"], lambda r: "[" + " ".join(vec(x) for x in r) + "]", 22)
           + "}}"]
    with open(OUT, "w") as f:
        f.write("\n".join(out) + "\n")


if __name__ == "__main__":
    main()
