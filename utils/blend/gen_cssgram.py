"""Generate test/resources/blend/cssgram_reference.edn (oracle for clojure2d.color.cssgram).

Run from the repository root:

    /home/ts/penv/.venv/bin/python -I utils/blend/gen_cssgram.py

Sections:

* :definitions  CSSgram filters transcribed from una/CSSgram source/scss (master): overlays in stacking
  order (::before, then ::after) as {:mode :color [r g b a] or :gradient true, :opacity}, CSS filter
  list as [[op amount] ...]; CSS names mapped to clojure2d (brightness -> :exposure, saturate ->
  :saturation, soft-light -> :w3c-softlight, hue -> :w3c-hue, color -> :w3c-color, color-dodge ->
  :dodge, color-burn -> :burn). willow ::before (background-color: radial-gradient(...), invalid CSS
  ignored by browsers) is left out.
* :colors, :blended  W3C Compositing Level 1 blend stage (overlays only, opaque backdrop) of the
  filters without gradients on 200 random opaque colors (seed 20261112) plus black, white, grays;
  {filter [[r g b a] ...]} on the 0-255 scale.
"""

import numpy as np

OUT = "test/resources/blend/cssgram_reference.edn"

MODES = {"screen": "screen", "darken": "darken", "lighten": "lighten", "overlay": "overlay",
         "multiply": "multiply", "exclusion": "exclusion", "soft-light": "w3c-softlight",
         "hue": "w3c-hue", "color": "w3c-color", "color-dodge": "dodge", "color-burn": "burn"}
FILTERS = {"brightness": "exposure", "saturate": "saturation", "contrast": "contrast", "sepia": "sepia",
           "grayscale": "grayscale", "hue-rotate": "hue-rotate"}

G = None  # gradient overlay (approximated in clojure2d)

# name: (overlays [(css mode, rgba or G, opacity)], filters [(css filter, amount)])
CSSGRAM = {
    "y1977": ([("screen", (243, 106, 188, 0.3), 1.0)], [("contrast", 1.1), ("brightness", 1.1), ("saturate", 1.3)]),
    "aden": ([("darken", G, 1.0)], [("hue-rotate", -20.0), ("contrast", 0.9), ("saturate", 0.85), ("brightness", 1.2)]),
    "brannan": ([("lighten", (161, 44, 199, 0.31), 1.0)], [("sepia", 0.5), ("contrast", 1.4)]),
    "brooklyn": ([("overlay", G, 1.0)], [("contrast", 0.9), ("brightness", 1.1)]),
    "clarendon": ([("overlay", (127, 187, 227, 0.2), 1.0)], [("contrast", 1.2), ("saturate", 1.35)]),
    "earlybird": ([("overlay", G, 1.0)], [("contrast", 0.9), ("sepia", 0.2)]),
    "gingham": ([("soft-light", (230, 230, 250, 1.0), 1.0)], [("brightness", 1.05), ("hue-rotate", -10.0)]),
    "hudson": ([("multiply", G, 0.5)], [("brightness", 1.2), ("contrast", 0.9), ("saturate", 1.1)]),
    "inkwell": ([], [("sepia", 0.3), ("contrast", 1.1), ("brightness", 1.1), ("grayscale", 1.0)]),
    "kelvin": ([("color-dodge", (56, 44, 52, 1.0), 1.0), ("overlay", (183, 125, 33, 1.0), 1.0)], []),
    "lark": ([("color-dodge", (34, 37, 63, 1.0), 1.0), ("darken", (242, 242, 242, 0.8), 1.0)], [("contrast", 0.9)]),
    "lofi": ([("multiply", G, 1.0)], [("saturate", 1.1), ("contrast", 1.5)]),
    "maven": ([("hue", (3, 230, 26, 0.2), 1.0)], [("sepia", 0.25), ("brightness", 0.95), ("contrast", 0.95), ("saturate", 1.5)]),
    "mayfair": ([("overlay", G, 0.4)], [("contrast", 1.1), ("saturate", 1.1)]),
    "moon": ([("soft-light", (160, 160, 160, 1.0), 1.0), ("lighten", (56, 56, 56, 1.0), 1.0)], [("grayscale", 1.0), ("contrast", 1.1), ("brightness", 1.1)]),
    "nashville": ([("darken", (247, 176, 153, 0.56), 1.0), ("lighten", (0, 70, 150, 0.4), 1.0)], [("sepia", 0.2), ("contrast", 1.2), ("brightness", 1.05), ("saturate", 1.2)]),
    "perpetua": ([("soft-light", G, 0.5)], []),
    "reyes": ([("soft-light", (239, 205, 173, 1.0), 0.5)], [("sepia", 0.22), ("brightness", 1.1), ("contrast", 0.85), ("saturate", 0.75)]),
    "rise": ([("multiply", G, 1.0), ("overlay", G, 0.6)], [("brightness", 1.05), ("sepia", 0.2), ("contrast", 0.9), ("saturate", 0.9)]),
    "slumber": ([("lighten", (69, 41, 12, 0.4), 1.0), ("soft-light", (125, 105, 24, 0.5), 1.0)], [("saturate", 0.66), ("brightness", 1.05)]),
    "stinson": ([("soft-light", (240, 149, 128, 0.2), 1.0)], [("contrast", 0.75), ("saturate", 0.85), ("brightness", 1.15)]),
    "toaster": ([("screen", G, 1.0)], [("contrast", 1.5), ("brightness", 0.9)]),
    "valencia": ([("exclusion", (58, 3, 57, 1.0), 0.5)], [("contrast", 1.08), ("brightness", 1.08), ("sepia", 0.08)]),
    "walden": ([("screen", (0, 68, 204, 1.0), 0.3)], [("brightness", 1.1), ("hue-rotate", -10.0), ("sepia", 0.3), ("saturate", 1.6)]),
    "willow": ([("color", (216, 205, 203, 1.0), 1.0)], [("grayscale", 0.5), ("contrast", 0.95), ("brightness", 0.9)]),
    "x-pro2": ([("color-burn", G, 1.0)], [("sepia", 0.3)]),
}


# W3C Compositing Level 1 on the 0-1 scale, colors as (n, 3) arrays
def div(n, d):
    with np.errstate(divide="ignore", invalid="ignore"):
        q = n / d
    return np.where(n == 0.0, 0.0, np.where(d == 0.0, np.inf, q))


def hardlight(cb, cs):
    return np.where(cs <= 0.5, cb * 2.0 * cs, cb + (2.0 * cs - 1.0) - cb * (2.0 * cs - 1.0))


def softlight(cb, cs):
    d = np.where(cb <= 0.25, ((16.0 * cb - 12.0) * cb + 4.0) * cb, np.sqrt(cb))
    return np.where(cs <= 0.5, cb - (1.0 - 2.0 * cs) * cb * (1.0 - cb), cb + (2.0 * cs - 1.0) * (d - cb))


def lum(c):
    return 0.3 * c[:, 0] + 0.59 * c[:, 1] + 0.11 * c[:, 2]


def clip_color(c):
    l = lum(c)[:, None]
    n = c.min(axis=1)[:, None]
    x = c.max(axis=1)[:, None]
    with np.errstate(divide="ignore", invalid="ignore"):
        c = np.where(n < 0.0, l + (c - l) * l / (l - n), c)
        c = np.where(x > 1.0, l + (c - l) * (1.0 - l) / (x - l), c)
    return c


def set_lum(c, l):
    return clip_color(c + (l - lum(c))[:, None])


def set_sat(c, s):
    out = np.zeros_like(c)
    for i in range(len(c)):
        mn, mid, mx = np.argsort(c[i], kind="stable")
        if c[i, mx] > c[i, mn]:
            out[i, mid] = (c[i, mid] - c[i, mn]) * s[i] / (c[i, mx] - c[i, mn])
            out[i, mx] = s[i]
    return out


BLEND = {
    "screen": lambda cb, cs: cb + cs - cb * cs,
    "darken": np.minimum,
    "lighten": np.maximum,
    "overlay": lambda cb, cs: hardlight(cs, cb),
    "multiply": lambda cb, cs: cb * cs,
    "exclusion": lambda cb, cs: cb + cs - 2.0 * cb * cs,
    "soft-light": softlight,
    "color-dodge": lambda cb, cs: np.minimum(1.0, div(cb, 1.0 - cs)),
    "color-burn": lambda cb, cs: 1.0 - np.minimum(1.0, div(1.0 - cb, cs)),
    "hue": lambda cb, cs: set_lum(set_sat(cs, cb.max(axis=1) - cb.min(axis=1)), lum(cb)),
    "color": lambda cb, cs: set_lum(cs, lum(cb)),
}


def blend_stage(colors, overlays):
    cb = colors / 255.0  # opaque backdrop: alpha stays 1
    for mode, (r, g, b, a), opacity in overlays:
        cs = np.tile(np.array([r, g, b]) / 255.0, (len(cb), 1))
        alpha = a * opacity
        cb = alpha * BLEND[mode](cb, cs) + (1.0 - alpha) * cb
    return np.column_stack([cb * 255.0, np.full(len(cb), 255.0)])


def num(x):
    x = float(x)
    return f"{int(x)}.0" if x == int(x) else repr(x)


def vec(xs):
    return "[" + " ".join(num(x) for x in xs) + "]"


def overlay_edn(mode, color, opacity):
    c = ":gradient true" if color is G else ":color " + vec(color)
    return "{:mode :" + MODES[mode] + " " + c + " :opacity " + num(opacity) + "}"


def main():
    rng = np.random.default_rng(20261112)
    colors = np.vstack([[0, 0, 0], [255, 255, 255], [128, 128, 128], [64, 64, 64], [250, 10, 10],
                        rng.uniform(0.0, 255.0, (200, 3))])
    out = [";; generated by utils/blend/gen_cssgram.py, numpy " + np.__version__, "{:definitions"]
    for i, (k, (overlays, filters)) in enumerate(CSSGRAM.items()):
        o = "[" + " ".join(overlay_edn(*ov) for ov in overlays) + "]"
        f = "[" + " ".join(f"[:{FILTERS[n]} {num(v)}]" for n, v in filters) + "]"
        out.append(("  {" if i == 0 else "   ") + f":{k} {{:overlays {o} :filters {f}}}" + ("}" if i == len(CSSGRAM) - 1 else ""))
    out.append(" :colors [" + " ".join(vec(c) for c in colors) + "]")
    exact = {k: ov for k, (ov, _) in CSSGRAM.items() if all(c is not G for _, c, _ in ov)}
    out.append(" :blended")
    for i, (k, ov) in enumerate(exact.items()):
        rows = "[" + " ".join(vec(r) for r in blend_stage(colors, ov)) + "]"
        out.append(("  {" if i == 0 else "   ") + f":{k} {rows}" + ("}}" if i == len(exact) - 1 else ""))
    with open(OUT, "w") as fh:
        fh.write("\n".join(out) + "\n")


if __name__ == "__main__":
    main()
