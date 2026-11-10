"""Generate the oracle for color adjustments, CSS filters and color vision deficiency (color audit, area G).

Run from the repository root (needs `npm install` in utils/color for the chroma.js oracle):

    /home/ts/penv/.venv/bin/python -I utils/color/gen_filters.py

Outputs:

* test/resources/color/filters_reference.edn - oracle for the tests
* resources/color/cvd_machado2009.edn - Machado et al. (2009) color vision deficiency matrices (protanomaly,
  deuteranomaly) used by clojure2d.color/cvd-filter and the protan / deutan filters
* resources/color/cvd_brettel1997.edn - Brettel, Viénot, Mollon (1997) tritanopia as two linear sRGB matrices and
  the normal of the separation plane (DaltonLens Simulator_Brettel1997, Smith-Pokorny LMS, 485 / 660 nm anchors,
  RGB white as neutral), used by the tritan filters

Colors (:colors, sRGB 0-255, alpha 255): primaries, secondaries, black, white, grays and 40 random colors
(numpy default_rng, seed 20261112).

CSS filters (:filters {name [[amount [[r g b a] ...]] ...]}): W3C Filter Effects Module Level 1 (2018), section
13.1 equivalents, written from the specification text: sepia, grayscale (feColorMatrix type matrix, weights 0.2126
0.7152 0.0722), saturate (feColorMatrix type saturate, weights 0.213 0.715 0.072), hue-rotate (type hueRotate,
angle in degrees), contrast (feComponentTransfer linear, slope amount, intercept 0.5 - 0.5 amount), brightness
(feComponentTransfer linear, slope amount; clojure2d exposure), invert (table amount, 1 - amount), opacity (alpha
table 0, amount). Filter functions operate on encoded sRGB (section 5); results are clamped to [0, 1]. Amounts as
given (the specification clamps grayscale, sepia, invert, opacity amounts above 1: the clamped value is used here).

chroma.js (:chroma {op [[[r g b] per amount] per color]}, :chroma-amounts): chroma-js 2.6.0 brighten, darken,
saturate, desaturate (utils/color/chroma_oracle.mjs, Node), unrounded clipped sRGB.

Color vision deficiency, linear sRGB (colour cctf_decoding / cctf_encoding), results clipped to [0, 1]:

* :cvd {deficiency [[severity [[r g b a] ...]] ...]} - protanomaly, deuteranomaly: colour matrix_cvd_Machado2009
  (linear interpolation between the published 0.1 severity steps); tritanomaly: DaltonLens Brettel 1997 dichromat
  blended with the color by severity (DaltonLens convention); severities 0, 0.35, 0.7, 1
* :cvd-vars {name [[r g b a] ...]} - the named filters: protanopia, deuteranopia (Machado 1), protanomaly,
  deuteranomaly (Machado 0.6), tritanopia (Brettel), tritanomaly (Brettel blend 0.6), achromatopsia (Y of
  clojure2d's sRGB matrix, CIE-2-D65), achromatomaly (50 % blend toward achromatopsia)
"""

import json
import subprocess
import warnings

import re

import numpy as np
import colour
from daltonlens import convert as dl_convert, simulate as dl

warnings.filterwarnings("ignore")

OUT = "test/resources/color/filters_reference.edn"
CVD_TABLE = "resources/color/cvd_machado2009.edn"
BRETTEL_TABLE = "resources/color/cvd_brettel1997.edn"

AMOUNTS = [0.0, 0.25, 0.5, 1.0, 1.5, 2.0]
ANGLES = [0.0, 30.0, 90.0, 180.0, -20.0]
CHROMA_AMOUNTS = [-1.0, -0.5, 0.5, 1.0, 2.0]
SEVERITIES = [0.0, 0.35, 0.7, 1.0]
DEFICIENCIES = {"protanomaly": "Protanomaly", "deuteranomaly": "Deuteranomaly"}
BRETTEL = dl.Simulator_Brettel1997()


def luminance_row():
    txt = open("resources/color/whitepoints.edn").read()
    xn, zn, x, y = (float(v) for v in re.search(r":CIE-2-D65 \[([^\]]+)\]", txt).group(1).split())
    return colour.normalised_primary_matrix(np.array([0.64, 0.33, 0.30, 0.60, 0.15, 0.06]), np.array([x, y]))[1]


Y_ROW = luminance_row()


def brettel_tritan_rgb():
    """DaltonLens Simulator_Brettel1997 tritan projection as linear sRGB matrices: (wing1, wing2, separation normal);
    wing2 where dot(separation, rgb) < 0 (transcribed from DaltonLens _simulate_dichromacy_linear_rgb)."""
    model = BRETTEL.color_model
    lms_neutral = model.LMS_from_linearRGB @ np.array([1.0, 1.0, 1.0])
    lms_485 = model.LMS_from_XYZ @ np.array([0.05795, 0.1693, 0.6162])
    lms_660 = model.LMS_from_XYZ @ np.array([0.1649, 0.0610, 0.0000])
    n1, n2 = np.cross(lms_neutral, lms_485), np.cross(lms_neutral, lms_660)
    n_sep = np.cross(lms_neutral, dl.lms_confusion_axis(dl.Deficiency.TRITAN))
    if np.dot(n_sep, lms_485) < 0:
        n1, n2 = n2, n1
    h1 = dl.plane_projection_matrix(n1, dl.Deficiency.TRITAN)
    h2 = dl.plane_projection_matrix(n2, dl.Deficiency.TRITAN)
    to_lms, from_lms = model.LMS_from_linearRGB, model.linearRGB_from_LMS
    return from_lms @ h1 @ to_lms, from_lms @ h2 @ to_lms, to_lms.T @ n_sep


def brettel_tritan(lin, severity):
    """DaltonLens itself (float64 linear RGB entry point), dichromat blended by severity."""
    return BRETTEL._simulate_cvd_linear_rgb(np.array(lin, dtype=np.float64)[None, :, :], dl.Deficiency.TRITAN,
                                            severity)[0]


def colors():
    fixed = [[255, 0, 0], [0, 255, 0], [0, 0, 255], [255, 255, 0], [0, 255, 255], [255, 0, 255],
             [0, 0, 0], [255, 255, 255], [128, 128, 128], [64, 64, 64], [30, 60, 120], [200, 50, 80]]
    rng = np.random.default_rng(20261112)
    return np.array(fixed + [list(c) for c in rng.uniform(0.0, 255.0, (40, 3))], dtype=float)


def saturate_matrix(s):
    return np.array([[0.213 + 0.787 * s, 0.715 - 0.715 * s, 0.072 - 0.072 * s],
                     [0.213 - 0.213 * s, 0.715 + 0.285 * s, 0.072 - 0.072 * s],
                     [0.213 - 0.213 * s, 0.715 - 0.715 * s, 0.072 + 0.928 * s]])


def grayscale_matrix(a):
    a = 1.0 - min(a, 1.0)
    return np.array([[0.2126 + 0.7874 * a, 0.7152 - 0.7152 * a, 0.0722 - 0.0722 * a],
                     [0.2126 - 0.2126 * a, 0.7152 + 0.2848 * a, 0.0722 - 0.0722 * a],
                     [0.2126 - 0.2126 * a, 0.7152 - 0.7152 * a, 0.0722 + 0.9278 * a]])


def sepia_matrix(a):
    a = 1.0 - min(a, 1.0)
    return np.array([[0.393 + 0.607 * a, 0.769 - 0.769 * a, 0.189 - 0.189 * a],
                     [0.349 - 0.349 * a, 0.686 + 0.314 * a, 0.168 - 0.168 * a],
                     [0.272 - 0.272 * a, 0.534 - 0.534 * a, 0.131 + 0.869 * a]])


def hue_rotate_matrix(deg):
    c, s = np.cos(np.radians(deg)), np.sin(np.radians(deg))
    base = np.array([[0.213, 0.715, 0.072]] * 3)
    cm = np.array([[0.787, -0.715, -0.072], [-0.213, 0.285, -0.072], [-0.213, -0.715, 0.928]])
    sm = np.array([[-0.213, -0.715, 0.928], [0.143, 0.140, -0.283], [-0.787, 0.715, 0.072]])
    return base + c * cm + s * sm


def apply_rgb(m, rgb, offset=0.0):
    return np.clip(rgb @ m.T + offset, 0.0, 1.0)


def css_filters(C):
    """{name [(amount, rows)]}, rows [r g b a] on 0-255."""
    rgb = C / 255.0
    alpha = np.ones(len(C))

    def rows(out_rgb, out_a=alpha):
        return [list(255.0 * r) + [255.0 * a] for r, a in zip(out_rgb, out_a)]

    out = {}
    out["sepia"] = [(a, rows(apply_rgb(sepia_matrix(a), rgb))) for a in AMOUNTS]
    out["grayscale"] = [(a, rows(apply_rgb(grayscale_matrix(a), rgb))) for a in AMOUNTS]
    out["saturate"] = [(a, rows(apply_rgb(saturate_matrix(a), rgb))) for a in AMOUNTS]
    out["hue-rotate"] = [(d, rows(apply_rgb(hue_rotate_matrix(d), rgb))) for d in ANGLES]
    out["contrast"] = [(a, rows(np.clip(a * rgb + (0.5 - 0.5 * a), 0.0, 1.0))) for a in AMOUNTS]
    out["brightness"] = [(a, rows(np.clip(a * rgb, 0.0, 1.0))) for a in AMOUNTS]
    out["invert"] = [(a, rows(np.clip(min(a, 1.0) + rgb * (1.0 - 2.0 * min(a, 1.0)), 0.0, 1.0))) for a in AMOUNTS]
    out["opacity"] = [(a, rows(rgb, alpha * min(a, 1.0))) for a in AMOUNTS]
    return out


def chroma(C):
    payload = json.dumps({"colors": C.tolist(), "amounts": CHROMA_AMOUNTS})
    res = subprocess.run(["node", "utils/color/chroma_oracle.mjs"], input=payload, capture_output=True, text=True,
                         check=True)
    return json.loads(res.stdout)


def encode_rows(lin):
    enc = 255.0 * colour.cctf_encoding(np.clip(lin, 0.0, 1.0), function="sRGB")
    return [list(r) + [255.0] for r in enc]


def machado(lin, name, severity):
    return lin @ colour.blindness.matrix_cvd_Machado2009(name, severity).T


def cvd(C):
    lin = colour.cctf_decoding(C / 255.0, function="sRGB")
    out = {k: [(s, encode_rows(machado(lin, name, s))) for s in SEVERITIES] for k, name in DEFICIENCIES.items()}
    out["tritanomaly"] = [(s, encode_rows(brettel_tritan(lin, s))) for s in SEVERITIES]
    return out


def cvd_vars(C):
    lin = colour.cctf_decoding(C / 255.0, function="sRGB")
    gray = np.repeat((lin @ Y_ROW)[:, None], 3, axis=1)
    return {"protanopia": encode_rows(machado(lin, "Protanomaly", 1.0)),
            "deuteranopia": encode_rows(machado(lin, "Deuteranomaly", 1.0)),
            "protanomaly": encode_rows(machado(lin, "Protanomaly", 0.6)),
            "deuteranomaly": encode_rows(machado(lin, "Deuteranomaly", 0.6)),
            "tritanopia": encode_rows(brettel_tritan(lin, 1.0)),
            "tritanomaly": encode_rows(brettel_tritan(lin, 0.6)),
            "achromatopsia": encode_rows(gray),
            "achromatomaly": encode_rows(0.5 * lin + 0.5 * gray)}


def num(x):
    x = float(x)
    return f"{int(x)}.0" if x == int(x) and abs(x) < 1e15 else repr(x)


def vec(v):
    return "[" + " ".join(num(x) for x in v) + "]"


def table(entries):
    return "[" + "\n   ".join(f"[{num(a)} [" + " ".join(vec(r) for r in rows) + "]]" for a, rows in entries) + "]"


def main():
    C = colors()
    with open(CVD_TABLE, "w") as f:
        f.write(f";; generated by utils/color/gen_filters.py, colour {colour.__version__}\n")
        f.write(";; Machado, Oliveira, Fernandes (2009) CVD matrices (linear sRGB), severity 0 to 1 every 0.1\n{")
        f.write("\n ".join(f":{k} [" + "\n   ".join(vec(np.asarray(colour.blindness.CVD_MATRICES_MACHADO2010[name][s]).ravel())
                                                  for s in sorted(colour.blindness.CVD_MATRICES_MACHADO2010[name])) + "]"
                           for k, name in DEFICIENCIES.items()))
        f.write("}\n")
    w1, w2, n = brettel_tritan_rgb()
    with open(BRETTEL_TABLE, "w") as f:
        f.write(f";; generated by utils/color/gen_filters.py, daltonlens Simulator_Brettel1997\n")
        f.write(";; Brettel, Vienot, Mollon (1997) tritanopia in linear sRGB: wing2 where dot(separation, rgb) < 0\n")
        f.write("{:tritan {:wing1 " + vec(w1.ravel()) + "\n          :wing2 " + vec(w2.ravel())
                + "\n          :separation " + vec(n) + "}}\n")
    ch = chroma(C)
    out = [f";; generated by utils/color/gen_filters.py, colour {colour.__version__}, chroma-js 2.6.0, numpy {np.__version__}",
           "{:colors [" + " ".join(vec(c) for c in C) + "]",
           " :filters {" + "\n           ".join(f":{k} {table(v)}" for k, v in css_filters(C).items()) + "}",
           " :chroma-amounts " + vec(CHROMA_AMOUNTS),
           " :chroma {" + "\n          ".join(f":{op} [" + " ".join("[" + " ".join(vec(r) for r in per_color) + "]"
                                                                  for per_color in ch[op]) + "]"
                                             for op in ["brighten", "darken", "saturate", "desaturate"]) + "}",
           " :cvd {" + "\n       ".join(f":{k} {table(v)}" for k, v in cvd(C).items()) + "}",
           " :cvd-vars {" + "\n            ".join(f":{k} [" + " ".join(vec(r) for r in rows) + "]"
                                                 for k, rows in cvd_vars(C).items()) + "}}"]
    with open(OUT, "w") as f:
        f.write("\n".join(out) + "\n")


if __name__ == "__main__":
    main()
