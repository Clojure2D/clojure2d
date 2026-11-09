"""Generate test/resources/color/colorspaces_reference.edn (oracle for the colorspace conversions).

Run from the repository root:

    /home/ts/penv/.venv/bin/python -I utils/color/gen_colorspaces.py

Samples (:samples, 0-255): primaries, secondaries, black, white, grays, a few fixed colors and 300 random
colors (numpy default_rng, seed 20261115).

Oracles ({space [[c1 c2 c3] ...]} in :oracle, raw clojure2d ranges):

* XYZ: sRGB decoding (colour cctf_decoding sRGB) and SRGB_TO_XYZ, colour's normalised_primary_matrix of
  the sRGB primaries with the white point CIE-2-D65 read from resources/color/whitepoints.edn (as
  clojure2d to-XYZ); XYZ itself is in the oracle
* colour-science on that XYZ: LAB, LCH, LUV, LCHuv, Yxy and UVW (black: white chromaticity, Lindbloom), UCS,
  HunterLAB (Ka, Kb from the white point by formula; black gives 0, 0, 0 as in clojure2d), IPT, IgPgTg, JAB / JCH (XYZ x 100 cd/m2), OSA
  (L shifted by (14.4 - 14.3993) / sqrt(2): clojure2d uses colorio's 14.3993), DIN99 (ASTMD2244-07),
  DIN99b, DIN99c, DIN99d
* colour-science on encoded sRGB: HCL (Sarifuddin, hue in degrees), YCgCo (channels reordered Y Cg Co)
* coloraide 8.14: HSV, HSL, HSI, HWB, Cubehelix, DIN99o (coloraide's D65 white: tested at 1e-2), Okhsl, Okhsv (hue / 360), CMY (x 255), Oklab, Oklch, XYB (raw B = coloraide b + y)
* matrices: YDbDr (SECAM), OHTA (Ohta 1980), LMS (Hunt-Pointer-Estevez normalized to D65, on XYZ 0-100),
  YUV and YIQ (BT.601 luma, clojure2d chroma rows)
* colour-science YCbCr (full range, no offset): YCbCr with BT.601 weights, YPbPr with BT.709 weights
* RYB: Python port of Arah J. Leonard's rgb2ryb (npm rgb2ryb 0.0.4)

:chromatic {space [bool ...]}: rows where the hue is defined, for spaces with hue: channel spread above
1e-6 (HSx, Cubehelix, HCL), chroma above 0.01 (LCH, LCHuv, JCH, Oklch).
"""

import math
import re
import warnings

import numpy as np
import colour
import coloraide
from coloraide.everything import ColorAll as Color

warnings.filterwarnings("ignore")

OUT = "test/resources/color/colorspaces_reference.edn"
HPE_D65 = np.array([[0.40024, 0.7076, -0.08081], [-0.2263, 1.16532, 0.0457], [0.0, 0.0, 0.91822]])


def whitepoint():
    txt = open("resources/color/whitepoints.edn").read()
    xn, zn, x, y = (float(v) for v in re.search(r":CIE-2-D65 \[([^\]]+)\]", txt).group(1).split())
    return np.array([xn, 1.0, zn]), np.array([x, y])


WP_XYZ, WP_XY = whitepoint()
# sRGB primaries with the CIE-2-D65 white (colour normalised_primary_matrix), as clojure2d to-XYZ
SRGB_TO_XYZ = colour.normalised_primary_matrix(np.array([0.64, 0.33, 0.30, 0.60, 0.15, 0.06]), WP_XY)


def samples():
    rng = np.random.default_rng(20261115)
    fixed = [[255, 0, 0], [0, 255, 0], [0, 0, 255], [255, 255, 0], [0, 255, 255], [255, 0, 255],
             [0, 0, 0], [255, 255, 255], [128, 128, 128], [1, 1, 1], [254, 254, 254], [18, 7, 4],
             [250, 128, 10], [10, 200, 30], [134, 12, 210]]
    return np.array(fixed + [list(c) for c in rng.uniform(0.0, 255.0, (300, 3))], dtype=float)


def coloraide_coords(space, S):
    out = []
    for r, g, b in S:
        v = Color("srgb", [r, g, b]).convert(space).coords()
        out.append([0.0 if (isinstance(x, float) and math.isnan(x)) else x for x in v])
    return np.array(out)


def rgb2ryb(r, g, b):
    w = min(r, g, b); r -= w; g -= w; b -= w
    mg = max(r, g, b)
    y = min(r, g); r -= y; g -= y
    if b and g:
        b /= 2.0; g /= 2.0
    y += g; b += g
    my = max(r, y, b)
    if my:
        n = mg / my; r *= n; y *= n; b *= n
    return [r + w, y + w, b + w]


def oracle(RGB):
    S = RGB / 255.0
    lin = colour.cctf_decoding(S, function="sRGB")
    xyz = lin @ SRGB_TO_XYZ.T
    lab = colour.XYZ_to_Lab(xyz, WP_XY)
    luv = colour.XYZ_to_Luv(xyz, WP_XY)
    xy = colour.XYZ_to_xy(xyz)
    black = xyz[:, 1] == 0.0
    xy[black] = WP_XY
    jab = colour.XYZ_to_Jzazbz(xyz * 100.0)
    osa = colour.XYZ_to_OSA_UCS(xyz * 100.0)
    osa[:, 0] += (14.4 - 14.3993) / math.sqrt(2.0)
    hunter = colour.XYZ_to_Hunter_Lab(xyz * 100.0, WP_XYZ * 100.0,
                                      np.array([175.0 / 198.04 * (WP_XYZ[0] + 1.0) * 100.0,
                                                70.0 / 218.11 * (1.0 + WP_XYZ[2]) * 100.0]))
    hunter[black] = 0.0  # a, b divide by sqrt(Y); clojure2d returns 0 for Y = 0
    uvw = colour.XYZ_to_UVW(xyz * 100.0, WP_XY)
    uvw[black] = [0.0, 0.0, -17.0]  # black: white-point chromaticity (Lindbloom), U = V = 0
    hcl = colour.RGB_to_HCL(RGB) * np.array([180.0 / math.pi, 1.0, 1.0])
    yco = colour.RGB_to_YCoCg(RGB)
    def okh(space):
        v = coloraide_coords(space, S)
        v[:, 0] /= 360.0  # clojure2d hue 0-1
        return v
    xyb = coloraide_coords("xyb", S)
    xyb[:, 2] += xyb[:, 1]
    O = {
        "XYZ": xyz * 100.0, "XYZ1": xyz,
        "LAB": lab, "LCH": colour.Lab_to_LCHab(lab), "LUV": luv, "LCHuv": colour.Luv_to_LCHuv(luv),
        "Yxy": np.column_stack([xyz[:, 1] * 100.0, xy]), "UCS": colour.XYZ_to_UCS(xyz),
        "UVW": uvw, "HunterLAB": hunter,
        "IPT": colour.XYZ_to_IPT(xyz), "IgPgTg": colour.XYZ_to_IgPgTg(xyz),
        "JAB": jab, "JCH": colour.Jzazbz_to_JzCHab(jab), "OSA": osa,
        "DIN99": colour.XYZ_to_DIN99(xyz, WP_XY, method="ASTMD2244-07"),
        "DIN99b": colour.XYZ_to_DIN99(xyz, WP_XY, method="DIN99b"),
        "DIN99c": colour.XYZ_to_DIN99(xyz, WP_XY, method="DIN99c"),
        "DIN99d": colour.XYZ_to_DIN99(xyz, WP_XY, method="DIN99d"),
        "HCL": hcl, "YCgCo": yco[:, [0, 2, 1]],
        "HSV": coloraide_coords("hsv", S), "HSL": coloraide_coords("hsl", S), "HSI": coloraide_coords("hsi", S),
        "HWB": coloraide_coords("hwb", S), "Cubehelix": coloraide_coords("cubehelix", S),
        "CMY": coloraide_coords("cmy", S) * 255.0, "Oklab": coloraide_coords("oklab", S),
        "Oklch": coloraide_coords("oklch", S), "XYB": xyb,
        "YDbDr": RGB @ np.array([[0.299, 0.587, 0.114], [-0.450, -0.883, 1.333], [-1.333, 1.116, 0.217]]).T,
        "OHTA": RGB @ np.array([[1 / 3, 1 / 3, 1 / 3], [0.5, 0.0, -0.5], [-0.25, 0.5, -0.25]]).T,
        "LMS": (xyz * 100.0) @ HPE_D65.T,
        "RYB": np.array([rgb2ryb(*c) for c in RGB]),
        "DIN99o": coloraide_coords("din99o", S),
        "Okhsl": okh("okhsl"), "Okhsv": okh("okhsv"),
        "YCbCr": colour.RGB_to_YCbCr(S, K=colour.WEIGHTS_YCBCR["ITU-R BT.601"], out_legal=False, out_int=False) * 255.0,
        "YPbPr": colour.RGB_to_YCbCr(S, K=colour.WEIGHTS_YCBCR["ITU-R BT.709"], out_legal=False, out_int=False) * 255.0,
        "YUV": RGB @ np.array([[0.299, 0.587, 0.114], [-0.147, -0.289, 0.436], [0.615, -0.515, -0.1]]).T,
        "YIQ": RGB @ np.array([[0.299, 0.587, 0.114], [0.595716, -0.274453, -0.321263], [0.211456, -0.522591, 0.311135]]).T,
    }
    spread = RGB.max(axis=1) - RGB.min(axis=1)
    chromatic = {k: list(spread > 1e-6) for k in ["HSV", "HSL", "HSI", "HWB", "Cubehelix", "HCL", "Okhsl", "Okhsv"]}
    # polar spaces: hue only where chroma > 0.01 (grays carry rounding-noise hue)
    chromatic.update({"LCH": list(O["LCH"][:, 1] > 0.01), "LCHuv": list(O["LCHuv"][:, 1] > 0.01),
                      "JCH": list(O["JCH"][:, 1] > 0.01), "Oklch": list(O["Oklch"][:, 1] > 0.01)})
    return O, chromatic


def num(x):
    x = float(x)
    return f"{int(x)}.0" if x == int(x) and abs(x) < 1e15 else repr(x)


def rows(m):
    return "[" + " ".join("[" + " ".join(num(v) for v in r) + "]" for r in m) + "]"


def main():
    RGB = samples()
    O, chromatic = oracle(RGB)
    out = [f";; generated by utils/color/gen_colorspaces.py, colour {colour.__version__}, coloraide {coloraide.__version__}, numpy {np.__version__}",
           "{:samples " + rows(RGB),
           " :oracle {" + "\n          ".join(f":{k} {rows(v)}" for k, v in O.items()) + "}",
           " :chromatic {" + "\n             ".join(f":{k} [" + " ".join("true" if b else "false" for b in v) + "]"
                                                   for k, v in chromatic.items()) + "}}"]
    with open(OUT, "w") as f:
        f.write("\n".join(out) + "\n")


if __name__ == "__main__":
    main()
