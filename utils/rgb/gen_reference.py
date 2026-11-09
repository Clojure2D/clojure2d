"""Generate test/resources/rgb/colour_reference.edn (oracle: colour-science).

Run from the repository root:

    /home/ts/penv/.venv/bin/python -I utils/rgb/gen_reference.py

Sections (clojure2d conventions: linear and encoded values in 0-1):

* :transfer  {name [[x encoded decoded] ...]}: encoding (cctf_encoding / OETF / log encoding) of x
  taken as a linear value and decoding of x taken as an encoded value. Where colour deviates from
  the standard, the standard is used:
    - BT.2020: CONSTANTS_BT2020_PRECISE (colour default uses the rounded 10-bit constants)
    - ACEScct decoding: capped at 65504 above (log2(65504) + 9.72) / 17.52 (ACES documentation)
* :spaces  {key {:red [x y] :green [x y] :blue [x y] :whitepoint wp-key
                 :rgb-to-xyz [9] :xyz-to-rgb [9] :encoded [...] :decoded [...]}}: colour
  RGB_COLOURSPACES primaries; matrices from colour normalised_primary_matrix with the clojure2d
  white point (xy read from resources/color/whitepoints.edn); cctf of colour (standard where colour
  deviates, as in :transfer) on GRID
* :builders  {:samples [[r g b] ...] (0-255), :rgb->xyz {space [[X Y Z] ...]} (0-100),
              :rgb->rgb {[source target] [[r g b] ...]} (0-255, Bradford)}: colour RGB_to_XYZ / RGB_to_RGB
  with colourspaces built from the :spaces data (clojure2d white points, derived matrices)
"""

import re
import warnings

import numpy as np
import colour
from colour.models import (eotf_inverse_sRGB, eotf_sRGB, gamma_function, log_encoding_ACEScc,
                           log_decoding_ACEScc, log_encoding_ACEScct, log_decoding_ACEScct,
                           oetf_BlackmagicFilmGeneration5, oetf_inverse_BlackmagicFilmGeneration5,
                           oetf_BT2020, oetf_inverse_BT2020)
from colour.models import (oetf_BT709, oetf_inverse_BT709, cctf_encoding_ROMMRGB,
                           cctf_decoding_ROMMRGB, oetf_SMPTE240M, eotf_SMPTE240M, eotf_BT1886,
                           eotf_inverse_BT1886, eotf_ST2084, eotf_inverse_ST2084, oetf_BT2100_HLG,
                           oetf_inverse_BT2100_HLG)
from colour.models.rgb.transfer_functions.itur_bt_2020 import CONSTANTS_BT2020_PRECISE

warnings.filterwarnings("ignore")

OUT = "test/resources/rgb/colour_reference.edn"
HALF_MAX = 65504.0
ACESCCT_MAX = (np.log2(HALF_MAX) + 9.72) / 17.52

# sRGB decoding threshold: clojure2d 0.040449936 (segment intersection) vs IEC 0.04045; the grid avoids
# (0.040449936, 0.04045], tested separately
GRID = sorted(set([-0.5, -0.1, -0.01, -1e-4, 0.0, 1e-6, 1e-5, 3.0517578125e-5, 1e-4, 1e-3, 0.0031308,
                   0.004, 0.005, 0.0078125, 0.01, 0.018, 0.018053968510807, 0.02, 0.04, 0.0405, 0.05,
                   0.08, 0.0812428582986, 0.1, 0.13388378308667, 0.155251141552511, 0.18, 0.25, 0.3,
                   0.35, 0.5, 0.75, 0.9, 1.0, 1.2, 1.4, 1.46, 1.5, 2.0, 10.0, 100.0, 1000.0]
                  + list(np.round(np.linspace(0.0, 1.0, 41), 10))))


def ecirgb():
    cs = colour.RGB_COLOURSPACES["ECI RGB v2"]
    return cs.cctf_encoding, cs.cctf_decoding


def acescct_decode(x):
    return HALF_MAX if x >= ACESCCT_MAX else log_decoding_ACEScct(x)


L_ENC, L_DEC = ecirgb()
TRANSFER = {
    "sRGB": (eotf_inverse_sRGB, eotf_sRGB),
    "gamma-2.2": (lambda x: gamma_function(x, 1 / 2.2, "Mirror"), lambda x: gamma_function(x, 2.2, "Mirror")),
    "gamma-1.8": (lambda x: gamma_function(x, 1 / 1.8, "Mirror"), lambda x: gamma_function(x, 1.8, "Mirror")),
    "L*": (L_ENC, L_DEC),
    "ACEScc": (log_encoding_ACEScc, log_decoding_ACEScc),
    "ACEScct": (log_encoding_ACEScct, acescct_decode),
    "Blackmagic-WG": (oetf_BlackmagicFilmGeneration5, oetf_inverse_BlackmagicFilmGeneration5),
    "BT2020": (lambda x: oetf_BT2020(x, constants=CONSTANTS_BT2020_PRECISE),
               lambda x: oetf_inverse_BT2020(x, constants=CONSTANTS_BT2020_PRECISE)),
    "BT709": (oetf_BT709, oetf_inverse_BT709),
    "ROMM": (cctf_encoding_ROMMRGB, cctf_decoding_ROMMRGB),
    "SMPTE240M": (oetf_SMPTE240M, eotf_SMPTE240M),
    "BT1886": (eotf_inverse_BT1886, eotf_BT1886),
    # display white 100, black 0.1 (luminance, not normalised)
    "BT1886-100-0.1": (lambda x: eotf_inverse_BT1886(x, L_B=0.1, L_W=100.0),
                       lambda x: eotf_BT1886(x, L_B=0.1, L_W=100.0)),
    # clojure2d PQ is normalised: linear 1.0 = 10000 cd/m2
    "PQ": (lambda x: eotf_inverse_ST2084(10000.0 * x), lambda x: eotf_ST2084(x) / 10000.0),
    "HLG": (oetf_BT2100_HLG, oetf_inverse_BT2100_HLG),
    "gamma-2.6": (lambda x: gamma_function(x, 1 / 2.6, "Mirror"), lambda x: gamma_function(x, 2.6, "Mirror")),
}


def num(x):
    x = float(x)
    if np.isnan(x):
        return "##NaN"
    if np.isinf(x):
        return "##Inf" if x > 0 else "##-Inf"
    return repr(x)


def records(rows):
    return "[" + "\n  ".join("[" + " ".join(num(v) for v in r) + "]" for r in rows) + "]"


def transfer():
    out = []
    for k, (enc, dec) in TRANSFER.items():
        out.append(f':{k} {records([[x, enc(x), dec(x)] for x in GRID])}')
    return "{" + "\n  ".join(out) + "}"


# clojure2d key -> (colour name, clojure2d white point var, transfer key of TRANSFER or None for linear)
SPACES = {
    "ACES2065-1": ("ACES2065-1", "CIE-2-ACES", None),
    "ACEScg": ("ACEScg", "CIE-2-ACES", None),
    "ACEScc": ("ACEScc", "CIE-2-ACES", "ACEScc"),
    "ACEScct": ("ACEScct", "CIE-2-ACES", "ACEScct"),
    "Adobe-RGB-1998": ("Adobe RGB (1998)", "CIE-2-D65", "gamma-563/256"),
    "Adobe-Wide-Gamut-RGB": ("Adobe Wide Gamut RGB", "CIE-2-D50", "gamma-563/256"),
    "Apple-RGB": ("Apple RGB", "CIE-2-D65", "gamma-1.8"),
    "Best-RGB": ("Best RGB", "CIE-2-D50", "gamma-2.2"),
    "Beta-RGB": ("Beta RGB", "CIE-2-D50", "gamma-2.2"),
    "Blackmagic-Wide-Gamut": ("Blackmagic Wide Gamut", "CIE-2-BLACKMAGIC-WG", "Blackmagic-WG"),
    "Canon-Cinema-Gamut": ("Cinema Gamut", "CIE-2-D65", None),
    "CIE-RGB": ("CIE RGB", "CIE-2-E", "gamma-2.2"),
    "ColorMatch-RGB": ("ColorMatch RGB", "CIE-2-D50", "gamma-1.8"),
    "Don-RGB": ("Don RGB 4", "CIE-2-D50", "gamma-2.2"),
    "Ekta-Space-PS-5": ("Ekta Space PS 5", "CIE-2-D50", "gamma-2.2"),
    "ECI-RGB-v2": ("ECI RGB v2", "CIE-2-D50", "L*"),
    "ITU-R-BT2020": ("ITU-R BT.2020", "CIE-2-D65", "BT2020"),
    # ITU-R BT.470 System M assumed display gamma is 2.2 (ITU-T H.264 Table E-4, ITU-R BT.2380);
    # colour uses 2.8, kept as :NTSC-colour
    "NTSC": ("NTSC (1953)", "CIE-2-C", "gamma-2.2"),
    "NTSC-colour": ("NTSC (1953)", "CIE-2-C", "gamma-2.8"),
    "PAL-SECAM": ("Pal/Secam", "CIE-2-D65", "gamma-2.8"),
    "sRGB": ("sRGB", "CIE-2-D65", "sRGB"),
    "SMPTE-C": ("SMPTE C", "CIE-2-D65", "gamma-2.2"),
    "Xtreme-RGB": ("Xtreme RGB", "CIE-2-D50", "gamma-2.2"),
    "ITU-R-BT709": ("ITU-R BT.709", "CIE-2-D65", "BT709"),
    "Display-P3": ("Display P3", "CIE-2-D65", "sRGB"),
    "P3-D65": ("P3-D65", "CIE-2-D65", "gamma-2.6"),
    "DCI-P3": ("DCI-P3", "CIE-2-DCI-P3", "gamma-2.6"),
    "SMPTE-240M": ("SMPTE 240M", "CIE-2-D65", "SMPTE240M"),
    "ProPhoto-RGB": ("ProPhoto RGB", "CIE-2-D50", "ROMM"),
}

SPACE_TRANSFER = dict(TRANSFER)
SPACE_TRANSFER["gamma-2.8"] = (lambda x: gamma_function(x, 1 / 2.8, "Mirror"),
                               lambda x: gamma_function(x, 2.8, "Mirror"))
SPACE_TRANSFER["gamma-563/256"] = (lambda x: gamma_function(x, 256 / 563, "Mirror"),
                                   lambda x: gamma_function(x, 563 / 256, "Mirror"))


def whitepoints():
    with open("resources/color/whitepoints.edn") as f:
        return {k: [float(v) for v in vs.split()]
                for k, vs in re.findall(r":(\S+) \[([^\]]+)\]", f.read())}


def flat(m):
    return " ".join(num(v) for v in np.asarray(m).ravel())


def spaces():
    wps = whitepoints()
    out = []
    for k, (name, wp, tf) in SPACES.items():
        cs = colour.RGB_COLOURSPACES[name]
        npm = colour.normalised_primary_matrix(cs.primaries, np.array(wps[wp][2:]))
        enc, dec = SPACE_TRANSFER[tf] if tf else (lambda x: x, lambda x: x)
        r, g, b = cs.primaries
        out.append(f":{k} {{:red [{num(r[0])} {num(r[1])}] :green [{num(g[0])} {num(g[1])}]"
                   f" :blue [{num(b[0])} {num(b[1])}] :whitepoint :{wp}"
                   f" :rgb-to-xyz [{flat(npm)}] :xyz-to-rgb [{flat(np.linalg.inv(npm))}]"
                   f" :encoded [{' '.join(num(enc(x)) for x in GRID)}]"
                   f" :decoded [{' '.join(num(dec(x)) for x in GRID)}]}}")
    return "{" + "\n  ".join(out) + "}"


SAMPLES_SEED = 20261109
RGB_PAIRS = [("sRGB", "Adobe-RGB-1998"), ("sRGB", "ECI-RGB-v2"), ("sRGB", "ACEScg"),
             ("Adobe-Wide-Gamut-RGB", "ITU-R-BT2020"), ("ACEScct", "sRGB"), ("NTSC", "PAL-SECAM"),
             ("sRGB", "ITU-R-BT709"), ("sRGB", "Display-P3"), ("Display-P3", "P3-D65"),
             ("sRGB", "DCI-P3"), ("sRGB", "SMPTE-240M"), ("sRGB", "ProPhoto-RGB")]


def colourspace(k, wps):
    name, wp, tf = SPACES[k]
    cs = colour.RGB_COLOURSPACES[name]
    enc, dec = SPACE_TRANSFER[tf] if tf else (lambda x: x, lambda x: x)
    return colour.RGB_Colourspace(k, cs.primaries, np.array(wps[wp][2:]), wp,
                                  cctf_encoding=np.vectorize(enc), cctf_decoding=np.vectorize(dec),
                                  use_derived_matrix_RGB_to_XYZ=True,
                                  use_derived_matrix_XYZ_to_RGB=True)


def builders():
    wps = whitepoints()
    rng = np.random.default_rng(SAMPLES_SEED)
    # channels >= 5 avoid the infinite slope of power-law encodings at 0
    samples = [[255.0, 255.0, 255.0], [128.0, 128.0, 128.0], [255.0, 5.0, 5.0], [5.0, 255.0, 5.0],
               [5.0, 5.0, 255.0]] + rng.uniform(5.0, 250.0, (25, 3)).tolist()
    css = {k: colourspace(k, wps) for k in SPACES}
    rgb2xyz = [(f":{k}", records([100.0 * colour.RGB_to_XYZ(np.array(s) / 255.0, css[k], None, None,
                                                             apply_cctf_decoding=True)
                                  for s in samples]))
               for k in SPACES]
    rgb2rgb = [(f"[:{a} :{b}]", records([255.0 * colour.RGB_to_RGB(np.array(s) / 255.0, css[a], css[b],
                                                                    "Bradford", apply_cctf_decoding=True,
                                                                    apply_cctf_encoding=True)
                                         for s in samples]))
               for a, b in RGB_PAIRS]
    kv = lambda es: "{" + "\n   ".join(f"{k} {v}" for k, v in es) + "}"
    return (f"{{:samples {records(samples)}\n  :rgb->xyz {kv(rgb2xyz)}\n  :rgb->rgb {kv(rgb2rgb)}}}")


def main():
    with open(OUT, "w") as f:
        f.write(f";; generated by utils/rgb/gen_reference.py, colour {colour.__version__}\n")
        f.write("{:grid [" + " ".join(num(x) for x in GRID) + "]\n :transfer\n " + transfer()
                + "\n :spaces\n " + spaces() + "\n :builders\n " + builders() + "}\n")


if __name__ == "__main__":
    main()
