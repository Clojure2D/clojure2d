"""Generate test/resources/blend/blend_reference.edn (oracle for clojure2d.color.blend).

Run from the repository root:

    /home/ts/penv/.venv/bin/python -I utils/blend/gen_reference.py

Conventions: a = base (backdrop), b = blend (source), channel values in 0-255. Formulas are written on
the 0-1 scale from their published definitions and scaled by 255.

Points:

* :grid  integer subgrid (0, 15, ..., 255 plus values next to the branch points 0, 127.5, 255) and
  127.5, all pairs
* :random  2000 uniform non-integer pairs in [0, 255) (numpy default_rng, seed 20261109)

Sections ({mode [value ...]} in the order of :grid followed by :random):

* :formula  clamped per-channel modes from their source definitions (W3C Compositing Level 1,
  pegtop.net, Photoshop): 0/0 singular points take the zero-numerator value (as W3C color-dodge
  and color-burn), x/0 with x > 0 is infinite before clamping
* :modulus  the author's modulus family: unclamped base formula with the clojure2d guard
  max(1e-10, denominator), stored unwrapped; the test wraps with mod 256 and caps at 255
* :bitwise  logical modes on the channel values truncated to integers, low 8 bits
* :pillow  Pillow ImageChops on the integer points only ({mode [byte ...]} in the order of
  :pillow-points)
"""

import numpy as np
from PIL import Image, ImageChops, __version__ as PIL_VERSION

OUT = "test/resources/blend/blend_reference.edn"
EPSILON = 1.0e-10  # fastmath.core/EPSILON, the clojure2d division guard

GRID_VALUES = sorted(set(list(range(0, 256, 15)) + [1, 2, 64, 126, 127, 128, 129, 192, 253, 254]
                         + [127.5]))
GRID = [(float(a), float(b)) for a in GRID_VALUES for b in GRID_VALUES]
RANDOM = [tuple(p) for p in np.random.default_rng(20261109).uniform(0.0, 255.0, (2000, 2))]
POINTS = GRID + RANDOM
A = np.array([p[0] for p in POINTS])
B = np.array([p[1] for p in POINTS])
a = A / 255.0
b = B / 255.0


def div(n, d):
    """n / d with 0/0 -> 0 and x/0 -> inf (x > 0)."""
    with np.errstate(divide="ignore", invalid="ignore"):
        q = n / d
    return np.where(n == 0.0, 0.0, np.where(d == 0.0, np.inf, q))


def clamp(x):
    return np.clip(x, 0.0, 1.0) * 255.0


def multiply(x, y):
    return x * y


def screen(x, y):
    return x + y - x * y


def hardlight(cb, cs):
    # W3C: cs <= 0.5 multiply(cb, 2 cs), else screen(cb, 2 cs - 1)
    return np.where(cs <= 0.5, multiply(cb, 2.0 * cs), screen(cb, 2.0 * cs - 1.0))


def dodge(cb, cs):
    # W3C color-dodge: cb = 0 -> 0, cs = 1 -> 1, min(1, cb / (1 - cs))
    return np.minimum(1.0, div(cb, 1.0 - cs))


def burn(cb, cs):
    # W3C color-burn: cb = 1 -> 1, cs = 0 -> 0, 1 - min(1, (1 - cb) / cs)
    return 1.0 - np.minimum(1.0, div(1.0 - cb, cs))


FORMULA = {
    # W3C Compositing Level 1, separable modes
    "normal": b * 255.0,
    "multiply": multiply(a, b) * 255.0,
    "screen": screen(a, b) * 255.0,
    "overlay": hardlight(b, a) * 255.0,
    "hardlight": hardlight(a, b) * 255.0,
    "darken": np.minimum(a, b) * 255.0,
    "lighten": np.maximum(a, b) * 255.0,
    "difference": np.abs(a - b) * 255.0,
    "exclusion": (a + b - 2.0 * a * b) * 255.0,
    "dodge": clamp(dodge(a, b)),
    "burn": clamp(burn(a, b)),
    # Photoshop: linear dodge, subtract, linear burn, divide, vivid light, hard mix
    "add": clamp(a + b),
    "subtract": clamp(a - b),
    "linearburn": clamp(a + b - 1.0),
    "divide": clamp(div(a, b)),
    "vividlight": clamp(np.where(b < 0.5, 1.0 - div(1.0 - a, 2.0 * b), div(a, 2.0 * (1.0 - b)))),
    "hardmix": np.where(a + b >= 1.0, 255.0, 0.0),
    "linearlight": clamp(a + 2.0 * b - 1.0),
    "pinlight": np.where(b < 0.5, np.minimum(a, 2.0 * b), np.maximum(a, 2.0 * b - 1.0)) * 255.0,
    # Photoshop soft light as given by pegtop.net
    "softlight": np.where(b < 0.5, a - (1.0 - 2.0 * b) * a * (1.0 - a),
                          a + (2.0 * b - 1.0) * (np.sqrt(a) - a)) * 255.0,
    # pegtop.net
    "pegtoplight": ((1.0 - 2.0 * b) * a * a + 2.0 * a * b) * 255.0,
    "inversedodge": clamp(dodge(b, a)),
    "inverseburn": clamp(burn(b, a)),
    "softburn": clamp(np.where(a + b <= 1.0, div(0.5 * b, 1.0 - a), 1.0 - div(0.5 * (1.0 - a), b))),
    "softdodge": clamp(np.where(a + b <= 1.0, div(0.5 * a, 1.0 - b), 1.0 - div(0.5 * (1.0 - b), a))),
    "reflect": clamp(div(a * a, 1.0 - b)),
    "glow": clamp(div(b * b, 1.0 - a)),
    "freeze": clamp(1.0 - div((1.0 - a) ** 2, b)),
    "heat": clamp(1.0 - div((1.0 - b) ** 2, a)),
    "stamp": clamp(a + 2.0 * b - 1.0),
    "negation": (1.0 - np.abs(1.0 - a - b)) * 255.0,
    "average": (a + b) / 2.0 * 255.0,
}


def guard(d):
    return np.maximum(EPSILON, d)


# modulus family on the 0-255 scale, as defined by the author (guarded denominators)
MODULUS = {
    "madd": A + B,
    "msubtract": A - B,
    "mlinearburn": A + B - 255.0,
    "mstamp": A + 2.0 * B - 255.0,
    "mlinearlight": A + 2.0 * B - 255.0,
    "mdodge": 255.0 * A / guard(255.0 - B),
    "minversedodge": 255.0 * B / guard(255.0 - A),
    "msoftdodge": np.where(A + B <= 255.0, 127.5 * A / guard(255.0 - B),
                           255.0 - 127.5 * (255.0 - B) / guard(A)),
    "msoftburn": np.where(A + B <= 255.0, 127.5 * B / guard(255.0 - A),
                          255.0 - 127.5 * (255.0 - A) / guard(B)),
    "mburn": 255.0 * (1.0 - (255.0 - A) / guard(B)),
    "minverseburn": 255.0 * (1.0 - (255.0 - B) / guard(A)),
    "mdivide": 255.0 * A / guard(B),
    "mreflect": A * A / guard(255.0 - B),
    "mglow": B * B / guard(255.0 - A),
    "mfreeze": 255.0 - (255.0 - A) ** 2 / guard(B),
    "mheat": 255.0 - (255.0 - B) ** 2 / guard(A),
    "mvividlight": np.where(B < 127.5, 255.0 - 255.0 * (255.0 - A) / guard(2.0 * B),
                            255.0 * A / guard(2.0 * (255.0 - B))),
}

IA = A.astype(np.int64)
IB = B.astype(np.int64)
BITWISE = {
    "or": (IA | IB) & 0xFF,
    "and": (IA & IB) & 0xFF,
    "xor": (IA ^ IB) & 0xFF,
    "nor": ~(IA | IB) & 0xFF,
    "nand": ~(IA & IB) & 0xFF,
    "xnor": ~(IA ^ IB) & 0xFF,
}

PILLOW_POINTS = [(int(x), int(y)) for x, y in GRID if x == int(x) and y == int(y)]
PILLOW_OPS = {
    "add": lambda x, y: ImageChops.add(x, y),
    "subtract": lambda x, y: ImageChops.subtract(x, y),
    "madd": ImageChops.add_modulo,
    "msubtract": ImageChops.subtract_modulo,
    "darken": ImageChops.darker,
    "lighten": ImageChops.lighter,
    "difference": ImageChops.difference,
    "multiply": ImageChops.multiply,
    "screen": ImageChops.screen,
    "overlay": ImageChops.overlay,
    "hardlight": ImageChops.hard_light,
    "pegtoplight": ImageChops.soft_light,
}


def pillow(op):
    img_a = Image.frombytes("L", (len(PILLOW_POINTS), 1), bytes(p[0] for p in PILLOW_POINTS))
    img_b = Image.frombytes("L", (len(PILLOW_POINTS), 1), bytes(p[1] for p in PILLOW_POINTS))
    return list(op(img_a, img_b).tobytes())


def num(x):
    x = float(x)
    if x == np.inf:
        return "##Inf"
    if x == int(x) and abs(x) < 1e15:
        return f"{int(x)}.0"
    return repr(x)


def vec(xs):
    return "[" + " ".join(num(x) for x in xs) + "]"


def section(name, table, fmt=vec):
    lines = [f" :{name}"]
    entries = [f"  {{:{k} {fmt(v)}" if i == 0 else f"   :{k} {fmt(v)}" for i, (k, v) in enumerate(table.items())]
    return "\n".join(lines + entries) + "}"


def main():
    for k, v in FORMULA.items():
        assert np.all(np.isfinite(v)) and v.min() >= 0.0 and v.max() <= 255.0, k
    out = [";; generated by utils/blend/gen_reference.py, Pillow " + PIL_VERSION + ", numpy " + np.__version__,
           "{:grid " + vec(np.array(GRID).ravel()),
           " :random " + vec(np.array(RANDOM).ravel()),
           section("formula", FORMULA),
           section("modulus", MODULUS),
           section("bitwise", BITWISE, lambda v: "[" + " ".join(str(int(x)) for x in v) + "]"),
           " :pillow-points " + "[" + " ".join(f"{x} {y}" for x, y in PILLOW_POINTS) + "]",
           section("pillow", {k: pillow(op) for k, op in PILLOW_OPS.items()},
                   lambda v: "[" + " ".join(str(x) for x in v) + "]") + "}"]
    with open(OUT, "w") as f:
        f.write("\n".join(out) + "\n")


if __name__ == "__main__":
    main()
