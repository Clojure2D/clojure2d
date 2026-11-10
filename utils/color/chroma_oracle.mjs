// chroma.js oracle for utils/color/gen_filters.py.
// Reads JSON {"colors": [[r g b] ...], "amounts": [...]} from stdin (sRGB 0-255) and prints
// {"brighten": [[[r g b] per amount] per color], "darken": ..., "saturate": ..., "desaturate": ...}
// with unrounded, clipped sRGB values (chroma .rgb(false)).
import chroma from "chroma-js";

let input = "";
process.stdin.on("data", (d) => (input += d));
process.stdin.on("end", () => {
  const { colors, amounts } = JSON.parse(input);
  const out = {};
  for (const op of ["brighten", "darken", "saturate", "desaturate"]) {
    out[op] = colors.map((c) => amounts.map((a) => chroma(c, "rgb")[op](a).rgb(false)));
  }
  process.stdout.write(JSON.stringify(out));
});
