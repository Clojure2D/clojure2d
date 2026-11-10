// chroma.js oracle for utils/color/gen_filters.py and utils/color/gen_mixing.py.
// Reads JSON from stdin (sRGB 0-255), prints JSON with unrounded, clipped sRGB values (chroma .rgb(false)).
//
// gen_filters.py: {"colors": [[r g b] ...], "amounts": [...]} ->
//   {"brighten": [[[r g b] per amount] per color], "darken": ..., "saturate": ..., "desaturate": ...}
// gen_mixing.py: {"mixing": {"colors": [[r g b] ...], "pairs": [[i j] ...], "ts": [...], "sets": [[i ...] ...],
//                            "weights": [[w ...] ...]}} ->
//   {"mix": {mode [[[r g b] per t] per pair]}, "average": {mode [[r g b] per set]},
//    "weighted": [[r g b] per set]}, modes of mix: lrgb (default), rgb, lab, hsl; of average: rgb, lrgb, lab, hsl;
//   weighted: average in rgb with the weights.
// gen_gradients.py: {"scale": {"stops": [[[r g b] ...] ...], "modes": [...], "ts": [...]}} ->
//   {"scale": {mode [[[r g b] per t] per stop list]}} - chroma.scale(stops).mode(mode)(t).
import chroma from "chroma-js";

const MIX_MODES = ["lrgb", "rgb", "lab", "hsl"];
const AVERAGE_MODES = ["rgb", "lrgb", "lab", "hsl"];

function mixing({ colors, pairs, ts, sets, weights }) {
  const col = (i) => chroma(colors[i], "rgb");
  const mix = {};
  for (const mode of MIX_MODES) {
    mix[mode] = pairs.map(([i, j]) => ts.map((t) => chroma.mix(col(i), col(j), t, mode).rgb(false)));
  }
  const average = {};
  for (const mode of AVERAGE_MODES) {
    average[mode] = sets.map((s) => chroma.average(s.map(col), mode).rgb(false));
  }
  const weighted = sets.map((s, k) => chroma.average(s.map(col), "rgb", weights[k]).rgb(false));
  return { mix, average, weighted };
}

let input = "";
process.stdin.on("data", (d) => (input += d));
process.stdin.on("end", () => {
  const data = JSON.parse(input);
  if (data.scale) {
    const { stops, modes, ts } = data.scale;
    const scale = {};
    for (const mode of modes) {
      scale[mode] = stops.map((st) => {
        const f = chroma.scale(st.map((c) => chroma(c, "rgb"))).mode(mode);
        return ts.map((t) => f(t).rgb(false));
      });
    }
    process.stdout.write(JSON.stringify({ scale }));
    return;
  }
  if (data.mixing) {
    process.stdout.write(JSON.stringify(mixing(data.mixing)));
    return;
  }
  const { colors, amounts } = data;
  const out = {};
  for (const op of ["brighten", "darken", "saturate", "desaturate"]) {
    out[op] = colors.map((c) => amounts.map((a) => chroma(c, "rgb")[op](a).rgb(false)));
  }
  process.stdout.write(JSON.stringify(out));
});
