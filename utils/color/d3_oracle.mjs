// d3-scale-chromatic oracle for utils/color/gen_gradients.py.
// Reads JSON {"ts": [...]} from stdin, prints
// {"api": {name [[r g b] per t]}, "unrounded": {name [[r g b] per t]}}:
// api - the d3-scale-chromatic interpolators (d3 formats colors as rgb() strings, rounded to integers);
// unrounded - the same cubehelix parameters converted by d3-color without rounding (interpolateCubehelixLong:
// h, s, l interpolated linearly, gamma 1; interpolateRainbow as in its source), not clamped.
import * as s from "d3-scale-chromatic";
import { rgb, cubehelix } from "d3-color";

const ENDS = {
  cubehelix: [[300, 0.5, 0.0], [-240, 0.5, 1.0]],
  warm: [[-100, 0.75, 0.35], [80, 1.5, 0.8]],
  cool: [[260, 0.75, 0.35], [80, 1.5, 0.8]],
};
const API = {
  cubehelix: s.interpolateCubehelixDefault,
  warm: s.interpolateWarm,
  cool: s.interpolateCool,
  rainbow: s.interpolateRainbow,
};

const triple = (c) => [c.r, c.g, c.b];
const lerp = (a, b, t) => a + t * (b - a);

function unrounded(name, t) {
  if (name === "rainbow") {
    const ts = Math.abs(t - 0.5);
    return triple(cubehelix(360 * t - 100, 1.5 - 1.5 * ts, 0.8 - 0.9 * ts).rgb());
  }
  const [a, b] = ENDS[name];
  return triple(cubehelix(lerp(a[0], b[0], t), lerp(a[1], b[1], t), lerp(a[2], b[2], t)).rgb());
}

let input = "";
process.stdin.on("data", (d) => (input += d));
process.stdin.on("end", () => {
  const { ts } = JSON.parse(input);
  const out = { api: {}, unrounded: {} };
  for (const [name, f] of Object.entries(API)) {
    out.api[name] = ts.map((t) => triple(rgb(f(t))));
    out.unrounded[name] = ts.map((t) => unrounded(name, t));
  }
  process.stdout.write(JSON.stringify(out));
});
