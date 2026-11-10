// paletton.com oracle for utils/color/gen_gradients.py.
// Usage: node utils/color/paletton_oracle.cjs <path to paletton.com js/app.compiled.js>  (JSON on stdin)
// Reads {"hues": [...], "angles": [...]} and prints
// {"presets": [...], "shades": {preset [[r g b] x 5 per hue]}, "schemes": {model [[compl sec1 sec2] per angle]}}.
// The bundle's AMD modules are captured with a stub `define` and the color modules are instantiated directly
// (color.class, color.presets, color.models.class; cyclic dependencies resolve to undefined, as unused there).
// Shades: color.class with the preset's [kS kV] pairs (paletton rounds the hue to integer degrees and RGB to
// integers). Schemes: models analog, triad, tetrad for hue 0 at the given angles (limited by paletton to [5, 175]).
"use strict";
const fs = require("fs");
const vm = require("vm");

const src = fs.readFileSync(process.argv[2], "utf8");
const mods = {};
const cache = {};
const busy = new Set();
const sandbox = {
  console, Math, setTimeout, clearTimeout,
  define: (n, d, f) => { if (typeof d === "function") { f = d; d = []; } mods[n] = { d, f }; },
  document: {}, navigator: { userAgent: "" }, $: () => ({}), requirejs: { config: () => {} }, require: () => {},
};
sandbox.window = sandbox;
vm.createContext(sandbox);
try { vm.runInContext(src, sandbox); } catch (e) { /* UI code needing a browser is never instantiated */ }

function req(n) {
  if (n in cache) return cache[n];
  if (busy.has(n)) return undefined;
  busy.add(n);
  const m = mods[n];
  if (!m) throw new Error("missing module " + n);
  return (cache[n] = m.f.apply(null, m.d.map(req)));
}

let input = "";
process.stdin.on("data", (d) => (input += d));
process.stdin.on("end", () => {
  const { hues, angles } = JSON.parse(input);
  const Col = req("color.class");
  const presets = req("color.presets").presetList;
  const models = req("color.models.class");
  const shade = (h, ks, kv) => { const c = new Col(h); c.setSV(ks, kv); return [c.rgb.r, c.rgb.g, c.rgb.b]; };
  const names = Object.keys(presets).sort();
  const shades = {};
  for (const p of names) shades[p] = hues.map((h) => presets[p].val.map(([ks, kv]) => shade(h, ks, kv)));
  const schemes = {};
  for (const m of ["analog", "triad", "tetrad"]) {
    schemes[m] = angles.map((a) => [models[m].getComplement(0), models[m].getSec1(0, a), models[m].getSec2(0, a)]);
  }
  process.stdout.write(JSON.stringify({ presets: names, shades, schemes }));
});
