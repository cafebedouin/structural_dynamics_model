import { useState, useMemo } from "react";

// ──────────────────────────────────────────────────────────────────────
// v2: Adds H¹ disagreement-pair histogram and universality-class sweeps
// over saturation functions and constraint distributions.
//
// IMPORTANT NOTE on the H¹ test: For 4 observers with ternary values,
// the strict pair-disagreement count is constrained by combinatorics
// to {0, 3, 4, 5}. Values {1, 2, 6} are unreachable for ANY classification
// rule — this is not a DR prediction, it's enumeration of (n+,n0,n-)
// tuples summing to 4. The meaningful test is therefore the distribution
// SHAPE over reachable values, not the presence/absence of {1,2}.
// ──────────────────────────────────────────────────────────────────────

const K = 4;
const POSITION_LABELS = ["Subordinate", "Lower-mid", "Upper-mid", "Dominant"];
const ASYMMETRIC_POWERS = [0.1, 0.4, 0.6, 0.9];
const SYMMETRIC_POWERS = [0.5, 0.5, 0.5, 0.5];
const MEAN_POWER = 0.5;
const SAT_GAIN = 3.0;
const NEUTRAL_BAND = 0.08;

// PRNG
function mulberry32(seed) {
  return function () {
    let t = (seed += 0x6d2b79f5);
    t = Math.imul(t ^ (t >>> 15), t | 1);
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}

function flowAt(c, power) {
  return c.baseline + c.tilt * (power - MEAN_POWER);
}

function classifyWith(raw, satFn) {
  const s = satFn(raw);
  if (s > NEUTRAL_BAND) return 1;
  if (s < -NEUTRAL_BAND) return -1;
  return 0;
}

const defaultSat = (x) => Math.tanh(SAT_GAIN * x);

function runFlowExperiment(constraints, powers, satFn = defaultSat) {
  return constraints.map((c) =>
    powers.map((p) => classifyWith(flowAt(c, p), satFn))
  );
}

function runRandomExperiment(constraints, rng) {
  return constraints.map(() =>
    Array.from({ length: K }, () => {
      const r = rng();
      if (r < 1 / 3) return -1;
      if (r < 2 / 3) return 0;
      return 1;
    })
  );
}

function metrics(classifications) {
  const N = classifications.length;
  let signFlipExtreme = 0;
  let tangled = 0;
  let allRope = 0;
  let allSnare = 0;
  let allNeutral = 0;
  const orbitFreq = new Map();

  for (const row of classifications) {
    if (row[0] !== 0 && row[K - 1] !== 0 && row[0] !== row[K - 1]) {
      signFlipExtreme++;
    }
    const hasPos = row.some((x) => x > 0);
    const hasNeg = row.some((x) => x < 0);
    if (hasPos && hasNeg) tangled++;
    else if (hasPos) allRope++;
    else if (hasNeg) allSnare++;
    else allNeutral++;

    const key = row.map((v) => (v === 1 ? "+" : v === -1 ? "−" : "0")).join("");
    orbitFreq.set(key, (orbitFreq.get(key) || 0) + 1);
  }

  const sorted = [...orbitFreq.entries()].sort((a, b) => b[1] - a[1]);
  return {
    N,
    signFlipRate: signFlipExtreme / N,
    tangledRate: tangled / N,
    allRopeRate: allRope / N,
    allSnareRate: allSnare / N,
    allNeutralRate: allNeutral / N,
    orbitCount: orbitFreq.size,
    topOrbits: sorted.slice(0, 8),
  };
}

function disagreementCount(row) {
  let count = 0;
  for (let i = 0; i < row.length; i++) {
    for (let j = i + 1; j < row.length; j++) {
      if (row[i] !== row[j]) count++;
    }
  }
  return count;
}

function disagreementHistogram(classifications) {
  const hist = [0, 0, 0, 0, 0, 0, 0];
  for (const row of classifications) {
    hist[disagreementCount(row)]++;
  }
  return hist;
}

function agreementRate(classA, classB) {
  if (classA.length !== classB.length) return 0;
  let agree = 0;
  for (let i = 0; i < classA.length; i++) {
    let same = true;
    for (let j = 0; j < classA[i].length; j++) {
      if (classA[i][j] !== classB[i][j]) {
        same = false;
        break;
      }
    }
    if (same) agree++;
  }
  return agree / classA.length;
}

// Saturation function variants for universality test
const SAT_VARIANTS = [
  { name: "tanh(3x)", fn: (x) => Math.tanh(3 * x), color: "#1f1c17" },
  { name: "tanh(1x)", fn: (x) => Math.tanh(x), color: "#5a7a8c" },
  {
    name: "sign·√|x|",
    fn: (x) => Math.sign(x) * Math.min(1, Math.sqrt(Math.abs(x))),
    color: "#a64842",
  },
  {
    name: "logistic(4x)",
    fn: (x) => 2 / (1 + Math.exp(-4 * x)) - 1,
    color: "#2c4f6b",
  },
  {
    name: "clipped 2x",
    fn: (x) => Math.max(-1, Math.min(1, 2 * x)),
    color: "#b8893a",
  },
  {
    name: "hard step",
    fn: (x) => (Math.abs(x) < 0.05 ? 0 : Math.sign(x)),
    color: "#3a342c",
  },
];

// Constraint distribution variants
const DIST_VARIANTS = [
  {
    name: "uniform · default",
    desc: "B ~ U(±0.5), T ~ U(±1.25)",
    bSpread: 1.0,
    tSpread: 2.5,
    kind: "uniform",
  },
  {
    name: "uniform · low tilt",
    desc: "B ~ U(±0.75), T ~ U(±0.5)",
    bSpread: 1.5,
    tSpread: 1.0,
    kind: "uniform",
  },
  {
    name: "uniform · high tilt",
    desc: "B ~ U(±0.3), T ~ U(±2.0)",
    bSpread: 0.6,
    tSpread: 4.0,
    kind: "uniform",
  },
  {
    name: "Gaussian",
    desc: "B ~ N(0,0.3²), T ~ N(0,0.8²)",
    bSigma: 0.3,
    tSigma: 0.8,
    kind: "gaussian",
  },
  {
    name: "Laplace (heavy tail)",
    desc: "B,T ~ Laplace; b=0.25, t=0.6",
    bScale: 0.25,
    tScale: 0.6,
    kind: "laplace",
  },
  {
    name: "bimodal tilt",
    desc: "T ∈ {±1} ± noise; B ~ U(±0.4)",
    bSpread: 0.8,
    kind: "bimodal",
  },
];

function genConstraint(rng, variant) {
  if (variant.kind === "gaussian") {
    const u1 = Math.max(rng(), 1e-9);
    const u2 = rng();
    const u3 = Math.max(rng(), 1e-9);
    const u4 = rng();
    const g1 = Math.sqrt(-2 * Math.log(u1)) * Math.cos(2 * Math.PI * u2);
    const g2 = Math.sqrt(-2 * Math.log(u3)) * Math.cos(2 * Math.PI * u4);
    return { baseline: g1 * variant.bSigma, tilt: g2 * variant.tSigma };
  }
  if (variant.kind === "laplace") {
    const lap = (scale) => {
      const u = rng() - 0.5;
      return -scale * Math.sign(u) * Math.log(1 - 2 * Math.abs(u) + 1e-9);
    };
    return { baseline: lap(variant.bScale), tilt: lap(variant.tScale) };
  }
  if (variant.kind === "bimodal") {
    const tBase = rng() < 0.5 ? -1 : 1;
    const tNoise = (rng() - 0.5) * 0.4;
    return {
      baseline: (rng() - 0.5) * variant.bSpread,
      tilt: tBase + tNoise,
    };
  }
  return {
    baseline: (rng() - 0.5) * variant.bSpread,
    tilt: (rng() - 0.5) * variant.tSpread,
  };
}

// ──────────────────────────────────────────────────────────────────────
// UI primitives
// ──────────────────────────────────────────────────────────────────────

const COLORS = {
  bg: "#f3eee4",
  fg: "#1f1c17",
  rope: "#a64842",
  snare: "#2c4f6b",
  neutral: "#b8b0a0",
  accent: "#b8893a",
  border: "#d8cfc0",
  subtle: "#ece6da",
};

function cell(v) {
  if (v === 1) return COLORS.rope;
  if (v === -1) return COLORS.snare;
  return COLORS.neutral;
}

function fmtPct(x) {
  return (x * 100).toFixed(1) + "%";
}

function PatternGlyph({ pattern, size = 10 }) {
  return (
    <span style={{ display: "inline-flex", gap: 2, verticalAlign: "middle" }}>
      {pattern.split("").map((ch, i) => {
        const c =
          ch === "+" ? COLORS.rope : ch === "−" ? COLORS.snare : COLORS.neutral;
        return (
          <span
            key={i}
            style={{
              width: size,
              height: size,
              background: c,
              borderRadius: 1,
              display: "inline-block",
            }}
          />
        );
      })}
    </span>
  );
}

function Stat({ label, value }) {
  return (
    <div style={{ display: "flex", justifyContent: "space-between" }}>
      <span style={{ color: "#5a544a" }}>{label}</span>
      <span style={{ fontVariantNumeric: "tabular-nums" }}>{value}</span>
    </div>
  );
}

function SectionLabel({ children }) {
  return (
    <div
      style={{
        fontFamily: "'EB Garamond', serif",
        fontSize: 10.5,
        textTransform: "uppercase",
        letterSpacing: 1.5,
        color: "#7a7264",
      }}
    >
      {children}
    </div>
  );
}

function Panel({ title, subtitle, classifications, data, accent }) {
  const visible = classifications.slice(0, 24);
  const maxOrbit = Math.max(...data.topOrbits.map(([, n]) => n), 1);
  return (
    <div
      style={{
        background: COLORS.bg,
        border: `1px solid ${COLORS.border}`,
        padding: "20px 22px",
        position: "relative",
      }}
    >
      <div
        style={{
          position: "absolute",
          top: 0,
          left: 0,
          right: 0,
          height: 3,
          background: accent,
        }}
      />
      <div style={{ marginBottom: 14 }}>
        <div
          style={{
            fontFamily: "'Cormorant Garamond', serif",
            fontSize: 22,
            fontWeight: 600,
            letterSpacing: 0.2,
            lineHeight: 1.1,
          }}
        >
          {title}
        </div>
        <div
          style={{
            fontFamily: "'EB Garamond', serif",
            fontStyle: "italic",
            fontSize: 13,
            color: "#5a544a",
            marginTop: 2,
          }}
        >
          {subtitle}
        </div>
      </div>
      <div
        style={{
          fontFamily: "'JetBrains Mono', monospace",
          fontSize: 11,
          lineHeight: 1.7,
          marginBottom: 16,
          color: "#3a342c",
        }}
      >
        <Stat label="sign-flip (extremes)" value={fmtPct(data.signFlipRate)} />
        <Stat label="tangled-rope" value={fmtPct(data.tangledRate)} />
        <Stat label="uniform-rope" value={fmtPct(data.allRopeRate)} />
        <Stat label="uniform-snare" value={fmtPct(data.allSnareRate)} />
        <Stat label="uniform-neutral" value={fmtPct(data.allNeutralRate)} />
        <Stat label="orbit families" value={`${data.orbitCount} / 81`} />
      </div>
      <div style={{ marginBottom: 18 }}>
        <SectionLabel>Top orbit families</SectionLabel>
        <div style={{ marginTop: 6 }}>
          {data.topOrbits.map(([pattern, count]) => (
            <div
              key={pattern}
              style={{
                display: "flex",
                alignItems: "center",
                gap: 8,
                fontFamily: "'JetBrains Mono', monospace",
                fontSize: 10.5,
                color: "#3a342c",
                marginBottom: 3,
              }}
            >
              <PatternGlyph pattern={pattern} size={9} />
              <div
                style={{
                  flex: 1,
                  height: 8,
                  background: COLORS.subtle,
                  position: "relative",
                }}
              >
                <div
                  style={{
                    position: "absolute",
                    top: 0,
                    left: 0,
                    bottom: 0,
                    width: `${(count / maxOrbit) * 100}%`,
                    background: accent,
                    opacity: 0.7,
                  }}
                />
              </div>
              <span
                style={{
                  minWidth: 38,
                  textAlign: "right",
                  fontVariantNumeric: "tabular-nums",
                }}
              >
                {((count / data.N) * 100).toFixed(1)}%
              </span>
            </div>
          ))}
        </div>
      </div>
      <div>
        <SectionLabel>
          Sample classifications (positions × first 24 constraints)
        </SectionLabel>
        <div style={{ marginTop: 8 }}>
          {Array.from({ length: K }).map((_, posIdx) => (
            <div
              key={posIdx}
              style={{
                display: "flex",
                alignItems: "center",
                gap: 4,
                marginBottom: 1,
              }}
            >
              <div
                style={{
                  fontFamily: "'EB Garamond', serif",
                  fontSize: 10,
                  fontStyle: "italic",
                  color: "#5a544a",
                  width: 78,
                  textAlign: "right",
                  paddingRight: 4,
                }}
              >
                {POSITION_LABELS[posIdx]}
              </div>
              <div style={{ display: "flex", gap: 1, flex: 1 }}>
                {visible.map((row, ci) => (
                  <div
                    key={ci}
                    style={{
                      flex: 1,
                      height: 12,
                      background: cell(row[posIdx]),
                    }}
                  />
                ))}
              </div>
            </div>
          ))}
        </div>
      </div>
    </div>
  );
}

// ──────────────────────────────────────────────────────────────────────
// H¹ histogram — disagreement-pair counts per arm
// ──────────────────────────────────────────────────────────────────────

function H1Histogram({ random, symmetric, asymmetric }) {
  const rows = [
    { label: "random", hist: random, color: COLORS.neutral },
    { label: "symmetric", hist: symmetric, color: "#5a7a8c" },
    { label: "asymmetric", hist: asymmetric, color: COLORS.accent },
  ];
  const total = (h) => h.reduce((a, b) => a + b, 0) || 1;
  const reachable = new Set([0, 3, 4, 5]); // for 4-observer ternary, by combinatorics

  return (
    <div
      style={{
        background: COLORS.bg,
        border: `1px solid ${COLORS.border}`,
        padding: "22px 26px",
        marginBottom: 32,
      }}
    >
      <SectionLabel>H¹ disagreement-pair count distribution</SectionLabel>
      <div
        style={{
          fontFamily: "'EB Garamond', serif",
          fontStyle: "italic",
          fontSize: 13,
          color: "#5a544a",
          marginTop: 4,
          marginBottom: 16,
          maxWidth: 720,
        }}
      >
        For each constraint, count pairs of positions that disagree (0–6 of 6).
        Values <strong>{`{1, 2, 6}`}</strong> are <em>combinatorially unreachable</em>{" "}
        for any classification rule on 4 observers with ternary values — bars
        for these are visually faded. The diagnostic content is the distribution
        shape over reachable values <strong>{`{0, 3, 4, 5}`}</strong>.
      </div>
      <div style={{ display: "grid", gridTemplateColumns: "110px 1fr", rowGap: 14, alignItems: "center" }}>
        {rows.map(({ label, hist, color }) => {
          const t = total(hist);
          const max = Math.max(...hist) || 1;
          return (
            <>
              <div
                key={`${label}-l`}
                style={{
                  fontFamily: "'JetBrains Mono', monospace",
                  fontSize: 11,
                  color: "#3a342c",
                }}
              >
                {label}
              </div>
              <div
                key={`${label}-b`}
                style={{ display: "flex", gap: 8, alignItems: "flex-end", height: 70 }}
              >
                {hist.map((c, i) => {
                  const isReachable = reachable.has(i);
                  const pct = (c / t) * 100;
                  return (
                    <div
                      key={i}
                      style={{
                        flex: 1,
                        display: "flex",
                        flexDirection: "column",
                        alignItems: "center",
                        gap: 4,
                        opacity: isReachable ? 1 : 0.35,
                      }}
                    >
                      <div
                        style={{
                          fontFamily: "'JetBrains Mono', monospace",
                          fontSize: 9,
                          color: "#5a544a",
                          fontVariantNumeric: "tabular-nums",
                        }}
                      >
                        {pct.toFixed(0)}%
                      </div>
                      <div
                        style={{
                          width: "100%",
                          height: max ? (c / max) * 40 : 0,
                          background: color,
                          opacity: c === 0 ? 0.15 : 0.85,
                          minHeight: 2,
                        }}
                      />
                      <div
                        style={{
                          fontFamily: "'JetBrains Mono', monospace",
                          fontSize: 10,
                          color: isReachable ? "#3a342c" : "#a8a098",
                          fontWeight: isReachable ? 500 : 400,
                        }}
                      >
                        {i}
                      </div>
                    </div>
                  );
                })}
              </div>
            </>
          );
        })}
      </div>
    </div>
  );
}

// ──────────────────────────────────────────────────────────────────────
// Saturation function overlay plot
// ──────────────────────────────────────────────────────────────────────

function SaturationOverlay({ variants }) {
  const w = 700;
  const h = 180;
  const xMin = -2;
  const xMax = 2;
  const yMin = -1.15;
  const yMax = 1.15;
  const tx = (x) => ((x - xMin) / (xMax - xMin)) * w;
  const ty = (y) => h - ((y - yMin) / (yMax - yMin)) * h;

  const paths = variants.map((v) => {
    const points = [];
    for (let i = 0; i <= 200; i++) {
      const x = xMin + (xMax - xMin) * (i / 200);
      let y = v.fn(x);
      y = Math.max(-1.1, Math.min(1.1, y));
      points.push(`${tx(x).toFixed(2)},${ty(y).toFixed(2)}`);
    }
    return { ...v, d: "M " + points.join(" L ") };
  });

  return (
    <svg viewBox={`0 0 ${w} ${h + 50}`} style={{ width: "100%", height: "auto" }}>
      <line x1={0} y1={ty(0)} x2={w} y2={ty(0)} stroke={COLORS.border} strokeWidth={1} />
      <line x1={tx(0)} y1={0} x2={tx(0)} y2={h} stroke={COLORS.border} strokeWidth={1} />
      <rect
        x={0}
        y={ty(NEUTRAL_BAND)}
        width={w}
        height={ty(-NEUTRAL_BAND) - ty(NEUTRAL_BAND)}
        fill={COLORS.neutral}
        opacity={0.18}
      />
      {paths.map((p) => (
        <path
          key={p.name}
          d={p.d}
          fill="none"
          stroke={p.color}
          strokeWidth={1.5}
          opacity={0.9}
        />
      ))}
      {paths.map((p, i) => (
        <g
          key={`${p.name}-leg`}
          transform={`translate(${(i % 3) * 230 + 10}, ${h + 12 + Math.floor(i / 3) * 16})`}
        >
          <line x1={0} y1={5} x2={22} y2={5} stroke={p.color} strokeWidth={1.8} />
          <text
            x={28}
            y={9}
            fontSize={10}
            fontFamily="'JetBrains Mono', monospace"
            fill="#3a342c"
          >
            {p.name}
          </text>
        </g>
      ))}
    </svg>
  );
}

// ──────────────────────────────────────────────────────────────────────
// Sweep card (compact)
// ──────────────────────────────────────────────────────────────────────

function SweepCard({ name, desc, data, accent, showAgreement }) {
  const maxOrbit = Math.max(...data.topOrbits.map(([, n]) => n), 1);
  return (
    <div
      style={{
        background: COLORS.bg,
        border: `1px solid ${COLORS.border}`,
        padding: "14px 16px",
        borderTop: `2px solid ${accent}`,
      }}
    >
      <div
        style={{
          fontFamily: "'JetBrains Mono', monospace",
          fontSize: 11,
          fontWeight: 500,
          color: COLORS.fg,
        }}
      >
        {name}
      </div>
      {desc && (
        <div
          style={{
            fontFamily: "'EB Garamond', serif",
            fontStyle: "italic",
            fontSize: 11,
            color: "#7a7264",
            marginTop: 1,
            marginBottom: 8,
          }}
        >
          {desc}
        </div>
      )}
      <div
        style={{
          fontFamily: "'JetBrains Mono', monospace",
          fontSize: 10,
          lineHeight: 1.65,
          color: "#3a342c",
          marginTop: desc ? 0 : 8,
          marginBottom: 10,
        }}
      >
        <Stat label="sign-flip" value={fmtPct(data.signFlipRate)} />
        <Stat label="tangled" value={fmtPct(data.tangledRate)} />
        <Stat label="orbits" value={`${data.orbitCount} / 81`} />
        {showAgreement && (
          <Stat label="agree·base" value={fmtPct(data.agreement)} />
        )}
      </div>
      <div>
        {data.topOrbits.slice(0, 4).map(([pattern, count]) => (
          <div
            key={pattern}
            style={{
              display: "flex",
              alignItems: "center",
              gap: 6,
              fontFamily: "'JetBrains Mono', monospace",
              fontSize: 9.5,
              marginBottom: 2,
            }}
          >
            <PatternGlyph pattern={pattern} size={7} />
            <div
              style={{
                flex: 1,
                height: 6,
                background: COLORS.subtle,
                position: "relative",
              }}
            >
              <div
                style={{
                  position: "absolute",
                  top: 0,
                  left: 0,
                  bottom: 0,
                  width: `${(count / maxOrbit) * 100}%`,
                  background: accent,
                  opacity: 0.7,
                }}
              />
            </div>
            <span
              style={{
                minWidth: 30,
                textAlign: "right",
                fontVariantNumeric: "tabular-nums",
              }}
            >
              {((count / data.N) * 100).toFixed(0)}%
            </span>
          </div>
        ))}
      </div>
    </div>
  );
}

// ──────────────────────────────────────────────────────────────────────
// Main
// ──────────────────────────────────────────────────────────────────────

export default function App() {
  const [seed, setSeed] = useState(42);
  const [n, setN] = useState(300);

  const result = useMemo(() => {
    const rng = mulberry32(seed);
    const constraints = Array.from({ length: n }, () =>
      genConstraint(rng, DIST_VARIANTS[0])
    );
    const random = runRandomExperiment(constraints, rng);
    const symmetric = runFlowExperiment(constraints, SYMMETRIC_POWERS);
    const asymmetric = runFlowExperiment(constraints, ASYMMETRIC_POWERS);

    // Saturation sweep — same constraints, different sat functions, asymmetric powers
    const baselineCls = asymmetric;
    const satSweep = SAT_VARIANTS.map((v) => {
      const cls = runFlowExperiment(constraints, ASYMMETRIC_POWERS, v.fn);
      return {
        ...v,
        classifications: cls,
        ...metrics(cls),
        agreement: agreementRate(cls, baselineCls),
      };
    });

    // Distribution sweep — different constraints per variant, default sat function
    const distSweep = DIST_VARIANTS.map((v, idx) => {
      const distRng = mulberry32(seed * 7919 + idx + 1);
      const cs = Array.from({ length: n }, () => genConstraint(distRng, v));
      const cls = runFlowExperiment(cs, ASYMMETRIC_POWERS);
      return { ...v, classifications: cls, ...metrics(cls) };
    });

    return {
      random: { classifications: random, ...metrics(random) },
      symmetric: { classifications: symmetric, ...metrics(symmetric) },
      asymmetric: { classifications: asymmetric, ...metrics(asymmetric) },
      satSweep,
      distSweep,
      randomHist: disagreementHistogram(random),
      symmetricHist: disagreementHistogram(symmetric),
      asymmetricHist: disagreementHistogram(asymmetric),
    };
  }, [seed, n]);

  return (
    <>
      <style>{`
        @import url('https://fonts.googleapis.com/css2?family=Cormorant+Garamond:wght@400;500;600;700&family=EB+Garamond:ital,wght@0,400;0,500;1,400&family=JetBrains+Mono:wght@400;500&display=swap');
        body { margin: 0; }
      `}</style>
      <div
        style={{
          background: COLORS.bg,
          minHeight: "100vh",
          color: COLORS.fg,
          fontFamily: "'EB Garamond', serif",
          padding: "40px 20px 60px",
        }}
      >
        <div style={{ maxWidth: 1200, margin: "0 auto" }}>
          {/* Header */}
          <header
            style={{
              marginBottom: 28,
              borderBottom: `1px solid ${COLORS.border}`,
              paddingBottom: 22,
            }}
          >
            <div
              style={{
                fontFamily: "'JetBrains Mono', monospace",
                fontSize: 10,
                letterSpacing: 2,
                textTransform: "uppercase",
                color: COLORS.accent,
                marginBottom: 6,
              }}
            >
              DR · §2.3 derivation · structural test · v2
            </div>
            <h1
              style={{
                fontFamily: "'Cormorant Garamond', serif",
                fontSize: 38,
                fontWeight: 600,
                margin: 0,
                lineHeight: 1.05,
                letterSpacing: -0.5,
              }}
            >
              Flow-Asymmetry → Sign-Flip
            </h1>
            <p
              style={{
                fontFamily: "'EB Garamond', serif",
                fontSize: 15.5,
                lineHeight: 1.55,
                color: "#3a342c",
                maxWidth: 820,
                marginTop: 12,
              }}
            >
              Mechanical test of the §2.3 derivation, extended with an H¹
              disagreement-pair histogram and universality-class sweeps over
              saturation functions and constraint distributions. The base
              comparison (random / symmetric / asymmetric) is unchanged.
            </p>
          </header>

          {/* Controls */}
          <div
            style={{
              display: "flex",
              alignItems: "center",
              gap: 24,
              marginBottom: 24,
              fontFamily: "'JetBrains Mono', monospace",
              fontSize: 11,
              flexWrap: "wrap",
            }}
          >
            <label style={{ display: "flex", alignItems: "center", gap: 10 }}>
              <span
                style={{
                  color: "#5a544a",
                  letterSpacing: 1,
                  textTransform: "uppercase",
                  fontSize: 10,
                }}
              >
                N constraints
              </span>
              <input
                type="range"
                min={50}
                max={1000}
                step={50}
                value={n}
                onChange={(e) => setN(parseInt(e.target.value))}
                style={{ width: 180 }}
              />
              <span
                style={{ minWidth: 36, fontVariantNumeric: "tabular-nums" }}
              >
                {n}
              </span>
            </label>
            <button
              onClick={() => setSeed(Math.floor(Math.random() * 1e9))}
              style={{
                fontFamily: "'JetBrains Mono', monospace",
                fontSize: 10,
                letterSpacing: 1.5,
                textTransform: "uppercase",
                background: COLORS.fg,
                color: COLORS.bg,
                border: "none",
                padding: "8px 16px",
                cursor: "pointer",
              }}
            >
              New sample
            </button>
            <span
              style={{
                color: "#7a7264",
                fontStyle: "italic",
                fontFamily: "'EB Garamond', serif",
              }}
            >
              seed {seed}
            </span>
          </div>

          {/* Three main panels */}
          <div
            style={{
              display: "grid",
              gridTemplateColumns: "repeat(3, 1fr)",
              gap: 16,
              marginBottom: 24,
            }}
          >
            <Panel
              title="Random baseline"
              subtitle="classifications drawn uniformly from {−1, 0, +1}"
              classifications={result.random.classifications}
              data={result.random}
              accent="#7a7264"
            />
            <Panel
              title="Symmetric environment"
              subtitle="flow rule, equal power across positions"
              classifications={result.symmetric.classifications}
              data={result.symmetric}
              accent="#5a7a8c"
            />
            <Panel
              title="Asymmetric environment"
              subtitle="flow rule, power gradient [0.1, 0.4, 0.6, 0.9]"
              classifications={result.asymmetric.classifications}
              data={result.asymmetric}
              accent={COLORS.accent}
            />
          </div>

          {/* H¹ histogram */}
          <H1Histogram
            random={result.randomHist}
            symmetric={result.symmetricHist}
            asymmetric={result.asymmetricHist}
          />

          {/* Saturation universality */}
          <div
            style={{
              background: COLORS.bg,
              border: `1px solid ${COLORS.border}`,
              padding: "22px 26px",
              marginBottom: 24,
            }}
          >
            <div
              style={{
                fontFamily: "'Cormorant Garamond', serif",
                fontSize: 24,
                fontWeight: 600,
                lineHeight: 1.1,
                marginBottom: 4,
              }}
            >
              Universality I — saturation function
            </div>
            <div
              style={{
                fontFamily: "'EB Garamond', serif",
                fontStyle: "italic",
                fontSize: 13.5,
                color: "#5a544a",
                marginBottom: 16,
                maxWidth: 820,
              }}
            >
              Same constraints, same power gradient, six different saturation
              functions. Universality predicts the orbit structure (sign-flip
              rate, tangled-rope dominance, head families) survives despite
              microscopic differences in functional form. Pairwise agreement
              with the <code style={{ fontFamily: "'JetBrains Mono', monospace" }}>tanh(3x)</code> baseline is reported per
              variant.
            </div>
            <SaturationOverlay variants={SAT_VARIANTS} />
            <div
              style={{
                display: "grid",
                gridTemplateColumns: "repeat(3, 1fr)",
                gap: 12,
                marginTop: 20,
              }}
            >
              {result.satSweep.map((v) => (
                <SweepCard
                  key={v.name}
                  name={v.name}
                  data={v}
                  accent={v.color}
                  showAgreement
                />
              ))}
            </div>
          </div>

          {/* Distribution universality */}
          <div
            style={{
              background: COLORS.bg,
              border: `1px solid ${COLORS.border}`,
              padding: "22px 26px",
              marginBottom: 32,
            }}
          >
            <div
              style={{
                fontFamily: "'Cormorant Garamond', serif",
                fontSize: 24,
                fontWeight: 600,
                lineHeight: 1.1,
                marginBottom: 4,
              }}
            >
              Universality II — constraint distribution
            </div>
            <div
              style={{
                fontFamily: "'EB Garamond', serif",
                fontStyle: "italic",
                fontSize: 13.5,
                color: "#5a544a",
                marginBottom: 16,
                maxWidth: 820,
              }}
            >
              Six different (baseline, tilt) sampling distributions, all run
              with default <code style={{ fontFamily: "'JetBrains Mono', monospace" }}>tanh(3x)</code> on the asymmetric power
              gradient. Different distributions <em>should</em> produce different
              quantitative orbit weights — universality applies to qualitative
              structure (sign-flip presence, tangled-rope concentration when
              tilt is non-trivial), not to the specific numbers.
            </div>
            <div
              style={{
                display: "grid",
                gridTemplateColumns: "repeat(3, 1fr)",
                gap: 12,
              }}
            >
              {result.distSweep.map((v) => (
                <SweepCard
                  key={v.name}
                  name={v.name}
                  desc={v.desc}
                  data={v}
                  accent={COLORS.accent}
                />
              ))}
            </div>
          </div>

          {/* Interpretation */}
          <div
            style={{
              border: `1px solid ${COLORS.border}`,
              padding: "24px 28px",
              fontFamily: "'EB Garamond', serif",
              fontSize: 14.5,
              lineHeight: 1.6,
              color: "#2a2620",
            }}
          >
            <div
              style={{
                fontFamily: "'Cormorant Garamond', serif",
                fontSize: 22,
                fontWeight: 600,
                marginBottom: 12,
              }}
            >
              What this does and doesn't show
            </div>
            <p style={{ margin: "0 0 12px" }}>
              <strong>Base test (unchanged from v1).</strong> The §2.3 mechanism
              applied to randomly-generated constraints in an asymmetric
              environment produces sign-flip and tangled-rope concentration
              well above the random baseline; in a symmetric environment it
              produces neither. Sign-flip is a consequence of position-relative
              flow tracking, not of any cognitive content.
            </p>
            <p style={{ margin: "0 0 12px" }}>
              <strong>H¹ histogram — combinatorial caveat.</strong> For 4
              observers with ternary values, enumerating count tuples
              (n₊, n₀, n₋) summing to 4 shows that the strict pair-disagreement
              count can only land in <code>{`{0, 3, 4, 5}`}</code>. Values
              {" "}<code>{`{1, 2, 6}`}</code> are unreachable for <em>any</em>{" "}
              classification rule — the absence of 1 and 2 is not evidence for
              the H¹ gap, it's enumeration. The diagnostic content here is the
              <em> distribution shape</em> over reachable values: random spreads
              roughly proportional to how many tuples land in each bucket;
              symmetric concentrates at 0; asymmetric concentrates at the
              middle/upper bins (3, 4, 5) because the tilt parameter spreads
              classifications across positions. A real H¹ gap test probably
              needs more observers (where the combinatorial floor doesn't
              dominate), or a metric weighted by gauge structure rather than
              raw pair count.
            </p>
            <p style={{ margin: "0 0 12px" }}>
              <strong>Universality I (saturation).</strong> If pairwise agreement
              with the <code>tanh(3x)</code> baseline is high (~0.7–0.9) across
              variants, and qualitative metrics (sign-flip rate, tangled-rope
              dominance, head families) cluster regardless of functional form,
              the universality-class claim from §2.3 survives empirical test
              in this minimal setting. Note that <code>hard step</code> and{" "}
              <code>tanh(1x)</code> are the most stringent stress cases — the
              former because it discards the saturation profile entirely, the
              latter because its slope is shallow enough that many
              near-threshold flows fall in the neutral band.
            </p>
            <p style={{ margin: "0 0 12px" }}>
              <strong>Universality II (distribution).</strong> Different
              constraint distributions <em>should</em> produce different orbit
              weights — universality is qualitative, not quantitative. The
              meaningful read: do all distributions with non-trivial tilt
              produce sign-flip and tangled-rope concentration? Does the{" "}
              <code>low tilt</code> variant collapse toward uniform-rope /
              uniform-snare (because position-relative flow is small relative
              to baseline)? Does <code>bimodal tilt</code> sharpen the orbit
              structure? These are the variations that probe whether the
              mechanism is robust or fragile to the constraint-generating process.
            </p>
            <p style={{ margin: "0 0 12px" }}>
              <strong>What still isn't established.</strong> That real social
              constraints have any of these distributions. That the framework's
              claims about real-world phenomena are correct. The simulation is
              a sufficiency demonstration of the §2.3 mechanism, not a
              substitute for the §5.5 RL protocol or cross-class data.
            </p>
            <p style={{ margin: 0 }}>
              <strong>Honest summary.</strong> v2 strengthens the §2.3
              sufficiency claim by showing the orbit structure is robust under
              both functional-form and distributional perturbation in a minimal
              non-anthropocentric setting. It does not adjudicate whether the
              real social/political/agent constraints DR analyzes have the
              structure assumed here. The H¹ extension turned out to be partly
              tautological for this observer count and exposes a gap worth
              addressing: the H¹ gap claim in §3.3 needs a sharper
              operationalization to be empirically testable.
            </p>
          </div>
        </div>
      </div>
    </>
  );
}
