#!/usr/bin/env python3
"""Phase 3 witness — coverage-carrying read (OQ-93 half-step).

Two-sided control pre-registered in PREREGISTRATION.md:
  HEALED:    the 8/32 one-level grid (grid_probe_partial8.pl) flips from
             increasing_coercion (pre-change witness:
             phase3_partial8_pre_witness.txt, G_sys=0.2160) to OPEN.
  UNCHANGED: the five probe stories keep their exact pinned values
             (REGRESSION-PIN, probe FINDINGS.md 2026-06-10, tol ±0.001):
             G_sys +0.588/-0.588/0.000/+0.156/+0.980; patterns inc/dec/
             stable/inc/inc; kappa 0.80/0.20/0.50/0.49/1.00.
  CORPUS:    full validation suite exit 0; load-warning gate clean.

Run from repo root:
  python3 audits/2026-06-11_oq93_grid_migration/phase3_coverage_read_witness.py
"""
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
AUDIT = ROOT / "audits/2026-06-11_oq93_grid_migration"
PROBES = ROOT / "audits/2026-06-10_oq93_grid_viability_probe/stories"

PINS = {  # id: (gsys, pattern, kappa)
    "grid_probe_rising":   (0.588,  "increasing_coercion", 0.80),
    "grid_probe_falling":  (-0.588, "decreasing_coercion", 0.20),
    "grid_probe_flat_authored": (0.000, "stable", 0.50),
    "grid_probe_divergent": (0.156, "increasing_coercion", 0.49),
    "grid_probe_intent_max": (0.980, "increasing_coercion", 1.00),
}

results = []


def rec(label, ok, detail=""):
    results.append((label, "PASS" if ok else "FAIL", detail))


def run_story(path, iid):
    goal = (
        "use_module(narrative_ontology), use_module(config), "
        "use_module(pattern_analysis), use_module(coercion_projection), "
        f"consult('{path}'), "
        f"pattern_analysis:analyze_interval({iid}, G, C, P), "
        f"narrative_ontology:interval({iid}, _, Tn), "
        f"findall(K, (config:level(L), once(coercion_projection:coercion_magnitude({iid}, L, Tn, K))), Ks), "
        "( Ks == [] -> AvgK = none ; sum_list(Ks, S), length(Ks, N), AvgK is S/N ), "
        "format('RES G=~w C=~w P=~w K=~w~n', [G, C, P, AvgK]), halt."
    )
    p = subprocess.run(["swipl", "-g", goal, "-t", "halt(1)"],
                       cwd=ROOT / "prolog", capture_output=True, text=True, timeout=180)
    out = [l for l in p.stdout.splitlines() if l.startswith("RES ")]
    return out[-1] if out else f"NO-RES stderr={p.stderr.strip()[-150:]}"


# HEALED side: 8/32 -> OPEN
out = run_story(AUDIT / "grid_probe_partial8.pl", "grid_probe_partial8")
ok = "P=open(missing_levels(" in out and "G=open" in out and "C=0.25" in out
rec("HEALED: 8/32 one-level grid flips increasing_coercion -> OPEN "
    "(pre-change witnessed 0.2160/increasing)", ok, out)

# UNCHANGED side: five probe stories
for iid, (gpin, ppin, kpin) in PINS.items():
    out = run_story(PROBES / f"{iid}.pl", iid)
    ok = False
    detail = out
    if out.startswith("RES "):
        try:
            parts = dict(kv.split("=", 1) for kv in out[4:].split(" "))
            g, p_, k = float(parts["G"]), parts["P"], float(parts["K"])
            ok = abs(g - gpin) <= 0.001 and p_ == ppin and abs(k - kpin) <= 0.005
        except (ValueError, KeyError) as e:
            detail = f"{out} (parse: {e})"
    rec(f"UNCHANGED: {iid} pinned (G={gpin}, {ppin}, kappa={kpin})", ok, detail)

# CORPUS side: full suite + warning gate
suite = subprocess.run(
    ["swipl", "-g", "[stack], [validation_suite], run_dynamic_suite, halt",
     "-t", "halt(1)"],
    cwd=ROOT / "prolog", capture_output=True, text=True, timeout=1800)
n_fail_lines = suite.stdout.count("[FAIL]")
n_open = suite.stdout.count("[OPEN]")
n_intent_open = suite.stdout.count("[INTENT] Result: OPEN")
n_intent_stable = suite.stdout.count("[INTENT] Result: stable")
(AUDIT / "phase3_suite_run.txt").write_text(suite.stdout[-20000:] + "\n--- STDERR ---\n" + suite.stderr[-5000:])
rec(f"CORPUS: suite exit 0 with 0 [FAIL] (got exit={suite.returncode}, "
    f"FAIL={n_fail_lines}, OPEN={n_open}, INTENT-OPEN={n_intent_open}, "
    f"INTENT-stable={n_intent_stable})",
    suite.returncode == 0 and n_fail_lines == 0)
rec("CORPUS: zero default-shaped intent verdicts on the grid-absent corpus "
    "(every [INTENT] line is OPEN)", n_intent_stable == 0 and n_intent_open > 0,
    f"stable={n_intent_stable} open={n_intent_open}")

gate = subprocess.run([sys.executable, "python/load_warning_gate.py"],
                      cwd=ROOT, capture_output=True, text=True, timeout=600)
rec("CORPUS: load-warning gate clean", gate.returncode == 0,
    (gate.stdout + gate.stderr).strip()[-150:])

for label, verdict, detail in results:
    print(f"  [{verdict}] {label}" + (f" — {detail}" if detail else ""))
n_fail = sum(1 for _, v, _ in results if v == "FAIL")
print(f"{len(results) - n_fail}/{len(results)} witness cases pass")
sys.exit(1 if n_fail else 0)
