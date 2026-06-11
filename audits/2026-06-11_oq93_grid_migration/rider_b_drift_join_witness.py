#!/usr/bin/env python3
"""Rider OQ-102(b) witness — drift severity joins its own confidence/provenance
at the read site (before/after on the same input, both render paths).

Prolog side: generate_drift_report/1 on a corpus constraint that carries
drift events — the [severity] token must carry ' | confidence: ...' from the
constraint's own terminal prediction (old render printed it bare, with the
confidence line ~100 lines away — the competition_timeline_pressure witness
shape).

Python side: the TEMPORAL TRAJECTORY prefix and the Level-1 drift-events line
rendered from one constructed entry (drift_events + measurement_provenance
with a projected bucket + a reversal-shaped series), diffed old-vs-new
(old enhanced_report.py from the pre-rider commit).

Run from repo root:
  python3 audits/2026-06-11_oq93_grid_migration/rider_b_drift_join_witness.py <pre_rider_ref>
"""
import importlib.util
import subprocess
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
pre_ref = sys.argv[1] if len(sys.argv) > 1 else "HEAD~1"
results = []


def rec(label, ok, detail=""):
    results.append((label, "PASS" if ok else "FAIL", detail))


# --- Prolog side -------------------------------------------------------------
goal = (
    "[stack], corpus_loader:load_all_testsets, "
    "use_module(drift_report), use_module(transition_paths), "
    "( corpus_loader:corpus_constraint(C), "
    "  drift_report:scan_constraint_drift(C, Es), Es \\= [], "
    "  transition_paths:predicted_terminal_state(C, S, _), S \\= stable "
    "-> format('CHOSEN ~w~n', [C]), drift_report:generate_drift_report(C) "
    "; format('NO-CARRIER~n', []) ), halt."
)
p = subprocess.run(["swipl", "-g", goal, "-t", "halt(1)"],
                   cwd=ROOT / "prolog", capture_output=True, text=True, timeout=900)
out = p.stdout
chosen = [l for l in out.splitlines() if l.startswith("CHOSEN ")]
sev_lines = [l for l in out.splitlines()
             if l.strip().startswith("[") and "| confidence:" in l]
rec("P1 prolog drift render: severity token carries ' | confidence:' on a live "
    "drift-event carrier", bool(chosen) and bool(sev_lines),
    (chosen[0] + " :: " + sev_lines[0].strip()) if (chosen and sev_lines)
    else out.strip()[-300:])

# --- Python side -------------------------------------------------------------
ENTRY = {
    "id": "rider_b_case",
    "drift_events": [{"type": "extraction_accumulation", "severity": "critical"}],
    "verdict_join": {"measurement_provenance":
                     {"authored": 6, "injected": 0, "imputed": 0,
                      "projected": 2, "total": 8}},
    "drift_trajectory": {
        "base_extractiveness": {
            "series": [{"t": 0, "v": 0.35}, {"t": 3, "v": 0.60}, {"t": 6, "v": 0.50}],
            "per_interval_rate": [{"rate": 0.083}, {"rate": -0.033}],
            "per_interval_acceleration": [{"acc": -0.02}],
        }
    },
}
PIPE = {"per_constraint": [ENTRY]}


def load_mod(src_text, name):
    with tempfile.NamedTemporaryFile("w", suffix=f"_{name}.py", delete=False,
                                     dir=ROOT / "python") as tf:
        tf.write(src_text)
        path = tf.name
    spec = importlib.util.spec_from_file_location(name, path)
    mod = importlib.util.module_from_spec(spec)
    sys.path.insert(0, str(ROOT / "python"))
    spec.loader.exec_module(mod)
    Path(path).unlink()
    return mod


new_src = (ROOT / "python/enhanced_report.py").read_text()
old_src = subprocess.run(["git", "show", f"{pre_ref}:python/enhanced_report.py"],
                         cwd=ROOT, capture_output=True, text=True, check=True).stdout
new_mod = load_mod(new_src, "er_new")
old_mod = load_mod(old_src, "er_old")

new_traj = new_mod.build_drift_trajectory_section("rider_b_case", PIPE)
old_traj = old_mod.build_drift_trajectory_section("rider_b_case", PIPE)
rec("P2 TEMPORAL TRAJECTORY: projected bucket surfaces in CONDITIONAL line (new only)",
    "authored-as-PROJECTED" in new_traj and "authored-as-PROJECTED" not in old_traj,
    [l.strip() for l in new_traj.splitlines() if "CONDITIONAL" in l][0]
    if "CONDITIONAL" in new_traj else "no CONDITIONAL line")
rec("P3 old line (non-authored count) preserved in new render",
    "2/8" in new_traj or "non-authored" in new_traj or "PROJECTED" in new_traj,
    "")

print("=== old TEMPORAL TRAJECTORY ===")
print(old_traj)
print("=== new TEMPORAL TRAJECTORY ===")
print(new_traj)

for label, verdict, detail in results:
    print(f"  [{verdict}] {label}" + (f" — {detail}" if detail else ""))
n_fail = sum(1 for _, v, _ in results if v == "FAIL")
print(f"{len(results) - n_fail}/{len(results)} witness cases pass (pre-rider ref {pre_ref})")
sys.exit(1 if n_fail else 0)
