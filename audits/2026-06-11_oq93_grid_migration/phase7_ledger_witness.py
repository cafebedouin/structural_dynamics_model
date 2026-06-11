#!/usr/bin/env python3
"""Phase 7 witness — OQ-101 tensions ledger (terminal consumer).

Cases:
  1. Ledger runs on the current pipeline output (full corpus); field-presence
     per source: every block carries headline-verdict, per-position,
     signature, omegas, drift, contamination, report lines.
  2. Fidelity spot-check vs two regenerated reports: the ledger's headline
     verdict and signature for those constraints match the pipeline entry
     fields the report renders (deterministic extraction, no drift).
  3. Orchestrator step path: DRAuditOrchestrator._step_ledger drives
     tensions_ledger.build_ledger and returns success with the output path.
  4. Rider-(a) closure: a constructed entry carrying the Phase-2 basis
     fixture's WITNESSED provenance tuple meas_prov(39,0,0,2,39) renders
     'projected 2' on the ledger's drift line (end of the firing chain:
     fixture JSON -> compiler -> measurement_basis/2 fact -> per-bucket
     count [Phase 2, swipl] -> ledger drift line [here]).

Run from repo root:
  python3 audits/2026-06-11_oq93_grid_migration/phase7_ledger_witness.py
"""
import json
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
AUDIT = ROOT / "audits/2026-06-11_oq93_grid_migration"
sys.path.insert(0, str(ROOT / "python"))
import tensions_ledger

results = []


def rec(label, ok, detail=""):
    results.append((label, "PASS" if ok else "FAIL", detail))


# 1. Full-corpus ledger + field presence
out_path, n = tensions_ledger.build_ledger(
    output_path=AUDIT / "tensions_ledger_full.md")
text = Path(out_path).read_text()
blocks = [b for b in text.split("\n## ") if b.strip()][1:]  # skip header
pipe = json.loads((ROOT / "outputs/pipeline_output.json").read_text())
n_pipe = pipe["manifest"]["n_constraints"]
rec(f"1a ledger blocks == manifest n_constraints ({n_pipe})", n == n_pipe,
    f"blocks={n}")
required = ["headline verdict:", "per-position types:", "signature:",
            "omegas:", "drift events:", "contamination edges:", "- report:"]
missing = [(i, fld) for i, b in enumerate(blocks)
           for fld in required if fld not in b]
rec("1b every block carries all 7 required line families", not missing,
    f"missing={missing[:5]}")
rec("1c manifest stamped in ledger header",
    pipe["manifest"]["pipeline_run_at"] in text and
    pipe["manifest"]["code_commit_short"] in text)
rec("1d OQ-103 provenance gap LABELED on every block with edges",
    all(("contamination edges: none" in b) or ("OQ-103 open" in b)
        for b in blocks))

# 2. Fidelity spot-check vs two regenerated reports
two = [e["id"] for e in pipe["per_constraint"][:2]]
gen = subprocess.run([sys.executable, "python/enhanced_report.py", *two],
                     cwd=ROOT, capture_output=True, text=True, timeout=900)
ok_reports = all((ROOT / "outputs/constraint_reports" / f"{c}_report.md").exists()
                 for c in two)
rec(f"2a two reports regenerated ({two})", ok_reports,
    (gen.stderr or gen.stdout).strip()[-120:] if not ok_reports else "")
fid_fail = []
for c in two:
    entry = next(e for e in pipe["per_constraint"] if e["id"] == c)
    block = next((b for b in blocks if b.startswith(f"{c} ")
                  or b.startswith(f"{c} —") or b.split(" —")[0] == c), "")
    vj = entry.get("verdict_join") or {}
    if vj and f"headline verdict: {vj.get('verdict')}" not in block:
        fid_fail.append((c, "verdict"))
    sig = entry.get("signature") or "none"
    if f"signature: {sig}" not in block:
        fid_fail.append((c, f"signature({sig})"))
rec("2b ledger fields match pipeline entries for both spot-checked constraints",
    not fid_fail, str(fid_fail))

# 3. Orchestrator step path
sys.path.insert(0, str(ROOT))
import importlib.util
spec = importlib.util.spec_from_file_location(
    "c_orch", ROOT / "agent/c-orchestrator.py")
c_orch = importlib.util.module_from_spec(spec)
spec.loader.exec_module(c_orch)
orch = c_orch.DRAuditOrchestrator(dry_run=True)
step = orch._step_ledger(two)
rec("3 orchestrator _step_ledger returns success with output path",
    step.status == "success" and step.data and Path(step.data).exists(),
    f"status={step.status} data={step.data}")

# 4. Rider-(a) closure: fixture provenance tuple on the drift line
fixture_entry = {
    "id": "basis_fixture_oq102a",
    "human_readable": "Basis fixture (OQ-102(a) firing chain)",
    "verdict_join": {
        "verdict": "green", "base_verdict": "green", "cap_applied": "none",
        "alerts": [], "signature_grade": None,
        "grid_provenance": {"authored": 32, "injected": 0, "imputed": 0,
                            "absent": 0, "total": 32},
        # the tuple WITNESSED in swipl at Phase 2: meas_prov(39,0,0,2,39)
        "measurement_provenance": {"authored": 39, "injected": 0,
                                   "imputed": 0, "projected": 2, "total": 39},
    },
    "perspectives": {"powerless": "snare", "moderate": "rope",
                     "institutional": "rope", "analytical": "tangled_rope"},
    "signature": "none",
    "omegas": [],
    "drift_events": [{"type": "extraction_accumulation", "severity": "warning"}],
    "contamination_network": {},
}
block = tensions_ledger.build_block(fixture_entry, report_dir=AUDIT)
drift_line = next(l for l in block.splitlines() if l.startswith("- drift events:"))
rec("4 fixture's witnessed meas_prov(39,0,0,2,39) renders 'projected 2' on the "
    "ledger drift line", "2/39 authored-as-projected" in drift_line, drift_line)
(AUDIT / "phase7_fixture_block.txt").write_text(block)

for label, verdict, detail in results:
    print(f"  [{verdict}] {label}" + (f" — {detail}" if detail else ""))
n_fail = sum(1 for _, v, _ in results if v == "FAIL")
print(f"{len(results) - n_fail}/{len(results)} witness cases pass")
sys.exit(1 if n_fail else 0)
