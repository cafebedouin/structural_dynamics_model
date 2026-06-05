#!/usr/bin/env python3
"""K1 — kernel-gate replication probe (2026-06-05): quantify P(kernel) per topic.

Background: the SCOPE kernel/flat gate routed the SAME contested substrate (gig-economy
classification, T5 of the count-distribution probe) onto the axiom axis in one run and
the observer axis in another (audits/2026-06-05_scope_count_distribution/). K3
hand-adjudication: T5 passes all three §1.3-K criteria → the flat take was a gate MISS
against explicit criteria (execution noise, not definitional ambiguity). A flat miss
destroys the axiom axis irrecoverably; Stage-2's cross-axis correlation would partly
measure this coin-flip.

K1 question (gates Stage-2): is the miss-rate a thin boundary band (stratify, proceed)
or broad stochasticity (structural fix first)?

PRE-REGISTERED (before any call):
- k=8 runs per topic, current prompt (arms agreed in the count probe), temp 0.2,
  research_context="".
- INSTRUMENT VALIDITY (invalidation conditions, not calibration):
  * C-HIGH (personhood boundary, uncontroversial §1.3-K kernel): P(kernel) must be
    >= 7/8. BELOW THAT THE INSTRUMENT VERDICT IS INVALIDATED: the gate under-fires on
    CLEAR kernels, "thin boundary band" is not an available diagnosis, and the finding
    becomes recognizer-noisy-everywhere (bigger; changes the fix).
  * C-LOW (drive-on-right convention, uncontroversial non-kernel): P(kernel) must be
    <= 1/8; above that the gate over-fires and intermediate P readings are inflated.
- Given a valid instrument, per-topic P(kernel) on the three contest topics is the
  readout. Thin-band: extremes for most topics, intermediate P confined to genuinely
  boundary contests. Broad-stochasticity: intermediate P widespread.
- No remediation in this pass; verdict + OQ-76 filing only.

Read-only wrt repo/corpus. Evidence: audits/2026-06-05_kernel_gate_replication/evidence/.
"""

import json
import sys
import time
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(REPO / "agent"))

EVIDENCE = REPO / "audits" / "2026-06-05_kernel_gate_replication" / "evidence"
K = 8

TOPICS = [
    ("C-LOW",  "control-nonkernel", "The convention of driving on the right-hand side of the road"),
    ("C-HIGH", "control-kernel",    "The personhood boundary in abortion law"),
    ("X1",     "observed-flipper",  "Gig economy platform labor classification"),
    ("X2",     "legal-contest",     "Affirmative action in university admissions"),
    ("X3",     "legal-contest",     "Content moderation obligations of private social media platforms"),
]


def main():
    EVIDENCE.mkdir(parents=True, exist_ok=True)
    import importlib.util
    spec = importlib.util.spec_from_file_location(
        "c_orchestrator", REPO / "agent" / "c-orchestrator.py")
    co = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(co)

    orch = co.DRAuditOrchestrator(axes=None, skip_search=True,
                                  skip_corpus_update=True, skip_essay=True)
    rows = []
    for tid, role, topic in TOPICS:
        for i in range(1, K + 1):
            label = f"{tid}_r{i}"
            t0 = time.time()
            res = orch._step_decompose(topic, "")
            dur = time.time() - t0
            if res.status != "success" or not res.data:
                print(f"[{label}] ERROR: {res.error}", flush=True)
                rows.append({"id": tid, "run": i, "role": role, "error": str(res.error)})
                continue
            man = res.data
            (EVIDENCE / f"manifest_{label}.json").write_text(
                json.dumps(man, indent=1), encoding="utf-8")
            csr = man.get("commitment_system_recognition") or {}
            is_k = bool(csr.get("is_contested_kernel"))
            n_read = len(csr.get("readings", [])) if is_k else 0
            rows.append({"id": tid, "run": i, "role": role, "kernel": is_k,
                         "readings": n_read, "axes": len(man.get("axes", [])),
                         "dur_s": round(dur, 1)})
            print(f"[{label}] kernel={is_k} readings={n_read} "
                  f"axes={len(man.get('axes', []))} ({dur:.0f}s)", flush=True)

    (EVIDENCE / "summary_rows.json").write_text(json.dumps(rows, indent=1), encoding="utf-8")

    print("\n| topic | role | P(kernel) | readings when kernel |")
    print("|---|---|---|---|")
    for tid, role, _ in TOPICS:
        rs = [r for r in rows if r["id"] == tid and "error" not in r]
        n = len(rs)
        hits = sum(1 for r in rs if r["kernel"])
        reads = sorted(r["readings"] for r in rs if r["kernel"])
        print(f"| {tid} | {role} | {hits}/{n} | {reads} |")


if __name__ == "__main__":
    main()
