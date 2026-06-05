#!/usr/bin/env python3
"""SCOPE count-distribution probe (2026-06-05) — resolves the 7-7-7 watch (OQ-75).

Runs SCOPE-only decomposition (DRAuditOrchestrator._step_decompose, temp 0.2, no axes
ceiling) over an 8-topic battery that deliberately spans structural richness, in TWO
ARMS:

  A (current)  — prompts/uke_scope_v2_json.md as in the working tree (post-d179423d
                 lens-diversity instruction).
  B (pre-lens) — the same file at d179423d~1, so a flat/clustered result can be
                 attributed to cap-removal framing vs the lens instruction.

Read-only with respect to the repo and corpus: output is raw manifests + a summary
table under audits/2026-06-05_scope_count_distribution/evidence/.

Pre-registered readout signatures live in the audit writeup; the headline metric is
the VARIANCE of axis counts across topics (esp. whether the upper tiers T4-T7 spread
among themselves), never the mean.
"""

import json
import subprocess
import sys
import time
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(REPO / "agent"))

EVIDENCE = REPO / "audits" / "2026-06-05_scope_count_distribution" / "evidence"
PRE_LENS_REF = "d179423d~1"

# Topic battery — pre-registered richness tiers (hypotheses, not pass criteria).
TOPICS = [
    ("T1", "binary",        "The convention of driving on the right-hand side of the road"),
    ("T2", "mountain",      "The second law of thermodynamics as a limit on heat-engine efficiency"),
    ("T3", "thin-coord",    "Daylight saving time"),
    ("T4", "moderate",      "Minimum wage law"),
    ("T5", "mid-bridge",    "Gig economy platform labor classification"),
    ("T6", "rich-kernel",   "The personhood boundary in abortion law"),
    ("T7", "very-rich",     "United States healthcare system financing"),
    ("T8", "binary-repl",   "The convention of driving on the right-hand side of the road"),
]


def summarize(manifest):
    axes = manifest.get("axes", [])
    gs = manifest.get("generation_sequence", [])
    deferred = manifest.get("deferred_axes", [])
    csr = manifest.get("commitment_system_recognition") or {}
    is_kernel = bool(csr.get("is_contested_kernel"))
    readings = csr.get("readings", []) if is_kernel else []
    return {
        "candidates": len(axes),
        "selected": len(gs),
        "deferred": len(deferred),
        "is_kernel": is_kernel,
        "readings": len(readings),
    }


def main():
    EVIDENCE.mkdir(parents=True, exist_ok=True)

    # Arm prompts
    pre_lens_prompt = subprocess.run(
        ["git", "show", f"{PRE_LENS_REF}:prompts/uke_scope_v2_json.md"],
        cwd=REPO, capture_output=True, text=True, check=True,
    ).stdout
    (EVIDENCE / "armB_uke_scope_pre_lens.md").write_text(pre_lens_prompt, encoding="utf-8")

    # Late import so REPO path insert applies. Module name has a dash → import via file.
    import importlib.util
    spec = importlib.util.spec_from_file_location(
        "c_orchestrator", REPO / "agent" / "c-orchestrator.py")
    co = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(co)

    rows = []
    for arm, prompt_override in (("A", None), ("B", pre_lens_prompt)):
        orch = co.DRAuditOrchestrator(axes=None, skip_search=True,
                                      skip_corpus_update=True, skip_essay=True)
        if prompt_override is not None:
            orch.protocols["uke_scope"] = prompt_override
        for tid, tier, topic in TOPICS:
            label = f"{tid}_{arm}"
            t0 = time.time()
            res = orch._step_decompose(topic, "")
            dur = time.time() - t0
            if res.status != "success" or not res.data:
                print(f"[{label}] ERROR: {res.error}", flush=True)
                rows.append({"id": tid, "arm": arm, "tier": tier, "error": str(res.error)})
                continue
            manifest = res.data
            (EVIDENCE / f"manifest_{label}.json").write_text(
                json.dumps(manifest, indent=1), encoding="utf-8")
            s = summarize(manifest)
            s.update({"id": tid, "arm": arm, "tier": tier, "dur_s": round(dur, 1)})
            rows.append(s)
            print(f"[{label}] cand={s['candidates']} sel={s['selected']} "
                  f"def={s['deferred']} kernel={s['is_kernel']} readings={s['readings']} "
                  f"({dur:.0f}s)", flush=True)

    (EVIDENCE / "summary_rows.json").write_text(
        json.dumps(rows, indent=1), encoding="utf-8")

    # Table
    print("\n| id | tier | arm | candidates | selected | deferred | kernel | readings |")
    print("|---|---|---|---|---|---|---|---|")
    for r in rows:
        if "error" in r:
            print(f"| {r['id']} | {r['tier']} | {r['arm']} | ERROR: {r['error'][:40]} |")
        else:
            print(f"| {r['id']} | {r['tier']} | {r['arm']} | {r['candidates']} | "
                  f"{r['selected']} | {r['deferred']} | {r['is_kernel']} | {r['readings']} |")


if __name__ == "__main__":
    main()
