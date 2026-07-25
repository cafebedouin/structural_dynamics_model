#!/usr/bin/env python3
"""Step-0 census of SCOPE decomposition manifests (OQ-254 re-witness).

Counts, per manifest under outputs/kernel_manifests/ (recursive):
  - deferred_axes present / non-empty / empty / absent
  - axes[].selection_reason coverage (present vs missing), centrality_score coverage
  - is_contested_kernel presence
Also classifies the empty-deferred_axes manifests with the mechanical discriminator
(empty deferred_axes + non-empty fracture_scan.notes => legitimately-nothing-deferred;
both empty => unauthored) and lists every missing-selection_reason axis for hand-read.
"""
import json, sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
POPULATIONS = [
    ROOT / "outputs" / "kernel_manifests",      # c-orchestrator _persist_manifest
    ROOT / "outputs" / "decompose" / "manifests",  # gkc batch path
    ROOT / "outputs" / "kernel_first_phase0",   # phase-0 probe (tracked twin in audits/)
]
# NOTE: outputs/*.manifest.json at the root (giant_component_analysis, orbit_data) are
# pipeline-output sidecars (single key "manifest"), not SCOPE manifests — excluded.
files = sorted(set(f for pop in POPULATIONS for f in pop.rglob("*.manifest.json")))
n = len(files)
stats = dict(n_manifests=n, deferred_present=0, deferred_nonempty=0,
             deferred_empty=0, deferred_absent=0,
             axes_total=0, selection_reason_present=0, selection_reason_missing=0,
             centrality_present=0, kernel_verdict_present=0,
             deferred_entries_total=0, deferral_reason_present=0)
empty_deferred = []   # (path, classification)
missing_selreason = []  # (path, axis_id_or_index)
parse_errors = []

for f in files:
    try:
        d = json.loads(f.read_text())
    except Exception as e:
        parse_errors.append((str(f), repr(e)))
        continue
    rel = str(f.relative_to(ROOT))
    da = d.get("deferred_axes", None)
    if da is None:
        stats["deferred_absent"] += 1
    else:
        stats["deferred_present"] += 1
        if da:
            stats["deferred_nonempty"] += 1
            stats["deferred_entries_total"] += len(da)
            for e in da:
                if isinstance(e, dict) and e.get("deferral_reason"):
                    stats["deferral_reason_present"] += 1
        else:
            stats["deferred_empty"] += 1
            fs = d.get("fracture_scan", {}) or {}
            notes = fs.get("notes") if isinstance(fs, dict) else None
            cls = "legit_nothing_deferred" if notes else "unauthored"
            empty_deferred.append((rel, cls, (notes or "")[:120]))
    csr = d.get("commitment_system_recognition") or {}
    if isinstance(csr, dict) and "is_contested_kernel" in csr:
        stats["kernel_verdict_present"] += 1
        key = "kernel_verdict_true" if csr["is_contested_kernel"] else "kernel_verdict_false"
        stats[key] = stats.get(key, 0) + 1
    for i, ax in enumerate(d.get("axes", []) or []):
        stats["axes_total"] += 1
        if isinstance(ax, dict) and ax.get("selection_reason"):
            stats["selection_reason_present"] += 1
        else:
            stats["selection_reason_missing"] += 1
            aid = ax.get("axis_id", f"index_{i}") if isinstance(ax, dict) else f"index_{i}"
            missing_selreason.append((rel, aid))
        if isinstance(ax, dict) and ax.get("centrality_score") is not None:
            stats["centrality_present"] += 1

out = dict(stats=stats,
           empty_deferred_classified=empty_deferred,
           missing_selection_reason=missing_selreason,
           parse_errors=parse_errors)
print(json.dumps(out, indent=2))
