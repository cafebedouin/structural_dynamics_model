#!/usr/bin/env python3
"""OQ-81 recon: census reading-typed vs kernel-concept-typed downstream_of edges.

Three probes:
  1. Archived kernel manifests (outputs/kernel_manifests/, gitignored — pass its
     path as argv[1]; default assumes the main checkout): count manifests whose
     supplementary axes name a READING in downstream_of. Positive control: must
     find the OQ-81 cited examples (dutch husk_reading, vatican_ii readings).
  2. Phase-0 manifests (audits/2026-06-06_kernel_first_phase0/manifests/, current
     SCOPE decompose format): same census.
  3. Dangling-edge check on Phase-0: deps that name no generation_sequence entry
     (no story will ever be generated for them -> wave filter ignores them and
     upstream_context injects nothing, silently).
"""
import json, glob, sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
REPO = HERE.parent.parent
ARCHIVE = Path(sys.argv[1]) if len(sys.argv) > 1 else \
    Path("/home/scott/bin/structural_dynamics_model/outputs/kernel_manifests")
PHASE0 = REPO / "audits/2026-06-06_kernel_first_phase0/manifests"


def reading_edge_rows(m):
    csr = m.get("commitment_system_recognition") or {}
    rids = {r.get("reading_id") for r in csr.get("readings", [])} - {None}
    rows = []
    for a in m.get("axes", []) or []:
        deps = a.get("downstream_of") or []
        hits = [d for d in deps if d in rids]
        if hits:
            rows.append((a.get("claim_id"), hits))
    return rows


def census(files, label):
    n_manifests = n_edge_manifests = n_axes = 0
    examples = []
    for f in files:
        try:
            m = json.load(open(f))
        except Exception:
            continue
        if not isinstance(m, dict) or "axes" not in m:
            continue
        n_manifests += 1
        rows = reading_edge_rows(m)
        if rows:
            n_edge_manifests += 1
            n_axes += len(rows)
            if len(examples) < 6:
                examples.append((Path(f).name, rows))
    print(f"[{label}] manifests with axes[]: {n_manifests}; "
          f"with reading-edges: {n_edge_manifests}; reading-edge axes: {n_axes}")
    for e in examples:
        print("   ", e)
    return n_edge_manifests


def dangling(files, label):
    print(f"[{label}] downstream_of deps naming no generation_sequence entry:")
    n = 0
    for f in sorted(files):
        m = json.load(open(f))
        gen_ids = set()
        for e in m.get("generation_sequence", []):
            cid = e if isinstance(e, str) else (e.get("claim_id") or e.get("constraint_id"))
            if cid:
                gen_ids.add(cid)
        for a in m.get("axes", []) or []:
            for d in (a.get("downstream_of") or []):
                if d not in gen_ids:
                    n += 1
                    print(f"    {Path(f).name}: {a.get('claim_id')} <- '{d}' DANGLING")
    print(f"  total dangling deps: {n}")


archive_files = glob.glob(str(ARCHIVE / "**/*.json"), recursive=True)
hits = census(archive_files, "archived kernel_manifests")
# Positive control: the census must fire on the OQ-81 cited examples.
assert hits > 0, "POSITIVE CONTROL FAILED: no reading-edges found in the archive store"

phase0_files = glob.glob(str(PHASE0 / "*.manifest.json"))
census(phase0_files, "phase0 (current SCOPE format)")
dangling(phase0_files, "phase0")
