#!/usr/bin/env python3
"""Probe D — pre-reset temporal-classification artifacts in the LIVE outputs/ tree.

Read-only scan of the MAIN tree's outputs/ (gitignored; does not exist in the
worktree). Reset boundary: 2026-06-05 (corpus reset).

Detectors:
  1. manifest provenance: any *.json whose manifest.pipeline_run_at predates the
     reset is a pre-reset pipeline artifact surviving live.
  2. temporal-classification content: per_constraint entries with populated
     drift_trajectory / drift_events / temporal_residual; or tripwire-shaped
     rows ({baseline,patched} type pairs keyed by (constraint, time)).

Positive-control note: the plan called for running the scanner against the
archive/audit location where the pre-reset temporal classifications are known
to exist. Recon found that location IS the live outputs/ tree
(outputs/tripwire_fabricated_defaults_results.json — the 2026-05-30 tripwire
evidence was never moved into its audit dir). The scanner firing on it is
therefore simultaneously the positive control (the machinery demonstrably
fires on the known-positive) and a live hit. No absence claim is made on an
unfired detector.
"""
import json, os, sys, datetime

OUTPUTS = "/home/scott/bin/structural_dynamics_model/outputs"
RESET = "2026-06-05"

def temporal_content(d):
    """Return list of temporal-classification content markers found."""
    marks = []
    if isinstance(d, dict):
        pc = d.get("per_constraint")
        if isinstance(pc, list) and pc and isinstance(pc[0], dict):
            for key in ("drift_trajectory", "drift_events", "temporal_residual"):
                n = sum(1 for e in pc if isinstance(e, dict) and e.get(key))
                if n:
                    marks.append(f"per_constraint.{key} populated in {n}/{len(pc)}")
        # tripwire-shaped: nested dicts with details_sample rows carrying
        # baseline/patched type pairs keyed by (constraint, time)
        for topk, v in d.items():
            if isinstance(v, dict):
                rows = v.get("details_sample")
                if isinstance(rows, list) and rows and isinstance(rows[0], dict) \
                        and {"baseline", "patched", "key"} <= set(rows[0]):
                    marks.append(f"{topk}.details_sample: {len(rows)} baseline/patched "
                                 f"temporal classification rows (e.g. {rows[0]})")
    return marks

def main():
    print("=== Probe D: pre-reset artifact scan of live outputs/ ===")
    print("as-of:", datetime.datetime.now().astimezone().isoformat())
    print("scan root:", OUTPUTS, " reset boundary:", RESET)
    hits = []
    n_json = n_manifest = 0
    for root, _dirs, files in os.walk(OUTPUTS):
        for fn in sorted(files):
            if not fn.endswith(".json"):
                continue
            path = os.path.join(root, fn)
            n_json += 1
            try:
                with open(path) as f:
                    d = json.load(f)
            except Exception as e:
                print(f"  [unparseable] {path}: {e}")
                continue
            man = d.get("manifest") if isinstance(d, dict) else None
            run_at = man.get("pipeline_run_at") if isinstance(man, dict) else None
            if run_at:
                n_manifest += 1
            pre = bool(run_at) and run_at[:10] < RESET
            marks = temporal_content(d)
            mtime = datetime.datetime.fromtimestamp(os.path.getmtime(path)).isoformat()
            if pre:
                hits.append((path, f"MANIFEST PRE-RESET (pipeline_run_at={run_at})", marks))
            elif marks and not run_at and mtime[:10] < RESET:
                hits.append((path, f"NO MANIFEST, mtime {mtime} pre-reset, "
                                   f"temporal-classification content", marks))
    print(f"\nscanned: {n_json} json files; {n_manifest} carried a manifest")
    if hits:
        print(f"\nPOSITIVE HITS: {len(hits)} (halt condition — falsifies Block 2 as planned)")
        for path, why, marks in hits:
            print(f"\n  HIT: {path}\n    why: {why}")
            for m in marks:
                print(f"    content: {m}")
    else:
        print("\nNO HITS — witnessed-clean")
    # canonical live artifacts, for the record
    for f in ("pipeline_output.json", "enriched_pipeline.json"):
        p = os.path.join(OUTPUTS, f)
        if os.path.exists(p):
            with open(p) as fh:
                man = json.load(fh).get("manifest", {})
            print(f"\nlive artifact {f}: pipeline_run_at={man.get('pipeline_run_at')} "
                  f"n_constraints={man.get('n_constraints')} "
                  f"commit={man.get('code_commit_short')} dirty={man.get('code_dirty')}")
    return 0 if not hits else 4

if __name__ == "__main__":
    sys.exit(main())
