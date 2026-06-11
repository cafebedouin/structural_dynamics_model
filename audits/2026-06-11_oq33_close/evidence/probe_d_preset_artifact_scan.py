#!/usr/bin/env python3
"""Probe D — pre-reset temporal-classification artifacts in the LIVE outputs/ tree.

Read-only. Reset boundary: 2026-06-05 (corpus reset).

Detectors:
  1. manifest provenance: any *.json whose manifest.pipeline_run_at predates the reset.
  2. temporal-classification content: per_constraint entries with populated
     drift_trajectory / drift_events / temporal_residual; or tripwire-shaped rows
     ({baseline,patched} type pairs keyed by (constraint, time)).
  3. no-manifest + pre-reset mtime + temporal content (live scan only; mtime is a
     weak signal and is destroyed by cp, so the CONTROL pass never relies on it).

ARCHIVE-SIDE POSITIVE CONTROL (load-bearing for any clean/absence verdict, added
2026-06-11 after the disposition ruling): before scanning live outputs/, the same
detectors run over the control roots where the relocated pre-reset artifacts now
live (the 2026-05-30 audit dir; prolog/archives/outputs/). The control PASSES only
if BOTH fire there: >=1 manifest-pre-reset hit AND >=1 tripwire-shaped content
detection. A clean live scan with a failed control is INVALID (exit 3) — the
scanner could not have seen what it reports absent.

History: the 2026-06-11 first run (pre-disposition) found 4 positive hits in live
outputs/ — saved as evidence/probe_d_output.txt; that run's hits doubled as its
control (machinery demonstrably fired). This version exists for re-scans, where
the expected live result is absence.
"""
import json, os, sys, datetime

MAIN = "/home/scott/bin/structural_dynamics_model"
OUTPUTS = os.path.join(MAIN, "outputs")
CONTROL_ROOTS = [
    os.path.join(MAIN, "audits", "2026-05-30_authoring_closure_fabricated_defaults"),
    os.path.join(MAIN, "prolog", "archives", "pre_reset_outputs"),
]
RESET = "2026-06-05"

def temporal_content(d):
    marks = []
    if isinstance(d, dict):
        pc = d.get("per_constraint")
        if isinstance(pc, list) and pc and isinstance(pc[0], dict):
            for key in ("drift_trajectory", "drift_events", "temporal_residual"):
                n = sum(1 for e in pc if isinstance(e, dict) and e.get(key))
                if n:
                    marks.append(("drift", f"per_constraint.{key} populated in {n}/{len(pc)}"))
        for topk, v in d.items():
            if isinstance(v, dict):
                rows = v.get("details_sample")
                if isinstance(rows, list) and rows and isinstance(rows[0], dict) \
                        and {"baseline", "patched", "key"} <= set(rows[0]):
                    marks.append(("tripwire", f"{topk}.details_sample: {len(rows)} "
                                  f"baseline/patched rows (e.g. {rows[0]})"))
    return marks

def scan(root, use_mtime):
    """Returns (hits, n_json, n_manifest, n_pre_manifest, n_tripwire_marks)."""
    hits, n_json, n_manifest, n_pre, n_trip = [], 0, 0, 0, 0
    for r, _dirs, files in os.walk(root):
        for fn in sorted(files):
            if not fn.endswith(".json"):
                continue
            path = os.path.join(r, fn)
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
            n_trip += sum(1 for kind, _ in marks if kind == "tripwire")
            mtime = datetime.datetime.fromtimestamp(os.path.getmtime(path)).isoformat()
            if pre:
                n_pre += 1
                hits.append((path, f"MANIFEST PRE-RESET (pipeline_run_at={run_at})", marks))
            elif marks and not run_at and use_mtime and mtime[:10] < RESET:
                hits.append((path, f"NO MANIFEST, mtime {mtime} pre-reset, "
                                   f"temporal-classification content", marks))
            elif marks and not run_at and not use_mtime:
                # control pass: content alone counts (mtime not trusted post-cp)
                hits.append((path, "temporal-classification content (control-root)", marks))
    return hits, n_json, n_manifest, n_pre, n_trip

def show(hits):
    for path, why, marks in hits:
        print(f"\n  HIT: {path}\n    why: {why}")
        for _kind, m in marks:
            print(f"    content: {m}")

def main():
    print("=== Probe D: pre-reset artifact scan of live outputs/ ===")
    print("as-of:", datetime.datetime.now().astimezone().isoformat())
    print("reset boundary:", RESET)

    # ---- archive-side positive control, BEFORE the live scan ----
    print("\n--- positive control: same detectors over the relocated artifacts ---")
    tot_pre = tot_trip = 0
    for root in CONTROL_ROOTS:
        print("control root:", root)
        hits, n_json, _nm, n_pre, n_trip = scan(root, use_mtime=False)
        print(f"  {n_json} json files; manifest-pre-reset hits: {n_pre}; "
              f"tripwire-content detections: {n_trip}")
        show(hits)
        tot_pre += n_pre
        tot_trip += n_trip
    if tot_pre >= 1 and tot_trip >= 1:
        print(f"\nCONTROL PASS: manifest detector fired ({tot_pre}) AND tripwire "
              f"content detector fired ({tot_trip}) on the control roots")
    else:
        print(f"\nCONTROL FAILURE (manifest hits={tot_pre}, tripwire detections="
              f"{tot_trip}) — any clean live scan below is INVALID")
        return 3

    # ---- live scan ----
    print("\n--- live scan:", OUTPUTS, "---")
    hits, n_json, n_manifest, _np, _nt = scan(OUTPUTS, use_mtime=True)
    print(f"\nscanned: {n_json} json files; {n_manifest} carried a manifest")
    if hits:
        print(f"\nPOSITIVE HITS: {len(hits)}")
        show(hits)
    else:
        print("\nNO HITS — witnessed-clean (control fired in this same run)")
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
