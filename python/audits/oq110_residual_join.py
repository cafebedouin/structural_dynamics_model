#!/usr/bin/env python3
"""OQ-110 §1.2 — offline residual join: observer flip-events × committer drift stages.

Observer side: per-constraint `temporal_residual` blocks from outputs/pipeline_output.json
(flip_events with t1/t2/from/to/d_eps/d_supp/d_theater). Committer side: cs_reference_frame /
cs_drift_moment / cs_drift_gap / cs_drift_terminal from the same file (id-keyed; the
id↔story_uid bridge happened at serialization, json_report.pl:572-592). The join is therefore
purely offline — hub separation preserved (v7 Theorem 7).

Coverage is carried to the read site (Build Discipline Pattern 6): every aggregate prints with
its denominator; the four buckets (both / flips_only / stages_only / neither) partition n and
the partition sum is checked. Committer stages are NAMED moments (atoms), not integer times —
no numeric moment↔t mapping exists, so stage alignment is reported at presence level plus
gap/terminal descriptors, never a fabricated time mapping.

OQ-105 cross-read: misaligned suppression rows are re-derived from the metric grids
(outputs/oq110_metric_grids.json, emitted by metric_grids_export.pl): a row t is misaligned iff
a suppression series exists and t lacks a suppression measurement (the SuppBacked=false
condition, drl_composition.pl:219-224, minus the static-marker sanction). Counted flips are
checked: ON a misaligned row (must be impossible — backed endpoints) and ADJACENT to one.

Positive control: a planted flip in a deep copy must move the coverage partition and appear in
the inventory before the real numbers are trusted.

Run from repo root: python3 python/audits/oq110_residual_join.py
Writes: outputs/oq110_residual_join.json
"""
import copy
import json
import sys
from datetime import datetime, timezone

PIPE = "outputs/pipeline_output.json"
GRIDS = "outputs/oq110_metric_grids.json"
OUT = "outputs/oq110_residual_join.json"

STAGE_KEYS = ("cs_reference_frame", "cs_drift_moment", "cs_drift_gap", "cs_drift_terminal")

# OQ-105 census of record (filed 2026-06-11, 48-file corpus): 21 rows / 10 constraints.
OQ105_NAMED_10 = {
    "agenda_conditioning", "digital_colonialism_data_extraction", "post_1998_convergence",
    "scale_ceiling", "substantive_employment_reading", "techno_optimist_reading",
    "technocratic_paradigm_vs_human_primacy", "truth_democracy_disinformation",
    "wage_convergence_mechanism", "wage_convergence_sustainability",
}

# Inherited flags (plan §1.2 / OQ-83 Step 1) — carried VERBATIM; all three constraints are
# kernel_v2_test-regime and NOT in the live corpus (checked on disk 2026-06-11), so none can
# touch a live counted flip.
INHERITED_FLAGS = [
    {"constraint": "clinical_deskilling_automation", "flag": "0->2 = documented exclusion",
     "in_live_corpus": False},
    {"constraint": "milblogger_legitimacy_erosion", "flag": "12->18 = clean",
     "in_live_corpus": False},
    {"constraint": "challenge_as_commons_maintenance",
     "flag": "T=5 unflagged (known eps-sourcing mismatch touching no counted flip)",
     "in_live_corpus": False},
]


def flips_of(entry):
    """[(context, flip_event), ...] for one per_constraint entry."""
    out = []
    for ctx, blk in (entry.get("temporal_residual") or {}).items():
        for fe in blk.get("flip_events", []):
            out.append((ctx, fe))
    return out


def fab_of(entry):
    return sum(blk.get("fabrication_adjacent_transitions", 0)
               for blk in (entry.get("temporal_residual") or {}).values())


def has_stages(entry):
    return any(entry.get(k) for k in STAGE_KEYS)


def coverage(per_constraint):
    buckets = {"both": [], "flips_only": [], "stages_only": [], "neither": []}
    for e in per_constraint:
        f, s = bool(flips_of(e)), has_stages(e)
        key = ("both" if f and s else "flips_only" if f else
               "stages_only" if s else "neither")
        buckets[key].append(e["id"])
    return buckets


def misaligned_supp_rows(grids):
    """{cid: [t, ...]} — suppression series exists, t lacks a suppression measurement.
    The static-marker sanction (OQ-46) cannot apply when a series exists, so no exclusion."""
    rows = {}
    for cid, g in grids.items():
        if g["supp_times"]:
            miss = [t for t in g["times"] if t not in set(g["supp_times"])]
            if miss:
                rows[cid] = miss
    return rows


def main():
    pipe = json.load(open(PIPE))
    grids = json.load(open(GRIDS))
    manifest = pipe["manifest"]
    pc = pipe["per_constraint"]
    n = len(pc)
    if n != manifest["n_constraints"]:
        print("HALT: per_constraint length %d != manifest n_constraints %s"
              % (n, manifest["n_constraints"]))
        sys.exit(1)

    # ---- positive control: planted flip must move the partition ----
    planted = copy.deepcopy(pc)
    victim = next(e for e in planted
                  if (e.get("temporal_residual") or {}) and not flips_of(e))
    ctx0 = next(iter(victim["temporal_residual"]))
    victim["temporal_residual"][ctx0]["flip_events"].append(
        {"t1": 0, "t2": 1, "from": "x", "to": "y",
         "d_eps": 0, "d_supp": 0, "d_theater": 0})
    cov_real = coverage(pc)
    cov_planted = coverage(planted)
    moved = (victim["id"] in cov_planted["both"] + cov_planted["flips_only"]
             and victim["id"] not in cov_real["both"] + cov_real["flips_only"])
    if not moved:
        print("POSITIVE CONTROL FAILED: planted flip in %s did not move coverage"
              % victim["id"])
        sys.exit(1)

    # ---- coverage (partition-sum checked) ----
    sizes = {k: len(v) for k, v in cov_real.items()}
    if sum(sizes.values()) != n:
        print("HALT: coverage buckets do not partition n: %r vs %d" % (sizes, n))
        sys.exit(1)
    no_temporal = sorted(e["id"] for e in pc if not e.get("temporal_residual"))

    # ---- flip inventory ----
    inventory = []
    for e in pc:
        for ctx, fe in flips_of(e):
            inventory.append(dict(id=e["id"], context=ctx, **fe))
    n_flips = len(inventory)
    n_fab = sum(fab_of(e) for e in pc)

    # ---- OQ-105 cross-read ----
    mis = misaligned_supp_rows(grids)
    n_mis_rows = sum(len(v) for v in mis.values())
    live_named = OQ105_NAMED_10 & set(grids)
    rederive_vs_named = {
        "named_10_still_live": sorted(live_named),
        "named_live_rederived": sorted(live_named & set(mis)),
        "rederived_not_in_named_10": sorted(set(mis) - OQ105_NAMED_10),
    }
    on_mis, adj_mis = [], []
    for f in inventory:
        rows = set(mis.get(f["id"], []))
        if not rows:
            continue
        times = grids[f["id"]]["times"]
        idx = {t: i for i, t in enumerate(times)}
        for t in (f["t1"], f["t2"]):
            if t in rows:
                on_mis.append(f)
        neighbors = set()
        for t in (f["t1"], f["t2"]):
            i = idx.get(t)
            if i is not None:
                if i > 0:
                    neighbors.add(times[i - 1])
                if i < len(times) - 1:
                    neighbors.add(times[i + 1])
        if neighbors & rows:
            adj_mis.append(f)
    if on_mis:
        # backed endpoints cannot sit on a SuppBacked=false row — this would mean the
        # Backed gate regressed; 1.1 just verified it, so halt loudly.
        print("HALT: %d counted flip endpoint(s) ON a misaligned row: %r"
              % (len(on_mis), on_mis))
        sys.exit(1)

    # ---- per-constraint join table ----
    per = []
    for e in pc:
        fl = flips_of(e)
        st = {k: e.get(k) for k in STAGE_KEYS}
        st["cs_drift_unacknowledged"] = e.get("cs_drift_unacknowledged")
        if not fl and not has_stages(e):
            continue
        per.append({
            "id": e["id"],
            "flips": [dict(context=ctx, **fe) for ctx, fe in fl],
            "fab_adjacent": fab_of(e),
            "stages": st if has_stages(e) else None,
            "alignment": ("both_surfaces_report_motion" if fl and has_stages(e)
                          else "observer_flips_only" if fl
                          else "committer_stages_only"),
        })

    out = {
        "manifest": manifest,
        "derived_at": datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
        "positive_control": "planted flip in %s moved coverage partition" % victim["id"],
        "coverage": {"n": n, "buckets": sizes, "ids": cov_real,
                     "no_temporal_data": no_temporal},
        "flip_totals": {"context_level_flips": n_flips,
                        "fabrication_adjacent": n_fab,
                        "constraints_with_flips": sizes["both"] + sizes["flips_only"]},
        "oq105_cross_read": {
            "misaligned_supp_rows": mis,
            "n_rows": n_mis_rows, "n_constraints": len(mis),
            "census_of_record_comparison": rederive_vs_named,
            "flips_on_misaligned_row": on_mis,
            "flips_adjacent_to_misaligned_row": adj_mis,
        },
        "inherited_flags": INHERITED_FLAGS,
        "join": per,
    }
    json.dump(out, open(OUT, "w"), indent=1)

    m = manifest
    print("OQ-110 residual join | substrate: run_at=%s commit=%s dirty=%s n=%s"
          % (m["pipeline_run_at"], m["code_commit_short"], m["code_dirty"],
             m["n_constraints"]))
    print("positive control: planted flip in %s moved the partition — detector fires"
          % victim["id"])
    print("coverage (n=%d): both=%d flips_only=%d stages_only=%d neither=%d"
          % (n, sizes["both"], sizes["flips_only"], sizes["stages_only"],
             sizes["neither"]))
    print("  no_temporal_data (exporter-gated null, zero measurement/5): %s"
          % ", ".join(no_temporal))
    print("flips: %d context-level events over %d constraints; fab_adjacent=%d (excluded)"
          % (n_flips, sizes["both"] + sizes["flips_only"], n_fab))
    print("OQ-105: %d misaligned supp rows / %d constraints (census of record: 21/10 on the"
          " 48-file corpus; re-derived on this substrate)" % (n_mis_rows, len(mis)))
    print("  named-10 still live: %d; re-derived hits beyond named-10: %s"
          % (len(live_named),
             ", ".join(rederive_vs_named["rederived_not_in_named_10"]) or "none"))
    print("  flips ON misaligned row: 0 (halt condition, passed)")
    print("  flips ADJACENT to misaligned row: %d" % len(adj_mis))
    for f in adj_mis:
        print("    - %s %s t=%s->%s %s->%s" % (f["id"], f["context"], f["t1"],
                                               f["t2"], f["from"], f["to"]))
    print("inherited flags: %d carried verbatim, none in live corpus" % len(INHERITED_FLAGS))
    print("stage-alignment: committer moments are NAMED atoms — presence-level join only,"
          " no numeric moment-to-t mapping fabricated")
    print("both-surfaces constraints (%d/%d):" % (sizes["both"], n))
    for cid in cov_real["both"]:
        print("    -", cid)
    print("wrote %s" % OUT)


if __name__ == "__main__":
    main()
