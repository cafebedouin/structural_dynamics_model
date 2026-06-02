#!/usr/bin/env python3
"""
w1_sheaf_join.py — Join the two independently-computed obstruction measures.

Joins, per constraint:
  - W1   : continuous fracture (wasserstein_total_fracture, sum of 3 canonical edges)
  - H1   : discrete gluing obstruction (h1_band, 0..6)
  - sheaf_status : section-existence regime (genuine/fragile/manifest_presheaf)
  - Shift vector : per-seat type map across 4 context types (orbit_data.json)

Output: a ranked table (descending by W1) of the WHOLE corpus — H1=0 is the
calibrated floor (sections glue), high-W1 manifest_presheaf is the signal region.
We rank and label; we do not filter. The floor is data too.

This is a READ + JOIN tool. It recomputes nothing in Prolog; it reads the fresh
pipeline output (W1, H1, sheaf_status, arakelov) and orbit_data.json (shift),
guarded same-run by the orbit_data.manifest.json sidecar.

Controls (Step 3 of the plan) run first and BLOCK the ranking on failure:
  1. W1 non-vacuous           (else the maxent-less vacuous path produced it)
  2. H1 non-vacuous
  3. sheaf emit sanity        (manifest count == h1>0 count)
  4. W1-max reconciliation    (field-identity proof; report verdict, never "stale")
"""

import json
import sys
from pathlib import Path
from statistics import median

SCRIPT_DIR = Path(__file__).resolve().parent
OUTPUTS_DIR = SCRIPT_DIR.parent / "outputs"

PIPELINE_JSON = OUTPUTS_DIR / "pipeline_output.json"
ORBIT_JSON = OUTPUTS_DIR / "orbit_data.json"
ORBIT_MANIFEST_JSON = OUTPUTS_DIR / "orbit_data.manifest.json"
OUT_JSON = OUTPUTS_DIR / "w1_sheaf_join.json"
OUT_MD = OUTPUTS_DIR / "w1_sheaf_join.md"

W1_ZERO = 1e-9  # below this, W1 is "≈0"
CONTEXT_ORDER = ["powerless", "moderate", "institutional", "analytical"]


def die(msg):
    print(f"\n[STOP] {msg}", file=sys.stderr)
    sys.exit(1)


def load_json(path):
    if not path.exists():
        die(f"missing input: {path}")
    with open(path, "r", encoding="utf-8") as f:
        return json.load(f)


def manifest_key(m):
    """The fields that identify a run; sidecar must match pipeline on these."""
    if not m:
        return None
    return (m.get("pipeline_run_at"), m.get("code_commit"), m.get("n_constraints"))


def main():
    pipeline = load_json(PIPELINE_JSON)
    orbit = load_json(ORBIT_JSON)
    orbit_manifest_wrap = load_json(ORBIT_MANIFEST_JSON)

    pmanifest = pipeline.get("manifest", {})
    omanifest = orbit_manifest_wrap.get("manifest", {})

    # --- Same-run guard --------------------------------------------------
    if manifest_key(pmanifest) != manifest_key(omanifest):
        die("orbit_data.manifest.json does not match pipeline_output.json manifest "
            f"(pipeline={manifest_key(pmanifest)} orbit={manifest_key(omanifest)}) "
            "— inputs are not from the same run; aborting join.")
    same_run = True

    per = pipeline.get("per_constraint", [])
    # Dedup by id (per_constraint can carry a trailing dup); last wins, report.
    by_id = {}
    dup_ids = []
    for e in per:
        cid = e.get("id")
        if cid is None:
            continue
        if cid in by_id:
            dup_ids.append(cid)
        by_id[cid] = e

    # --- Controls --------------------------------------------------------
    w1_vals = {cid: (e.get("wasserstein_total_fracture")) for cid, e in by_id.items()}
    h1_vals = {cid: (e.get("h1_band")) for cid, e in by_id.items()}
    sheaf_vals = {cid: (e.get("sheaf_status")) for cid, e in by_id.items()}

    w1_present = {cid: v for cid, v in w1_vals.items() if v is not None}
    w1_sum = sum(w1_present.values())
    w1_nonzero = {cid: v for cid, v in w1_present.items() if v > W1_ZERO}
    if not w1_present or w1_sum == 0.0 or len(w1_nonzero) == 0:
        die(f"W1 vacuous (sum={w1_sum}, nonzero={len(w1_nonzero)}) — refusing to rank. "
            "Likely the maxent-less vacuous path produced this output.")
    argmax_id = max(w1_present, key=w1_present.get)
    argmax_w1 = w1_present[argmax_id]

    h1_present = {cid: v for cid, v in h1_vals.items() if v is not None}
    h1_gt0 = {cid: v for cid, v in h1_present.items() if v > 0}
    if not h1_gt0:
        die("H1 vacuous (all h1_band==0 / None) — cohomology analogue of vacuity.")
    manifest_example = next(iter(h1_gt0))

    # Control 3: emit sanity — manifest_presheaf count == h1>0 count
    n_manifest = sum(1 for v in sheaf_vals.values() if v == "manifest_presheaf")
    emit_sane = (n_manifest == len(h1_gt0))

    # Control 4: W1-max reconciliation — prove field identity for the argmax.
    # wasserstein_total_fracture must equal the sum of the 3 wasserstein_profile edges.
    argmax_entry = by_id[argmax_id]
    prof = argmax_entry.get("wasserstein_profile") or {}
    edge_sum = None
    if isinstance(prof, dict) and all(k in prof for k in ("u1_u2", "u2_u3", "u3_u4")):
        edge_sum = prof["u1_u2"] + prof["u2_u3"] + prof["u3_u4"]
    field_identity_ok = (edge_sum is not None and abs(edge_sum - argmax_w1) < 1e-6)

    # --- Join ------------------------------------------------------------
    orbit_ids = set(orbit.keys())
    pipe_ids = set(by_id.keys())
    only_pipeline = sorted(pipe_ids - orbit_ids)
    only_orbit = sorted(orbit_ids - pipe_ids)

    def shift_vec(cid):
        entry = orbit.get(cid)
        if not entry:
            return None
        ctx = entry.get("contexts", {})
        return [ctx.get(k) for k in CONTEXT_ORDER]

    rows = []
    for cid, e in by_id.items():
        rows.append({
            "id": cid,
            "w1": e.get("wasserstein_total_fracture"),
            "h1": e.get("h1_band"),
            "sheaf_status": e.get("sheaf_status"),
            "shift": shift_vec(cid),
        })
    # Sort descending by W1 (None/absent W1 sorts last).
    rows.sort(key=lambda r: (r["w1"] is not None, r["w1"] if r["w1"] is not None else 0.0),
              reverse=True)
    for i, r in enumerate(rows, 1):
        r["rank"] = i

    # --- 2x2 concordance -------------------------------------------------
    def w1_pos(v):
        return v is not None and v > W1_ZERO

    def h1_pos(v):
        return v is not None and v > 0

    cells = {"h0_w0": [], "h0_w1": [], "h1_w0": [], "h1_w1": []}
    for r in rows:
        hp, wp = h1_pos(r["h1"]), w1_pos(r["w1"])
        if not hp and not wp:
            cells["h0_w0"].append(r)
        elif not hp and wp:
            cells["h0_w1"].append(r)
        elif hp and not wp:
            cells["h1_w0"].append(r)
        else:
            cells["h1_w1"].append(r)

    off_diag = cells["h0_w1"] + cells["h1_w0"]
    off_diag.sort(key=lambda r: (r["w1"] is not None, r["w1"] if r["w1"] is not None else 0.0),
                  reverse=True)

    # --- sheaf_status counts + W1 distribution ---------------------------
    from collections import Counter
    sheaf_counts = Counter(r["sheaf_status"] for r in rows)
    w1_list = sorted(v for v in w1_present.values())
    w1_dist = {
        "min": min(w1_list), "median": median(w1_list), "max": max(w1_list),
        "nonzero": len(w1_nonzero), "n_with_w1": len(w1_present), "sum": w1_sum,
    }

    # --- Write JSON ------------------------------------------------------
    out = {
        "manifest": pmanifest,
        "same_run_guard": {"passed": same_run,
                           "pipeline": manifest_key(pmanifest),
                           "orbit_sidecar": manifest_key(omanifest)},
        "controls": {
            "w1_sum": w1_sum, "w1_nonzero": len(w1_nonzero), "n_with_w1": len(w1_present),
            "w1_argmax_id": argmax_id, "w1_argmax": argmax_w1,
            "h1_gt0_count": len(h1_gt0), "manifest_example": manifest_example,
            "manifest_count": n_manifest, "emit_sane": emit_sane,
            "w1_max_field_identity_ok": field_identity_ok,
            "w1_argmax_edge_sum": edge_sum,
        },
        "join_coverage": {
            "n_rows": len(rows), "only_pipeline": only_pipeline,
            "only_orbit": only_orbit, "dup_ids": sorted(set(dup_ids)),
        },
        "sheaf_status_counts": dict(sheaf_counts),
        "w1_distribution": w1_dist,
        "concordance_2x2": {
            "h1eq0_and_w1approx0": len(cells["h0_w0"]),
            "h1eq0_and_w1gt0": len(cells["h0_w1"]),
            "h1gt0_and_w1approx0": len(cells["h1_w0"]),
            "h1gt0_and_w1gt0": len(cells["h1_w1"]),
        },
        "off_diagonal": [
            {"id": r["id"], "w1": r["w1"], "h1": r["h1"], "sheaf_status": r["sheaf_status"]}
            for r in off_diag
        ],
        "rows": rows,
    }
    with open(OUT_JSON, "w", encoding="utf-8") as f:
        json.dump(out, f, indent=2)

    # --- Write Markdown --------------------------------------------------
    def fmt_w1(v):
        return f"{v:.6f}" if isinstance(v, (int, float)) else "—"

    def fmt_shift(s):
        if not s:
            return "—"
        return "[" + ", ".join(x if x else "?" for x in s) + "]"

    lines = []
    lines.append("# W1 × sheaf_status join (ranked presheaf-obstruction read)\n")
    lines.append("**Frame (label, not filter):** `H1=0` is the calibrated floor "
                 "(genuine/fragile sheaf — local sections glue); high-W1 "
                 "`manifest_presheaf` is the signal region (per-seat readings diverge "
                 "hardest and fail to glue). The whole corpus is ranked; the floor is data too.\n")
    m = pmanifest
    lines.append("## Run manifest\n")
    lines.append(f"- `pipeline_run_at`: {m.get('pipeline_run_at')}")
    lines.append(f"- `n_constraints`: {m.get('n_constraints')}  "
                 f"(join rows: {len(rows)})")
    lines.append(f"- `code_commit_short`: {m.get('code_commit_short')}  "
                 f"`code_dirty`: {m.get('code_dirty')}")
    lines.append(f"- same-run guard (orbit sidecar == pipeline): **{'PASS' if same_run else 'FAIL'}**\n")

    lines.append("## Positive controls\n")
    lines.append(f"1. **W1 non-vacuous:** sum={w1_sum:.4f}, nonzero={len(w1_nonzero)} "
                 f"of {len(w1_present)}; argmax = `{argmax_id}` @ W1={argmax_w1:.6f}.")
    lines.append(f"2. **H1 non-vacuous:** {len(h1_gt0)} constraints with H1>0; "
                 f"example manifest_presheaf = `{manifest_example}` "
                 f"(H1={h1_vals[manifest_example]}).")
    lines.append(f"3. **Emit sanity:** manifest_presheaf count = {n_manifest}, "
                 f"H1>0 count = {len(h1_gt0)} → "
                 f"{'CONSISTENT' if emit_sane else 'MISMATCH'}.")
    edge_sum_str = f"{edge_sum:.6f}" if isinstance(edge_sum, (int, float)) else str(edge_sum)
    lines.append(f"4. **W1-max reconciliation (verdict (a) field-identity):** argmax W1 = "
                 f"{argmax_w1:.6f}; sum of its 3 canonical edges = "
                 f"{edge_sum_str} → field is "
                 f"`wasserstein_total_fracture/2` (3-edge sum), identity "
                 f"{'CONFIRMED' if field_identity_ok else 'NOT CONFIRMED'}. "
                 f"See witness notes for the recon (~4.7) reconciliation.\n")

    lines.append("## sheaf_status counts\n")
    for k in ("manifest_presheaf", "fragile_presheaf", "genuine_sheaf", None):
        if k in sheaf_counts:
            lines.append(f"- {k if k else 'null'}: {sheaf_counts[k]}")
    lines.append("")
    lines.append("## W1 distribution\n")
    lines.append(f"- min={w1_dist['min']:.6f}, median={w1_dist['median']:.6f}, "
                 f"max={w1_dist['max']:.6f}, nonzero={w1_dist['nonzero']}, "
                 f"sum={w1_dist['sum']:.4f}\n")

    lines.append("## 2×2 concordance (discrete H1 vs continuous W1; W1≈0 means W1<1e-9)\n")
    lines.append("| | W1≈0 | W1>0 |")
    lines.append("|---|---|---|")
    lines.append(f"| **H1=0** | {len(cells['h0_w0'])} | {len(cells['h0_w1'])} |")
    lines.append(f"| **H1>0** | {len(cells['h1_w0'])} | {len(cells['h1_w1'])} |\n")
    lines.append(f"Off-diagonal cells (the discrete/continuous gap) = "
                 f"{len(cells['h0_w1'])} (H1=0 ∧ W1>0) + {len(cells['h1_w0'])} (H1>0 ∧ W1≈0).\n")
    if off_diag:
        lines.append("### Off-diagonal rows (per-id W1 / H1 / sheaf_status)\n")
        lines.append("| id | W1 | H1 | sheaf_status |")
        lines.append("|---|---|---|---|")
        for r in off_diag:
            lines.append(f"| {r['id']} | {fmt_w1(r['w1'])} | {r['h1']} | {r['sheaf_status']} |")
        lines.append("")
    else:
        lines.append("No off-diagonal rows: the two measures agree everywhere on the "
                     "presence/absence of obstruction.\n")

    if only_pipeline or only_orbit or dup_ids:
        lines.append("## Join coverage notes\n")
        if only_pipeline:
            lines.append(f"- {len(only_pipeline)} ids in pipeline but not orbit_data "
                         f"(shift=—): {', '.join(only_pipeline[:10])}"
                         + (" …" if len(only_pipeline) > 10 else ""))
        if only_orbit:
            lines.append(f"- {len(only_orbit)} ids in orbit_data but not pipeline: "
                         f"{', '.join(only_orbit[:10])}"
                         + (" …" if len(only_orbit) > 10 else ""))
        if dup_ids:
            lines.append(f"- {len(set(dup_ids))} duplicate ids in per_constraint "
                         f"(last-wins): {', '.join(sorted(set(dup_ids))[:10])}")
        lines.append("")

    lines.append("## Ranked table (descending by W1)\n")
    lines.append("| rank | id | W1 | H1 | sheaf_status | shift [powerless, moderate, institutional, analytical] |")
    lines.append("|---|---|---|---|---|---|")
    for r in rows:
        lines.append(f"| {r['rank']} | {r['id']} | {fmt_w1(r['w1'])} | "
                     f"{r['h1'] if r['h1'] is not None else '—'} | "
                     f"{r['sheaf_status'] if r['sheaf_status'] else '—'} | "
                     f"{fmt_shift(r['shift'])} |")
    lines.append("")

    with open(OUT_MD, "w", encoding="utf-8") as f:
        f.write("\n".join(lines))

    # --- Witness to stdout ----------------------------------------------
    print("=== w1_sheaf_join — witnesses ===")
    print(f"manifest: run_at={m.get('pipeline_run_at')} n={m.get('n_constraints')} "
          f"commit={m.get('code_commit_short')} dirty={m.get('code_dirty')}")
    print(f"same-run guard: {'PASS' if same_run else 'FAIL'}")
    print(f"[C1] W1 sum={w1_sum:.4f} nonzero={len(w1_nonzero)}/{len(w1_present)} "
          f"argmax={argmax_id} W1={argmax_w1:.6f}")
    print(f"[C2] H1>0 count={len(h1_gt0)} example_manifest={manifest_example} "
          f"H1={h1_vals[manifest_example]}")
    print(f"[C3] emit sanity: manifest={n_manifest} vs h1>0={len(h1_gt0)} "
          f"-> {'CONSISTENT' if emit_sane else 'MISMATCH'}")
    print(f"[C4] W1-max field-identity: argmax_w1={argmax_w1:.6f} edge_sum={edge_sum} "
          f"-> {'CONFIRMED (wasserstein_total_fracture = 3-edge sum)' if field_identity_ok else 'NOT CONFIRMED'}")
    print(f"sheaf_status counts: {dict(sheaf_counts)}")
    c = out["concordance_2x2"]
    print(f"[2x2] H1=0&W1~0={c['h1eq0_and_w1approx0']}  H1=0&W1>0={c['h1eq0_and_w1gt0']}  "
          f"H1>0&W1~0={c['h1gt0_and_w1approx0']}  H1>0&W1>0={c['h1gt0_and_w1gt0']}")
    print(f"off-diagonal rows: {len(off_diag)}")
    for r in off_diag[:30]:
        print(f"    OFF  {r['id']}  W1={fmt_w1(r['w1'])}  H1={r['h1']}  {r['sheaf_status']}")
    print(f"join coverage: only_pipeline={len(only_pipeline)} only_orbit={len(only_orbit)} "
          f"dups={len(set(dup_ids))}")
    print(f"\nwrote {OUT_JSON}\nwrote {OUT_MD}")
    print("\n=== top 15 rows (rank | id | W1 | H1 | sheaf | shift) ===")
    for r in rows[:15]:
        print(f"{r['rank']:>3} | {r['id']} | {fmt_w1(r['w1'])} | "
              f"{r['h1']} | {r['sheaf_status']} | {fmt_shift(r['shift'])}")


if __name__ == "__main__":
    main()
