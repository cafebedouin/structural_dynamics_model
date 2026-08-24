#!/usr/bin/env python3
"""leg_diagnostic_table.py — the corpus-level `diagnostic` block across every per-leg
classify_corpus output (2026-08-23, audits/2026-08-23_leg_diagnostic_table/).

Reads outputs/pipeline_output.<leg>.json for every leg directory under prolog/testsets*/
(plus the canonical outputs/pipeline_output.json for `testsets`, flagged because it sits at
an older engine commit), flattens each file's top-level `diagnostic` block into scalar
statistics (shares, per-story rates), and writes:

  outputs/leg_diagnostic_table.tsv      legs x statistics (one row per leg)
  outputs/leg_diagnostic_pairs.tsv      per statistic: within-pair |Δ| on every same-model
                                        redraw pair vs the between-model spread
  outputs/leg_diagnostic_table.json     everything above + per-leg provenance summary

No Prolog is run. Nothing is recomputed from per_constraint except n, so the table is a
READ of what classify_corpus already emitted, not a new measurement.

Leg provenance (model, prompt_commit, sampling) is derived from the story_provenance facts
in the leg's own .pl files — never from the directory name (OQ-78). A redraw pair is
classed PURE when both legs share model, prompt_commit and sampling string on every story;
otherwise the confound is named in the pair row.

Tripwires carried: one file per leg, no in-process corpus loading (OQ-246 does not apply —
we never load a corpus); ε is NOT in this table (per-author rails, never pooled); purity
shares are reported beside their coverage (OQ-236).
"""
from __future__ import annotations

import collections
import json
import re
import statistics
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
OUT = REPO / "outputs"
PROLOG = REPO / "prolog"

PROV_RE = re.compile(
    r"story_provenance\(\s*([^,]+),\s*'([^']*)',\s*'([^']*)',\s*'([^']*)',\s*'([^']*)',"
    r"\s*'([^']*)',\s*'([^']*)',\s*'([^']*)'\s*\)", re.S)

TYPES = ["mountain", "piton", "rope", "scaffold", "snare", "tangled_rope", "unknown"]
PURITY = ["pristine", "sound", "borderline", "contaminated", "degraded"]
COUPLING = ["independent", "weakly_coupled", "strongly_coupled", "nonsensically_coupled", "inconclusive"]
BOLTZ = ["compliant", "non_compliant", "inconclusive"]
MONO = ["constant", "ascending", "descending", "non_monotone", "incomparable"]


def leg_dirs():
    return sorted(p.name for p in PROLOG.glob("testsets*") if p.is_dir())


def output_for(leg: str) -> Path | None:
    if leg == "testsets":
        p = OUT / "pipeline_output.json"
    else:
        p = OUT / f"pipeline_output.{leg[len('testsets_'):]}.json"
    return p if p.exists() else None


def provenance(leg: str) -> dict:
    """Per-leg summary of story_provenance facts: model / prompt_commit / sampling / source."""
    models, prompts, sampling, sources = (collections.Counter() for _ in range(4))
    n_files = 0
    for f in (PROLOG / leg).glob("*.pl"):
        n_files += 1
        m = PROV_RE.search(f.read_text(encoding="utf-8", errors="replace"))
        if not m:
            models["<no story_provenance>"] += 1
            continue
        _cid, prompt, _schema, _date, source, _example, model, samp = m.groups()
        models[model] += 1
        prompts[prompt[:8]] += 1
        sampling[samp] += 1
        sources[source] += 1
    return {
        "n_files": n_files,
        "models": dict(models.most_common()),
        "prompt_commits": dict(prompts.most_common()),
        "sampling": dict(sampling.most_common()),
        "sources": dict(sources.most_common()),
    }


def flatten(diag: dict, n: int) -> dict:
    """Scalar view of the diagnostic block. Shares are over the block's own denominators."""
    row = {}
    td = diag.get("type_distribution", {})
    tot = sum(td.values()) or 1
    for t in TYPES:
        row[f"type.{t}"] = td.get(t, 0) / tot
    ps = diag.get("purity_summary", {})
    scored = diag.get("purity_n_scored") or sum(ps.values()) or 1
    row["purity.coverage"] = (diag.get("purity_n_scored") or 0) / (diag.get("purity_n_total") or n or 1)
    row["purity.n_no_data"] = diag.get("purity_n_no_data", 0)
    row["purity.n_gate_fail"] = diag.get("purity_n_gate_fail", 0)
    for b in PURITY:
        row[f"purity.{b}"] = ps.get(b, 0) / scored
    cs = diag.get("coupling_summary", {})
    ctot = sum(cs.values()) or 1
    for c in COUPLING:
        row[f"coupling.{c}"] = cs.get(c, 0) / ctot
    bs = diag.get("boltzmann_summary", {})
    btot = sum(bs.values()) or 1
    for b in BOLTZ:
        row[f"boltzmann.{b}"] = bs.get(b, 0) / btot
    de = diag.get("drift_event_counts", {})
    for k in ("critical", "warning", "watch"):
        row[f"drift_events_per_story.{k}"] = de.get(k, 0) / (n or 1)
    row["network.stability"] = diag.get("network_stability")
    row["network.drifting_share"] = (diag.get("network_n_drifting") or 0) / (n or 1)
    row["network.severe_share"] = (diag.get("network_n_severe") or 0) / (n or 1)
    row["network.cascade_threshold"] = diag.get("network_cascade_count_threshold")
    row["wasserstein.fracture_total"] = diag.get("corpus_wasserstein_fracture")
    row["wasserstein.fracture_per_story"] = (diag.get("corpus_wasserstein_fracture") or 0) / (n or 1)
    row["arakelov.threshold"] = diag.get("arakelov_threshold")
    cx = diag.get("contextuality", {})
    row["contextuality.corpus_fraction"] = cx.get("corpus_fraction")
    for t in TYPES:
        row[f"contextuality.by_type.{t}"] = cx.get("by_type", {}).get(t)
    mo = diag.get("monotonicity", {})
    mtot = sum(mo.get(k, 0) for k in MONO) or 1
    for k in MONO:
        row[f"monotonicity.{k}"] = mo.get(k, 0) / mtot
    bd = mo.get("boundary_distribution", {})
    for k in ("pos_1", "pos_2", "pos_3"):
        row[f"monotonicity.boundary.{k}_per_story"] = bd.get(k, 0) / (n or 1)
    sbt = diag.get("severity_by_type", {})
    for t in TYPES:
        st = sbt.get(t)
        if st and (td.get(t) or 0) > 0:
            row[f"severe_share_within_type.{t}"] = st.get("severe", 0) / td[t]
        else:
            row[f"severe_share_within_type.{t}"] = None
    return row


def main() -> int:
    legs = []
    for leg in leg_dirs():
        p = output_for(leg)
        if p is None:
            print(f"[skip] {leg}: no classify output", file=sys.stderr)
            continue
        d = json.loads(p.read_text(encoding="utf-8"))
        man = d["manifest"]
        diag = d.get("diagnostic")
        if not isinstance(diag, dict):
            print(f"[skip] {leg}: no diagnostic block", file=sys.stderr)
            continue
        n = man.get("n_stories") or man.get("n_constraints")
        prov = provenance(leg)
        if prov["n_files"] != man.get("n_constraints"):
            print(f"[note] {leg}: on-disk files {prov['n_files']} != manifest n_constraints "
                  f"{man.get('n_constraints')} (output predates the current directory state)",
                  file=sys.stderr)
        legs.append({
            "leg": leg,
            "output": p.name,
            "code_commit_short": man.get("code_commit_short"),
            "run_at": man.get("pipeline_run_at"),
            "n_constraints": man.get("n_constraints"),
            "n_stories": man.get("n_stories"),
            "n_files_now": prov["n_files"],
            "model": max(prov["models"], key=prov["models"].get) if prov["models"] else None,
            "model_mix": prov["models"],
            "prompt_commits": prov["prompt_commits"],
            "sampling": prov["sampling"],
            "sources": prov["sources"],
            "stats": flatten(diag, n),
        })

    stat_names = list(legs[0]["stats"].keys())
    # ---- table
    tsv = OUT / "leg_diagnostic_table.tsv"
    with tsv.open("w") as fh:
        fh.write("\t".join(["leg", "model", "commit", "n_stories"] + stat_names) + "\n")
        for L in legs:
            vals = []
            for s in stat_names:
                v = L["stats"][s]
                vals.append("" if v is None else (f"{v:.4f}" if isinstance(v, float) else str(v)))
            fh.write("\t".join([L["leg"], str(L["model"]), str(L["code_commit_short"]),
                                str(L["n_stories"])] + vals) + "\n")

    # ---- pairs: every pair of legs sharing a dominant model
    by_leg = {L["leg"]: L for L in legs}
    pairs = []
    names = [L["leg"] for L in legs if L["leg"] != "testsets"]
    for i, a in enumerate(names):
        for b in names[i + 1:]:
            A, B = by_leg[a], by_leg[b]
            if A["model"] != B["model"]:
                continue
            same_prompt = set(A["prompt_commits"]) == set(B["prompt_commits"]) and len(A["prompt_commits"]) == 1
            same_sampling = set(A["sampling"]) == set(B["sampling"]) and len(A["sampling"]) == 1
            conf = []
            if not same_prompt:
                conf.append("prompt")
            if not same_sampling:
                conf.append("sampling")
            pairs.append({"a": a, "b": b, "model": A["model"],
                          "kind": "pure" if not conf else "confounded:" + "+".join(conf)})

    numeric = [s for s in stat_names if all(isinstance(L["stats"][s], (int, float)) for L in legs)]
    # between-model spread: one representative leg per (model, sampling-regime) — the first
    # leg of each model whose sampling is the model's majority regime; spread = max - min.
    reps = {}
    for L in legs:
        if L["leg"] == "testsets":
            continue
        key = (L["model"], next(iter(L["sampling"])) if L["sampling"] else None)
        reps.setdefault(key, L)
    pair_rows = []
    for s in numeric:
        vals_between = [R["stats"][s] for R in reps.values()]
        between = (max(vals_between) - min(vals_between)) if vals_between else None
        within_pure = []
        within_conf = []
        for P in pairs:
            d = abs(by_leg[P["a"]]["stats"][s] - by_leg[P["b"]]["stats"][s])
            (within_pure if P["kind"] == "pure" else within_conf).append(d)
        row = {
            "stat": s,
            "between_model_spread": between,
            "n_pure_pairs": len(within_pure),
            "within_pure_max": max(within_pure) if within_pure else None,
            "within_pure_median": statistics.median(within_pure) if within_pure else None,
            "within_confounded_max": max(within_conf) if within_conf else None,
        }
        if between is not None and within_pure:
            row["ratio_between_over_within_max"] = (between / row["within_pure_max"]) if row["within_pure_max"] > 0 else float("inf")
        else:
            row["ratio_between_over_within_max"] = None
        pair_rows.append(row)
    ptsv = OUT / "leg_diagnostic_pairs.tsv"
    with ptsv.open("w") as fh:
        cols = ["stat", "between_model_spread", "n_pure_pairs", "within_pure_max",
                "within_pure_median", "within_confounded_max", "ratio_between_over_within_max"]
        fh.write("\t".join(cols) + "\n")
        for r in sorted(pair_rows, key=lambda r: -(r["ratio_between_over_within_max"] or -1)):
            fh.write("\t".join("" if r[c] is None else (f"{r[c]:.4f}" if isinstance(r[c], float) else str(r[c])) for c in cols) + "\n")

    (OUT / "leg_diagnostic_table.json").write_text(json.dumps({
        "legs": legs, "pairs": pairs, "pair_stats": pair_rows,
        "between_model_representatives": {f"{k[0]}|{k[1]}": R["leg"] for k, R in reps.items()},
    }, indent=1, default=str), encoding="utf-8")

    print(f"legs={len(legs)} stats={len(stat_names)} numeric={len(numeric)} "
          f"same-model pairs={len(pairs)} (pure={sum(p['kind']=='pure' for p in pairs)})")
    for P in pairs:
        print(f"  pair {P['a']} ~ {P['b']}  [{P['model']}]  {P['kind']}")
    print(f"wrote {tsv}, {ptsv}, {OUT / 'leg_diagnostic_table.json'}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
