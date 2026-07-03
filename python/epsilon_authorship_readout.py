#!/usr/bin/env python3
"""
OQ-78 standing ε-authorship readout (OQ-205 spec §8 — the first customer).

Converts the OQ-78 fingerprint from a one-off census into a standing per-run
readout: per-Author/Route-stratum mode fraction, distinct-value count,
last-digit histogram, and exactly-at-threshold count. Pure JSON — no swipl:
reads the pipeline provenance emission (per_constraint.epsilon_provenance +
base_extractiveness, U6) and joins the stability flags
(outputs/epsilon_stability_results.json, U7) where fresh.

STRATIFICATION UNDER NO-BACKFILL (operator ruling 2026-07-03): where
epsilon_provenance is absent (the whole pre-build corpus) the Author stratum
derives AT READ TIME from the emission's author_derived field
(story_provenance/8 arg7, stamped by json_report) — a read-time derivation,
never a file edit — so the model-specific-rail readout (flash .x5/.x0 vs
.x8/.x2) works day one. Where story_provenance is also absent (kernel_v1
authors none on disk) the stratum degrades to the counted `unknown_author`,
never an error, so the all-legs audit passes through the provenance-less
archive. The epsilon_provenance-authored stratum takes over as generation
proceeds.

Usage:
    python3 python/epsilon_authorship_readout.py
        [--pipeline outputs/pipeline_output.json]
        [--stability outputs/epsilon_stability_results.json]
        [--out-prefix outputs/epsilon_authorship_readout]
"""

import argparse
import json
import sys
from collections import Counter, defaultdict
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "python"))
from corpus_hash import compute_corpus_hash  # noqa: E402

THRESHOLDS = [0.10, 0.25, 0.30, 0.45, 0.46]


def stratum_of(entry):
    """(stratum_kind, author, route) from the U6 emission object."""
    ep = entry.get("epsilon_provenance")
    if not isinstance(ep, dict):
        # pre-U6 pipeline JSON: absence of the field itself — counted, loud
        return ("emission_missing", "emission_missing", None)
    if ep.get("status") == "authored":
        return ("authored", ep.get("author") or "unknown_author", ep.get("route"))
    return ("derived" if ep.get("author_derived_basis") == "derived(story_provenance)"
            else "unknown_author",
            ep.get("author_derived") or "unknown_author", None)


def last_digit(eps):
    """Last digit of the two-decimal authored value (the OQ-78 rail axis)."""
    return int(round(eps * 100)) % 10


def analyze(per_constraint, stability_by_id, stability_state):
    strata = defaultdict(list)
    for e in per_constraint:
        eps = e.get("base_extractiveness")
        if eps is None:
            continue  # the no-ε stratum (contradictions meta-files) — counted below
        kind, author, route = stratum_of(e)
        strata[(kind, author, route)].append((e["id"], float(eps)))

    out = []
    for (kind, author, route), members in sorted(strata.items(),
                                                 key=lambda kv: -len(kv[1])):
        vals = [v for _, v in members]
        n = len(vals)
        counts = Counter(round(v, 6) for v in vals)
        mode_val, mode_n = counts.most_common(1)[0]
        digits = Counter(last_digit(v) for v in vals)
        at_thr = sum(1 for v in vals
                     if any(abs(v - t) < 1e-9 for t in THRESHOLDS))
        flags = Counter()
        if stability_state == "fresh":
            for cid, _ in members:
                for f in stability_by_id.get(cid, {}).get("flags", []):
                    flags[f] += 1
        out.append({
            "stratum": kind,
            "author": author,
            "route": route,
            "n": n,
            "mode_value": mode_val,
            "mode_count": mode_n,
            "mode_fraction": round(mode_n / n, 4),
            "distinct_values": len(counts),
            "last_digit_histogram": {str(d): digits[d] for d in sorted(digits)},
            "exactly_at_threshold": at_thr,
            "stability_flags": dict(flags) if stability_state == "fresh" else None,
        })
    return out


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--pipeline", default=str(ROOT / "outputs" / "pipeline_output.json"))
    ap.add_argument("--stability", default=str(ROOT / "outputs" / "epsilon_stability_results.json"))
    ap.add_argument("--out-prefix", default=str(ROOT / "outputs" / "epsilon_authorship_readout"))
    a = ap.parse_args()

    pipe = json.loads(Path(a.pipeline).read_text(encoding="utf-8"))
    per = pipe["per_constraint"]
    if isinstance(per, dict):
        per = list(per.values())
    manifest = pipe.get("manifest", {})

    # Stability join — OQ-29 posture: absent/stale is SURFACED, never silently
    # rendered (the flag columns become null, with the reason recorded).
    stability_by_id, stability_state, stability_reason = {}, "absent", None
    spath = Path(a.stability)
    if spath.exists():
        sdata = json.loads(spath.read_text(encoding="utf-8"))
        stored = sdata.get("corpus_hash")
        current = compute_corpus_hash(ROOT / "prolog" / "testsets")
        if stored != current:
            stability_state = "stale"
            stability_reason = f"corpus_hash {stored} != current {current}"
        else:
            stability_state = "fresh"
            stability_by_id = {e["id"]: e for e in sdata.get("per_constraint", [])}

    no_eps = [e["id"] for e in per if e.get("base_extractiveness") is None]
    rows = analyze(per, stability_by_id, stability_state)

    result = {
        "pipeline_manifest": {k: manifest.get(k) for k in
                              ("pipeline_run_at", "n_constraints",
                               "code_commit_short", "corpus_path")},
        "stability_join": {"state": stability_state, "reason": stability_reason},
        "n_no_epsilon": len(no_eps),
        "no_epsilon_ids": no_eps,
        "strata": rows,
    }
    out_json = Path(a.out_prefix + ".json")
    out_json.parent.mkdir(parents=True, exist_ok=True)
    out_json.write_text(json.dumps(result, indent=2), encoding="utf-8")

    # Markdown twin
    lines = ["# ε-authorship standing readout (OQ-78 / OQ-205 §8)", ""]
    lines.append(f"Pipeline run: {manifest.get('pipeline_run_at')} "
                 f"(n={manifest.get('n_constraints')}, "
                 f"code {manifest.get('code_commit_short')})")
    lines.append(f"Stability join: {stability_state}"
                 + (f" — {stability_reason}" if stability_reason else ""))
    lines.append(f"No-ε stratum: {len(no_eps)} (counted, measured-empty)")
    lines.append("")
    lines.append("| stratum | author | route | n | mode | mode frac | distinct | at-threshold | last digits |")
    lines.append("|---|---|---|---|---|---|---|---|---|")
    for r in rows:
        digs = " ".join(f"{d}:{c}" for d, c in r["last_digit_histogram"].items())
        lines.append(
            f"| {r['stratum']} | {r['author']} | {r['route'] or '—'} | {r['n']} "
            f"| {r['mode_value']} × {r['mode_count']} | {r['mode_fraction']:.1%} "
            f"| {r['distinct_values']} | {r['exactly_at_threshold']} | {digs} |")
    Path(a.out_prefix + ".md").write_text("\n".join(lines) + "\n", encoding="utf-8")

    for r in rows:
        print(f"{r['stratum']:16} {str(r['author']):40.40} n={r['n']:<5} "
              f"mode={r['mode_value']}×{r['mode_count']} ({r['mode_fraction']:.1%}) "
              f"distinct={r['distinct_values']} at_thr={r['exactly_at_threshold']}")
    print(f"no_epsilon={len(no_eps)}  stability_join={stability_state}")
    print(f"wrote {out_json} and {a.out_prefix}.md")


if __name__ == "__main__":
    main()
