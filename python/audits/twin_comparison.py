#!/usr/bin/env python3
"""Twin-model cross-classification comparison harness (Part B2).

Joins 2 twin `pipeline_output.*.json` (same engine + commit, different generating
model) over the LIVE id intersection and adjudicates the pre-registered H1/H2
falsifiers (see audits/2026-06-13_twin_comparison/PRE_REGISTRATION.md). n-agnostic:
keys on the intersection, so it re-runs unchanged as the corpora grow.

A third, disjoint corpus (the mixed essay/control regime) may be supplied via
`--essay`; it contributes per-corpus marginal tables only (labelled not-paired).

Usage:
    python3 python/audits/twin_comparison.py \
        --twin haiku=outputs/pipeline_output.haiku.json \
        --twin flash=outputs/pipeline_output.flash.json \
        [--essay mixed=outputs/pipeline_output.json] \
        --permute 1000 \
        --outdir audits/2026-06-13_twin_comparison

Validity guards (refuse-to-join): schema_version != 2; two inputs sharing
corpus_path; twins differing in code_commit.
"""
import argparse
import json
import math
import random
from collections import Counter
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent.parent
OUTPUTS_DIR = REPO_ROOT / "outputs"

# Structural fields (H1) and continuous fields (H2). Perspective order fixed.
PERSPECTIVES = ["powerless", "moderate", "institutional", "analytical"]
STRUCTURAL_FIELDS = (
    ["verdict"] + [f"persp:{p}" for p in PERSPECTIVES] + ["signature", "claimed_type"]
)
CONTINUOUS_FIELDS = ["theater_ratio"] + [f"chi:{p}" for p in PERSPECTIVES]
H2_MIN_PAIRS = 30  # below this a continuous field's H2 ships OPEN (statistic not stood up)


# --------------------------------------------------------------------------- field access
def get_struct(entry, field):
    if field == "verdict":
        return (entry.get("verdict_join") or {}).get("verdict")
    if field.startswith("persp:"):
        return (entry.get("perspectives") or {}).get(field.split(":", 1)[1])
    return entry.get(field)  # signature, claimed_type


def get_cont(entry, field):
    if field == "theater_ratio":
        return entry.get("theater_ratio")
    if field.startswith("chi:"):
        pc = (entry.get("perspective_chi") or {}).get(field.split(":", 1)[1]) or {}
        return pc.get("chi")
    return None


def populated(v):
    """A structural field counts as populated unless it is null/absent/empty.
    NOTE: 'unknown' is a real engine outcome (honest-unknown, OQ-37) and counts as
    populated — both engines independently typing a story 'unknown' is agreement."""
    return v is not None and v != ""


def isnum(v):
    return isinstance(v, (int, float)) and not isinstance(v, bool)


# --------------------------------------------------------------------------- statistics
def wilson(k, n, z=1.96):
    """(point, lo, hi) Wilson score interval for a binomial proportion."""
    if n == 0:
        return (None, None, None)
    p = k / n
    denom = 1 + z * z / n
    centre = (p + z * z / (2 * n)) / denom
    half = (z * math.sqrt(p * (1 - p) / n + z * z / (4 * n * n))) / denom
    return (p, max(0.0, centre - half), min(1.0, centre + half))


def percentile(xs, q):
    if not xs:
        return None
    s = sorted(xs)
    idx = (q / 100) * (len(s) - 1)
    lo = int(math.floor(idx)); hi = int(math.ceil(idx))
    if lo == hi:
        return s[lo]
    return s[lo] + (s[hi] - s[lo]) * (idx - lo)


# --------------------------------------------------------------------------- H1 / H2
def struct_rate(hvals, fvals):
    """agreement-rate over both-populated pairs; returns (rate, agree, n_both, one_sided)."""
    both = [(a, b) for a, b in zip(hvals, fvals) if populated(a) and populated(b)]
    one_sided = sum(1 for a, b in zip(hvals, fvals)
                    if populated(a) != populated(b))
    if not both:
        return (None, 0, 0, one_sided)
    agree = sum(1 for a, b in both if a == b)
    return (agree / len(both), agree, len(both), one_sided)


def analyse_structural(inter, haiku, flash, field, n_perm, rng):
    hvals = [get_struct(haiku[i], field) for i in inter]
    fvals = [get_struct(flash[i], field) for i in inter]
    rate, agree, n_both, one_sided = struct_rate(hvals, fvals)
    point, lo, hi = wilson(agree, n_both)
    # permutation null: shuffle flash values, recompute conditional agreement-rate
    null = []
    if n_both:
        for _ in range(n_perm):
            fs = fvals[:]
            rng.shuffle(fs)
            r, _, _, _ = struct_rate(hvals, fs)
            if r is not None:
                null.append(r)
    band95 = percentile(null, 95)
    h1 = None
    if lo is not None and band95 is not None:
        h1 = bool(lo > band95)
    # disparity exemplars (up to 5 ids where both populated and differ)
    exemplars = [
        {"id": i, "haiku": a, "flash": b}
        for i, a, b in zip(inter, hvals, fvals)
        if populated(a) and populated(b) and a != b
    ][:5]
    return {
        "field": field,
        "n_both_populated": n_both,
        "one_sided": one_sided,
        "agreement": agree,
        "disparity": n_both - agree,
        "agreement_rate": rate,
        "wilson_lo": lo, "wilson_hi": hi,
        "permute_band_95": band95,
        "n_permutations": len(null),
        "H1_holds": h1,
        "disparity_exemplars": exemplars,
    }


def analyse_continuous(inter, haiku, flash, field, n_perm, rng):
    hv = [get_cont(haiku[i], field) for i in inter]
    fv = [get_cont(flash[i], field) for i in inter]
    pairs = [(a, b) for a, b in zip(hv, fv) if isnum(a) and isnum(b)]
    if len(pairs) < H2_MIN_PAIRS:
        return {
            "field": field, "n_both_numeric": len(pairs),
            "status": "OPEN",
            "reason": f"only {len(pairs)} both-numeric pairs (< {H2_MIN_PAIRS}); "
                      f"statistic not stood up",
        }
    hnum = [a for a, b in pairs]
    fnum = [b for a, b in pairs]
    obs = sum(abs(a - b) for a, b in pairs) / len(pairs)
    null = []
    for _ in range(n_perm):
        fs = fnum[:]
        rng.shuffle(fs)
        null.append(sum(abs(a - b) for a, b in zip(hnum, fs)) / len(hnum))
    band5, band95 = percentile(null, 5), percentile(null, 95)
    if obs > band95:
        tail = "above"   # pre-registered literal: H2 holds (true pairs MORE dispersed)
    elif obs < band5:
        tail = "below"   # natural invariance reading: true pairs MORE similar than chance
    else:
        tail = "within"  # indistinguishable from chance re-pairing
    return {
        "field": field, "n_both_numeric": len(pairs),
        "status": "MEASURED",
        "observed_mean_abs_delta": obs,
        "permute_band_5": band5, "permute_band_95": band95,
        "tail": tail,
        "H2_holds_literal": bool(obs > band95),
    }


# --------------------------------------------------------------------------- marginals
def marginal_table(records, field):
    c = Counter()
    for e in records.values():
        v = get_struct(e, field)
        c[v if populated(v) else "<null>"] += 1
    return dict(c.most_common())


# --------------------------------------------------------------------------- guards / load
def load_input(spec):
    """spec = 'label=path'; path relative to repo root, OUTPUTS_DIR, or absolute."""
    label, _, path = spec.partition("=")
    if not path:
        raise SystemExit(f"--twin/--essay must be label=path, got {spec!r}")
    p = Path(path)
    for cand in (p, REPO_ROOT / path, OUTPUTS_DIR / path):
        if cand.exists():
            data = json.loads(cand.read_text(encoding="utf-8"))
            return label, cand, data
    raise SystemExit(f"input not found: {path}")


def index_by_id(data):
    return {e["id"]: e for e in data.get("per_constraint", [])}


def manifest_of(data, label):
    m = data.get("manifest") or {}
    if m.get("schema_version") != 2:
        raise SystemExit(f"REFUSE: input {label!r} schema_version="
                         f"{m.get('schema_version')} != 2")
    return m


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--twin", action="append", required=True,
                    help="label=path (exactly two)")
    ap.add_argument("--essay", action="append", default=[],
                    help="label=path (disjoint distribution corpus; marginals only)")
    ap.add_argument("--permute", type=int, default=1000)
    ap.add_argument("--seed", type=int, default=20260613)
    ap.add_argument("--outdir", default="audits/2026-06-13_twin_comparison")
    args = ap.parse_args()

    if len(args.twin) != 2:
        raise SystemExit(f"REFUSE: need exactly two --twin inputs, got {len(args.twin)}")
    if args.permute < 1000:
        raise SystemExit(f"REFUSE: --permute {args.permute} < 1000 (pre-registered N>=1000)")
    rng = random.Random(args.seed)

    twins = [load_input(s) for s in args.twin]
    (la, pa, da), (lb, pb, db) = twins
    ma, mb = manifest_of(da, la), manifest_of(db, lb)

    # refuse-to-join guards
    cpa, cpb = ma.get("corpus_path"), mb.get("corpus_path")
    if cpa is not None and cpa == cpb:
        raise SystemExit(f"REFUSE: both twins share corpus_path={cpa!r} (corpus vs itself)")
    if ma.get("code_commit") != mb.get("code_commit"):
        raise SystemExit(
            f"REFUSE: twins differ in code_commit ({ma.get('code_commit_short')} vs "
            f"{mb.get('code_commit_short')}) — model-difference would alias onto "
            f"code-difference. Re-classify both at one commit.")

    ha, hb = index_by_id(da), index_by_id(db)
    inter = sorted(set(ha) & set(hb))
    if not inter:
        raise SystemExit("REFUSE: empty id intersection")

    structural = [analyse_structural(inter, ha, hb, f, args.permute, rng)
                  for f in STRUCTURAL_FIELDS]
    continuous = [analyse_continuous(inter, ha, hb, f, args.permute, rng)
                  for f in CONTINUOUS_FIELDS]

    # essay / distribution corpora — marginals only
    essays = []
    for spec in args.essay:
        le, pe, de = load_input(spec)
        me = manifest_of(de, le)
        recs = index_by_id(de)
        essays.append({
            "label": le, "path": str(pe), "n": len(recs),
            "manifest": {k: me.get(k) for k in
                         ("code_commit_short", "n_constraints", "corpus_path")},
            "marginals": {f: marginal_table(recs, f) for f in STRUCTURAL_FIELDS},
        })

    result = {
        "pre_registration": "audits/2026-06-13_twin_comparison/PRE_REGISTRATION.md",
        "permutations": args.permute, "seed": args.seed,
        "twins": [
            {"label": la, "path": str(pa),
             "manifest": {k: ma.get(k) for k in
                          ("code_commit_short", "n_constraints", "corpus_path")}},
            {"label": lb, "path": str(pb),
             "manifest": {k: mb.get(k) for k in
                          ("code_commit_short", "n_constraints", "corpus_path")}},
        ],
        "intersection_n": len(inter),
        "structural_H1": structural,
        "continuous_H2": continuous,
        "essay_marginals": essays,
        "notes": [
            "verdict_join.verdict is the only headline verdict (OQ-98).",
            "verdict & perspectives are CORRELATED, not independent confirmations "
            "(verdict folds perspectives via compute_verdict/4).",
            "signature agreement is STRUCTURAL-coding, not detection (OQ-70).",
            "Per-field adjudication only; no aggregate H1 claim.",
        ],
    }

    outdir = REPO_ROOT / args.outdir
    outdir.mkdir(parents=True, exist_ok=True)
    (outdir / "twin_comparison.json").write_text(
        json.dumps(result, indent=2, ensure_ascii=False), encoding="utf-8")
    write_results_md(outdir / "RESULTS.md", result, la, lb)
    print(f"wrote {outdir/'twin_comparison.json'} and RESULTS.md "
          f"(intersection n={len(inter)})")


def write_results_md(path, r, la, lb):
    L = []
    L.append("# Twin-model cross-classification — RESULTS\n")
    L.append(f"Pre-registration: `{r['pre_registration']}` (committed before this run).\n")
    L.append("## Inputs\n")
    L.append("| label | commit | n_constraints | corpus_path |")
    L.append("|---|---|---|---|")
    for t in r["twins"]:
        m = t["manifest"]
        L.append(f"| {t['label']} | {m['code_commit_short']} | {m['n_constraints']} "
                 f"| {m.get('corpus_path')} |")
    L.append(f"\n**Matched intersection: n = {r['intersection_n']}** "
             f"(both twins classified at commit "
             f"{r['twins'][0]['manifest']['code_commit_short']}).\n")
    L.append(f"Permutations: {r['permutations']}; seed {r['seed']}.\n")

    L.append("## H1 — structural type model-invariance (per-field; NOT aggregated)\n")
    L.append("agreement-rate over both-populated pairs; H1 holds iff Wilson-95% lower "
             "bound > permute band (95th pct). verdict & perspectives are CORRELATED "
             "(verdict folds perspectives) — not independent confirmations.\n")
    L.append("| field | both-pop | agree | disp | one-sided | rate | Wilson-95 lo | "
             "band95 | H1 |")
    L.append("|---|---|---|---|---|---|---|---|---|")
    for s in r["structural_H1"]:
        rate = "—" if s["agreement_rate"] is None else f"{s['agreement_rate']:.3f}"
        lo = "—" if s["wilson_lo"] is None else f"{s['wilson_lo']:.3f}"
        b95 = "—" if s["permute_band_95"] is None else f"{s['permute_band_95']:.3f}"
        h1 = {True: "HOLDS", False: "FALSIFIED", None: "n/a"}[s["H1_holds"]]
        L.append(f"| `{s['field']}` | {s['n_both_populated']} | {s['agreement']} | "
                 f"{s['disparity']} | {s['one_sided']} | {rate} | {lo} | {b95} | {h1} |")

    L.append("\n### Disparity exemplars (both populated, values differ)\n")
    for s in r["structural_H1"]:
        if s["disparity_exemplars"]:
            L.append(f"- `{s['field']}`:")
            for ex in s["disparity_exemplars"]:
                L.append(f"  - `{ex['id']}`: {la}={ex['haiku']!r} vs {lb}={ex['flash']!r}")

    L.append("\n## H2 — continuous drift (per-field)\n")
    L.append("observed mean|Δ| (haiku−flash, paired) vs permuted-Δ band; pre-registered "
             "literal: H2 holds iff observed > band95 (true pairs MORE dispersed than "
             "chance). 'below' = natural invariance tail (more similar than chance).\n")
    L.append("| field | both-numeric | obs mean\\|Δ\\| | band5 | band95 | tail | status |")
    L.append("|---|---|---|---|---|---|---|")
    for c in r["continuous_H2"]:
        if c["status"] == "OPEN":
            L.append(f"| `{c['field']}` | {c['n_both_numeric']} | — | — | — | — | OPEN |")
        else:
            L.append(f"| `{c['field']}` | {c['n_both_numeric']} | "
                     f"{c['observed_mean_abs_delta']:.4f} | {c['permute_band_5']:.4f} | "
                     f"{c['permute_band_95']:.4f} | {c['tail']} | "
                     f"{'H2' if c['H2_holds_literal'] else 'no'} |")

    if r["essay_marginals"]:
        L.append("\n## Essay / distribution corpus (disjoint — marginals only, NOT paired)\n")
        for e in r["essay_marginals"]:
            L.append(f"### {e['label']} (n={e['n']}, commit "
                     f"{e['manifest']['code_commit_short']})\n")
            for f in ["verdict", "signature"]:
                L.append(f"- `{f}`: {e['marginals'][f]}")

    L.append("\n## Validity notes\n")
    for n in r["notes"]:
        L.append(f"- {n}")
    path.write_text("\n".join(L) + "\n", encoding="utf-8")


if __name__ == "__main__":
    main()
