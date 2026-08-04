#!/usr/bin/env python3
"""OQ-258 Phase 4 — measurement with positive control (PROPOSAL.md, pinned before spend).

Positive control FIRST: this script's extractor, run over the four baseline legs,
must reproduce items_baseline.json (itself exact-matched against the recorded
2026-07-27 top_divergers for 14/18 items) EXACTLY before any arm is read.

Then: per-item 4-author spread for Arm B and Arm A, paired deltas, two-sided
Wilcoxon signed-rank, stratum means, and the pre-registered interpretation table
applied mechanically (first row that fires wins; boundaries pinned in PROPOSAL.md:
Delta=0.15, elevated/collapsed boundary for B = 0.38, A~B equivalence quantum 0.05).
"""
import json
import re
import sys
from pathlib import Path

REPO = Path("/home/scott/bin/structural_dynamics_model")
HERE = Path(__file__).resolve().parent
LEGS = ["haiku", "flash", "kimi", "sonnet"]

DELTA = 0.15          # pinned minimum effect (PROPOSAL: Primary statistic)
B_ELEVATED = 0.38     # pinned elevated/collapsed boundary for Arm B
EQUIV = 0.05          # pinned A~B equivalence quantum

CM_RE = re.compile(
    r"narrative_ontology:constraint_metric\(\s*([a-zA-Z0-9_]+)\s*,\s*extractiveness\s*,\s*([0-9.]+)\s*\)")
BE_RE = re.compile(
    r"domain_priors:base_extractiveness\(\s*([a-zA-Z0-9_]+)\s*,\s*([0-9.]+)\s*\)")


def extract_eps(path: Path):
    text = path.read_text(errors="replace")
    cm, be = CM_RE.findall(text), BE_RE.findall(text)
    base = path.stem

    def pick(hits):
        if not hits:
            return None
        for cid, v in hits:
            if cid == base:
                return float(v)
        return float(hits[0][1])

    c, b = pick(cm), pick(be)
    return c if c is not None else b


def leg_dir(leg, arm):
    """Arm output namespaces (PROPOSAL: Arms). arm in {'baseline','armb','arma'}.
    Generation wrote to prolog/ namespaces; Phase 5 archived them under
    generated/{armA,armB}/<leg>/pl/ (audit location mandate) — resolve either."""
    if arm == "baseline":
        return REPO / "prolog" / f"testsets_{leg}"
    tag = f"oq258_{arm}"
    live = (REPO / "prolog" / "testsets" / tag if leg == "haiku"
            else REPO / "prolog" / f"testsets_{leg}_{tag}")
    archived = HERE / "generated" / ("armA" if arm == "arma" else "armB") / leg / "pl"
    return live if live.exists() else archived


def collect(items, arm):
    """{item: {leg: eps}} — missing files recorded as None (declared, never skipped)."""
    out = {}
    for item in items:
        row = {}
        for leg in LEGS:
            f = leg_dir(leg, arm) / f"{item}.pl"
            row[leg] = extract_eps(f) if f.exists() else None
        out[item] = row
    return out


def spread(row):
    vals = [v for v in row.values() if v is not None]
    return round(max(vals) - min(vals), 4) if len(vals) == 4 else None


def main():
    baseline_doc = json.loads((HERE / "items_baseline.json").read_text())
    pinned = {r["id"]: r for r in baseline_doc["items"]}
    items = sorted(pinned)
    assert len(items) == 18

    # ---- POSITIVE CONTROL: reproduce items_baseline.json exactly, before arms ----
    base_now = collect(items, "baseline")
    for item in items:
        for leg in LEGS:
            got, want = base_now[item][leg], pinned[item][leg]
            assert got == want, \
                f"POSITIVE CONTROL FAIL {item}.{leg}: {got} != pinned {want}"
        assert round(spread(base_now[item]), 3) == pinned[item]["spread"], \
            f"POSITIVE CONTROL FAIL {item}.spread"
    print(f"[control] baseline re-extraction reproduces items_baseline.json exactly "
          f"(18 items x 4 legs) PASS")

    # ---- Arms ----
    arms = {"armb": collect(items, "armb"), "arma": collect(items, "arma")}
    rows, dropped = [], []
    for item in items:
        r = {"id": item, "channel": pinned[item]["channel"],
             "baseline_spread": pinned[item]["spread"],
             "armb": arms["armb"][item], "arma": arms["arma"][item],
             "armb_spread": spread(arms["armb"][item]),
             "arma_spread": spread(arms["arma"][item])}
        if r["armb_spread"] is None or r["arma_spread"] is None:
            dropped.append(item)
        rows.append(r)
    paired = [r for r in rows if r["id"] not in dropped]
    n = len(paired)
    print(f"[pairing] {n}/18 items pairwise-complete; dropped: {dropped or 'none'}")

    sb = [r["armb_spread"] for r in paired]
    sa = [r["arma_spread"] for r in paired]
    mean_b, mean_a = sum(sb) / n, sum(sa) / n
    delta = mean_b - mean_a

    from scipy.stats import wilcoxon
    try:
        w = wilcoxon(sa, sb, alternative="two-sided")
        wstat, pval = float(w.statistic), float(w.pvalue)
    except ValueError as e:   # all-zero differences
        wstat, pval = float("nan"), 1.0
        print(f"[wilcoxon] degenerate ({e}); p treated as 1.0")

    def stratum(rows_, ch, key):
        xs = [r[key] for r in rows_ if r["channel"] == ch]
        return round(sum(xs) / len(xs), 4) if xs else None

    strata = {arm_key: {ch: stratum(paired, ch, f"{arm_key}_spread")
                        for ch in ("tacit", "none_apparent")}
              for arm_key in ("armb", "arma")}

    # ---- Interpretation table, applied in pinned order ----
    if delta >= DELTA and pval < 0.05:
        verdict = ("row1_referent_owned: referent ambiguity owned the channel-legibility "
                   "finding (specificity confound declared)")
    elif mean_b < B_ELEVATED:
        verdict = ("row2_regression_dominates: Arm B itself collapsed; discriminator "
                   "underpowered — OPEN, consider declared-referent-field fallback")
    elif abs(delta) < EQUIV or pval >= 0.05:
        verdict = ("row3_reader_position_survives: A ~ B with B elevated; "
                   "channel-conditional reliability caveat hardens")
    elif EQUIV <= delta < DELTA and pval < 0.05:
        verdict = "row4_inconclusive: A below B but sub-Delta — OPEN with measured delta"
    else:
        verdict = f"row_none: delta={delta:.4f} p={pval:.4f} outside pinned rows (A ABOVE B?)"

    witness = arms["arma"]["animal_status__abolitionist_reading"]["haiku"]
    out = {
        "n_paired": n, "dropped": dropped,
        "mean_spread_baseline_18": baseline_doc["summary"]["baseline_mean_spread"],
        "mean_spread_armb": round(mean_b, 4), "mean_spread_arma": round(mean_a, 4),
        "delta_b_minus_a": round(delta, 4),
        "wilcoxon_stat": wstat, "wilcoxon_p": round(pval, 6),
        "pinned": {"DELTA": DELTA, "B_ELEVATED": B_ELEVATED, "EQUIV": EQUIV},
        "strata_mean_spread": strata,
        "baseline_strata": {
            "tacit": baseline_doc["summary"]["baseline_mean_spread_tacit"],
            "none_apparent": baseline_doc["summary"]["baseline_mean_spread_none_apparent"]},
        "single_item_witness_haiku_arma_animal_status": witness,
        "verdict": verdict,
        "per_item": rows,
    }
    (HERE / "measurement_results.json").write_text(
        json.dumps(out, indent=2) + "\n", encoding="utf-8")
    for k, v in out.items():
        if k != "per_item":
            print(f"{k}: {json.dumps(v)}")


if __name__ == "__main__":
    sys.exit(main())
