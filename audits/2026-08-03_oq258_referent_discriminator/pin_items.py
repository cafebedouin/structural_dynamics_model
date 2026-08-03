#!/usr/bin/env python3
"""OQ-258 Phase 0 — pin the discriminator item set + baseline per-leg epsilon.

Scripted extraction (never hand-copied): the 18 items are exactly the
2026-07-27 audit's group==top30 items whose coded channel is tacit or
none_apparent (codes.json + coding_batches.json).

Baseline per-leg epsilon is RE-EXTRACTED from the four live legs with the same
regexes as epsilon_cross_author.py. Positive control: every item that appears
in the recorded top_divergers of epsilon_cross_author_results.json must match
the re-extraction EXACTLY (leg values and rounded spread); items absent from
top_divergers (the results JSON records only the top 25) get their baseline
from the re-extraction, flagged "reextracted_only".

Output: items_baseline.json next to this script.
"""
import json
import re
import sys
from pathlib import Path

REPO = Path("/home/scott/bin/structural_dynamics_model")
PROBE = REPO / "audits" / "2026-07-27_cross_author_epsilon_probe"
HERE = Path(__file__).resolve().parent
LEGS = ["testsets_haiku", "testsets_flash", "testsets_kimi", "testsets_sonnet"]

# same regexes as epsilon_cross_author.py (the baseline instrument)
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


def main():
    codes = {c["id"]: c for c in json.loads((PROBE / "codes.json").read_text())}
    batches = json.loads((PROBE / "coding_batches.json").read_text())
    items = sorted(
        i for i, g in batches["group"].items()
        if g == "top30" and codes[i]["channel"] in ("tacit", "none_apparent"))
    assert len(items) == 18, f"expected 18 items, got {len(items)}"
    n_tacit = sum(1 for i in items if codes[i]["channel"] == "tacit")
    assert n_tacit == 10 and len(items) - n_tacit == 8, "stratum split != 10/8"

    results = json.loads((PROBE / "epsilon_cross_author_results.json").read_text())
    recorded = {d["reading"]: d for d in results["top_divergers"]}

    out, n_ctl = [], 0
    for item in items:
        row = {"id": item, "channel": codes[item]["channel"]}
        for leg in LEGS:
            f = REPO / "prolog" / leg / f"{item}.pl"
            assert f.exists(), f"baseline file missing: {f}"
            row[leg.replace("testsets_", "")] = extract_eps(f)
        vals = [row[k] for k in ("haiku", "flash", "kimi", "sonnet")]
        assert all(v is not None for v in vals), f"no eps extracted: {item}"
        row["spread"] = round(max(vals) - min(vals), 3)
        if item in recorded:
            rec = recorded[item]
            for k in ("haiku", "flash", "kimi", "sonnet"):
                assert row[k] == rec[k], \
                    f"CONTROL FAIL {item}.{k}: re-extracted {row[k]} != recorded {rec[k]}"
            assert row["spread"] == rec["spread"], \
                f"CONTROL FAIL {item}.spread: {row['spread']} != {rec['spread']}"
            row["baseline_source"] = "recorded+reextracted (exact match)"
            n_ctl += 1
        else:
            row["baseline_source"] = "reextracted_only"
        out.append(row)

    spreads = [r["spread"] for r in out]
    tac = [r["spread"] for r in out if r["channel"] == "tacit"]
    na = [r["spread"] for r in out if r["channel"] == "none_apparent"]
    summary = {
        "n_items": len(out),
        "n_tacit": len(tac),
        "n_none_apparent": len(na),
        "n_positive_control_matched": n_ctl,
        "baseline_mean_spread": round(sum(spreads) / len(spreads), 4),
        "baseline_mean_spread_tacit": round(sum(tac) / len(tac), 4),
        "baseline_mean_spread_none_apparent": round(sum(na) / len(na), 4),
        "baseline_gap_none_apparent_minus_tacit": round(
            sum(na) / len(na) - sum(tac) / len(tac), 4),
    }
    payload = {"summary": summary, "items": out}
    (HERE / "items_baseline.json").write_text(
        json.dumps(payload, indent=2) + "\n", encoding="utf-8")
    print(json.dumps(summary, indent=2))
    print(f"\n[control] {n_ctl}/18 items matched recorded top_divergers exactly "
          f"({18 - n_ctl} reextracted_only — results JSON records only top 25)")
    for r in out:
        print(f"  {r['channel']:14s} {r['spread']:5.3f}  {r['id']}")


if __name__ == "__main__":
    sys.exit(main())
