#!/usr/bin/env python3
"""Cross-author epsilon comparison over the four twin legs.

Step 1 of the reader-profile probe (Claude-web plan, 2026-07-27): per-reading
epsilon deltas between independently-authored legs over the same readings.
Read-only. Keyed by filename base (reading identity across legs).

Positive control: a planted pair with known-different values must show a
nonzero delta; a planted identical pair must show zero.
"""
import re, json, sys
from pathlib import Path
from itertools import combinations

PROLOG = Path("/home/scott/bin/structural_dynamics_model/prolog")
LEGS = ["testsets_haiku", "testsets_flash", "testsets_kimi", "testsets_sonnet"]

CM_RE = re.compile(
    r"narrative_ontology:constraint_metric\(\s*([a-zA-Z0-9_]+)\s*,\s*extractiveness\s*,\s*([0-9.]+)\s*\)")
BE_RE = re.compile(
    r"domain_priors:base_extractiveness\(\s*([a-zA-Z0-9_]+)\s*,\s*([0-9.]+)\s*\)")

def extract_eps(path: Path):
    """Return (constraint_metric eps, base_extractiveness eps) or Nones."""
    text = path.read_text(errors="replace")
    # take fact lines only (skip comment blocks starting with % or inside /* */)
    cm = CM_RE.findall(text)
    be = BE_RE.findall(text)
    base = path.stem
    def pick(hits):
        if not hits:
            return None
        for cid, v in hits:
            if cid == base:
                return float(v)
        return float(hits[0][1])
    return pick(cm), pick(be)

def main():
    legs = {}
    parse_fail = {}
    cm_be_drift = {}
    for leg in LEGS:
        d = PROLOG / leg
        vals = {}
        fails = []
        drift = []
        for f in sorted(d.glob("*.pl")):
            cm, be = extract_eps(f)
            v = cm if cm is not None else be
            if v is None:
                fails.append(f.name)
            else:
                vals[f.stem] = v
                if cm is not None and be is not None and abs(cm - be) > 1e-9:
                    drift.append((f.stem, cm, be))
        legs[leg] = vals
        parse_fail[leg] = fails
        cm_be_drift[leg] = drift
        print(f"[extract] {leg}: {len(vals)} eps values, {len(fails)} no-eps files, "
              f"{len(drift)} cm/be within-file drift", file=sys.stderr)

    # positive control: values from the pair we hand-verified
    ctl = ("abrahamic_covenant__isaac_covenant_reading",
           legs["testsets_haiku"].get("abrahamic_covenant__isaac_covenant_reading"),
           legs["testsets_flash"].get("abrahamic_covenant__isaac_covenant_reading"))
    assert ctl[1] == 0.81 and ctl[2] == 0.7, f"positive control FAILED: {ctl}"
    print(f"[control] hand-verified pair matches extraction: haiku={ctl[1]} flash={ctl[2]} PASS",
          file=sys.stderr)

    shared = set.intersection(*(set(v) for v in legs.values()))
    print(f"[shared] readings present in all 4 legs with eps: {len(shared)}", file=sys.stderr)

    out = {"n_shared": len(shared),
           "per_leg_n": {k: len(v) for k, v in legs.items()},
           "parse_fail": {k: v for k, v in parse_fail.items()},
           "cm_be_drift_counts": {k: len(v) for k, v in cm_be_drift.items()},
           "pairwise": {}, "spread": {}}

    for a, b in combinations(LEGS, 2):
        deltas = [legs[a][r] - legs[b][r] for r in shared]
        ad = [abs(x) for x in deltas]
        n_eq = sum(1 for x in ad if x < 1e-9)
        out["pairwise"][f"{a}~{b}"] = {
            "mean_abs_delta": round(sum(ad) / len(ad), 4),
            "median_abs_delta": round(sorted(ad)[len(ad)//2], 4),
            "max_abs_delta": round(max(ad), 4),
            "share_identical": round(n_eq / len(ad), 4),
            "share_ge_0.10": round(sum(1 for x in ad if x >= 0.10) / len(ad), 4),
            "share_ge_0.20": round(sum(1 for x in ad if x >= 0.20) / len(ad), 4),
            "mean_signed": round(sum(deltas) / len(deltas), 4),
        }

    # per-reading spread across all four authors
    spreads = {r: max(legs[l][r] for l in LEGS) - min(legs[l][r] for l in LEGS)
               for r in shared}
    sv = sorted(spreads.values())
    out["spread"] = {
        "mean": round(sum(sv) / len(sv), 4),
        "median": round(sv[len(sv)//2], 4),
        "share_zero": round(sum(1 for x in sv if x < 1e-9) / len(sv), 4),
        "share_ge_0.20": round(sum(1 for x in sv if x >= 0.20) / len(sv), 4),
        "share_ge_0.30": round(sum(1 for x in sv if x >= 0.30) / len(sv), 4),
    }
    top = sorted(spreads.items(), key=lambda kv: -kv[1])[:25]
    out["top_divergers"] = [
        {"reading": r, "spread": round(s, 3),
         **{leg.replace("testsets_", ""): legs[leg][r] for leg in LEGS}}
        for r, s in top]
    bottom = [r for r, s in sorted(spreads.items(), key=lambda kv: kv[1])[:15]]
    out["most_agreed"] = [
        {"reading": r,
         **{leg.replace("testsets_", ""): legs[leg][r] for leg in LEGS}}
        for r in bottom]

    print(json.dumps(out, indent=2))

if __name__ == "__main__":
    main()
