#!/usr/bin/env python3
"""rqd_analyze.py — the analysis stage for RQ-d, applying the FROZEN outcome rules.

The preregistration declared at freeze that this file did not yet exist. It is
written before the full run and pinned in the writeup with its own md5.

It applies §6's rules verbatim and prints the verdict rather than leaving it to
prose: an analysis stage that reports numbers and lets the writeup pick the
verdict is where the pre-registration quietly stops binding.
"""
import json
import sys
from collections import defaultdict
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(Path(__file__).resolve().parent))
from rqd_scorer import score_response  # noqa: E402
from rqd_materials import SPECS  # noqa: E402

RESP = ROOT / "audits" / "2026-08-13_rqd_recognition_vs_enumeration" / "responses"
FLOOR = 0.20          # pre-registered effect-size floor
CEIL_HI, CEIL_LO = 0.80, 0.10   # pre-registered ceiling/floor rule
PROTOCOL_FAILURE_KILL = 0.10


def _legacy_score_defect(section, keys):
    """The PRE-AMENDMENT scorer, retained verbatim so Amendment 1's effect on the
    result can be MEASURED per arm rather than assumed benign (PREREGISTRATION §9).
    Split on sentence punctuation; 'vs.' and friends break the chunk."""
    import re
    from rqd_scorer import GAP_MARKERS
    parts = re.split(r"(?<=[.!?])\s+|\n+|(?:^|\n)\s*[-*•]\s*", section)
    for sent in [p.strip() for p in parts if p and p.strip()]:
        s = sent.lower()
        if any(k.lower() in s for k in keys) and any(m in s for m in GAP_MARKERS):
            return True
    return False


def load():
    from rqd_scorer import extract_section
    from rqd_materials import by_id
    rows = []
    for f in sorted(RESP.glob("*.json")):
        obj = json.loads(f.read_text())
        sc = score_response(obj["spec_id"], obj["response"])
        spec = by_id(obj["spec_id"])
        sec = extract_section(obj["response"])
        sc.update(protocol=obj["protocol"], rep=obj["rep"],
                  tokens_out=obj["tokens_out"], chars=len(obj["response"]),
                  legacy_omission=_legacy_score_defect(sec, spec["omission_keys"]),
                  legacy_error=_legacy_score_defect(sec, spec["error_keys"]))
        rows.append(sc)
    return rows


def amendment_delta(rows):
    """Per-arm hits ADDED by Amendment 1. An asymmetric repair would inflate or
    deflate the headline, so it is reported rather than assumed conservative."""
    print("\n=== Amendment 1 effect (hits added by the scorer repair, per arm) ===")
    for proto in ("recognition", "enumeration"):
        sel = [r for r in rows if r["protocol"] == proto]
        if not sel:
            continue
        for kind, new, old in (("omission", "omission_strict", "legacy_omission"),
                               ("error", "error_strict", "legacy_error")):
            added = sum(1 for r in sel if r[new] and not r[old])
            lost = sum(1 for r in sel if r[old] and not r[new])
            print(f"  {proto:12} {kind:9} added {added:2}  lost {lost:2}  "
                  f"of {len(sel)}  ({added/len(sel):+.2f} rate)")


def rate(rows, protocol, field):
    sel = [r for r in rows if r["protocol"] == protocol]
    return (sum(1 for r in sel if r[field]) / len(sel)) if sel else float("nan")


def main():
    rows = load()
    if not rows:
        print("no responses on disk")
        return 1
    print(f"units scored: {len(rows)}")

    # Instrument kill condition FIRST — a Delta computed over a broken protocol
    # is a number about nothing.
    for proto in ("recognition", "enumeration"):
        sel = [r for r in rows if r["protocol"] == proto]
        if not sel:
            continue
        fails = sum(1 for r in sel if not r["section_present"])
        frac = fails / len(sel)
        print(f"  protocol_failure {proto:12} {fails}/{len(sel)} = {frac:.2f}")
        if frac > PROTOCOL_FAILURE_KILL:
            print(f"KILL CONDITION MET ({proto}): protocol_failure > "
                  f"{PROTOCOL_FAILURE_KILL:.0%} — instrument failure, no Delta claimed")
            return 1

    # Verbosity, reported because it is the confound the error arm exists to catch.
    for proto in ("recognition", "enumeration"):
        sel = [r for r in rows if r["protocol"] == proto]
        if sel:
            print(f"  mean chars {proto:12} {sum(r['chars'] for r in sel)/len(sel):8.0f}"
                  f"   mean tokens_out {sum(r['tokens_out'] for r in sel)/len(sel):7.0f}")

    # Per-item rows. A pooled bit over heterogeneous items is the reporting shape
    # this project has been burned by, so items come first.
    print(f"\n{'spec':28} {'OMISSION rec->enum':>22}  {'ERROR rec->enum':>20}  note")
    per_item, uninformative = {}, []
    for spec in SPECS:
        sel = [r for r in rows if r["spec_id"] == spec["id"]]
        if not sel:
            continue
        o_r, o_e = rate(sel, "recognition", "omission_strict"), rate(sel, "enumeration", "omission_strict")
        e_r, e_e = rate(sel, "recognition", "error_strict"), rate(sel, "enumeration", "error_strict")
        note = ""
        if o_r > CEIL_HI:
            note = "ceiling (recog>0.80) — uninformative for Delta"; uninformative.append(spec["id"])
        elif o_r < CEIL_LO and o_e < CEIL_LO:
            note = "floor (both<0.10) — uninformative for Delta"; uninformative.append(spec["id"])
        per_item[spec["id"]] = (o_r, o_e, e_r, e_e)
        print(f"{spec['id']:28} {o_r:8.2f} -> {o_e:<8.2f}  {e_r:7.2f} -> {e_e:<7.2f}  {note}")

    def pooled(subset):
        sel = [r for r in rows if r["spec_id"] in subset]
        return (rate(sel, "recognition", "omission_strict"),
                rate(sel, "enumeration", "omission_strict"),
                rate(sel, "recognition", "error_strict"),
                rate(sel, "enumeration", "error_strict"))

    all_ids = {s["id"] for s in SPECS if any(r["spec_id"] == s["id"] for r in rows)}
    informative = all_ids - set(uninformative)

    for label, subset in (("ALL ITEMS", all_ids), ("INFORMATIVE ONLY", informative)):
        if not subset:
            continue
        o_r, o_e, e_r, e_e = pooled(subset)
        do, de = o_e - o_r, e_e - e_r
        print(f"\n=== {label}  (n_items={len(subset)}) ===")
        print(f"  omission  recognition {o_r:.2f}  enumeration {o_e:.2f}   Delta {do:+.2f}")
        print(f"  error     recognition {e_r:.2f}  enumeration {e_e:.2f}   Delta {de:+.2f}")
        if do <= -FLOOR:
            v = "REVERSED — evidence against A3"
        elif do < FLOOR:
            v = "REFUTED HERE — enumeration does not surface omissions better on these shapes"
        elif de >= FLOOR:
            v = "CONFOUNDED — enumeration lifts BOTH; consistent with effort/verbosity, not the specific claim"
        else:
            v = "SUPPORTED — dissociation (omission lifts, error does not)"
        print(f"  VERDICT [frozen rule, floor {FLOOR:+.2f}]: {v}")

    amendment_delta(rows)

    # Sensitivity: lenient scoring, reported, never the headline.
    lo_r, lo_e = rate(rows, "recognition", "omission_lenient"), rate(rows, "enumeration", "omission_lenient")
    print(f"\n  [sensitivity] lenient omission  recog {lo_r:.2f}  enum {lo_e:.2f}  "
          f"Delta {lo_e-lo_r:+.2f}   (not the headline)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
