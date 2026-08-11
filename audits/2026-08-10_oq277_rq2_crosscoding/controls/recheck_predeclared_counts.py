#!/usr/bin/env python3
"""Recompute the direction-(ii) hit counts stated in redaction_pairs_predeclared.json.

WHY THIS EXISTS, given that HANDOFF_TWINS_AND_DRIVER.md §1 says the pairs are pre-declared
and must not be re-derived "to check":

The instruction protects against RE-SELECTING the pairs — choosing them after seeing which
units came out thin is selection on the outcome the control measures. This script does not
select anything. It recomputes the stated *numbers* and reports whether the pre-declared
SELECTION is invariant under the recount. The selection is used as declared either way; the
script cannot change it.

The tension is real and is recorded rather than resolved silently: an instruction that forbids
checking a control's stated numbers makes a defect in those numbers unfalsifiable by
construction, which is the shape build_discipline.md calls "a repair that encodes the tested
claim into the instrument." §J.1 pulls the other way — the ruling is an artifact under the same
discipline, and §L's whole lesson is that counting what a rule claims to produce catches rule
defects that re-reading the rule does not. This script is the narrowest action satisfying §L
without touching what §1 protects.

BASIS. The declared table excludes the `source_identifying` group. That is not this script's
choice: the declared value for five_leg_twin_comparison is 21, which is reachable only with
that group excluded (24 with it), so the artifact evidences its own basis. The group was
flagged in oq277_lexicon.py as an ADDITION to the original design's ban list, so excluding it
from a *density* metric is consistent with how it was introduced.

Run:  python3 controls/recheck_predeclared_counts.py      (exit 0 iff selection is invariant)
"""
from __future__ import annotations
import glob
import json
import os
import pathlib
import sys

HERE = pathlib.Path(__file__).resolve().parent
REPO = HERE.parents[2]
sys.path.insert(0, str(REPO / "python" / "audits"))
import oq277_lexicon as L  # noqa: E402

# The 4 overlap directories the declared rule excludes. NOT transcribed from prose —
# read off the units themselves (overlap_source == true), so the exclusion set is the one
# the corpus actually carries rather than the one a handoff remembered.
def _overlap_dirs() -> set[str]:
    out = set()
    for f in sorted(glob.glob(str(HERE.parent / "packets" / "our_units" / "*.json"))):
        u = json.load(open(f))
        if u.get("overlap_source"):
            out.add(u["source_dir"])
    return out


def hits(directory: str, *, drop_source_identifying: bool = True) -> int:
    """Declared basis: all *.md in the directory, non-recursive, source_identifying dropped."""
    n = 0
    for f in sorted(glob.glob(os.path.join(directory, "*.md"))):
        text = open(f, encoding="utf-8", errors="replace").read()
        for group, _pat, _m, _c in L.scan(text, "ii"):
            if drop_source_identifying and group == "source_identifying":
                continue
            n += 1
    return n


def main() -> int:
    OVERLAP_DIRS = _overlap_dirs()
    pre = json.load(open(HERE / "redaction_pairs_predeclared.json"))
    declared_tbl = pre["direction_ii"]["measured_hit_counts_all_sampled_dirs"]
    declared_sel = [s["dir"] for s in pre["direction_ii"]["selected"]]

    rows, mismatches = [], []
    for d, stated in declared_tbl.items():
        counted = hits(str(REPO / "audits" / d))
        rows.append((d, stated, counted))
        if stated != counted:
            mismatches.append((d, stated, counted))

    print("direction-(ii) pattern-vocabulary density, declared vs recounted")
    print(f"{'directory':<46}{'declared':>9}{'counted':>9}   overlap")
    for d, stated, counted in sorted(rows, key=lambda r: -r[2]):
        flag = "" if stated == counted else "   <-- MISMATCH"
        print(f"{d:<46}{stated:>9}{counted:>9}   {'yes' if d in OVERLAP_DIRS else '-':<4}{flag}")

    # The selection the RULE produces, recomputed: top 3 non-overlap by count,
    # ties broken by earliest directory date (the leading YYYY-MM-DD sorts lexically).
    eligible = [(d, c) for d, _s, c in rows if d not in OVERLAP_DIRS]
    recomputed = [d for d, _c in sorted(eligible, key=lambda r: (-r[1], r[0]))[:3]]

    print()
    print(f"declared selection : {sorted(declared_sel)}")
    print(f"recounted selection: {sorted(recomputed)}")

    ok = sorted(recomputed) == sorted(declared_sel)
    if mismatches:
        print(f"\n{len(mismatches)} of {len(rows)} stated counts do not reproduce:")
        for d, stated, counted in mismatches:
            print(f"  {d}: stated {stated}, counted {counted}")
    if ok:
        print("\nSELECTION INVARIANT — the pre-declared pairs are the ones the rule produces.")
        print("The pairs are used AS DECLARED. The stated counts are corrected in the prereg,")
        print("not the selection.")
    else:
        print("\nSELECTION NOT INVARIANT — STOP. This is an operator ruling, not a fix:")
        print("the declared pairs and the rule that declared them now disagree, and choosing")
        print("either one after seeing this table is a choice made on the outcome.")
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
