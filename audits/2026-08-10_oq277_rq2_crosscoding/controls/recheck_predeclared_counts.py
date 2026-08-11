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

WHICH LEXICON, AND WHY IT IS PINNED (operator ruling, 2026-08-11). This script reads
`oq277_lexicon.LEXICON_SELECTION_20260811` — the FROZEN snapshot of the lists in force when
redaction_pairs_predeclared.json was written — and NOT the live detector.

The detector was widened on 2026-08-11 to catch hyphen-joined forms, because three real leak
forms escaped it ("Build-Discipline Pattern-1", "Build-discipline spine", "Pattern-6
success-shaped-absorption"). Widening is a strengthening for DETECTION and is inadmissible
for SELECTION: under the widened lists this script's recomputed top-3 changes —
oq97_pattern6_census rises 4 -> 9 and overtakes oq138 at 5 — and it moves TOWARD the
corrected set, i.e. in the direction that flatters the both-residue row. Re-declaring the
selection under a lexicon widened after the counts were visible is reselection with an extra
step, and the fact that the movement is convenient is precisely why it cannot be taken.

So: two roles, two pinned versions, one module. This script reproduces a PAST selection and
must therefore use the instrument that selection was made under. A defect found in the frozen
lists is a finding to report, never an edit to make.

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


# The pinned selection instrument. See "WHICH LEXICON" above — this is deliberately NOT
# the live detector, and swapping it for L.LEXICON_DETECT silently changes a pre-declared
# selection into a post-hoc one.
SELECTION_LEXICON = L.LEXICON_SELECTION_20260811


def hits(directory: str, *, drop_source_identifying: bool = True) -> int:
    """Declared basis: all *.md in the directory, non-recursive, source_identifying dropped."""
    n = 0
    for f in sorted(glob.glob(os.path.join(directory, "*.md"))):
        text = open(f, encoding="utf-8", errors="replace").read()
        for group, _pat, _m, _c in L.scan(text, "ii", SELECTION_LEXICON):
            if drop_source_identifying and group == "source_identifying":
                continue
            n += 1
    return n


BARE_P_TOKEN = r"\bP[1-6]\b"


def hits_split(directory: str) -> tuple[int, int, list[str]]:
    """(ambiguous, unambiguous, terms) — split the density metric by whether a hit can
    only mean OUR taxonomy.

    `\\bP[1-6]\\b` is AMBIGUOUS: audit directories use P1/P2/P3 for their own local
    numbering (probe names, field arms) with no relation to the six patterns. That is
    harmless for a leak-grep, where a false positive is conservative — you investigate and
    clear it. It is NOT harmless for a density metric used to SELECT, where a false
    positive silently determines the choice. Same matcher, two roles, opposite failure
    direction.

    Unambiguous = `Pattern N`, the pattern names, the nicknames, the taxonomy phrases.
    """
    amb = unamb = 0
    terms: list[str] = []
    for f in sorted(glob.glob(os.path.join(directory, "*.md"))):
        text = open(f, encoding="utf-8", errors="replace").read()
        for group, pat, matched, _c in L.scan(text, "ii", SELECTION_LEXICON):
            if group == "source_identifying":
                continue
            if pat == BARE_P_TOKEN:
                amb += 1
            else:
                unamb += 1
                terms.append(matched)
    return amb, unamb, terms


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
    # ---- second table: is the density metric measuring taxonomy vocabulary at all? ----
    print("\n\ndensity split by whether a hit can ONLY mean our taxonomy")
    print(f"{'directory':<46}{'bare P#':>8}{'taxonomy':>10}   terms")
    split = {d: hits_split(str(REPO / "audits" / d)) for d, _s, _c in rows}
    for d, (amb, unamb, terms) in sorted(split.items(), key=lambda kv: -kv[1][1]):
        mark = "  <-- SELECTED" if d in declared_sel else ""
        print(f"{d:<46}{amb:>8}{unamb:>10}   {sorted(set(terms))[:4]}{mark}")

    elig2 = [(d, split[d][1]) for d, _s, _c in rows if d not in OVERLAP_DIRS]
    corrected = [d for d, _c in sorted(elig2, key=lambda r: (-r[1], r[0]))[:3]]
    print(f"\nselection under taxonomy-only density: {sorted(corrected)}")
    empty = [d for d in declared_sel if split[d][1] == 0]
    if empty:
        print(f"\nPRE-DECLARATION PREMISE FAILS for {len(empty)} of {len(declared_sel)} pairs:")
        for d in empty:
            print(f"  {d}: 0 taxonomy-vocabulary hits — nothing to un-redact")
        print("  The declared rationale ('non-empty by construction rather than by luck')")
        print("  does not hold for these. OPERATOR RULING REQUIRED — see")
        print("  redaction_pair_selection_defect.md. Do not reselect without it.")

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
