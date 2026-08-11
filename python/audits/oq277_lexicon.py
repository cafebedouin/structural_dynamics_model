#!/usr/bin/env python3
"""OQ-277 banned lexicons + leak matcher — CANONICAL, single source.

Both the redaction sweep and the payload leak-grep in the coding driver import from
here. One matcher, one pair of lists: a second copy would be a P2 fork inside the
experiment measuring P2.

Word-boundary matching is mandatory. A substring matcher fired H2 on a phantom during
this audit's own step 1 ("permission *class b*y default" matched banned "Class B"), and
H2 voids a whole direction. That false positive is a permanent selftest control below.

SEPARATOR TOLERANCE (added 2026-08-11, pre-freeze, while authoring the direction-(ii)
twins). Every multiword pattern below joins its words with `[-\\s]+`, not a literal space.
The bare-space form silently missed the hyphen-joined variant, which is the form the
source corpus actually uses at the three points that matter most:

    audits/2026-06-07_stakeholder_layer_migration/AUDIT.md:144  "Build-Discipline Pattern-1"
    audits/2026-06-10_oq93_grid_viability_probe/FINDINGS.md:22  "Build-discipline spine"
    audits/2026-06-11_oq97_pattern6_census/WRITEUP.md:1         "Pattern-6 success-shaped-absorption"

A leak-grep that misses `Pattern-1` while catching `Pattern 1` is a false-absence in the
instrument that certifies H2 — the exact defect class this experiment codes for. Measured
before and after the widening: ZERO coder-facing texts (54 unit/anchor/decoy/planted
blobs) change hit status, so this strengthens the sweep and loosens nothing. Hyphen
variants are permanent selftest controls below.

Frozen into PREREGISTRATION.md verbatim at freeze. Editing a list after the prereg md5
is recorded invalidates the freeze.

Usage:
    python3 python/audits/oq277_lexicon.py --check
    python3 python/audits/oq277_lexicon.py --sweep <units.json> --direction {i,ii}
"""
from __future__ import annotations
import argparse, json, re, sys

# ---------------------------------------------------------------------------
# Direction (i): Wu's incidents, coded against OUR six. Strip WU's vocabulary.
# ---------------------------------------------------------------------------
BANNED_DIRECTION_I = {
    "class_letters": [
        r"\bClass[-\s]+[A-E]\b",
        r"\b[A-E]\s*类\b",
        r"\bclass[-\s]+[A-E]\s*[:\-—]",
    ],
    "class_names": [
        r"\benvironment(?:al)?[/\-\s]+platform[-\s]+quirk\b", r"\bplatform[-\s]+quirk\b",
        r"\bdesign[-\s]+assumption[-\s]+mismatch\b", r"\bassumption[-\s]+mismatch\b",
        r"\berror[-\s]+swallow(?:ing)?\b", r"\bswallow(?:ed|ing)[-\s]+(?:and|&)[-\s]+dilut\w+\b",
        r"\bdilution\b", r"\bdiluted\b",
        r"\bchain(?:ed)?[-\s]+hallucination\b", r"\bfabrication[-\s]+chain\b",
        r"\boperational[-\s]+omission\b", r"\bforensic[-\s]+blind[-\s]+spot\b",
        r"环境.{0,2}平台", r"设计假设错配", r"错误吞噬", r"稀释",
        r"链式幻觉", r"编造", r"运维遗漏", r"取证盲区",
    ],
    "taxonomy_terms": [
        r"\bfail[-\s]+plausible\b", r"\bgr[ae]y[-\s]+failure\b", r"\btaxonomy\b",
        r"\bsilent[-\s]+failure[-\s]+taxonomy\b", r"\bfailure[-\s]+mode(?:s)?[-\s]+catalog\b",
    ],
    "root_cause_structure": [
        r"\btrigger\b", r"\bamplifier\b", r"\bconcealer\b",
        r"触发器", r"放大器", r"掩护者",
    ],
    "rule_ids": [
        r"\bMR-\d+\b", r"\bINV-[A-Z]", r"\b元规则\b", r"\b不变式\b",
        r"\bmeta[-\s]+rule\b", r"\binvariant[-\s]+INV\b",
    ],
    # Source-identifying terms. NOT in the original design's ban list — added here on the
    # reasoning that a coder recognising the source system could recall the published
    # taxonomy from training data, which defeats the blind exactly as a class name would.
    # Flagged as an ADDITION for operator awareness rather than folded in silently.
    "source_identifying": [
        r"\bopenclaw\b", r"\bmodel[- ]bridge\b", r"\bbaileys\b",
        r"\bWu\b", r"\barxiv\b", r"2606\.14589",
    ],
}

# ---------------------------------------------------------------------------
# Direction (ii): OUR incidents, coded against WU's five. Strip the P-lexicon.
# ---------------------------------------------------------------------------
BANNED_DIRECTION_II = {
    "p_tokens": [
        r"\bP[1-6]\b", r"\bPattern[-\s]+[1-6]\b",
        r"\bthe[-\s]+(?:published[-\s]+)?six[-\s]+patterns\b",
    ],
    "pattern_names": [
        r"\bproduced[-\s]+but[-\s]+not[-\s]+consumed\b",
        r"\bone[-\s]+canonical[-\s]+thing(?:[-\s]+became[-\s]+two)?\b",
        r"\bdestructive[-\s]+replace\b",
        r"\brecap[-\s]+as[-\s]+witness\b",
        r"\babsence[-\s]+satisfies[-\s]+the[-\s]+gate\b",
        r"\bsuccess[-\s]+shaped[-\s]+absorption\b",
    ],
    "pattern_nicknames": [
        r"\bdangling[-\s]+wire\b", r"\bsilent[-\s]+fork\b", r"\bfaith[-\s]+merge\b",
    ],
    "taxonomy_phrases": [
        r"\bsuccess[-\s]+shaped\b", r"\bmeasured[-\s]+empty\b", r"\bdidn'?t[-\s]+look\b",
        r"\bauthored[-\s]+zero\b", r"\bpaste[-\s]+or[-\s]+untag\b",
        r"\bwitness[-\s]+calculus\b",
        r"\bbuild[-\s]+discipline\b", r"\bfail[-\s]+closed[-\s]+on[-\s]+absence\b",
    ],
    # R5's directional expectation must not reach a coder. Recorded as `parasitic`,
    # `cross-cutting`, `layer` — but BARE `layer` is scoped to collocations below.
    # See LAYER_SCOPING_NOTE.
    "r5_framing": [
        r"\bparasitic\b", r"\bcross[-\s]+cutting\b",
        r"\bsix[-\s]+layers\b", r"\blayer[-\s]+column\b", r"\blayer[-\s]+cut\b",
        r"\bsorts?[-\s]+by[-\s]+(?:system[-\s]+)?layer\b", r"\bat[-\s]+different[-\s]+layers\b",
        r"\blayer[-\s]+sorted\b", r"\blayer[-\s]+indexed\b",
    ],
    "source_identifying": [
        r"\bdeferential realism\b", r"\bamnesiac institution\b",
        r"\bOQ-\d+\b", r"\bISSUES\.md\b", r"\bKNOWN_STATE\b", r"\bCLAUDE\.md\b",
        r"\bbuild_discipline\b",
    ],
}

# ===========================================================================
# TWO ROLES, TWO PINNED VERSIONS, ONE MODULE (operator ruling, 2026-08-11).
# ===========================================================================
# The lists above are the DETECTOR. The dicts below are a frozen historical
# artifact used by exactly one caller, and the split is a ruling, not a style
# choice:
#
#   detection (leak-grep)  — a false positive is CONSERVATIVE. You investigate
#                            and clear it, and nothing is lost. Widening is a
#                            strict improvement, so the detector must be as wide
#                            as the evidence supports.
#   selection (density)    — a false positive is SILENTLY DECISIVE. It determines
#                            a choice, and the pre-declaration's entire value is
#                            that it was fixed BEFORE any content was seen.
#
# This is the same distinction controls/redaction_pair_selection_defect.md
# established one ruling earlier: an instrument's error profile is a property of
# its ROLE, not of the instrument. Widening is right for the detector and
# INADMISSIBLE for the selection metric — under the widened lists the declared
# rule's top-3 changes (oq97_pattern6_census 4->9 overtakes oq138 at 5), and it
# moves TOWARD the corrected set, i.e. in the direction that flatters the
# both-residue row. That the movement is convenient is exactly why it cannot be
# taken by re-declaring; see the refusal of option 3 in the ruling.
#
# *** LEXICON_SELECTION_20260811 IS FROZEN. WIDENING IT IS PROHIBITED. ***
# It is a snapshot of the lists in force when controls/redaction_pairs_predeclared.json
# was written. It exists so a past selection can be REPRODUCED, not improved.
# If a defect is found in it, that is a finding to report — never an edit to make.
# Its sole caller is controls/recheck_predeclared_counts.py.

_FROZEN_DIRECTION_I_20260811 = {
    "class_letters": [
        r"\bClass\s+[A-E]\b",
        r"\b[A-E]\s*类\b",
        r"\bclass\s+[A-E]\s*[:\-—]",
    ],
    "class_names": [
        r"\benvironment(?:al)?[/ ]platform quirk\b", r"\bplatform quirk\b",
        r"\bdesign[- ]assumption mismatch\b", r"\bassumption mismatch\b",
        r"\berror swallow(?:ing)?\b", r"\bswallow(?:ed|ing) (?:and|&) dilut\w+\b",
        r"\bdilution\b", r"\bdiluted\b",
        r"\bchain(?:ed)? hallucination\b", r"\bfabrication chain\b",
        r"\boperational omission\b", r"\bforensic blind spot\b",
        r"环境.{0,2}平台", r"设计假设错配", r"错误吞噬", r"稀释",
        r"链式幻觉", r"编造", r"运维遗漏", r"取证盲区",
    ],
    "taxonomy_terms": [
        r"\bfail[- ]plausible\b", r"\bgr[ae]y failure\b", r"\btaxonomy\b",
        r"\bsilent[- ]failure taxonomy\b", r"\bfailure mode(?:s)? catalog\b",
    ],
    "root_cause_structure": [
        r"\btrigger\b", r"\bamplifier\b", r"\bconcealer\b",
        r"触发器", r"放大器", r"掩护者",
    ],
    "rule_ids": [
        r"\bMR-\d+\b", r"\bINV-[A-Z]", r"\b元规则\b", r"\b不变式\b",
        r"\bmeta[- ]rule\b", r"\binvariant\s+INV\b",
    ],
    "source_identifying": [
        r"\bopenclaw\b", r"\bmodel[- ]bridge\b", r"\bbaileys\b",
        r"\bWu\b", r"\barxiv\b", r"2606\.14589",
    ],
}

_FROZEN_DIRECTION_II_20260811 = {
    "p_tokens": [
        r"\bP[1-6]\b", r"\bPattern\s+[1-6]\b", r"\bthe (?:published )?six patterns\b",
    ],
    "pattern_names": [
        r"\bproduced[- ]but[- ]not[- ]consumed\b",
        r"\bone[- ]canonical[- ]thing(?:[- ]became[- ]two)?\b",
        r"\bdestructive[- ]replace\b",
        r"\brecap[- ]as[- ]witness\b",
        r"\babsence satisfies the gate\b",
        r"\bsuccess[- ]shaped absorption\b",
    ],
    "pattern_nicknames": [
        r"\bdangling wire\b", r"\bsilent fork\b", r"\bfaith merge\b",
    ],
    "taxonomy_phrases": [
        r"\bsuccess[- ]shaped\b", r"\bmeasured[- ]empty\b", r"\bdidn'?t[- ]look\b",
        r"\bauthored[- ]zero\b", r"\bpaste[- ]or[- ]untag\b", r"\bwitness calculus\b",
        r"\bbuild discipline\b", r"\bfail[- ]closed on absence\b",
    ],
    "r5_framing": [
        r"\bparasitic\b", r"\bcross[- ]cutting\b",
        r"\bsix layers\b", r"\blayer column\b", r"\blayer cut\b",
        r"\bsorts? by (?:system )?layer\b", r"\bat different layers\b",
        r"\blayer[- ]sorted\b", r"\blayer[- ]indexed\b",
    ],
    "source_identifying": [
        r"\bdeferential realism\b", r"\bamnesiac institution\b",
        r"\bOQ-\d+\b", r"\bISSUES\.md\b", r"\bKNOWN_STATE\b", r"\bCLAUDE\.md\b",
        r"\bbuild_discipline\b",
    ],
}

#: The live detector. Every leak-grep, redaction sweep and payload check uses this.
LEXICON_DETECT = {"i": BANNED_DIRECTION_I, "ii": BANNED_DIRECTION_II}

#: FROZEN. Reproduces the pre-declared selection. Do not widen. One caller only.
LEXICON_SELECTION_20260811 = {"i": _FROZEN_DIRECTION_I_20260811,
                             "ii": _FROZEN_DIRECTION_II_20260811}

LAYER_SCOPING_NOTE = """\
R5 records the banned terms as `parasitic`, `cross-cutting`, and `layer`. BARE `layer` is
deliberately NOT banned, and the deviation is measured rather than argued:

  in the 22 sampled direction-(ii) source dirs (728 KB, 89 files)
    bare "layer"/"layers"          48 hits in 27/89 files
    taxonomy-framing collocations   0 hits in  0/89 files

Banning the bare word would fire on a third of the source files while catching zero real
leaks, and it would strip vocabulary that real mechanisms need ("three layers each
discarded part of the cause" describes an incident, it does not hint at our taxonomy).
Over-redaction destroys codeability and biases units toward `other` — the exact bias
control (c) exists to measure — so an over-broad ban would corrupt the control meant to
catch it. The collocations carry R5's actual hint (that our six sort by system layer) at
zero measured cost. This scoping is frozen with the lexicon and stated in the prereg.
"""

# Shared subject matter. Belongs to NEITHER taxonomy; stripping it destroys codeability.
# The selftest asserts each of these is unmatched by BOTH direction lists, which makes
# "we did not strip shared vocabulary" a checked fact rather than an intention.
PRESERVED = [
    "silent", "silently", "never fired", "green", "empty", "absent", "absence",
    "unknown", "gate", "witness", "control", "count", "zero", "aggregate",
    "layer", "layers", "stale", "fallback", "no error", "reported success",
]

#: Back-compatible alias. `LISTS` has always meant the live detector, and it still does —
#: every existing caller keeps the detection role without changing a line.
LISTS = LEXICON_DETECT
CODER_FACING_FIELDS = ("symptom", "mechanism_as_described", "detection_path", "consequence")


def scan(text: str, direction: str, lexicon: dict | None = None):
    """Return [(group, pattern, matched_text, context)] for every banned hit.

    `lexicon` defaults to LEXICON_DETECT. The ONLY legitimate other value is
    LEXICON_SELECTION_20260811, passed by controls/recheck_predeclared_counts.py to
    reproduce a pre-declared selection. Detection must never be run under the frozen
    lists — it is the version with the known hyphen false-negative.
    """
    hits = []
    for group, pats in (lexicon or LEXICON_DETECT)[direction].items():
        for pat in pats:
            for m in re.finditer(pat, text, re.I):
                hits.append((group, pat, m.group(0),
                             text[max(0, m.start() - 40):m.end() + 40].replace("\n", " ")))
    return hits


def scan_units(path: str, direction: str):
    """Sweep the coder-facing fields of a units file. Returns [(unit_id, field, *hit)]."""
    data = json.load(open(path))
    units = data["units"] if isinstance(data, dict) else data
    out = []
    for u in units:
        for f in CODER_FACING_FIELDS:
            for h in scan(u.get(f, ""), direction):
                out.append((u.get("id", "?"), f, *h))
    return out


def selftest() -> bool:
    ok = True

    def check(label, cond):
        nonlocal ok
        print(f"  {'PASS' if cond else 'FAIL'}  {label}")
        ok = ok and cond

    print("positive controls — a planted leak MUST be caught:")
    for direction, probes in {
        "i": ["This was a Class B failure.", "a classic fail-plausible case",
              "see MR-12 and INV-ONTOLOGY-001", "the concealer was the status file",
              "错误吞噬 与 稀释", "logged in the openclaw repo"],
        "ii": ["this is a P6 instance", "textbook success-shaped absorption",
               "the dangling wire again", "Pattern 5 fired here",
               "P6 is parasitic on the others", "our six sort by system layer",
               "tracked as OQ-97"],
    }.items():
        for probe in probes:
            check(f"dir({direction}) catches {probe!r}", bool(scan(probe, direction)))

    # Separator-tolerance controls (2026-08-11). Each string below is the HYPHEN-JOINED
    # form of a pattern the lists previously expressed with a literal space, and each is
    # drawn from prose that really exists in the source corpus. Before the widening every
    # one of these swept CLEAN — a false absence in the instrument that certifies H2.
    print("\nseparator-tolerance controls — hyphen-joined variants MUST be caught:")
    for direction, probes in {
        "i": ["a Class-B failure", "the fail-plausible rows", "a forensic-blind-spot case",
              "an operational-omission incident", "the failure-modes-catalog rows"],
        "ii": ["Build-Discipline Pattern-1 dangling wire", "Build-discipline spine, twice over",
               "Pattern-6 success-shaped-absorption census", "One-canonical-thing (Pattern-2)",
               "a recap-as-witness substitution", "the absence-satisfies-the-gate shape"],
    }.items():
        for probe in probes:
            check(f"dir({direction}) catches hyphenated {probe!r}", bool(scan(probe, direction)))

    # The two pinned versions must be DIFFERENT, and different in the declared
    # direction. Without this, "we pinned the selection metric" is a comment rather
    # than a fact, and a copy-paste that made the frozen lists identical to the
    # widened ones would look exactly like a correct pin.
    print("\nrole-pinning controls — two versions, each in its declared role:")
    check("the two lexicons are not the same object",
          LEXICON_DETECT is not LEXICON_SELECTION_20260811)
    check("frozen selection lexicon is genuinely DIFFERENT from the detector",
          LEXICON_SELECTION_20260811["ii"] != LEXICON_DETECT["ii"])
    # Probes must ISOLATE the hyphenated token. The full source phrases
    # ("Build-Discipline Pattern-1 dangling wire") also contain vocabulary the frozen
    # list catches for other reasons — `dangling wire`, `success-shaped` — so asserting
    # on them would test nothing about separator handling and would pass either way.
    for probe in ("Pattern-1", "Pattern-6", "Build-discipline"):
        check(f"DETECT catches isolated {probe!r}", bool(scan(probe, "ii")))
        check(f"FROZEN does NOT catch isolated {probe!r} (its known false-negative, "
              f"preserved on purpose so a past selection reproduces)",
              not scan(probe, "ii", LEXICON_SELECTION_20260811))
    # ...and the widening must not have touched anything else: on the full source
    # phrases both versions fire, because both carry the non-hyphenated vocabulary.
    for probe in ("Build-Discipline Pattern-1 dangling wire",
                  "Pattern-6 success-shaped-absorption census"):
        check(f"both versions fire on the full phrase {probe!r} (differing only in WHY)",
              bool(scan(probe, "ii")) and bool(scan(probe, "ii", LEXICON_SELECTION_20260811)))
    for d in ("i", "ii"):
        nd = sum(len(v) for v in LEXICON_DETECT[d].values())
        nf = sum(len(v) for v in LEXICON_SELECTION_20260811[d].values())
        check(f"dir({d}) frozen is a SNAPSHOT not a truncation — same pattern count "
              f"({nf} == {nd}), separators only", nd == nf)

    print("\nfalse-positive controls — legitimate text must NOT be flagged:")
    check("dir(i) ignores 'permission class by default' (the H2 phantom)",
          not scan("permission class by default", "i"))
    check("dir(ii) ignores bare 'three layers each discarded part of the cause'",
          not scan("three layers each discarded part of the cause", "ii"))
    check("dir(ii) ignores 'the reporting layer was silent'",
          not scan("the reporting layer was silent", "ii"))
    check("dir(i) ignores 'the backup failed silently for six days'",
          not scan("the backup failed silently for six days", "i"))

    print("\nshared-vocabulary controls — PRESERVED terms unmatched by BOTH lists:")
    for term in PRESERVED:
        check(f"{term!r} survives both", not scan(term, "i") and not scan(term, "ii"))

    print("\nmatcher-integrity control — a matcher that never fires must fail this:")
    check("scan() is capable of returning hits at all",
          len(scan("Class A fail-plausible MR-4", "i")) >= 3)
    return ok


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--check", action="store_true", help="run the selftest")
    ap.add_argument("--sweep", metavar="UNITS_JSON")
    ap.add_argument("--direction", choices=["i", "ii"])
    ap.add_argument("--note", action="store_true", help="print the layer-scoping note")
    a = ap.parse_args()
    if a.note:
        print(LAYER_SCOPING_NOTE)
        return 0
    if a.check:
        print("OQ-277 lexicon selftest\n")
        good = selftest()
        print(f"\n{'GREEN — every control fired as pre-registered' if good else 'RED'}")
        return 0 if good else 1
    if a.sweep:
        if not a.direction:
            print("--sweep requires --direction", file=sys.stderr)
            return 2
        hits = scan_units(a.sweep, a.direction)
        for uid, field, group, pat, txt, ctx in hits:
            print(f"  LEAK {uid}.{field}  [{group}] {pat} -> {txt!r}\n       ...{ctx}...")
        n = json.load(open(a.sweep))
        n = len(n["units"] if isinstance(n, dict) else n)
        print(f"\nswept {n} units x {len(CODER_FACING_FIELDS)} fields, direction ({a.direction}): "
              f"{len(hits)} hits")
        return 1 if hits else 0
    ap.print_help()
    return 2


if __name__ == "__main__":
    sys.exit(main())
