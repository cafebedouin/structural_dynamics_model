#!/usr/bin/env python3
"""OQ-277 banned lexicons + leak matcher — CANONICAL, single source.

Both the redaction sweep and the payload leak-grep in the coding driver import from
here. One matcher, one pair of lists: a second copy would be a P2 fork inside the
experiment measuring P2.

Word-boundary matching is mandatory. A substring matcher fired H2 on a phantom during
this audit's own step 1 ("permission *class b*y default" matched banned "Class B"), and
H2 voids a whole direction. That false positive is a permanent selftest control below.

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
    # R5's directional expectation must not reach a coder. Recorded as `parasitic`,
    # `cross-cutting`, `layer` — but BARE `layer` is scoped to collocations below.
    # See LAYER_SCOPING_NOTE.
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

LISTS = {"i": BANNED_DIRECTION_I, "ii": BANNED_DIRECTION_II}
CODER_FACING_FIELDS = ("symptom", "mechanism_as_described", "detection_path", "consequence")


def scan(text: str, direction: str):
    """Return [(group, pattern, matched_text, context)] for every banned hit."""
    hits = []
    for group, pats in LISTS[direction].items():
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
