#!/usr/bin/env python3
"""rqd_scorer.py — mechanical scorer for RQ-d, written BEFORE the spend.

This file exists because the previous arc's spend (219 calls) produced answers
nothing in the repository could score: the analysis half existed in neither code
nor design. The scorer is part of the design, not downstream plumbing, and it is
pinned in the preregistration manifest alongside the prompts.

WHAT IS SCORED. Only the `PROBLEMS OR GAPS:` section of a response. Both
protocols are required to end with that identically-worded section, so the
scored field is the same for both and the scorer never sees which protocol
produced the text — blind by construction, not by a redaction step that could
leak.

HIT CRITERION (strict, the pre-registered primary). A defect counts as detected
when a concept key for that defect occurs in a SENTENCE that also contains a gap
marker. Two consequences, both deliberate:

  * Merely using the word does not count. An instruction that says "compute the
    denominator" has not detected that the denominator is unspecified.
  * SILENTLY SUPPLYING a missing element does not count. Surfacing the gap to
    the sender is the entire value being measured; a receiver who quietly fills
    it in leaves the defect in the design, where it fires on the next receiver.

A lenient count (key anywhere in the section, no marker required) is reported
alongside as a sensitivity check, never as the headline.

CONTROLS: `--selftest`. The scorer is an introduced instrument and owes its own
two-sided record — it must FIRE on a response that flags the defect, DECLINE on
one that is silent, DECLINE on one that mentions the concept without flagging it
(the discriminating case), and must not cross-score omission as error.
"""
import re
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from rqd_materials import SPECS, by_id  # noqa: E402

SECTION_RE = re.compile(r"PROBLEMS\s+OR\s+GAPS\s*:?", re.I)

# A response has detected something only if it MARKS it. These are the markers.
GAP_MARKERS = [
    "not specified", "unspecified", "not stated", "never states", "never say",
    "does not say", "doesn't say", "does not specify", "doesn't specify",
    "not defined", "undefined", "missing", "omit", "absent", "no mention",
    "not mentioned", "unclear", "ambiguous", "gap", "fails to", "fail to",
    "should specify", "must specify", "needs to specify", "need to specify",
    "should state", "must state", "no rule", "not described", "left open",
    "underspecified", "under-specified", "cannot be determined", "unknown",
    "problem", "issue", "wrong", "incorrect", "error", "bug", "contradic",
    "inconsistent", "mismatch", "does not match", "will not", "cannot",
    "never checked", "not checked", "no way to", "impossible", "silently",
    "?",
]


def extract_section(response):
    """The PROBLEMS OR GAPS section, or '' if the response never produced one.

    '' is a real outcome (the protocol was not followed) and is distinct from a
    section that exists and lists nothing — the caller is told which.
    """
    m = SECTION_RE.search(response or "")
    if not m:
        return ""
    return response[m.end():]


# AMENDED 2026-08-13, mid-run, DECLARED in PREREGISTRATION.md §9. The original
# split on sentence punctuation `(?<=[.!?])\s+`, which real model output breaks:
# a bullet reading "...the exact material/prompt shown to coders (full record
# vs. summary only) is not specified." splits at "vs." and strands the concept
# key in one fragment and the gap marker in the next. Scored FALSE on a bullet
# that flags the omission in as many words. Found by reading real pilot output
# against the scorer's verdict; the synthetic selftest fixtures contained no
# abbreviations and passed throughout. A false-NEGATIVE generator, i.e. it would
# have suppressed detections in whichever arm writes more parenthetical prose.
#
# Responses are bulleted claims, so the unit is now a LINE/BULLET, not a
# sentence, with a proximity cap so a long chunk cannot manufacture a spurious
# co-occurrence between unrelated clauses.
PROXIMITY = 300


def _chunks(text):
    parts = re.split(r"\n+|(?:^|\n)\s*[-*•]\s*", text)
    return [p.strip() for p in parts if p and p.strip()]


def score_defect(section, keys):
    """(strict_hit, lenient_hit, evidence_chunk_or_None)."""
    low = section.lower()
    lenient = any(k.lower() in low for k in keys)
    for chunk in _chunks(section):
        c = chunk.lower()
        kpos = [c.find(k.lower()) for k in keys if k.lower() in c]
        mpos = [c.find(m) for m in GAP_MARKERS if m in c]
        if kpos and mpos and min(abs(a - b) for a in kpos for b in mpos) <= PROXIMITY:
            return True, lenient, chunk[:200]
    return False, lenient, None


def score_response(spec_id, response):
    spec = by_id(spec_id)
    section = extract_section(response)
    o_strict, o_len, o_ev = score_defect(section, spec["omission_keys"])
    e_strict, e_len, e_ev = score_defect(section, spec["error_keys"])
    return {
        "spec_id": spec_id,
        "section_present": bool(section.strip()),
        "omission_strict": o_strict, "omission_lenient": o_len, "omission_evidence": o_ev,
        "error_strict": e_strict, "error_lenient": e_len, "error_evidence": e_ev,
    }


# ---------------------------------------------------------------- selftest

def _selftest():
    ok = True

    def check(label, cond, detail=""):
        nonlocal ok
        ok = ok and cond
        print(f"  {'PASS' if cond else 'FAIL'}  {label}" + (f"  [{detail}]" if detail else ""))

    print("rqd_scorer selftest")

    # 1. FIRES — a response that flags the omission.
    r = ("Here is the receiver prompt.\n\nPROBLEMS OR GAPS:\n"
         "- The design never says what material is shown to the coder. "
         "That is unspecified and a receiver could not proceed.\n")
    s = score_response("judged_not_shown", r)
    check("fires: flagged omission scores strict", s["omission_strict"], s["omission_evidence"] or "")

    # 2. DECLINES — a response that flags nothing.
    r = "Looks complete and correct to me.\n\nPROBLEMS OR GAPS:\nNone identified.\n"
    s = score_response("judged_not_shown", r)
    check("declines: clean response scores no omission", not s["omission_strict"])

    # 3. THE DISCRIMINATING CASE — the concept appears, but as silent supply
    #    rather than as a flag. Must DECLINE strict and register lenient, or the
    #    scorer is measuring vocabulary rather than detection.
    r = ("PROBLEMS OR GAPS:\n"
         "- Step 3: assemble the packet shown to the coder and send it.\n")
    s = score_response("judged_not_shown", r)
    check("declines silent supply (concept present, not marked) — strict off, lenient on",
          (not s["omission_strict"]) and s["omission_lenient"],
          "this is the case that separates detection from vocabulary")

    # 4. NO CROSS-SCORING — flagging the ERROR must not register as the OMISSION.
    r = ("PROBLEMS OR GAPS:\n"
         "- Taking the first 40 records in directory order is a non-random "
         "sample and is wrong for this claim.\n")
    s = score_response("judged_not_shown", r)
    check("no cross-scoring: error flag scores error, not omission",
          s["error_strict"] and not s["omission_strict"])

    # 5. MISSING SECTION is distinguishable from an empty one.
    a = score_response("judged_not_shown", "I have no section at all.")
    b = score_response("judged_not_shown", "PROBLEMS OR GAPS:\nNone.")
    check("absent section distinguishable from empty section",
          (not a["section_present"]) and b["section_present"])

    # 6. Every spec's two key sets must be DISJOINT under the scorer, or the
    #    within-artifact dissociation is unmeasurable by construction. This is a
    #    property of the MATERIALS, checked here because the scorer is what makes
    #    it matter.
    bad = []
    for spec in SPECS:
        probe = "PROBLEMS OR GAPS:\n- " + " ".join(spec["omission_keys"]) + " is missing.\n"
        sc = score_response(spec["id"], probe)
        if sc["error_strict"]:
            bad.append(spec["id"])
    check("omission keys do not trip the error scorer on any spec",
          not bad, f"cross-tripping specs: {bad}" if bad else "10/10 disjoint")

    # 7. NATURALLY-ARISING REGRESSION FIXTURE — verbatim from real pilot output
    #    (judged_not_shown / recognition / r1). This exact bullet scored FALSE
    #    under the pre-amendment sentence splitter because "vs." split it. It is
    #    a real positive drawn from the population, not an authored decoy, and it
    #    is the control the synthetic fixtures could not supply.
    r = ('PROBLEMS OR GAPS:\n'
         '- The exact material/prompt shown to coders (full record vs. summary '
         'only, any length limits) is not specified.\n')
    s = score_response("judged_not_shown", r)
    check("real-output fixture: abbreviation inside a flagged bullet still scores",
          s["omission_strict"], "pre-amendment this scored False")

    # 8. The proximity cap must still DECLINE when key and marker are unrelated
    #    and far apart in one chunk — otherwise the amendment traded a false
    #    negative for a false positive.
    filler = " padding" * 90
    r = ("PROBLEMS OR GAPS:\n- The packet is fine." + filler +
         " Separately, the kappa threshold is wrong.\n")
    s = score_response("judged_not_shown", r)
    check("proximity cap declines distant key/marker pair in one chunk",
          not s["omission_strict"], "guards against the amendment over-firing")

    print("SELFTEST", "GREEN" if ok else "RED")
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(_selftest())
