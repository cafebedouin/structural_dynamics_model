"""Fixtures for the OQ-216 stage-2 invariant-contract extractor (redesigned 2026-07-25).

The pre-redesign guard checked header-string-at-position — a proxy that failed
BOTH directions in production: it blocked four drifted-but-complete Sonnet-5
outputs (prometheus ×2, quellcrist, 112_ergodocity_kids) and it passed
the_floating_city_xixi while over-capturing to EOF (18,266-byte payload
consumed by stages 9/10). Each fixture below models a witnessed shape; the
synthetic negative controls (falsifier deleted, EOF truncation) model the
shapes the content-level guard exists to catch and the old guard could not.

Run: python3 python/tests/test_stage2_contract_extraction.py   (exit 0 = all pass)
"""
import sys, pathlib
sys.path.insert(0, str(pathlib.Path(__file__).resolve().parents[2]))
from agent.uke_narrative_orchestrator import (  # noqa: E402
    _extract_invariant_contract_checked,
)

CONTRACT_BODY = (
    "**The invariant:** a real the instruments cannot read.\n"
    "**Falsifier:** if the story contains a recoverable true value, lost.\n"
    "**Substrate:** deep rock that holds it.\n"
    "**Substrate inhabitation sentence:** *You stand on what no meter weighs.*\n"
)

# rotation_seven / the_empty_pan family: canonical shape, SECTION 0 first.
CANONICAL = (
    "# STAGE 2: NATURALIZATION OUTPUT\n\n"
    "## SECTION 0: INVARIANT CONTRACT\n\n" + CONTRACT_BODY +
    "\n## SECTION 1: CONTEXT\n\nSetting prose.\n\n"
    "## SECTION 2: OMEGA LOG\n\nRESOLVED: none.\n"
)

# prometheus_1785030750 (×2 draws): contract demoted to an H3 subsection
# inside SECTION 1, no SECTION 0 header anywhere.
DRIFTED_SUBSECTION = (
    "# STAGE 2: NATURALIZATION\n\n"
    "## Step 0 — Invariant Recovery (worked, not shown in output)\n\n"
    "Working notes.\n\n"
    "## SECTION 1: CONTEXT DESCRIPTION\n\n"
    "### Invariant Contract (carried forward verbatim)\n\n" + CONTRACT_BODY +
    "\n### Setting\n\nSetting prose.\n\n"
    "## SECTION 2: OMEGA LOG\n\nRESOLVED: none.\n"
)

# the_floating_city_xixi_1784000706: SECTION 0 misplaced AFTER the SECTION 1
# header. Old extractor over-captured to EOF here (its end-anchor searched for
# a FOLLOWING 'SECTION 1'); the level-bounded extractor must stop at the next
# same-level heading.
MISORDERED = (
    "# STAGE 2: NATURALIZATION\n\n"
    "## Step 0 — Invariant Recovery (Worked, Before Output)\n\nNotes.\n\n"
    "# SECTION 1: CONTEXT DESCRIPTION\n\n"
    "## SECTION 0: INVARIANT CONTRACT\n\n" + CONTRACT_BODY +
    "\n## Setting\n\nLong setting prose that must NOT be captured.\n\n"
    "## SECTION 2: OMEGA LOG\n\nRESOLVED: none.\n"
)

# quellcrist_1784034874: contract content only inside Step-0 working notes,
# no contract heading of any form. Must fail (re-run is the correct path).
NO_HEADER = (
    "# STAGE 2: NATURALIZATION\n\n"
    "## Step 0 — Invariant Recovery (worked, not shown to reader)\n\n"
    + CONTRACT_BODY +
    "\n# SECTION 1: CONTEXT DESCRIPTION\n\nSetting prose.\n"
)

# Synthetic: truncated draw — contract present but output dies inside it.
# SECTION 0 is mandated first, so EOF-termination is always over-capture or
# truncation; either way stages 9/10 must not consume it silently.
EOF_TRUNCATED = (
    "# STAGE 2: NATURALIZATION OUTPUT\n\n"
    "## SECTION 0: INVARIANT CONTRACT\n\n" + CONTRACT_BODY
)

# Synthetic: component missing — the 30%-shorter-draw failure shape
# (9,304 vs 6,437 tokens on identical input, prometheus). Header and bound
# fine; a mandated component (falsifier) is gone.
FALSIFIER_DELETED = CANONICAL.replace(
    "**Falsifier:** if the story contains a recoverable true value, lost.\n",
    "")

# Synthetic: prose mention must not match as a header (anchor specificity).
PROSE_MENTION_ONLY = (
    "# STAGE 2: NATURALIZATION\n\n"
    "The invariant contract from stage 0 says things.\n\n"
    "## SECTION 1: CONTEXT\n\nSetting prose.\n\n"
    "## SECTION 2: OMEGA LOG\n\nnone.\n"
)

CASES = [
    # (name, text, want_ok, want_err_substring, forbidden_in_block)
    ("canonical SECTION-0-first extracts", CANONICAL, True, "", "SECTION 1"),
    ("drifted H3 subsection extracts", DRIFTED_SUBSECTION, True, "", "Setting prose"),
    ("misordered bounds at next heading", MISORDERED, True, "", "must NOT be captured"),
    ("no header fails loud", NO_HEADER, False, "no contract heading", None),
    ("EOF termination fails loud", EOF_TRUNCATED, False, "terminates at EOF", None),
    ("missing falsifier fails loud", FALSIFIER_DELETED, False, "falsifier", None),
    ("prose mention is not a header", PROSE_MENTION_ONLY, False, "no contract heading", None),
]


def main():
    failures = []
    for name, text, want_ok, want_err, forbidden in CASES:
        block, err = _extract_invariant_contract_checked(text)
        ok = (err == "") if want_ok else (err != "" and want_err in err)
        if ok and want_ok and forbidden and forbidden in block:
            ok = False
            err = f"over-capture: block contains {forbidden!r}"
        if ok and want_ok:
            for comp in ("invariant", "falsifier", "substrate", "inhabitation"):
                if comp not in block.lower():
                    ok = False
                    err = f"extracted block lost component {comp!r}"
        status = "PASS" if ok else "FAIL"
        print(f"[{status}] {name}"
              + ("" if ok else f"  (block={len(block)}ch err={err!r})"))
        if not ok:
            failures.append(name)
    if failures:
        print(f"\n{len(failures)} failure(s): {failures}")
        return 1
    print(f"\nAll {len(CASES)} cases pass.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
