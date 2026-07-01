#!/usr/bin/env python3
"""Gate check (OQ-197): human-facing gap surfaces MUST distinguish no_gap from undetermined.

Converts the Pattern-6-downstream class — an `undetermined` gap reading as "no finding" /
"(none)" / "not enriched" the way the old collapsed `[]` did — from "caught if someone asks
the right question" (how enhanced_report was caught, on the highest-propagation surface) to
"fails red in the gate". This bug has now appeared at 4-5 sites across the OQ-197 work and been
caught each time by attention, not a control; the recurrence is the argument for this check.

Covers the three dedicated gap-operability renderers with a paired synthetic fixture (one
no_gap, one undetermined). Includes a SELF-TEST positive control: a degenerate renderer that
collapses the two states MUST be flagged, else this check is vacuous (an introduced instrument
is itself a claim).

Run: python3 python/check_gap_status_surfaces.py   (exit 0 green / 1 red). Wired into scripts/gate.sh.
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))  # python/

from tensions_ledger import build_block                 # noqa: E402
from enhanced_report import build_omega_section          # noqa: E402
from query import format_gaps_block                      # noqa: E402

# Paired fixture — identical except the gap operability state.
_PERSP = {"powerless": "scaffold", "moderate": "scaffold", "institutional": "scaffold"}
NO_GAP = {"id": "_fixture_no_gap", "gap_status": "no_gap",
          "gap_undetermined_reason": None, "gaps": [], "perspectives": dict(_PERSP)}
UNDET = {"id": "_fixture_undet", "gap_status": "undetermined",
         "gap_undetermined_reason": "single_power_position", "gaps": None,
         "perspectives": dict(_PERSP)}

def check_surface(name, text_no_gap, text_undet):
    """Assert the two renders are distinguishable and correctly labeled. Returns errors.

    Three assertions, each unambiguous: (1) the two states do not render identical text;
    (2) the undetermined render carries the 'undetermined' label (a collapse to "(none)"/
    "not yet enriched"/"Gaps (0)" drops it, so this catches the collapse without matching
    the legitimate "NOT 'no gap'" clarification the correct labels contain); (3) the no_gap
    render is not mislabeled 'undetermined'.
    """
    errs = []
    lo_no, lo_un = text_no_gap.lower(), text_undet.lower()
    if text_no_gap.strip() == text_undet.strip():
        errs.append(f"{name}: no_gap and undetermined render IDENTICAL text")
    if "undetermined" not in lo_un:
        errs.append(f"{name}: undetermined render never says 'undetermined' (collapsed to no-finding)")
    if "undetermined" in lo_no:
        errs.append(f"{name}: no_gap render wrongly says 'undetermined'")
    return errs


def _omega_text(entry):
    return build_omega_section(entry["id"], None, {"per_constraint": [entry]})


def main():
    # SELF-TEST first (positive control): a renderer that collapses the two MUST be caught.
    degenerate = "  Gaps (0)\n  (none)"
    if not check_surface("_selftest", degenerate, degenerate):
        print("gap surfaces check: SELF-TEST FAILED — the check does not catch a collapsing renderer")
        return 1

    errors = []
    errors += check_surface(
        "tensions_ledger.build_block",
        build_block(dict(NO_GAP)), build_block(dict(UNDET)))
    errors += check_surface(
        "enhanced_report.build_omega_section",
        _omega_text(NO_GAP), _omega_text(UNDET))
    errors += check_surface(
        "query.format_gaps_block",
        "\n".join(format_gaps_block(NO_GAP)), "\n".join(format_gaps_block(UNDET)))

    if errors:
        for e in errors:
            print("  FAIL:", e)
        print(f"gap surfaces check: RED ({len(errors)} problems)")
        return 1
    print("gap surfaces check: 3/3 human surfaces distinguish no_gap vs undetermined (self-test OK)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
