"""Negative-control fixture for the OQ-219 Stage-2 dominance-ordering gate.

Structural gate ⇒ fixture ⇒ no cost excuse for skipping it (operator, 2026-07-13).
Asserts the clause reaches the Stage-2 prompt IFF the Stage-0 contract authors
missing_floor present="yes" AND primary="yes" — and is INERT otherwise. The
INERT cases are the ones that matter: the failure mode this guards is OVER-FIRING
(the hard-ban mistake relocated) — the clause suppressing the grain globally and
flattening the legitimate dual-real richness of grain-primary (Margins-class) stories.

Run: python3 agent/tests/test_stage2_dominance_gate.py   (exit 0 = all pass)
"""
import sys, pathlib
sys.path.insert(0, str(pathlib.Path(__file__).resolve().parents[2]))
from agent.uke_narrative_orchestrator import (  # noqa: E402
    _stage2_dominance_suffix, _contract_marks_floor_primary, _STAGE2_DOMINANCE_CLAUSE,
)

FLOOR_PRIMARY = (
    '<invariant_contract>\n'
    '  <untranslatable_real present="yes" primary="no">a real</untranslatable_real>\n'
    '  <missing_floor present="yes" primary="yes">a chosen zero</missing_floor>\n'
    '  <inherent_instrument value="yes">x</inherent_instrument>\n'
    '</invariant_contract>'
)
GRAIN_PRIMARY = (
    '<invariant_contract>\n'
    '  <untranslatable_real present="yes" primary="yes">a real</untranslatable_real>\n'
    '  <missing_floor present="yes" primary="no">a chosen zero</missing_floor>\n'
    '  <inherent_instrument value="yes">x</inherent_instrument>\n'
    '</invariant_contract>'
)
FLOOR_PRESENT_NO_PRIMARY_ATTR = (  # legacy contracts (pre-flag): must be INERT
    '<invariant_contract>\n'
    '  <untranslatable_real present="yes">a real</untranslatable_real>\n'
    '  <missing_floor present="yes">a chosen zero</missing_floor>\n'
    '</invariant_contract>'
)
FLOOR_ABSENT = (
    '<invariant_contract>\n'
    '  <untranslatable_real present="yes" primary="yes">a real</untranslatable_real>\n'
    '  <missing_floor present="no" primary="no">absent</missing_floor>\n'
    '</invariant_contract>'
)

CASES = [
    ("floor-primary → clause injected", FLOOR_PRIMARY, True),
    ("grain-primary → INERT (over-fire guard)", GRAIN_PRIMARY, False),
    ("floor present, no primary attr (legacy) → INERT", FLOOR_PRESENT_NO_PRIMARY_ATTR, False),
    ("floor absent → INERT", FLOOR_ABSENT, False),
    ("empty contract → INERT", "", False),
]


def main():
    failures = []
    for name, contract, want_clause in CASES:
        suffix = _stage2_dominance_suffix(contract)
        got_clause = suffix == _STAGE2_DOMINANCE_CLAUSE
        got_empty = suffix == ""
        ok = got_clause if want_clause else got_empty
        # extra: when a clause is expected it must be non-empty and carry the header
        if want_clause and "DOMINANCE ORDERING" not in suffix:
            ok = False
        print(f"  {'PASS' if ok else 'FAIL'}  {name}"
              f"  (suffix {'=clause' if got_clause else 'empty' if got_empty else 'OTHER'})")
        if not ok:
            failures.append(name)
    # symmetry: the predicate must agree with the suffix
    assert _contract_marks_floor_primary(FLOOR_PRIMARY) is True
    assert _contract_marks_floor_primary(GRAIN_PRIMARY) is False
    if failures:
        print(f"\nFAILED: {len(failures)} case(s): {failures}")
        return 1
    print("\nAll dominance-gate fixture cases PASS (clause fires iff floor-primary; INERT otherwise).")
    return 0


if __name__ == "__main__":
    sys.exit(main())
