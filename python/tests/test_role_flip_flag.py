"""
OQ-188 role-flip flag + OQ-186 common-cause discriminator regression test.

Witness for: ISSUES.md OQ-188 (read-site flag for verdicts knife-edge on a
discrete authored stakeholder-role choice) and OQ-186 (common-cause clique
must not read as independent corroboration). Pre-registration + Phase-1
census: audits/2026-07-11_oq186_oq188_readsite/.

Synthetic perspective_chi + config fixtures — no live corpus dependency —
so the Phase-2/3 two-sided witnesses don't stay one-shot pastes:

  1. flag fires at d=0.12 (agenda_setter; nearest-alt beneficiary 0.25 is
     across the f(d) sign root) and at d=0.25 (the straddle is symmetric).
  2. flag silent at d=0.85 (payer; nearest-alt excluded 0.90, same sign).
  3. out-of-domain buckets return None, never False-as-robust: null d,
     canonical 0.0, unmatched d (0.15), missing config (Pattern 6).
  4. render: _compact_types names a flagged seat even when another seat
     already represents its type, and appends the glyph.
  5. OQ-186: common-cause pair fires on shared beneficiary+victim at
     |d-eps| <= 0.02, stays False when either side's agents are disjoint or
     eps is far, None when eps is null or the entry is missing.
  6. defensibility downgrade: with either evidence boolean true the
     coordination ruling is NOT emitted as indefensible and the caveat rides
     constrained_positions; with both false the original ruling is unchanged.
"""

import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "python"))

from shared.role_flip import role_flip_fired_seats, seat_fires, GLYPH  # noqa: E402
from shared.independence import is_common_cause_pair  # noqa: E402
from enhanced_report import _compact_types  # noqa: E402
from evaluative_convergence import build_defensibility  # noqa: E402

CONFIG = {
    "sigmoid_lower": -0.20,
    "sigmoid_upper": 1.50,
    "sigmoid_midpoint": 0.50,
    "sigmoid_steepness": 6.00,
    "stakeholder_role_d_agenda_setter": 0.12,
    "stakeholder_role_d_beneficiary": 0.25,
    "stakeholder_role_d_payer": 0.85,
    "stakeholder_role_d_excluded": 0.90,
    "stakeholder_role_d_observer": 0.72,
}


def _entry(inst_d, powerless_d=0.95):
    return {
        "id": "synthetic",
        "perspective_chi": {
            "powerless": {"d": powerless_d},
            "moderate": {"d": 0.70},
            "institutional": {"d": inst_d},
            "analytical": {"d": 0.72},
        },
    }


def test_flag_fires_on_straddle():
    assert seat_fires(0.12, CONFIG) is True, "agenda_setter 0.12 must fire"
    assert seat_fires(0.25, CONFIG) is True, "beneficiary 0.25 must fire (symmetric)"
    assert role_flip_fired_seats(_entry(0.12), CONFIG) == frozenset({"institutional"})
    print("PASS: flag fires at d=0.12 and d=0.25 (the straddle pair)")


def test_flag_silent_off_straddle():
    assert seat_fires(0.85, CONFIG) is False, "payer 0.85 must be silent"
    assert seat_fires(0.72, CONFIG) is False, "observer 0.72 must be silent"
    assert role_flip_fired_seats(_entry(0.85), CONFIG) == frozenset()
    print("PASS: flag silent at d=0.85 / d=0.72 (matched, same-sign nearest-alt)")


def test_out_of_domain_is_none_not_false():
    assert seat_fires(None, CONFIG) is None, "null d is out of domain"
    assert seat_fires(0.0, CONFIG) is None, "canonical fallback is out of domain"
    assert seat_fires(0.15, CONFIG) is None, "unmatched d is out of domain"
    assert seat_fires(0.12, {}) is None, "missing config is not computable"
    assert role_flip_fired_seats(None, CONFIG) == frozenset()
    print("PASS: null/canonical/unmatched/no-config are None (never False-as-robust)")


def test_render_names_flagged_seat():
    # institutional shares its type with powerless; unflagged compaction names
    # only powerless — the flagged render must ALSO name institutional + glyph.
    persp = {"powerless": "rope", "institutional": "rope", "analytical": "snare"}
    plain = _compact_types(persp)
    assert "institutional" not in plain, "precondition: compaction hides institutional"
    flagged = _compact_types(persp, frozenset({"institutional"}))
    assert f"institutional{GLYPH}" in flagged, "flagged seat must be named with glyph"
    assert GLYPH not in plain
    print("PASS: _compact_types names the flagged seat with the glyph")


def _cc_entry(bens, vics, eps):
    return {"beneficiaries": bens, "victims": vics, "base_extractiveness": eps}


def test_common_cause_pair():
    a = _cc_entry(["ben1"], ["vic1"], 0.68)
    assert is_common_cause_pair(a, _cc_entry(["ben1"], ["vic1"], 0.69)) is True
    assert is_common_cause_pair(a, _cc_entry(["ben2"], ["vic1"], 0.68)) is False, \
        "beneficiary-disjoint pair must be distinct"
    assert is_common_cause_pair(a, _cc_entry(["ben1"], ["vic2"], 0.68)) is False, \
        "victim-disjoint pair must be distinct"
    assert is_common_cause_pair(a, _cc_entry(["ben1"], ["vic1"], 0.75)) is False, \
        "far eps must be distinct"
    assert is_common_cause_pair(a, _cc_entry(["ben1"], ["vic1"], None)) is None, \
        "null eps is not computable"
    assert is_common_cause_pair(a, None) is None, "missing entry is not computable"
    print("PASS: common-cause pair (both-sides overlap + eps margin; None on absence)")


def _patterns(knife, clique):
    return [
        {"pattern": "convergent_signature",
         "evidence": {"shared_signature": "s", "constraints": ["a", "b"]}},
        {"pattern": "convergent_institutional",
         "evidence": {"institutional_type": "rope", "analytical_type": "snare",
                      "constraints_with_split": ["a", "b"],
                      "all_members_knife_edge": knife,
                      "members_common_cause_clique": clique}},
    ]


def test_defensibility_downgrade_two_sided():
    down = build_defensibility(_patterns(True, True))
    assert not any("independent coordination" in p.get("position", "")
                   for p in down["indefensible_positions"]), \
        "artifact-channel set must not emit the coordination ruling"
    assert any("not by itself evidence of coordination" in c
               for c in down["constrained_positions"]), "caveat must ride constrained"
    up = build_defensibility(_patterns(False, False))
    assert any("independent coordination" in p.get("position", "")
               for p in up["indefensible_positions"]), \
        "clean set must keep the original ruling"
    assert any("coordinated rather than independent" in p.get("ruled_out_by", "")
               for p in up["indefensible_positions"])
    print("PASS: defensibility downgrade two-sided (caveated on artifact, unchanged clean)")


def _run_all():
    test_flag_fires_on_straddle()
    test_flag_silent_off_straddle()
    test_out_of_domain_is_none_not_false()
    test_render_names_flagged_seat()
    test_common_cause_pair()
    test_defensibility_downgrade_two_sided()


if __name__ == "__main__":
    _run_all()
    print("ALL PASS")
