"""
Contamination-network provenance + salience rendering test (OQ-103).

Witness for: ISSUES.md OQ-103 (story-authored vs corpus-derived edge provenance
made load-bearing at the read site, plus a count-based salience floor).

build_contamination_network renders the FPN neighbor table. These cases pin the
read-site behavior that OQ-103 adds, driven off synthetic pipeline entries so the
checks do not depend on a live corpus run:

  1. authored vs corpus-derived partition (edge_type == 'explicit' => authored).
  2. salience floor: a single-shared-agent edge (shared_agent_count == 1,
     strength 0.30) is floored 'low'; a >=2-agent edge is 'salient'.
  3. empty-above-floor: when a real negative delta is carried ENTIRELY by
     floored edges, the interpretation sentence says so explicitly and does NOT
     promote a weak edge to 'primarily'.
  4. inferred_coupling branch — no live corpus coverage as of 2026-06-11, so it
     is exercised here: shared_agent_count is null and salience falls back to
     edge strength.
"""

import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "python"))

from enhanced_report import (  # noqa: E402
    build_contamination_network,
    _edge_is_authored,
    _edge_is_salient,
)


def _entry(constraint_id, neighbors, intrinsic, effective):
    return {
        "id": constraint_id,
        "contamination_network": {
            "intrinsic_purity": intrinsic,
            "effective_purity": effective,
            "propagation_delta": effective - intrinsic,
            "neighbors": neighbors,
        },
    }


def _pipeline(entries):
    return {"per_constraint": entries}


def test_provenance_partition():
    authored = {"edge_type": "explicit", "edge_strength": 1.0,
                "shared_agent_count": None}
    derived = {"edge_type": "shared_beneficiary", "edge_strength": 0.30,
               "shared_agent_count": 1}
    assert _edge_is_authored(authored) is True
    assert _edge_is_authored(derived) is False
    print("PASS: provenance partition (explicit=authored, shared_*=derived)")


def test_salience_floor():
    single = {"edge_type": "shared_beneficiary", "edge_strength": 0.30,
              "shared_agent_count": 1}
    multi = {"edge_type": "shared_beneficiary", "edge_strength": 0.60,
             "shared_agent_count": 2}
    authored = {"edge_type": "explicit", "edge_strength": 1.0,
                "shared_agent_count": None}
    assert _edge_is_salient(single) is False, "single shared agent must floor 'low'"
    assert _edge_is_salient(multi) is True, ">=2 shared agents must be salient"
    assert _edge_is_salient(authored) is True, "authored edge is always salient"
    print("PASS: salience floor (count<2 floored, count>=2 and authored salient)")


def test_inferred_coupling_fallback():
    # No shared_agent_count => fall back to strength threshold (>=0.6 salient).
    weak = {"edge_type": "inferred_coupling", "edge_strength": 0.30,
            "shared_agent_count": None}
    strong = {"edge_type": "inferred_coupling", "edge_strength": 0.80,
              "shared_agent_count": None}
    assert _edge_is_authored(weak) is False
    assert _edge_is_salient(weak) is False
    assert _edge_is_salient(strong) is True
    print("PASS: inferred_coupling falls back to strength (no live coverage)")


def test_empty_above_floor_sentence():
    # Real negative delta carried entirely by a single-shared-agent edge:
    # the OQ-103 witness shape (reprogramming -> digital_colonialism, 0.30).
    neighbors = [{
        "constraint_id": "digital_colonialism_data_extraction",
        "neighbor_type": "snare",
        "edge_type": "shared_beneficiary",
        "edge_strength": 0.30,
        "shared_agent_count": 1,
        "neighbor_purity": 0.5788,
    }]
    out = build_contamination_network(
        "reprogramming_safety_toxicity",
        _pipeline([_entry("reprogramming_safety_toxicity", neighbors,
                          0.9480, 0.8926)]),
    )
    assert "low-salience" in out, "empty-above-floor sentence missing"
    assert "primarily" not in out, "weak edge must NOT be promoted to 'primarily'"
    assert "corpus-derived" in out, "provenance column/legend missing"
    print("PASS: empty-above-floor sentence (no weak-edge promotion)")


def test_salient_edge_still_headlines():
    # A >=2-agent derived edge survives the floor and DOES headline, tagged
    # corpus-derived (wage_convergence pair shape: strength 0.60, count 2).
    neighbors = [{
        "constraint_id": "wage_convergence_sustainability",
        "neighbor_type": "snare",
        "edge_type": "shared_beneficiary",
        "edge_strength": 0.60,
        "shared_agent_count": 2,
        "neighbor_purity": 0.40,
    }]
    out = build_contamination_network(
        "wage_convergence_mechanism",
        _pipeline([_entry("wage_convergence_mechanism", neighbors,
                          0.97, 0.65)]),
    )
    assert "primarily wage_convergence_sustainability" in out
    assert "corpus-derived" in out
    assert "low-salience" not in out
    print("PASS: salient derived edge headlines, tagged corpus-derived")


def _run_all():
    test_provenance_partition()
    test_salience_floor()
    test_inferred_coupling_fallback()
    test_empty_above_floor_sentence()
    test_salient_edge_still_headlines()


if __name__ == "__main__":
    _run_all()
    print("ALL PASS")
