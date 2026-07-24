"""
OQ-61 render-side regression tests: Q1 severe-fraction render (four branches +
fail-closed inconsistency asserts), Q2 type×band cross-tab marginal asserts, and
the Q1 backstop severity_by_type render.

Synthetic diagnostic dicts — no live corpus dependency — so the four
network-render branches (which one corpus each would exercise only one of) are
all witnessed in one place, and the Q2 renderer's marginal asserts are exercised
on both a well-formed tab and a deliberately-broken one.

No pytest (AGENTS.md §5 "No pytest setup"): a self-contained main() runner with
plain asserts, exit 1 on any failure.

Branches (derived from network_dynamics.pl clause order — cascading/degrading
only when n_drifting>0; stable/undetermined only when n_drifting==0):
  1. n_drifting>0, cascading  -> "N/M drifting are severe (pct%)"
  2. n_drifting>0, degrading  -> same fraction render
  3. n_drifting==0, stable    -> "no drifting (full coverage)"
  4. n_drifting==0, undetermined -> "no drifting observed, coverage incomplete"
Fail-closed: n_severe>n_drifting, token/threshold mismatch, and a wrong-shape
band tab each raise AssertionError rather than rendering a plausible line.
"""

import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "python"))

from enhanced_report import (  # noqa: E402
    _network_severity_render,
    build_purity_type_band_block,
    build_severity_by_type_block,
)


def _raises(fn):
    """True iff fn() raises AssertionError."""
    try:
        fn()
    except AssertionError:
        return True
    return False


# --- Q1: severe-fraction render, four branches ---

def test_cascading_fraction():
    diag = {"network_stability": "cascading", "network_n_drifting": 10,
            "network_n_severe": 7, "network_cascade_count_threshold": 3}
    out = _network_severity_render(diag)
    assert out == ("Network severity: 7/10 drifting are severe (70%) "
                   "[severe = effective purity < 0.70]"), out


def test_degrading_fraction():
    diag = {"network_stability": "degrading", "network_n_drifting": 10,
            "network_n_severe": 2, "network_cascade_count_threshold": 3}
    out = _network_severity_render(diag)
    assert out.startswith("Network severity: 2/10 drifting are severe (20%)"), out


def test_zero_drift_stable():
    diag = {"network_stability": "stable", "network_n_drifting": 0,
            "network_n_severe": 0}
    assert _network_severity_render(diag) == "Network severity: no drifting (full coverage)"


def test_zero_drift_undetermined():
    diag = {"network_stability": "undetermined", "network_n_drifting": 0,
            "network_n_severe": 0}
    assert (_network_severity_render(diag)
            == "Network severity: no drifting observed, coverage incomplete")


def test_pre_q1_output_falls_back_to_token():
    diag = {"network_stability": "cascading"}
    assert _network_severity_render(diag) == "Network stability: cascading"


# --- Q1: fail-closed inconsistency ---

def test_severe_exceeds_drifting_raises():
    diag = {"network_stability": "cascading", "network_n_drifting": 3,
            "network_n_severe": 5, "network_cascade_count_threshold": 3}
    assert _raises(lambda: _network_severity_render(diag))


def test_token_threshold_mismatch_raises():
    diag = {"network_stability": "cascading", "network_n_drifting": 10,
            "network_n_severe": 1, "network_cascade_count_threshold": 3}
    assert _raises(lambda: _network_severity_render(diag))


def test_stable_token_with_drift_raises():
    diag = {"network_stability": "stable", "network_n_drifting": 4,
            "network_n_severe": 1, "network_cascade_count_threshold": 3}
    assert _raises(lambda: _network_severity_render(diag))


def test_cascading_at_zero_drift_is_inconsistent_marker():
    diag = {"network_stability": "cascading", "network_n_drifting": 0,
            "network_n_severe": 0}
    assert "INCONSISTENT" in _network_severity_render(diag)


# --- Q2: type×band cross-tab marginal asserts ---

def _pc(claimed_type, purity_class, purity_band=None):
    return {"claimed_type": claimed_type, "purity_class": purity_class,
            "purity_band": purity_band}


def _well_formed_corpus():
    return [
        _pc("rope", "scored", "sound"),           # in-band
        _pc("rope", "scored", "contaminated"),    # off-diagonal -> cover-story
        _pc("rope", "gate_fail"),
        _pc("rope", "no_data"),
        _pc("tangled_rope", "scored", "sound"),   # off-diagonal -> fragile-rope
    ]


def test_q2_marginals_pass_on_well_formed():
    pc = _well_formed_corpus()
    diag = {"purity_n_scored": 3, "purity_n_gate_fail": 1, "purity_n_no_data": 1}
    text = "\n".join(build_purity_type_band_block(pc, diag))
    assert "cover-story candidates" in text
    assert "fragile-rope candidates" in text
    assert "totals" in text


def test_q2_grand_scored_mismatch_raises():
    pc = _well_formed_corpus()
    diag = {"purity_n_scored": 99, "purity_n_gate_fail": 1, "purity_n_no_data": 1}
    assert _raises(lambda: build_purity_type_band_block(pc, diag))


def test_q2_gf_column_mismatch_raises():
    pc = _well_formed_corpus()
    diag = {"purity_n_scored": 3, "purity_n_gate_fail": 99, "purity_n_no_data": 1}
    assert _raises(lambda: build_purity_type_band_block(pc, diag))


def test_q2_pre_q3_output_skips_tab():
    pc = [{"claimed_type": "rope", "purity_band": "sound"}]
    assert build_purity_type_band_block(pc, {}) == []


# --- Q1 backstop: severity_by_type render + marginal assert ---

def test_backstop_severe_total_matches():
    diag = {
        "network_n_severe": 3, "network_n_drifting": 5,
        "severity_by_type": {
            "rope": {"critical": 1, "warning": 1, "watch": 1,
                     "undetermined": 0, "severe": 2, "drifting": 3},
            "snare": {"critical": 1, "warning": 0, "watch": 1,
                      "undetermined": 0, "severe": 1, "drifting": 2},
        },
    }
    assert any("SEVERITY x TYPE" in ln for ln in build_severity_by_type_block(diag))


def test_backstop_severe_total_mismatch_raises():
    diag = {
        "network_n_severe": 99, "network_n_drifting": 5,
        "severity_by_type": {
            "rope": {"critical": 1, "warning": 1, "watch": 1,
                     "undetermined": 0, "severe": 2, "drifting": 3},
        },
    }
    assert _raises(lambda: build_severity_by_type_block(diag))


def test_backstop_absent_field_empty():
    assert build_severity_by_type_block({}) == []


def main():
    tests = [v for k, v in sorted(globals().items()) if k.startswith("test_")]
    failures = 0
    for t in tests:
        try:
            t()
            print(f"  PASS {t.__name__}")
        except Exception as e:  # noqa: BLE001
            failures += 1
            print(f"  FAIL {t.__name__}: {e}")
    print(f"\n{len(tests) - failures}/{len(tests)} passed")
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
