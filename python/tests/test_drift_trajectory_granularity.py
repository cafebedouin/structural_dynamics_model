"""
Drift-trajectory granularity-guard test (OQ-19).

Witness for: ISSUES.md OQ-19 (temporal-shape trigger thresholds are
corpus-specific magic numbers, justified against a 2-decimal measurement
granularity floor). The fix hoists the thresholds into named _DRIFT_* constants
and adds a runtime guard (_series_granularity) that flags, at the read site,
when the actual series are FINER than the calibration floor — so the assumption
stops being silent.

These cases pin the read-site behavior, driven off synthetic pipeline entries so
the checks do not depend on a live corpus run:

  1. Negative/baseline: a 2-decimal series => _series_granularity == 0.01 and the
     rendered section carries NO [CALIBRATION WARNING].
  2. Positive control: the SAME series with one value at 3 decimals =>
     _series_granularity == 0.001 and the rendered section DOES carry
     [CALIBRATION WARNING]. Without this, an empty-warning result cannot be
     distinguished from "the guard never looked" (Build Discipline: every
     diagnostic needs a positive control).
  3. Threshold sanity: the calibrated values still hold, AND Trigger A's derived
     form (4 * granularity) is intact — so an accidental edit during refactor is
     caught.
"""

import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "python"))

from enhanced_report import (  # noqa: E402
    build_drift_trajectory_section,
    _series_granularity,
    _DRIFT_MEASUREMENT_GRANULARITY,
    _DRIFT_REVERSAL_FLOOR,
    _DRIFT_DIVERGENCE_FLOOR,
    _DRIFT_PLATEAU_REVERSAL_CEIL,
    _DRIFT_RATE_NOISE_FLOOR,
    _DRIFT_RATE_DECAY_FRAC,
    _DRIFT_PLATEAU_RISE_FLOOR,
)


def _metric(vals):
    """A drift_trajectory metric block: series + per-interval rate/acceleration."""
    series = [{"t": i, "v": v} for i, v in enumerate(vals)]
    deltas = [vals[i + 1] - vals[i] for i in range(len(vals) - 1)]
    rates = [{"rate": d} for d in deltas]
    accels = [{"acc": deltas[i + 1] - deltas[i]} for i in range(len(deltas) - 1)]
    return {"series": series, "per_interval_rate": rates,
            "per_interval_acceleration": accels}


def _entry(constraint_id, dt):
    return {"id": constraint_id, "drift_trajectory": dt}


def _pipeline(entries):
    return {"per_constraint": entries}


# A series that fires Trigger A (up then a >=0.04 reversal) so the section renders.
_FIRING_VALS_2DEC = [0.10, 0.20, 0.30, 0.20]
_FIRING_VALS_3DEC = [0.10, 0.20, 0.30, 0.205]  # one 3-decimal point


def test_series_granularity_2decimal():
    dt = {"base_extractiveness": _metric(_FIRING_VALS_2DEC)}
    assert _series_granularity(dt) == 0.01
    print("PASS: 2-decimal series => granularity 0.01")


def test_series_granularity_3decimal():
    dt = {"base_extractiveness": _metric(_FIRING_VALS_3DEC)}
    assert _series_granularity(dt) == 0.001
    print("PASS: 3-decimal series => granularity 0.001")


def test_no_warning_on_2decimal_section():
    dt = {"base_extractiveness": _metric(_FIRING_VALS_2DEC)}
    out = build_drift_trajectory_section(
        "synthetic_2dec", _pipeline([_entry("synthetic_2dec", dt)]))
    assert "--- TEMPORAL TRAJECTORY ---" in out, "section must render (Trigger A)"
    assert "[CALIBRATION WARNING" not in out
    print("PASS: 2-decimal firing section renders with NO calibration warning")


def test_warning_on_3decimal_section():
    # POSITIVE CONTROL: same firing shape, one 3-decimal point => warning.
    dt = {"base_extractiveness": _metric(_FIRING_VALS_3DEC)}
    out = build_drift_trajectory_section(
        "synthetic_3dec", _pipeline([_entry("synthetic_3dec", dt)]))
    assert "--- TEMPORAL TRAJECTORY ---" in out, "section must render (Trigger A)"
    assert "[CALIBRATION WARNING" in out
    print("PASS: 3-decimal firing section renders WITH calibration warning")


def test_threshold_sanity():
    assert _DRIFT_MEASUREMENT_GRANULARITY == 0.01
    assert _DRIFT_REVERSAL_FLOOR == 0.04
    # A is DERIVED, not a literal — the derived form must be intact.
    assert _DRIFT_REVERSAL_FLOOR == 4 * _DRIFT_MEASUREMENT_GRANULARITY
    assert _DRIFT_DIVERGENCE_FLOOR == 0.06
    assert _DRIFT_PLATEAU_REVERSAL_CEIL == 0.025
    assert _DRIFT_RATE_NOISE_FLOOR == 0.001
    assert _DRIFT_RATE_DECAY_FRAC == 0.20
    assert _DRIFT_PLATEAU_RISE_FLOOR == 0.05
    print("PASS: calibrated thresholds intact (A still derived as 4x granularity)")


def _run_all():
    test_series_granularity_2decimal()
    test_series_granularity_3decimal()
    test_no_warning_on_2decimal_section()
    test_warning_on_3decimal_section()
    test_threshold_sanity()


if __name__ == "__main__":
    _run_all()
    print("ALL PASS")
