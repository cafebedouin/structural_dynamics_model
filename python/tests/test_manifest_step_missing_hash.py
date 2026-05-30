"""
_manifest_step guard test — missing corpus_hash in orbits file causes RuntimeError.

Witness for: run_pipeline.check_orbits_corpus_hash() guard (Task 4, 2026-05-29).
The guard replaces the contingent _stamp_orbits_corpus_hash call in _manifest_step.
It fires when product_site_orbits.json exists but lacks corpus_hash, forcing the
caller to use regenerate_orbits.py (which stamps atomically).

Test method: write a temp orbits file without corpus_hash, call
check_orbits_corpus_hash, assert RuntimeError with correct message.
Zero risk to live orbits file — temp path only.
"""

import json
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "python"))


def test_missing_corpus_hash_raises():
    import run_pipeline as rp

    tmp = ROOT / "outputs" / "test_manifest_guard_tmp.json"
    tmp.write_text(json.dumps({"some_constraint": {"h1": 0, "orbit_signature": []}}))
    try:
        try:
            rp.check_orbits_corpus_hash(tmp)
            raise AssertionError("Expected RuntimeError — guard did not fire")
        except RuntimeError as e:
            msg = str(e)
            assert "corpus_hash" in msg, f"Expected 'corpus_hash' in error: {msg}"
            assert "regenerate_orbits" in msg, f"Expected 'regenerate_orbits' in error: {msg}"
            print(f"PASS: {msg}")
    finally:
        tmp.unlink(missing_ok=True)


def test_present_corpus_hash_does_not_raise():
    import run_pipeline as rp

    tmp = ROOT / "outputs" / "test_manifest_guard_hash_present_tmp.json"
    tmp.write_text(json.dumps({"corpus_hash": "abc123def456", "some_constraint": {"h1": 0}}))
    try:
        rp.check_orbits_corpus_hash(tmp)  # must not raise
        print("PASS: no raise when corpus_hash present")
    finally:
        tmp.unlink(missing_ok=True)


def test_nonexistent_file_does_not_raise():
    import run_pipeline as rp

    absent = ROOT / "outputs" / "nonexistent_orbits_tmp.json"
    assert not absent.exists()
    rp.check_orbits_corpus_hash(absent)  # must not raise when file absent
    print("PASS: no raise when file does not exist")


if __name__ == "__main__":
    test_missing_corpus_hash_raises()
    test_present_corpus_hash_does_not_raise()
    test_nonexistent_file_does_not_raise()
