"""
Guard 2 staleness test — corpus_hash mismatch causes RuntimeError.

Witness for: ISSUES.md OQ-29 (Guard 2 injection test)
Audited: 2026-05-29 — confirmed guard fires on stored='000000000000' vs
  current='c70e6a2b1aad'. See plan file you-are-picking-up-enumerated-parnas.md.

The 000000000000 hash is an INJECTED TEST SENTINEL — a deliberately-corrupted
value written by this test to force the guard to fire. It is not a real stale
file observed in the wild. The distinction matters: injected vs found is the
exact defect class OQ-29 exists to prevent.
"""

import json
import shutil
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "python"))


def test_guard2_fires_on_stale_hash():
    import sweeps.perturb as p

    orbits_path = ROOT / "outputs" / "product_site_orbits.json"
    if not orbits_path.exists():
        print("SKIP: product_site_orbits.json not found — run pipeline first")
        return

    stale = ROOT / "outputs" / "perturb_guard2_witness_tmp.json"
    shutil.copy(orbits_path, stale)
    try:
        data = json.loads(stale.read_text())
        # Inject sentinel: 000000000000 is deliberately wrong, not a real stale value
        data["corpus_hash"] = "000000000000"
        stale.write_text(json.dumps(data))

        orig = p.ORBITS_PATH
        p.ORBITS_PATH = stale
        try:
            p.perturb("snare_epsilon_floor", [0.46],
                      kernels=["end_of_life_decision_authority"])
            raise AssertionError("Guard 2 did not fire — RuntimeError expected")
        except RuntimeError as e:
            msg = str(e)
            assert "000000000000" in msg, f"Expected injected hash in error: {msg}"
            assert "c70e6a" in msg, f"Expected real hash in error: {msg}"
            print(f"PASS: {msg}")
        finally:
            p.ORBITS_PATH = orig
    finally:
        stale.unlink(missing_ok=True)


if __name__ == "__main__":
    test_guard2_fires_on_stale_hash()
