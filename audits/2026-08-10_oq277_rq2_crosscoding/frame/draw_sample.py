#!/usr/bin/env python3
"""draw_sample.py — OQ-277 Phase 1, step 2. Draw the seeded samples from the FROZEN frame.

Reads only the frozen listings (never the live `audits/` tree), so the draw is
reproducible from committed artifacts after the tree has moved on. Re-running with the
same seed and the same frozen listings reproduces both samples byte-identically; the
script asserts the frame md5s it was frozen against, so a silently-changed frame cannot
be sampled without the assertion firing.

Two draws, from disjoint strata:
  - PRIMARY  n=22 from the 73 incident-bearing dirs  -> direction (ii) units
  - ESCAPE   n=8  from the 101 non-census dirs       -> the escape-check row

The escape draw's purpose is NOT to find defects; it is to bound the keyword proxy's
miss rate. Zero confirmed hits licenses ONLY "miss rate <= ~31% at 95%" (1 - 0.05^(1/8)),
never "the proxy is complete."
"""
from __future__ import annotations

import hashlib
import json
import random
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent

SEED = 20260810  # pinned; stated in PREREGISTRATION.md before any draw
N_PRIMARY = 22
N_ESCAPE = 8

# md5s of the frozen listings this draw is defined against (from frame_manifest.txt).
EXPECTED_MD5 = {
    "incident_bearing_dirs.txt": "57149263fef05f1439d9ed98e755a363",
    "non_census_dirs.txt": "ecc91562c0888aeb246d90fa6dd56da2",
}


def read_frozen(name: str) -> list[str]:
    p = HERE / name
    raw = p.read_bytes()
    got = hashlib.md5(raw).hexdigest()
    want = EXPECTED_MD5[name]
    if got != want:
        sys.exit(
            f"FRAME MISMATCH on {name}: md5 {got} != frozen {want}.\n"
            "The frame moved under the sample. Re-freeze and re-pin deliberately, or "
            "restore the frozen listing — do not sample a frame you did not freeze."
        )
    return [ln for ln in raw.decode().splitlines() if ln.strip()]


def main() -> None:
    incident = read_frozen("incident_bearing_dirs.txt")
    non_census = read_frozen("non_census_dirs.txt")

    assert len(incident) == 73, len(incident)
    assert len(non_census) == 101, len(non_census)
    assert not (set(incident) & set(non_census)), "strata overlap — partition violated"

    # Separate Random instances per stratum: one shared stream would make the escape
    # draw depend on N_PRIMARY, so changing the primary n would silently move the
    # escape sample. Independent streams keep each draw a function of its own inputs.
    primary = sorted(random.Random(SEED).sample(sorted(incident), N_PRIMARY))
    escape = sorted(random.Random(SEED + 1).sample(sorted(non_census), N_ESCAPE))

    assert not (set(primary) & set(escape))

    out = {
        "seed": SEED,
        "n_primary": N_PRIMARY,
        "n_escape": N_ESCAPE,
        "population_incident_bearing": len(incident),
        "population_non_census": len(non_census),
        "frame_md5": EXPECTED_MD5,
        "primary_sample": primary,
        "escape_sample": escape,
        "escape_null_license": (
            "Zero OPERATOR-CONFIRMED hits at n=8 licenses only: keyword-proxy miss rate "
            "<= 31.2% at 95% confidence (1 - 0.05**(1/8)). It does NOT license 'the "
            "proxy is complete'. Coder-proposed hits are QUARANTINED pending operator "
            "confirmation from the source directory."
        ),
    }
    (HERE / "sample.json").write_text(json.dumps(out, indent=2) + "\n")

    print(f"seed={SEED}")
    print(f"PRIMARY n={len(primary)} of {len(incident)} incident-bearing")
    for d in primary:
        print(f"  {d}")
    print(f"ESCAPE  n={len(escape)} of {len(non_census)} non-census")
    for d in escape:
        print(f"  {d}")


if __name__ == "__main__":
    main()
