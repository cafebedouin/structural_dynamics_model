#!/usr/bin/env python3
"""Normalize orbit_data.json keys to canonical constraint IDs (from constraint_claim/2)."""

import json
import os
import re
from pathlib import Path

SCRIPT_DIR = Path(os.path.dirname(os.path.abspath(__file__)))
ORBIT_JSON = SCRIPT_DIR / '..' / 'outputs' / 'orbit_data.json'
TESTSETS_DIR = SCRIPT_DIR / '..' / 'prolog' / 'testsets'

CLAIM_PATTERN = re.compile(r'constraint_claim\((\w+),')


def build_stem_to_canonical():
    """Scan .pl files → {filename_stem: canonical_id} from constraint_claim/2."""
    mapping = {}
    for pl_file in TESTSETS_DIR.glob('*.pl'):
        stem = pl_file.stem
        content = pl_file.read_text(encoding='utf-8', errors='replace')
        match = CLAIM_PATTERN.search(content)
        if match:
            atom = match.group(1)
            if atom != stem:
                mapping[stem] = atom
    return mapping


def normalize():
    if not ORBIT_JSON.exists():
        print("orbit_data.json not found — skipping normalization")
        return

    stem_to_canonical = build_stem_to_canonical()
    # Also build reverse set of all canonical IDs for quick membership check
    canonical_ids = set(stem_to_canonical.values())

    orbit = json.loads(ORBIT_JSON.read_text(encoding='utf-8'))

    normalized = {}
    renamed, unchanged, collisions = 0, 0, 0
    for key, data in orbit.items():
        # If key is already a canonical ID, keep it
        # If key is a filename stem with a different canonical ID, convert it
        canonical = stem_to_canonical.get(key, key)
        if canonical in normalized:
            collisions += 1
            continue
        normalized[canonical] = data
        if canonical != key:
            renamed += 1
        else:
            unchanged += 1

    ORBIT_JSON.write_text(
        json.dumps(normalized, indent=2, ensure_ascii=False) + '\n',
        encoding='utf-8'
    )
    print(f"Normalized orbit_data.json: {renamed} renamed, "
          f"{unchanged} unchanged, {collisions} collisions")


if __name__ == '__main__':
    normalize()
