#!/usr/bin/env python3
"""Normalize orbit_data.json keys: internal constraint IDs → .pl filename stems."""

import json
import os
import re
from pathlib import Path

SCRIPT_DIR = Path(os.path.dirname(os.path.abspath(__file__)))
ORBIT_JSON = SCRIPT_DIR / '..' / 'outputs' / 'orbit_data.json'
TESTSETS_DIR = SCRIPT_DIR / '..' / 'prolog' / 'testsets'

ID_PATTERN = re.compile(
    r'(?:constraint_metric|base_extractiveness)\s*\(\s*(\w+)\s*,'
)


def build_internal_to_filename():
    """Scan .pl files → {internal_id: filename_stem}"""
    mapping = {}
    for pl_file in TESTSETS_DIR.glob('*.pl'):
        stem = pl_file.stem
        content = pl_file.read_text(encoding='utf-8', errors='replace')
        for match in ID_PATTERN.finditer(content):
            atom = match.group(1)
            if atom[0].isupper() or atom.startswith('_'):
                continue
            if atom not in mapping:
                mapping[atom] = stem
    return mapping


def normalize():
    if not ORBIT_JSON.exists():
        print("orbit_data.json not found — skipping normalization")
        return

    mapping = build_internal_to_filename()
    orbit = json.loads(ORBIT_JSON.read_text(encoding='utf-8'))

    normalized = {}
    renamed, unchanged, collisions = 0, 0, 0
    for internal_id, data in orbit.items():
        canonical = mapping.get(internal_id, internal_id)
        if canonical in normalized:
            collisions += 1
            continue
        normalized[canonical] = data
        if canonical != internal_id:
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
