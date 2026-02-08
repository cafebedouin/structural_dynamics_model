#!/usr/bin/env python3
"""Detect duplicate constraint IDs and module names across testset files.

Two files declaring the same constraint_id will cause Prolog fact collisions
(conflicting base_extractiveness, constraint_claim, etc.). Two files with the
same module name will cause a Prolog module-redefinition error.

Usage (standalone):
    python python/duplicate_checker.py                    # scan testsets/
    python python/duplicate_checker.py --dir prolog/testsets --json

Usage (library):
    from duplicate_checker import scan_duplicates, check_incoming_file
    dupes = scan_duplicates("/path/to/testsets")
    conflict = check_incoming_file("/path/to/testsets", "new_file.pl")
"""

import argparse
import json
import os
import re
import sys
from collections import defaultdict
from pathlib import Path


def extract_ids(filepath):
    """Extract constraint_id and module name from a .pl file.

    Returns (constraint_id, module_name) or (None, None) on failure.
    """
    try:
        with open(filepath, 'r', encoding='utf-8') as f:
            content = f.read()
    except (OSError, UnicodeDecodeError):
        return None, None

    # Module name: :- module(constraint_foo, []).
    mod_match = re.search(r':- module\((\w+)', content)
    module_name = mod_match.group(1) if mod_match else None

    # Constraint ID: extracted from base_extractiveness(ID, ...) — most canonical
    cid_match = re.search(r'base_extractiveness\((\w+)', content)
    if not cid_match:
        # Fallback: constraint_claim(ID, ...)
        cid_match = re.search(r'constraint_claim\((\w+)', content)
    if not cid_match:
        # Fallback: constraint_classification(ID, ...)
        cid_match = re.search(r'constraint_classification\((\w+)', content)
    constraint_id = cid_match.group(1) if cid_match else None

    return constraint_id, module_name


def scan_duplicates(directory):
    """Scan a directory for constraint_id and module name collisions.

    Returns a dict:
        {
            "constraint_id": {id: [file1, file2, ...]},
            "module_name":   {name: [file1, file2, ...]},
        }
    Only entries with 2+ files (actual duplicates) are included.
    """
    cid_map = defaultdict(list)
    mod_map = defaultdict(list)

    for filename in sorted(os.listdir(directory)):
        if not filename.endswith('.pl'):
            continue
        filepath = os.path.join(directory, filename)
        cid, mod = extract_ids(filepath)
        if cid:
            cid_map[cid].append(filename)
        if mod:
            mod_map[mod].append(filename)

    return {
        "constraint_id": {k: v for k, v in cid_map.items() if len(v) > 1},
        "module_name":   {k: v for k, v in mod_map.items() if len(v) > 1},
    }


def check_incoming_file(testsets_dir, incoming_path):
    """Check if an incoming file would collide with existing testset files.

    Returns a list of conflict descriptions (empty = no conflicts).
    """
    conflicts = []
    new_cid, new_mod = extract_ids(incoming_path)
    incoming_name = os.path.basename(incoming_path)

    for filename in os.listdir(testsets_dir):
        if not filename.endswith('.pl'):
            continue
        # Don't flag self or temp files
        if filename == incoming_name or filename.startswith('.tmp_'):
            continue

        existing_path = os.path.join(testsets_dir, filename)
        ex_cid, ex_mod = extract_ids(existing_path)

        if new_cid and ex_cid and new_cid == ex_cid:
            conflicts.append(
                f"DUPLICATE_CONSTRAINT_ID: '{new_cid}' already exists in {filename}"
            )
        if new_mod and ex_mod and new_mod == ex_mod:
            conflicts.append(
                f"DUPLICATE_MODULE: '{new_mod}' already exists in {filename}"
            )

    return conflicts


def main():
    parser = argparse.ArgumentParser(description="Detect duplicate constraint IDs in testsets.")
    parser.add_argument(
        "--dir", default=None,
        help="Directory to scan (default: prolog/testsets relative to repo root)."
    )
    parser.add_argument(
        "--json", action="store_true",
        help="Output results as JSON."
    )
    args = parser.parse_args()

    if args.dir:
        scan_dir = args.dir
    else:
        root = Path(__file__).resolve().parent.parent
        scan_dir = str(root / "prolog" / "testsets")

    if not os.path.isdir(scan_dir):
        print(f"Error: {scan_dir} is not a directory", file=sys.stderr)
        sys.exit(1)

    dupes = scan_duplicates(scan_dir)
    total_cid = len(dupes["constraint_id"])
    total_mod = len(dupes["module_name"])

    if args.json:
        print(json.dumps(dupes, indent=2))
    else:
        if total_cid == 0 and total_mod == 0:
            print(f"No duplicates found in {os.path.basename(scan_dir)}/ "
                  f"({len(os.listdir(scan_dir))} files scanned)")
        else:
            if total_cid > 0:
                print(f"DUPLICATE CONSTRAINT IDs ({total_cid}):")
                for cid, files in sorted(dupes["constraint_id"].items()):
                    print(f"  {cid}:")
                    for f in files:
                        print(f"    - {f}")
            if total_mod > 0:
                print(f"\nDUPLICATE MODULE NAMES ({total_mod}):")
                for mod, files in sorted(dupes["module_name"].items()):
                    print(f"  {mod}:")
                    for f in files:
                        print(f"    - {f}")

    sys.exit(1 if (total_cid + total_mod) > 0 else 0)


if __name__ == "__main__":
    main()
