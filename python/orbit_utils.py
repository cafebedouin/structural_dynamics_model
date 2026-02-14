#!/usr/bin/env python3
"""
Orbit Data Utilities — Shared loader for outputs/orbit_data.json

Used by category reporters, meta_reporter, and extract_corpus_data
to access gauge orbit signatures computed by prolog/orbit_report.pl.
"""

import json
import os
import re
from pathlib import Path

# Default paths
_SCRIPT_DIR = Path(os.path.dirname(os.path.abspath(__file__)))
_DEFAULT_ORBIT_PATH = _SCRIPT_DIR / '..' / 'outputs' / 'orbit_data.json'
_DEFAULT_TESTSETS_DIR = _SCRIPT_DIR / '..' / 'prolog' / 'testsets'

# Cached filename→internal_id mapping
_filename_to_ids = None


def _build_filename_mapping(testsets_dir=None):
    """Build mapping from .pl filenames to internal constraint IDs.

    Some constraints have internal IDs that differ from their filename
    (e.g., file s1_airbnb.pl defines constraint airbnb_str_regulation).
    This scans .pl files for constraint_metric declarations to build
    the reverse mapping.
    """
    global _filename_to_ids
    if _filename_to_ids is not None:
        return _filename_to_ids

    ts_dir = Path(testsets_dir) if testsets_dir else _DEFAULT_TESTSETS_DIR
    _filename_to_ids = {}

    if not ts_dir.exists():
        return _filename_to_ids

    # Pattern matches constraint_metric(ATOM, ...) or base_extractiveness(ATOM, ...)
    id_pattern = re.compile(
        r'(?:constraint_metric|base_extractiveness)\s*\(\s*(\w+)\s*,'
    )

    for pl_file in ts_dir.glob('*.pl'):
        filename = pl_file.stem
        try:
            content = pl_file.read_text(encoding='utf-8', errors='replace')
        except OSError:
            continue

        # Collect internal IDs found in this file
        ids = set()
        for match in id_pattern.finditer(content):
            atom = match.group(1)
            # Skip Prolog variables (start with uppercase)
            if atom[0].isupper() or atom.startswith('_'):
                continue
            ids.add(atom)

        # Map filename to its internal IDs
        if ids:
            _filename_to_ids[filename] = ids

    return _filename_to_ids


def load_orbit_data(path=None):
    """Load orbit data from JSON file.

    Returns:
        dict: {constraint_id: {"orbit_signature": [...], "contexts": {...}}}
        Empty dict if file not found.
    """
    orbit_path = Path(path) if path else _DEFAULT_ORBIT_PATH
    if not orbit_path.exists():
        return {}
    try:
        with open(orbit_path, 'r', encoding='utf-8') as f:
            return json.load(f)
    except (json.JSONDecodeError, OSError):
        return {}


def get_orbit_signature(orbit_data, constraint_id):
    """Get orbit signature for a constraint.

    Tries exact match on constraint_id first (works when filename == internal ID).
    Falls back to filename→internal_id mapping for mismatched cases.

    Returns:
        list or None: e.g. ["rope", "snare"] or None if not found.
    """
    # Direct match
    entry = orbit_data.get(constraint_id)
    if entry:
        return entry.get('orbit_signature')

    # Fallback: check if this filename maps to a different internal ID
    mapping = _build_filename_mapping()
    internal_ids = mapping.get(constraint_id, set())
    for iid in internal_ids:
        entry = orbit_data.get(iid)
        if entry:
            return entry.get('orbit_signature')

    return None


def format_orbit_signature(signature):
    """Format orbit signature for display.

    Returns:
        str: e.g. "[rope, snare]" or "N/A"
    """
    if signature:
        return '[' + ', '.join(signature) + ']'
    return 'N/A'
