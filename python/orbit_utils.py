#!/usr/bin/env python3
"""
Orbit Data Utilities — Shared loader for outputs/orbit_data.json

Used by category reporters, meta_reporter, and extract_corpus_data
to access gauge orbit signatures computed by prolog/orbit_report.pl.
"""

import json
import os
from pathlib import Path

# Default paths
_SCRIPT_DIR = Path(os.path.dirname(os.path.abspath(__file__)))
_DEFAULT_ORBIT_PATH = _SCRIPT_DIR / '..' / 'outputs' / 'orbit_data.json'

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
    """Get orbit signature for a constraint (direct lookup).

    Returns:
        list or None: e.g. ["rope", "snare"] or None if not found.
    """
    entry = orbit_data.get(constraint_id)
    return entry.get('orbit_signature') if entry else None


def format_orbit_signature(signature):
    """Format orbit signature for display.

    Returns:
        str: e.g. "[rope, snare]" or "N/A"
    """
    if signature:
        return '[' + ', '.join(signature) + ']'
    return 'N/A'
