"""Shared data loading utilities for the structural dynamics pipeline.

Canonical implementations of load_json(), read_config(), and load_all_data(),
previously duplicated across ~14 analysis scripts.
"""

import json
import re
import sys
from pathlib import Path

# ---------------------------------------------------------------------------
# Path constants
# ---------------------------------------------------------------------------

ROOT_DIR = Path(__file__).resolve().parent.parent.parent
OUTPUT_DIR = ROOT_DIR / "outputs"
PROLOG_DIR = ROOT_DIR / "prolog"

PIPELINE_JSON = OUTPUT_DIR / "pipeline_output.json"
CORPUS_JSON = OUTPUT_DIR / "corpus_data.json"
ORBIT_JSON = OUTPUT_DIR / "orbit_data.json"

# ---------------------------------------------------------------------------
# JSON loader
# ---------------------------------------------------------------------------

def load_json(path, label=None):
    """Load a JSON file, returning {} on failure."""
    try:
        with open(path, "r", encoding="utf-8") as f:
            return json.load(f)
    except (FileNotFoundError, json.JSONDecodeError) as e:
        if label:
            print(f"Warning: Could not load {label} ({path}): {e}", file=sys.stderr)
        else:
            print(f"Warning: Could not load {path}: {e}", file=sys.stderr)
        return {}

# ---------------------------------------------------------------------------
# Config reader
# ---------------------------------------------------------------------------

def read_config(config_path=None):
    """Read param/2 values from prolog/config.pl.

    Returns a dict mapping param names to float values.
    """
    if config_path is None:
        config_path = PROLOG_DIR / "config.pl"
    thresholds = {}
    try:
        with open(config_path, "r", encoding="utf-8") as f:
            for line in f:
                match = re.search(r"param\((\w+),\s*(-?[\d.]+)\)", line)
                if match:
                    try:
                        thresholds[match.group(1)] = float(match.group(2))
                    except ValueError:
                        pass
    except Exception as e:
        print(f"Warning: Could not read {config_path}: {e}", file=sys.stderr)
    return thresholds

# ---------------------------------------------------------------------------
# Unified data loader
# ---------------------------------------------------------------------------

def load_all_data():
    """Load pipeline, corpus, and orbit data. Return unified per-constraint dict."""
    pipeline_raw = load_json(PIPELINE_JSON, "pipeline_output")
    corpus_raw = load_json(CORPUS_JSON, "corpus_data")
    orbit_raw = load_json(ORBIT_JSON, "orbit_data")

    per_constraint = pipeline_raw.get("per_constraint", [])
    pipeline_by_id = {e["id"]: e for e in per_constraint}
    corpus_constraints = corpus_raw.get("constraints", {})

    constraints = {}
    for cid, pdata in pipeline_by_id.items():
        cdata = corpus_constraints.get(cid, {})
        metrics = cdata.get("metrics", {})

        entry = {
            "id": cid,
            "claimed_type": pdata.get("claimed_type"),
            "extractiveness": pdata.get("base_extractiveness", metrics.get("extractiveness", 0.0)),
            "suppression": pdata.get("suppression", metrics.get("suppression", 0.0)),
            "theater_ratio": pdata.get("theater_ratio", 0.0),
            "signature": pdata.get("signature"),
            "emerges_naturally": pdata.get("emerges_naturally", metrics.get("emerges_naturally", False)),
            "requires_active_enforcement": pdata.get("requires_active_enforcement",
                                                      metrics.get("requires_enforcement", False)),
            "beneficiaries": pdata.get("beneficiaries", cdata.get("beneficiaries", [])),
            "victims": pdata.get("victims", cdata.get("victims", [])),
            "perspectives": pdata.get("perspectives", {}),
            "purity_score": pdata.get("purity_score"),
            "purity_band": pdata.get("purity_band"),
            "human_readable": pdata.get("human_readable"),
            "domain": pdata.get("topic_domain") or pdata.get("domain"),
            "orbit_contexts": orbit_raw.get(cid, {}).get("contexts", {}),
            "orbit_signature": orbit_raw.get(cid, {}).get("orbit_signature", []),
        }
        constraints[cid] = entry

    return constraints
