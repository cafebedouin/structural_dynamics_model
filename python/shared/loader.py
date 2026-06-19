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
PRODUCT_ORBITS_JSON = OUTPUT_DIR / "product_site_orbits.json"
ENRICHED_PIPELINE_JSON = OUTPUT_DIR / "enriched_pipeline.json"

# ---------------------------------------------------------------------------
# JSON loader
# ---------------------------------------------------------------------------

def load_json(path, label=None, schema=None):
    """Load a JSON file, returning {} on failure.

    If *schema* is provided it must be a callable ``(dict) -> list[str]``
    that returns validation errors (empty list = valid).  When a schema is
    given, a load failure or validation error calls ``sys.exit(1)`` -- the
    file is considered required.
    """
    try:
        with open(path, "r", encoding="utf-8") as f:
            data = json.load(f)
    except (FileNotFoundError, json.JSONDecodeError) as e:
        if label:
            print(f"Warning: Could not load {label} ({path}): {e}", file=sys.stderr)
        else:
            print(f"Warning: Could not load {path}: {e}", file=sys.stderr)
        if schema is not None:
            sys.exit(1)
        return {}

    if schema is not None:
        errors = schema(data)
        if errors:
            tag = label or str(path)
            print(f"Schema validation failed for {tag}:", file=sys.stderr)
            for err in errors[:50]:
                print(f"  {err}", file=sys.stderr)
            if len(errors) > 50:
                print(f"  ... and {len(errors) - 50} more errors", file=sys.stderr)
            sys.exit(1)

    return data

# ---------------------------------------------------------------------------
# Orbits loader (partition-and-assert)
# ---------------------------------------------------------------------------

# Top-level non-constraint keys legitimately stamped into orbits files (OQ-29).
_ORBITS_METADATA_KEYS = {"corpus_hash"}


def load_orbits_constraints(path=None):
    """Load an orbits JSON, returning ONLY the per-constraint entries.

    Orbits files (product_site_orbits.json, orbit_data.json) are flat
    {id: {..., "contexts": ...}} dicts that also carry top-level METADATA as
    siblings (e.g. corpus_hash, OQ-29). A constraint entry is a dict with a
    "contexts" key. Anything else must be a NAMED metadata key
    (_ORBITS_METADATA_KEYS); an unrecognized leftover RAISES -- so a real
    constraint with a malformed shape, or a new metadata key, fails loud
    instead of being silently dropped (undercount). Use this anywhere you
    iterate orbits as constraints -- never raw json.load + .items().

    The allowlist (_ORBITS_METADATA_KEYS) is the PRIMARY guard; the "contexts"
    shape predicate is a soft SECONDARY. A metadata *value* shaped like a
    constraint (a dict that itself carries a "contexts" key) would misclassify
    into constraints -- safe today because the only metadata is the str
    corpus_hash, but the contract is "metadata values are assumed
    non-constraint-shaped; classification is by allowlisted key name first."

    Crash-over-drop is safe BY PRODUCER CONSTRUCTION: product_site_export.pl
    write_one_entry emits "contexts" unconditionally for every constraint, and
    the context key set is a static Cartesian product
    (constraint_indexing.pl site_contexts_product/1) that never reads the
    corpus -- so every entry in the live corpus and every archive carries
    "contexts". A top-level entry LACKING it can only be post-export metadata
    or file corruption -- exactly what should raise.
    """
    raw = load_json(path or PRODUCT_ORBITS_JSON, "orbits")
    constraints, leftover = {}, {}
    for k, v in raw.items():
        (constraints if isinstance(v, dict) and "contexts" in v else leftover)[k] = v
    unknown = set(leftover) - _ORBITS_METADATA_KEYS
    if unknown:
        raise ValueError(
            f"{path}: top-level key(s) {sorted(unknown)} are neither a constraint entry "
            f"(dict with 'contexts') nor known metadata {sorted(_ORBITS_METADATA_KEYS)}. "
            "If new metadata, add to _ORBITS_METADATA_KEYS; if a constraint, its shape is wrong."
        )
    return constraints


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
