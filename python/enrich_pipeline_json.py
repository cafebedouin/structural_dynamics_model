#!/usr/bin/env python3
"""Produce enriched_pipeline.json from pipeline_output.json (v1.1)

Reads pipeline_output.json (immutable after Prolog) plus orbit_data.json, computes
all derived per-constraint fields, and writes the superset to enriched_pipeline.json.
pipeline_output.json is never modified.

Enriched fields per constraint:
  confidence, rival_type, rival_prob, confidence_margin, confidence_entropy,
  confidence_band, boundary, raw_rival_prob, coalition_type, tangled_psi, tangled_band

Usage:
  python3 python/enrich_pipeline_json.py
"""

import json
import sys
from pathlib import Path

from shared.loader import PIPELINE_JSON, ENRICHED_PIPELINE_JSON, OUTPUT_DIR
from shared.constants import (
    shannon_entropy, compute_psi, classify_band, classify_coalition,
)
from shared.schemas import validate_pipeline_output, validate_enriched_pipeline

# ---------------------------------------------------------------------------
# Paths
# ---------------------------------------------------------------------------

ORBIT_JSON = OUTPUT_DIR / "orbit_data.json"
ABDUCTIVE_JSON = OUTPUT_DIR / "abductive_data.json"


# ---------------------------------------------------------------------------
# Enrichment
# ---------------------------------------------------------------------------

def enrich_entry(entry, orbit_data, abd_data=None):
    """Add all derived fields to a single per_constraint entry."""
    dist = entry.get("maxent_probs")
    raw_dist = entry.get("raw_maxent_probs")
    claimed = entry.get("claimed_type")
    cid = entry.get("id")

    # --- Confidence metrics (from maxent_probs) ---
    if dist and claimed:
        confidence = dist.get(claimed, 0.0)

        # Rival: argmax excluding claimed_type
        rival_type = None
        rival_prob = -1.0
        for t, p in dist.items():
            if t != claimed and p > rival_prob:
                rival_type = t
                rival_prob = p

        margin = confidence - rival_prob if rival_prob >= 0 else confidence
        entropy = shannon_entropy(dist)

        # Band classification
        if confidence > 0.8 and margin > 0.5:
            band = "deep"
        elif confidence >= 0.5:
            band = "moderate"
        else:
            band = "borderline"

        boundary = f"{claimed}->{rival_type}" if rival_type else None

        entry["confidence"] = round(confidence, 6)
        entry["rival_type"] = rival_type
        entry["rival_prob"] = round(rival_prob, 6)
        entry["confidence_margin"] = round(margin, 6)
        entry["confidence_entropy"] = round(entropy, 6)
        entry["confidence_band"] = band
        entry["boundary"] = boundary
    else:
        entry["confidence"] = None
        entry["rival_type"] = None
        entry["rival_prob"] = None
        entry["confidence_margin"] = None
        entry["confidence_entropy"] = None
        entry["confidence_band"] = None
        entry["boundary"] = None

    # --- Raw rival prob (from raw_maxent_probs) ---
    if raw_dist and claimed:
        raw_rival_type = None
        raw_rival_prob = -1.0
        for t, p in raw_dist.items():
            if t != claimed and p > raw_rival_prob:
                raw_rival_type = t
                raw_rival_prob = p
        entry["raw_rival_prob"] = round(raw_rival_prob, 6) if raw_rival_prob >= 0 else None
    else:
        entry["raw_rival_prob"] = None

    # --- Coalition type (from orbit_data.json) ---
    orbit_entry = orbit_data.get(cid, {})
    orbit_contexts = orbit_entry.get("contexts", {})
    entry["coalition_type"] = classify_coalition(orbit_contexts)

    # --- Tangled psi / band (only for tangled_rope) ---
    if claimed == "tangled_rope" and dist:
        psi = compute_psi(dist)
        entry["tangled_psi"] = round(psi, 6)
        entry["tangled_band"] = classify_band(psi)
    else:
        entry["tangled_psi"] = None
        entry["tangled_band"] = None

    # --- Abductive triggers (from abductive_data.json) ---
    if abd_data is None:
        abd_data = {}
    entry["abductive_triggers"] = abd_data.get(cid, [])


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    # Load pipeline_output.json
    print("[ENRICH] Loading pipeline_output.json...", file=sys.stderr)
    try:
        with open(PIPELINE_JSON, "r", encoding="utf-8") as f:
            pipeline = json.load(f)
    except (FileNotFoundError, json.JSONDecodeError) as e:
        print(f"[ENRICH] ERROR: Could not load {PIPELINE_JSON}: {e}", file=sys.stderr)
        sys.exit(1)

    # Validate input schema
    errors = validate_pipeline_output(pipeline)
    if errors:
        print("[ENRICH] Schema validation failed for pipeline_output.json:", file=sys.stderr)
        for err in errors[:50]:
            print(f"  {err}", file=sys.stderr)
        if len(errors) > 50:
            print(f"  ... and {len(errors) - 50} more errors", file=sys.stderr)
        sys.exit(1)

    # Load orbit_data.json
    print("[ENRICH] Loading orbit_data.json...", file=sys.stderr)
    try:
        with open(ORBIT_JSON, "r", encoding="utf-8") as f:
            orbit_data = json.load(f)
    except (FileNotFoundError, json.JSONDecodeError) as e:
        print(f"[ENRICH] Warning: Could not load {ORBIT_JSON}: {e}", file=sys.stderr)
        orbit_data = {}

    # Load abductive_data.json
    print("[ENRICH] Loading abductive_data.json...", file=sys.stderr)
    abd_data = {}
    try:
        with open(ABDUCTIVE_JSON, "r", encoding="utf-8") as f:
            abd_data = json.load(f).get("per_constraint", {})
    except (FileNotFoundError, json.JSONDecodeError) as e:
        print(f"[ENRICH] Warning: Could not load {ABDUCTIVE_JSON}: {e}", file=sys.stderr)

    # Enrich each per_constraint entry
    per_constraint = pipeline.get("per_constraint", [])
    print(f"[ENRICH] Enriching {len(per_constraint)} constraints...", file=sys.stderr)

    for entry in per_constraint:
        enrich_entry(entry, orbit_data, abd_data)

    # Validate enriched output schema
    errors = validate_enriched_pipeline(pipeline)
    if errors:
        print("[ENRICH] Schema validation failed for enriched output:", file=sys.stderr)
        for err in errors[:50]:
            print(f"  {err}", file=sys.stderr)
        if len(errors) > 50:
            print(f"  ... and {len(errors) - 50} more errors", file=sys.stderr)
        sys.exit(1)

    # Write to separate enriched file (pipeline_output.json stays immutable)
    print(f"[ENRICH] Writing enriched_pipeline.json...", file=sys.stderr)
    with open(ENRICHED_PIPELINE_JSON, "w", encoding="utf-8") as f:
        json.dump(pipeline, f, indent=2)

    # Summary
    bands = {}
    for entry in per_constraint:
        band = entry.get("confidence_band")
        if band:
            bands[band] = bands.get(band, 0) + 1
    tangled_count = sum(1 for e in per_constraint if e.get("tangled_psi") is not None)

    print(f"[ENRICH] Done. Bands: {bands}, tangled_psi computed: {tangled_count}", file=sys.stderr)


if __name__ == "__main__":
    main()
