#!/usr/bin/env python3
"""Enrich pipeline_output.json with derived per-constraint fields (v1.0)

Reads pipeline_output.json (which already contains maxent_probs and raw_maxent_probs
from Prolog) plus orbit_data.json, and computes all derived per-constraint fields
in-place. No MaxEnt replication needed — everything derives from the two distribution
vectors already exported by Prolog.

Enriched fields per constraint:
  confidence, rival_type, rival_prob, confidence_margin, confidence_entropy,
  confidence_band, boundary, raw_rival_prob, coalition_type, tangled_psi, tangled_band

Usage:
  python3 python/enrich_pipeline_json.py
"""

import json
import math
import sys
from pathlib import Path

# ---------------------------------------------------------------------------
# Paths
# ---------------------------------------------------------------------------

ROOT_DIR = Path(__file__).resolve().parent.parent
OUTPUT_DIR = ROOT_DIR / "outputs"

PIPELINE_JSON = OUTPUT_DIR / "pipeline_output.json"
ORBIT_JSON = OUTPUT_DIR / "orbit_data.json"

MAXENT_TYPES = ["mountain", "rope", "tangled_rope", "snare", "scaffold", "piton"]
N_TYPES = len(MAXENT_TYPES)

# ---------------------------------------------------------------------------
# Imported logic (inlined from tangled_decomposition.py / classification_confidence.py)
# ---------------------------------------------------------------------------

PSI_ROPE_LEANING = 0.3
PSI_SNARE_LEANING = 0.7


def shannon_entropy(dist):
    """Normalized Shannon entropy of a probability distribution."""
    h = 0.0
    for p in dist.values():
        if p > 1e-15:
            h -= p * math.log(p)
    h_max = math.log(N_TYPES)
    return h / h_max if h_max > 0 else 0.0


def compute_psi(dist):
    """psi = P(snare) / (P(rope) + P(snare) + 0.001). Continuous snare-lean in [0,1]."""
    p_rope = dist.get("rope", 0.0)
    p_snare = dist.get("snare", 0.0)
    return p_snare / (p_rope + p_snare + 0.001)


def classify_band(psi):
    """Classify into rope_leaning / genuinely_tangled / snare_leaning."""
    if psi < PSI_ROPE_LEANING:
        return "rope_leaning"
    if psi > PSI_SNARE_LEANING:
        return "snare_leaning"
    return "genuinely_tangled"


def classify_coalition(orbit_contexts):
    """Classify coalition type from orbit contexts (4 perspectives)."""
    if not orbit_contexts or len(orbit_contexts) < 4:
        return "other"

    inst = orbit_contexts.get("institutional")
    anal = orbit_contexts.get("analytical")
    mod = orbit_contexts.get("moderate")
    pwl = orbit_contexts.get("powerless")

    all_types = [inst, anal, mod, pwl]
    distinct = set(all_types)

    if all(t == "tangled_rope" for t in all_types):
        return "uniform_tangled"
    if len(distinct) >= 3:
        return "split_field"
    others = [anal, mod, pwl]
    if inst in ("rope", "scaffold"):
        tangled_snare_count = sum(1 for t in others if t in ("tangled_rope", "snare"))
        if tangled_snare_count >= 2:
            return "institutional_dissent"
    if pwl == mod and anal != pwl:
        return "analytical_dissent"
    return "other"


# ---------------------------------------------------------------------------
# Enrichment
# ---------------------------------------------------------------------------

def enrich_entry(entry, orbit_data):
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

    # Load orbit_data.json
    print("[ENRICH] Loading orbit_data.json...", file=sys.stderr)
    try:
        with open(ORBIT_JSON, "r", encoding="utf-8") as f:
            orbit_data = json.load(f)
    except (FileNotFoundError, json.JSONDecodeError) as e:
        print(f"[ENRICH] Warning: Could not load {ORBIT_JSON}: {e}", file=sys.stderr)
        orbit_data = {}

    # Enrich each per_constraint entry
    per_constraint = pipeline.get("per_constraint", [])
    print(f"[ENRICH] Enriching {len(per_constraint)} constraints...", file=sys.stderr)

    for entry in per_constraint:
        enrich_entry(entry, orbit_data)

    # Write back in-place
    print(f"[ENRICH] Writing enriched pipeline_output.json...", file=sys.stderr)
    with open(PIPELINE_JSON, "w", encoding="utf-8") as f:
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
