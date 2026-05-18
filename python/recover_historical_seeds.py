"""Recovery script for historical seed generation failures.

Re-submits the 17 failed seeds as a new batch, then applies post-processing
to fix the common schema violations:
  - Extra fields in commentary, omegas, base_properties (additionalProperties)
  - Null measurement time_point/value (set to 0)
  - Invalid lowercase constraint IDs in omega ids (lowercase + sanitize)
  - Out-of-range directionality_override d_values (clamp to [0, 1])
  - Missing mandatrophy_resolved in base_properties (default False)
  - Negative measurement values (clamp to 0)

Usage:
    python3 python/recover_historical_seeds.py --seeds /tmp/historical_empirical_seeds.json
"""
import argparse
import json
import re
import time
from pathlib import Path

import anthropic

REPO_ROOT = Path(__file__).resolve().parent.parent
PROLOG_DIR = REPO_ROOT / "prolog"
PROCESSED_LOG = PROLOG_DIR / "beta_processed.txt"

COMMENTARY_ALLOWED = {
    "narrative_context", "key_agents", "logic_rationale",
    "perspectival_gap", "directionality_logic", "mandatrophy_analysis",
}
OMEGA_ALLOWED = {
    "id", "question", "resolution_mechanism", "impact",
    "confidence", "type_class", "description",
}
PERSPECTIVE_ALLOWED = {
    "classification_type", "agent_power", "time_horizon",
    "exit_options", "spatial_scope", "label", "comment",
}
MEASUREMENT_ALLOWED = {"metric", "time_point", "value", "id_override"}
TOP_LEVEL_ALLOWED = {
    "header", "base_properties", "perspectives", "omegas",
    "measurements", "interval", "commentary", "boltzmann",
    "network", "directionality_overrides", "uke_scope",
}
BASE_PROPS_ALLOWED = {
    "extractiveness", "suppression", "theater_ratio", "claimed_type",
    "human_readable", "topic_domain", "requires_active_enforcement",
    "emerges_naturally", "has_sunset_clause", "accessibility_collapse",
    "resistance", "beneficiaries", "victims", "mandatrophy_resolved",
}
VALID_ID_RE = re.compile(r'^[a-z][a-z0-9_]*$')


def sanitize_id(s):
    """Convert a string to a valid constraint ID."""
    s = s.lower()
    s = re.sub(r'[^a-z0-9_]', '_', s)
    s = re.sub(r'_+', '_', s)
    if s and not s[0].isalpha():
        s = 'id_' + s
    return s


def fix_story(story_dict, constraint_id):
    """Strip extra fields and fix common schema violations in place."""
    # Strip unknown top-level fields
    for k in list(story_dict.keys()):
        if k not in TOP_LEVEL_ALLOWED:
            del story_dict[k]

    # Fix commentary
    if "commentary" in story_dict and isinstance(story_dict["commentary"], dict):
        c = story_dict["commentary"]
        for k in list(c.keys()):
            if k not in COMMENTARY_ALLOWED:
                # Try to merge into an allowed field if semantically close
                if "directionality_logic" in k and "directionality_logic" not in c:
                    c["directionality_logic"] = c.pop(k)
                elif "perspectival_gap" in k and "perspectival_gap" not in c:
                    c["perspectival_gap"] = c.pop(k)
                else:
                    del c[k]

    # Fix perspectives
    if "perspectives" in story_dict and isinstance(story_dict["perspectives"], list):
        for persp in story_dict["perspectives"]:
            if not isinstance(persp, dict):
                continue
            for k in list(persp.keys()):
                if k not in PERSPECTIVE_ALLOWED:
                    del persp[k]

    # Fix omegas
    if "omegas" in story_dict and isinstance(story_dict["omegas"], list):
        for omega in story_dict["omegas"]:
            if not isinstance(omega, dict):
                continue
            for k in list(omega.keys()):
                if k not in OMEGA_ALLOWED:
                    del omega[k]
            # Fix invalid omega IDs
            if "id" in omega and not VALID_ID_RE.match(str(omega["id"])):
                omega["id"] = sanitize_id(str(omega["id"]))

    # Fix measurements
    if "measurements" in story_dict and isinstance(story_dict["measurements"], list):
        for m in story_dict["measurements"]:
            if not isinstance(m, dict):
                continue
            for k in list(m.keys()):
                if k not in MEASUREMENT_ALLOWED:
                    del m[k]
            # Fix null/negative time_point
            if m.get("time_point") is None:
                m["time_point"] = 0
            elif isinstance(m.get("time_point"), (int, float)) and m["time_point"] < 0:
                m["time_point"] = 0
            # Fix null/negative value
            if m.get("value") is None:
                m["value"] = 0.0
            elif isinstance(m.get("value"), (int, float)) and m["value"] < 0:
                m["value"] = 0.0

    # Fix base_properties
    if "base_properties" in story_dict and isinstance(story_dict["base_properties"], dict):
        bp = story_dict["base_properties"]
        for k in list(bp.keys()):
            if k not in BASE_PROPS_ALLOWED:
                del bp[k]
        # Add missing mandatrophy_resolved
        if "mandatrophy_resolved" not in bp:
            extractiveness = bp.get("extractiveness", 0)
            bp["mandatrophy_resolved"] = extractiveness < 0.46

    # Fix directionality_overrides
    if "directionality_overrides" in story_dict:
        overrides = story_dict["directionality_overrides"]
        if isinstance(overrides, list):
            for ov in overrides:
                if isinstance(ov, dict) and "d_value" in ov:
                    ov["d_value"] = max(0.0, min(1.0, float(ov["d_value"])))
        elif isinstance(overrides, dict):
            # Schema expects array — wrap it
            story_dict["directionality_overrides"] = [overrides]

    # Fix beneficiaries/victims: ensure list of strings matching pattern
    for field in ("beneficiaries", "victims"):
        bp = story_dict.get("base_properties", {})
        if field in bp and isinstance(bp[field], list):
            bp[field] = [
                sanitize_id(str(v)) if not VALID_ID_RE.match(str(v)) else v
                for v in bp[field]
                if v is not None
            ]

    return story_dict


def load_failed_seeds(seeds_path):
    """Load seeds that have not been processed yet."""
    seeds = json.loads(Path(seeds_path).read_text(encoding="utf-8"))
    processed = set(PROCESSED_LOG.read_text().splitlines()) if PROCESSED_LOG.exists() else set()
    return [s for s in seeds if s["constraint_id"] not in processed]


def build_source_desc(seed):
    return (
        f"TOPIC: {seed['human_readable']}\n"
        f"DOMAIN: {seed.get('topic_domain', 'General')}\n"
        f"CONSTRAINT_ID: {seed['constraint_id']}"
    )


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--seeds", default="/tmp/historical_empirical_seeds.json")
    parser.add_argument("--model", default="claude-haiku-4-5-20251001")
    parser.add_argument("--poll-interval", type=int, default=30)
    args = parser.parse_args()

    failed_seeds = load_failed_seeds(args.seeds)
    if not failed_seeds:
        print("No failed seeds to recover.")
        return

    print(f"Recovering {len(failed_seeds)} failed seeds...")

    # Import shared infrastructure
    import sys
    sys.path.insert(0, str(REPO_ROOT))
    sys.path.insert(0, str(REPO_ROOT / "python"))
    from agent.generate_json_haiku import build_cached_messages, poll_batch
    from generate_constraint_pl import validate_json, generate_pl
    from agent.story_generator_base import (
        _SYSTEM_INSTRUCTION, append_to_log, process_response, save_story,
    )

    client = anthropic.Anthropic()

    system = [{"type": "text", "text": _SYSTEM_INSTRUCTION, "cache_control": {"type": "ephemeral"}}]
    requests = []
    for seed in failed_seeds:
        cid = seed["constraint_id"]
        messages = build_cached_messages(build_source_desc(seed), seed.get("summary", ""))
        requests.append({
            "custom_id": cid,
            "params": {"model": args.model, "max_tokens": 8192, "system": system, "messages": messages},
        })

    print(f"Submitting batch of {len(requests)} requests...")
    batch = client.messages.batches.create(requests=requests)
    batch_id = batch.id
    print(f"Batch created: {batch_id}")

    poll_batch(client, batch_id, args.poll_interval)

    seeds_by_id = {s["constraint_id"]: s for s in failed_seeds}
    succeeded = 0
    failed = 0

    for result in client.messages.batches.results(batch_id):
        cid = result.custom_id
        if result.result.type != "succeeded":
            print(f"  API FAIL {cid}: {result.result.type}")
            failed += 1
            continue

        response = result.result.message
        raw_text = "".join(block.text for block in response.content if block.type == "text")

        story_dict, errors = process_response(raw_text)
        if story_dict is None:
            print(f"  PARSE FAIL {cid}: {errors[0] if errors else 'unknown'}")
            failed += 1
            continue

        # Apply post-processing before validation
        story_dict = fix_story(story_dict, cid)

        # Re-validate
        _, errors = process_response(json.dumps(story_dict))
        if errors:
            print(f"  STILL FAILING {cid} ({len(errors)} errors):")
            for e in errors[:3]:
                print(f"    - {e}")
            failed += 1
            continue

        # Save
        json_path, pl_path = save_story(story_dict, overwrite=False)
        if json_path:
            append_to_log(PROCESSED_LOG, cid)
            print(f"  RECOVERED {cid}")
            succeeded += 1
        else:
            print(f"  SKIP {cid} (already exists)")

    print(f"\nDone: {succeeded} recovered, {failed} still failing out of {len(failed_seeds)}")


if __name__ == "__main__":
    main()
