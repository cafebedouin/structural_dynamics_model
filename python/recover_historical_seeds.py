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

import sys
sys.path.insert(0, str(Path(__file__).resolve().parent))
from story_repair import repair_story  # canonical single-source repair


def fix_story(story_dict, constraint_id):
    """Thin wrapper over the canonical, single-source repair (was a local duplicate).

    repair_story is a strict superset of the prior logic — it additionally sanitises
    cs_structure atoms / network ids / id_override and transliterates non-ASCII. The one
    prior nicety it drops: folding mis-named commentary keys (e.g. directionality_logic_*)
    into their canonical field; stray commentary keys are now deleted, not merged."""
    return repair_story(story_dict)


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
