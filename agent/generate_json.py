"""Generate constraint stories from seed entries in beta_seeds.json.

Supports two modes:
  --batch   (default) Submit all seeds via the Gemini batch API with optional
            context caching for the shared prompt/schema/example prefix.
  --sequential        Original one-at-a-time mode with 30-60s cooldowns.

Usage:
    python3 -m agent.generate_json --limit 10
    python3 -m agent.generate_json --start-at 200 --limit 50
    python3 -m agent.generate_json --model gemini-2.0-flash --dry-run
    python3 -m agent.generate_json --overwrite --limit 5
    python3 -m agent.generate_json --sequential --limit 5
"""

import argparse
import json
import random
import time
from pathlib import Path

from agent.story_generator_base import (
    PROLOG_DIR,
    PROMPT_PATH,
    SCHEMA_PATH,
    EXAMPLE_PATH,
    _SYSTEM_INSTRUCTION,
    _get_client,
    _load_context_file,
    append_to_log,
    generate_story,
    load_processed_log,
    process_response,
    save_story,
)

SEEDS_PATH = PROLOG_DIR / "beta_seeds.json"
PROCESSED_LOG = PROLOG_DIR / "beta_processed.txt"
DEFAULT_MODEL = "gemini-2.0-flash"
BATCH_POLL_INTERVAL = 30  # seconds


# ---------------------------------------------------------------------------
# Shared helpers
# ---------------------------------------------------------------------------

def load_seeds(seeds_path, start_at=0, limit=0):
    """Load seeds, filter already-processed, apply start/limit."""
    seeds = json.loads(Path(seeds_path).read_text(encoding="utf-8"))
    processed = load_processed_log(PROCESSED_LOG)
    remaining = [s for s in seeds if s["constraint_id"] not in processed]

    if start_at > 0:
        remaining = remaining[start_at:]
    if limit > 0:
        remaining = remaining[:limit]

    return remaining


def build_source_desc(seed):
    """Build a rich source_description from seed fields."""
    return (
        f"TOPIC: {seed['human_readable']}\n"
        f"DOMAIN: {seed.get('topic_domain', 'General')}\n"
        f"CONSTRAINT_ID: {seed['constraint_id']}"
    )


# ---------------------------------------------------------------------------
# Context caching
# ---------------------------------------------------------------------------

def create_cache(client, model):
    """Try to create a Gemini context cache for the static prompt prefix.

    Returns the cache name on success, or None if caching is unavailable
    (e.g. content below the model's minimum token threshold).
    """
    from google.genai import types

    prompt_text = _load_context_file(PROMPT_PATH)
    schema_text = _load_context_file(SCHEMA_PATH)
    example_text = _load_context_file(EXAMPLE_PATH)

    static_content = (
        f"=== GENERATION PROMPT ===\n{prompt_text}\n\n"
        f"=== JSON SCHEMA ===\n{schema_text}\n\n"
        f"=== EXAMPLE JSON ===\n{example_text}"
    )

    try:
        cache = client.caches.create(
            model=model,
            config=types.CreateCachedContentConfig(
                system_instruction=_SYSTEM_INSTRUCTION,
                contents=[{"role": "user", "parts": [{"text": static_content}]}],
                ttl="3600s",
                display_name="constraint-story-prompt",
            ),
        )
        print(f"  Cache created: {cache.name}")
        return cache.name
    except Exception as e:
        print(f"  Caching unavailable ({e}), proceeding without cache")
        return None


def delete_cache(client, cache_name):
    """Best-effort cleanup of a context cache."""
    try:
        client.caches.delete(name=cache_name)
        print(f"  Cache deleted: {cache_name}")
    except Exception as e:
        print(f"  Cache cleanup failed (will auto-expire): {e}")


# ---------------------------------------------------------------------------
# Batch mode
# ---------------------------------------------------------------------------

def build_batch_requests(seeds, model, cache_name=None):
    """Build a list of InlinedRequest dicts for the Gemini batch API.

    If cache_name is set, each request references the cache and only sends
    the per-seed task text.  Otherwise the full prompt is inlined.
    """
    from agent.story_generator_base import build_prompt

    requests = []
    for seed in seeds:
        cid = seed["constraint_id"]
        source_desc = build_source_desc(seed)
        context_text = seed.get("summary", "")

        if cache_name:
            # Cache holds system_instruction + static prefix; only send the task
            task_parts = [
                f"\n\n=== YOUR TASK ===\n"
                f"Generate a complete constraint story JSON for: {source_desc}\n"
                f"Follow the schema exactly. Output ONLY valid JSON — "
                f"no markdown fences, no commentary.\n"
            ]
            if context_text:
                task_parts.append(f"\n{context_text}")
            task_content = "".join(task_parts)

            requests.append({
                "contents": [{"role": "user", "parts": [{"text": task_content}]}],
                "metadata": {"key": cid},
                "config": {"cached_content": cache_name},
            })
        else:
            # No cache — inline full prompt
            prompt = build_prompt(source_desc, context_text)
            requests.append({
                "contents": [{"role": "user", "parts": [{"text": prompt}]}],
                "metadata": {"key": cid},
                "config": {"system_instruction": _SYSTEM_INSTRUCTION},
            })

    return requests


def poll_batch(client, batch_name, poll_interval=BATCH_POLL_INTERVAL):
    """Poll a batch job until it reaches a terminal state."""
    terminal_states = {
        "JOB_STATE_SUCCEEDED", "JOB_STATE_FAILED",
        "JOB_STATE_CANCELLED", "JOB_STATE_EXPIRED",
        "JOB_STATE_PARTIALLY_SUCCEEDED",
    }
    while True:
        batch = client.batches.get(name=batch_name)
        state = batch.state.name if batch.state else "UNKNOWN"
        stats = batch.completion_stats
        if stats:
            print(
                f"  Batch: {state} — "
                f"succeeded={stats.successful_count}, "
                f"failed={stats.failed_count}"
            )
        else:
            print(f"  Batch: {state}")
        if state in terminal_states:
            return batch
        time.sleep(poll_interval)


def process_batch_results(batch, seeds_by_id, overwrite=False):
    """Process inlined batch responses, validate, save, and log successes."""
    succeeded = 0
    failed = 0

    if not batch.dest or not batch.dest.inlined_responses:
        print("  No inlined responses in batch result")
        return 0, len(seeds_by_id)

    for resp in batch.dest.inlined_responses:
        cid = (resp.metadata or {}).get("key", "unknown")

        if resp.error:
            print(f"  FAIL {cid}: {resp.error}")
            failed += 1
            continue

        if not resp.response or not resp.response.text:
            print(f"  FAIL {cid}: empty response")
            failed += 1
            continue

        story_dict, errors = process_response(resp.response.text)

        if story_dict is None:
            print(f"  FAIL {cid}: JSON parse error — {errors[0]}")
            failed += 1
            continue

        if errors:
            print(f"  FAIL {cid}: {len(errors)} validation error(s)")
            for err in errors[:3]:
                print(f"    - {err}")
            failed += 1
            continue

        # Patch constraint_id if model diverged
        actual_id = story_dict.get("header", {}).get("constraint_id", "")
        if actual_id != cid:
            story_dict["header"]["constraint_id"] = cid
            print(f"  Patched constraint_id: {actual_id} -> {cid}")

        json_path, pl_path = save_story(story_dict, overwrite=overwrite)
        if json_path is None:
            failed += 1
            continue

        append_to_log(PROCESSED_LOG, cid)
        succeeded += 1
        print(f"  OK {cid}")

    return succeeded, failed


def run_batch(args, remaining):
    """Submit all seeds as a Gemini batch job with optional caching."""
    client = _get_client()

    # Try to create a cache for the shared prefix
    cache_name = create_cache(client, args.model)

    try:
        seeds_by_id = {s["constraint_id"]: s for s in remaining}
        requests = build_batch_requests(remaining, args.model, cache_name)

        print(f"Submitting batch of {len(requests)} requests...")
        batch = client.batches.create(model=args.model, src=requests)
        print(f"Batch created: {batch.name}")

        batch = poll_batch(client, batch.name, args.poll_interval)

        print(f"\nProcessing results...")
        succeeded, failed = process_batch_results(
            batch, seeds_by_id, overwrite=args.overwrite
        )
        print(f"\nDone: {succeeded} succeeded, {failed} failed out of {len(remaining)}")
    finally:
        if cache_name:
            delete_cache(client, cache_name)


# ---------------------------------------------------------------------------
# Sequential mode (original behavior)
# ---------------------------------------------------------------------------

def run_sequential(args, remaining):
    """Process seeds one at a time with cooldowns (original behavior)."""
    for i, seed in enumerate(remaining):
        cid = seed["constraint_id"]
        print(f"\n--- [{i+1}/{len(remaining)}] {cid}: {seed['human_readable']} ---")

        generate_story(
            source_description=build_source_desc(seed),
            processed_log_path=PROCESSED_LOG,
            log_key=cid,
            context_text=seed.get("summary", ""),
            model=args.model,
            overwrite=args.overwrite,
            constraint_id=cid,
        )

        if i < len(remaining) - 1:
            delay = random.uniform(30, 60)
            print(f"Cooling down for {delay:.1f}s...")
            time.sleep(delay)


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(
        description="Generate constraint stories from beta seeds"
    )
    parser.add_argument("--seeds", default=str(SEEDS_PATH),
                        help=f"Seeds JSON path (default: {SEEDS_PATH})")
    parser.add_argument("--model", default=DEFAULT_MODEL,
                        help=f"Gemini model (default: {DEFAULT_MODEL})")
    parser.add_argument("--start-at", type=int, default=0,
                        help="Start at seed index N (after filtering processed)")
    parser.add_argument("--limit", type=int, default=0,
                        help="Process at most N seeds (0 = all)")
    parser.add_argument("--overwrite", action="store_true",
                        help="Overwrite existing files")
    parser.add_argument("--dry-run", action="store_true",
                        help="Print seeds without generating")
    parser.add_argument("--sequential", action="store_true",
                        help="Use original one-at-a-time mode instead of batch")
    parser.add_argument("--poll-interval", type=int, default=BATCH_POLL_INTERVAL,
                        help=f"Batch poll interval in seconds (default: {BATCH_POLL_INTERVAL})")
    args = parser.parse_args()

    seeds_path = Path(args.seeds)
    if not seeds_path.exists():
        print(f"ERROR: {seeds_path} not found")
        return

    remaining = load_seeds(seeds_path, args.start_at, args.limit)
    if not remaining:
        print("No seeds to process.")
        return

    mode = "sequential" if args.sequential else "batch"
    print(f"Processing {len(remaining)} seeds with {args.model} ({mode} mode)")

    if args.dry_run:
        for i, seed in enumerate(remaining):
            cid = seed["constraint_id"]
            print(f"  [{i+1}/{len(remaining)}] {cid}: {seed['human_readable']}")
        print(f"\nDRY RUN — {len(remaining)} seeds would be processed ({mode} mode)")
        return

    if args.sequential:
        run_sequential(args, remaining)
    else:
        run_batch(args, remaining)


if __name__ == "__main__":
    main()
