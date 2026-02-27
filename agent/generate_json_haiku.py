"""Generate constraint stories from seed entries using Claude Haiku 4.5 batch API.

Functionally identical to generate_json.py but uses the Anthropic batch API
with prompt caching. The static prompt/schema/example prefix is marked with
cache_control so it is cached across all requests in the batch.

Usage:
    python3 -m agent.generate_json_haiku
    python3 -m agent.generate_json_haiku --limit 10
    python3 -m agent.generate_json_haiku --start-at 200 --limit 50
    python3 -m agent.generate_json_haiku --overwrite --limit 5
    python3 -m agent.generate_json_haiku --dry-run
"""

import argparse
import json
import time
from pathlib import Path

import anthropic

from agent.story_generator_base import (
    PROLOG_DIR,
    PROMPT_PATH,
    SCHEMA_PATH,
    EXAMPLE_PATH,
    _SYSTEM_INSTRUCTION,
    _load_context_file,
    append_to_log,
    load_processed_log,
    process_response,
    save_story,
)

SEEDS_PATH = PROLOG_DIR / "beta_seeds.json"
PROCESSED_LOG = PROLOG_DIR / "beta_processed.txt"
DEFAULT_MODEL = "claude-haiku-4-5-20251001"
BATCH_POLL_INTERVAL = 30  # seconds


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


def build_cached_messages(source_description, context_text=""):
    """Build messages with prompt caching for the static prefix.

    The prompt template, schema, and example are identical across all
    requests and marked with cache_control so Anthropic caches the prefix.
    The per-seed task is appended as a separate (non-cached) block.
    """
    prompt_text = _load_context_file(PROMPT_PATH)
    schema_text = _load_context_file(SCHEMA_PATH)
    example_text = _load_context_file(EXAMPLE_PATH)

    static_content = (
        f"=== GENERATION PROMPT ===\n{prompt_text}\n\n"
        f"=== JSON SCHEMA ===\n{schema_text}\n\n"
        f"=== EXAMPLE JSON ===\n{example_text}"
    )

    task_parts = [
        f"=== YOUR TASK ===\n"
        f"Generate a complete constraint story JSON for: {source_description}\n"
        f"Follow the schema exactly. Output ONLY valid JSON — no markdown fences, no commentary.\n"
    ]
    if context_text:
        task_parts.append(f"\n{context_text}")
    task_content = "".join(task_parts)

    return [
        {
            "role": "user",
            "content": [
                {
                    "type": "text",
                    "text": static_content,
                    "cache_control": {"type": "ephemeral"},
                },
                {
                    "type": "text",
                    "text": task_content,
                },
            ],
        }
    ]


def build_batch_requests(seeds, model):
    """Build a list of batch request dicts for all seeds."""
    system = [
        {
            "type": "text",
            "text": _SYSTEM_INSTRUCTION,
            "cache_control": {"type": "ephemeral"},
        }
    ]

    requests = []
    for seed in seeds:
        cid = seed["constraint_id"]
        messages = build_cached_messages(
            build_source_desc(seed),
            seed.get("summary", ""),
        )
        requests.append({
            "custom_id": cid,
            "params": {
                "model": model,
                "max_tokens": 8192,
                "system": system,
                "messages": messages,
            },
        })
    return requests


def poll_batch(client, batch_id, poll_interval=BATCH_POLL_INTERVAL):
    """Poll a batch until it reaches a terminal state."""
    terminal_states = {"ended", "canceled", "expired"}
    while True:
        batch = client.messages.batches.retrieve(batch_id)
        status = batch.processing_status
        counts = batch.request_counts
        print(
            f"  Batch {batch_id}: {status} — "
            f"succeeded={counts.succeeded}, errored={counts.errored}, "
            f"processing={counts.processing}"
        )
        if status in terminal_states:
            return batch
        time.sleep(poll_interval)


def process_batch_results(client, batch_id, seeds_by_id, overwrite=False):
    """Stream batch results, validate, save, and log successes."""
    succeeded = 0
    failed = 0

    for result in client.messages.batches.results(batch_id):
        cid = result.custom_id

        if result.result.type == "succeeded":
            response = result.result.message
            raw_text = "".join(
                block.text for block in response.content if block.type == "text"
            )

            story_dict, errors = process_response(raw_text)

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

        elif result.result.type == "errored":
            error = result.result.error
            print(f"  FAIL {cid}: API error — {error.type}: {error.message}")
            failed += 1

        elif result.result.type == "canceled":
            print(f"  SKIP {cid}: canceled")
            failed += 1

        elif result.result.type == "expired":
            print(f"  SKIP {cid}: expired")
            failed += 1

    return succeeded, failed


def main():
    parser = argparse.ArgumentParser(
        description="Generate constraint stories from beta seeds (Claude Haiku batch)"
    )
    parser.add_argument("--seeds", default=str(SEEDS_PATH),
                        help=f"Seeds JSON path (default: {SEEDS_PATH})")
    parser.add_argument("--model", default=DEFAULT_MODEL,
                        help=f"Anthropic model (default: {DEFAULT_MODEL})")
    parser.add_argument("--start-at", type=int, default=0,
                        help="Start at seed index N (after filtering processed)")
    parser.add_argument("--limit", type=int, default=0,
                        help="Process at most N seeds (0 = all)")
    parser.add_argument("--overwrite", action="store_true",
                        help="Overwrite existing files")
    parser.add_argument("--dry-run", action="store_true",
                        help="Print seeds without generating")
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

    print(f"Processing {len(remaining)} seeds with {args.model} (batch mode)")

    if args.dry_run:
        for i, seed in enumerate(remaining):
            cid = seed["constraint_id"]
            print(f"  [{i+1}/{len(remaining)}] {cid}: {seed['human_readable']}")
        print(f"\nDRY RUN — {len(remaining)} seeds would be submitted as a batch")
        return

    # Build and submit batch
    seeds_by_id = {s["constraint_id"]: s for s in remaining}
    requests = build_batch_requests(remaining, args.model)

    client = anthropic.Anthropic()
    print(f"Submitting batch of {len(requests)} requests...")
    batch = client.messages.batches.create(requests=requests)
    print(f"Batch created: {batch.id}")

    # Poll until terminal
    batch = poll_batch(client, batch.id, args.poll_interval)

    # Process results
    print(f"\nProcessing results...")
    succeeded, failed = process_batch_results(
        client, batch.id, seeds_by_id, overwrite=args.overwrite
    )

    print(f"\nDone: {succeeded} succeeded, {failed} failed out of {len(remaining)}")


if __name__ == "__main__":
    main()
