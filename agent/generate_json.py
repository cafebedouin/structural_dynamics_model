"""Generate constraint stories from seed entries in beta_seeds.json.

Thin wrapper around story_generator_base.generate_story().
Reads seeds, builds rich source descriptions, delegates everything else.

Usage:
    python3 -m agent.generate_json
    python3 -m agent.generate_json --limit 10
    python3 -m agent.generate_json --start-at 200 --limit 50
    python3 -m agent.generate_json --model gemini-2.0-flash --dry-run
    python3 -m agent.generate_json --overwrite --limit 5
"""

import argparse
import json
import random
import time
from pathlib import Path

from agent.story_generator_base import (
    PROLOG_DIR,
    generate_story,
    load_processed_log,
)

SEEDS_PATH = PROLOG_DIR / "beta_seeds.json"
PROCESSED_LOG = PROLOG_DIR / "beta_processed.txt"
DEFAULT_MODEL = "gemini-2.0-flash"


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
    args = parser.parse_args()

    seeds_path = Path(args.seeds)
    if not seeds_path.exists():
        print(f"ERROR: {seeds_path} not found")
        return

    remaining = load_seeds(seeds_path, args.start_at, args.limit)
    if not remaining:
        print("No seeds to process.")
        return

    print(f"Processing {len(remaining)} seeds with {args.model}")

    for i, seed in enumerate(remaining):
        cid = seed["constraint_id"]
        print(f"\n--- [{i+1}/{len(remaining)}] {cid}: {seed['human_readable']} ---")

        if args.dry_run:
            print("  DRY RUN — would generate")
            continue

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


if __name__ == "__main__":
    main()
