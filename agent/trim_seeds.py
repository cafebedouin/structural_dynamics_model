"""Trim beta_seeds.json to only seeds that don't yet have a .pl file in testsets/.

Checks the filesystem (not beta_processed.txt) as the source of truth,
then overwrites beta_seeds.json with only the remaining seeds.

Usage:
    python3 -m agent.trim_seeds              # dry-run by default
    python3 -m agent.trim_seeds --write      # overwrite beta_seeds.json
"""

import argparse
import json
from pathlib import Path

from agent.story_generator_base import PROLOG_DIR, TESTSETS_DIR

SEEDS_PATH = PROLOG_DIR / "beta_seeds.json"
PROCESSED_LOG = PROLOG_DIR / "beta_processed.txt"


def main():
    parser = argparse.ArgumentParser(
        description="Trim beta_seeds.json to seeds missing a .pl file"
    )
    parser.add_argument("--seeds", default=str(SEEDS_PATH),
                        help=f"Seeds JSON path (default: {SEEDS_PATH})")
    parser.add_argument("--write", action="store_true",
                        help="Overwrite seeds file (default is dry-run)")
    args = parser.parse_args()

    seeds_path = Path(args.seeds)
    seeds = json.loads(seeds_path.read_text(encoding="utf-8"))
    existing_pl = {p.stem for p in TESTSETS_DIR.glob("*.pl")}

    already_done = [s for s in seeds if s["constraint_id"] in existing_pl]
    remaining = [s for s in seeds if s["constraint_id"] not in existing_pl]

    print(f"Seeds total:     {len(seeds)}")
    print(f"Have .pl file:   {len(already_done)}")
    print(f"Still needed:    {len(remaining)}")

    # Also sync beta_processed.txt with filesystem reality
    try:
        logged = set(Path(PROCESSED_LOG).read_text(encoding="utf-8").splitlines())
    except FileNotFoundError:
        logged = set()
    done_ids = {s["constraint_id"] for s in already_done}
    log_missing = done_ids - logged
    if log_missing:
        print(f"\nbeta_processed.txt is missing {len(log_missing)} entries that have .pl files")

    if args.write:
        seeds_path.write_text(
            json.dumps(remaining, indent=2) + "\n", encoding="utf-8"
        )
        print(f"\nWrote {len(remaining)} seeds to {seeds_path}")

        # Backfill beta_processed.txt with any IDs that have .pl but weren't logged
        if log_missing:
            with open(PROCESSED_LOG, "a", encoding="utf-8") as f:
                for cid in sorted(log_missing):
                    f.write(cid + "\n")
            print(f"Backfilled {len(log_missing)} entries into {PROCESSED_LOG}")
    else:
        print(f"\nDry run — pass --write to overwrite {seeds_path.name}")


if __name__ == "__main__":
    main()
