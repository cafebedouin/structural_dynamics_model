"""Generate constraint stories from SOTU seeds via Claude Haiku batch API.

Adapted from agent/generate_json_haiku.py. Reads seeds from
sotu/sotu_seeds.json and writes output to sotu/json/ and sotu/pl/
instead of the main corpus directories.

Usage:
    python3 python/sotu_generate_batch.py
    python3 python/sotu_generate_batch.py --limit 10
    python3 python/sotu_generate_batch.py --start-at 20 --limit 50
    python3 python/sotu_generate_batch.py --overwrite --limit 5
    python3 python/sotu_generate_batch.py --dry-run
"""

import argparse
import json
import re
import sys
import time
from pathlib import Path

import anthropic

# ---------------------------------------------------------------------------
# Path constants
# ---------------------------------------------------------------------------
REPO_ROOT = Path(__file__).resolve().parent.parent
SOTU_DIR = REPO_ROOT / "sotu"
SOTU_JSON_DIR = SOTU_DIR / "json"
SOTU_PL_DIR = SOTU_DIR / "pl"
SOTU_SEEDS_PATH = SOTU_DIR / "sotu_seeds.json"
SOTU_PROCESSED_LOG = SOTU_DIR / "sotu_processed.txt"

# Linting requires temp files in TESTSETS_DIR so config.pl is found via
# dirname(dirname(path)) == prolog/
sys.path.insert(0, str(REPO_ROOT / "python"))
sys.path.insert(0, str(REPO_ROOT))

from agent.story_generator_base import (  # noqa: E402
    TESTSETS_DIR,
    _SYSTEM_INSTRUCTION,
    _load_context_file,
    load_processed_log,
    append_to_log,
)
from agent.story_generator_base import strip_json_fences  # noqa: E402
from agent.generate_json_haiku import (  # noqa: E402
    build_source_desc,
    build_cached_messages,
    poll_batch,
    DEFAULT_MODEL,
    BATCH_POLL_INTERVAL,
)
from generate_constraint_pl import generate_pl  # noqa: E402
from linter import lint_file  # noqa: E402

# ---------------------------------------------------------------------------
# SOTU-specific ID normalization
# ---------------------------------------------------------------------------

def normalize_sotu_id(cid):
    """Ensure constraint_id starts with a letter.

    SOTU seeds use year-prefixed IDs like '1945_truman_foo'. The main corpus
    schema requires ^[a-z][a-z0-9_]*$ (starts with letter). Prefix with
    'sotu_' so the ID is schema-valid and still clearly SOTU-scoped.
    """
    if not re.match(r'^[a-z]', cid):
        return 'sotu_' + cid
    return cid


# ---------------------------------------------------------------------------
# SOTU-specific permissive validator
# ---------------------------------------------------------------------------

_REQUIRED_TOP_LEVEL = {"header", "base_properties", "perspectives", "omegas",
                        "measurements", "interval", "commentary", "boltzmann", "network"}
_REQUIRED_HEADER = {"constraint_id", "version", "generated_date", "status"}
_REQUIRED_BASE = {"extractiveness", "suppression", "theater_ratio", "claimed_type",
                  "human_readable", "topic_domain"}

def validate_sotu_minimal(story_dict):
    """Minimal structural check — no additionalProperties enforcement.

    SOTU stories are cross-corpus validation data, not main corpus material.
    Haiku often adds extra commentary fields; we strip unknown top-level
    keys before compiling to Prolog, so schema strictness isn't required.
    Returns a list of error strings (empty = ok).
    """
    errors = []
    missing_top = _REQUIRED_TOP_LEVEL - set(story_dict.keys())
    if missing_top:
        errors.append(f"Missing top-level keys: {missing_top}")
    header = story_dict.get("header", {})
    missing_header = _REQUIRED_HEADER - set(header.keys())
    if missing_header:
        errors.append(f"Missing header keys: {missing_header}")
    base = story_dict.get("base_properties", {})
    missing_base = _REQUIRED_BASE - set(base.keys())
    if missing_base:
        errors.append(f"Missing base_properties keys: {missing_base}")
    perspectives = story_dict.get("perspectives", [])
    if not isinstance(perspectives, list) or len(perspectives) == 0:
        errors.append("perspectives must be a non-empty list")
    # Check numeric fields aren't None
    for field in ("extractiveness", "suppression", "theater_ratio"):
        val = base.get(field)
        if val is not None and not isinstance(val, (int, float)):
            errors.append(f"base_properties.{field} must be a number, got {type(val).__name__}")
        if val is None:
            errors.append(f"base_properties.{field} is null")
    return errors


def process_response_sotu(raw_text):
    """Parse and minimally validate a SOTU constraint story response.

    Uses permissive validation (no additionalProperties enforcement) since
    Haiku frequently adds extra commentary sub-fields that don't affect
    Prolog compilation.

    Returns (story_dict, errors) — errors empty means proceed.
    """
    json_text = strip_json_fences(raw_text)
    try:
        story_dict = json.loads(json_text)
    except json.JSONDecodeError as e:
        return None, [f"JSON_PARSE_ERROR: {e}"]
    errors = validate_sotu_minimal(story_dict)
    return story_dict, errors


# ---------------------------------------------------------------------------
# SOTU-specific save function
# ---------------------------------------------------------------------------

def save_story_sotu(story_dict, overwrite=False):
    """Compile story_dict to .pl, lint, and write to sotu/json/ and sotu/pl/.

    Linting uses a temp file in prolog/testsets/ so that the linter can
    find config.pl via dirname(dirname(filepath)).

    Returns (json_path, pl_path) on success, (None, None) on skip/error.
    """
    cid = story_dict["header"]["constraint_id"]
    json_path = SOTU_JSON_DIR / f"{cid}.json"
    pl_path = SOTU_PL_DIR / f"{cid}.pl"

    if not overwrite and (json_path.exists() or pl_path.exists()):
        print(f"  Skipping {cid} — already exists (use --overwrite to replace)")
        return None, None

    pl_content = generate_pl(story_dict)

    # Lint via temp file in TESTSETS_DIR (required for config.pl resolution)
    tmp_path = TESTSETS_DIR / f".tmp_sotu_{cid}.pl"
    try:
        tmp_path.write_text(pl_content, encoding="utf-8")
        lint_errors = lint_file(str(tmp_path))
        if lint_errors:
            print(f"  Lint warnings for {cid} (non-blocking):")
            for err in lint_errors[:5]:
                print(f"    - {err}")
    except Exception as e:
        print(f"  Linter crashed for {cid}: {e}")
    finally:
        tmp_path.unlink(missing_ok=True)

    SOTU_JSON_DIR.mkdir(parents=True, exist_ok=True)
    SOTU_PL_DIR.mkdir(parents=True, exist_ok=True)

    json_path.write_text(json.dumps(story_dict, indent=2) + "\n", encoding="utf-8")
    pl_path.write_text(pl_content, encoding="utf-8")

    print(f"  Saved: {json_path.name} + {pl_path.name}")
    return json_path, pl_path


# ---------------------------------------------------------------------------
# Seed loading
# ---------------------------------------------------------------------------

def load_seeds(seeds_path, start_at=0, limit=0):
    """Load SOTU seeds, normalize IDs, filter already-processed, apply start/limit."""
    if not Path(seeds_path).exists():
        print(f"ERROR: {seeds_path} not found. Run sotu_scope_batch.py first.")
        sys.exit(1)

    seeds = json.loads(Path(seeds_path).read_text(encoding="utf-8"))

    # Normalize constraint_ids so they start with a letter (schema requires ^[a-z])
    for s in seeds:
        s["constraint_id"] = normalize_sotu_id(s["constraint_id"])

    # Normalize processed log entries too — old entries may have digit-starting IDs
    raw_processed = load_processed_log(SOTU_PROCESSED_LOG)
    processed = {normalize_sotu_id(p) for p in raw_processed}
    remaining = [s for s in seeds if s["constraint_id"] not in processed]

    if start_at > 0:
        remaining = remaining[start_at:]
    if limit > 0:
        remaining = remaining[:limit]

    return remaining


# ---------------------------------------------------------------------------
# Batch request building (SOTU-specific: truncates custom_id to 64 chars)
# ---------------------------------------------------------------------------

_MAX_CUSTOM_ID = 64  # Anthropic batch API hard limit

def build_sotu_batch_requests(seeds, model):
    """Build batch requests with custom_ids truncated to 64 chars.

    Returns (requests, id_map) where id_map maps short_id → full constraint_id.
    The full constraint_id is patched back into the story after generation.
    """
    from agent.story_generator_base import _SYSTEM_INSTRUCTION, _load_context_file, PROMPT_PATH, SCHEMA_PATH, EXAMPLE_PATH

    system = [
        {
            "type": "text",
            "text": _SYSTEM_INSTRUCTION,
            "cache_control": {"type": "ephemeral"},
        }
    ]

    id_map = {}
    requests = []
    seen_short = {}

    for seed in seeds:
        full_cid = seed["constraint_id"]
        short_cid = full_cid[:_MAX_CUSTOM_ID]

        # Collision guard (extremely unlikely but handle it)
        if short_cid in seen_short and seen_short[short_cid] != full_cid:
            # Truncate one less char and append a disambiguator
            short_cid = full_cid[:_MAX_CUSTOM_ID - 2] + "_x"
        seen_short[short_cid] = full_cid
        id_map[short_cid] = full_cid

        messages = build_cached_messages(build_source_desc(seed), seed.get("summary", ""))
        requests.append({
            "custom_id": short_cid,
            "params": {
                "model": model,
                "max_tokens": 8192,
                "system": system,
                "messages": messages,
            },
        })

    return requests, id_map


# ---------------------------------------------------------------------------
# Batch result processing
# ---------------------------------------------------------------------------

def process_batch_results_sotu(client, batch_id, id_map, overwrite=False):
    """Stream batch results, validate, save to sotu/, and log successes."""
    succeeded = 0
    failed = 0

    for result in client.messages.batches.results(batch_id):
        short_id = result.custom_id
        # Resolve full constraint_id from the map (fallback to short_id if missing)
        cid = id_map.get(short_id, short_id)

        if result.result.type == "succeeded":
            response = result.result.message
            raw_text = "".join(
                block.text for block in response.content if block.type == "text"
            )

            story_dict, errors = process_response_sotu(raw_text)

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

            # Always patch constraint_id to our normalized seed ID
            actual_id = story_dict.get("header", {}).get("constraint_id", "")
            if actual_id != cid:
                story_dict["header"]["constraint_id"] = cid
                print(f"  Patched constraint_id: {actual_id} -> {cid}")

            json_path, pl_path = save_story_sotu(story_dict, overwrite=overwrite)
            if json_path is None:
                failed += 1
                continue

            append_to_log(SOTU_PROCESSED_LOG, cid)
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


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(
        description="Generate constraint stories from SOTU seeds (Haiku batch)"
    )
    parser.add_argument("--seeds", default=str(SOTU_SEEDS_PATH),
                        help=f"Seeds JSON path (default: {SOTU_SEEDS_PATH})")
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

    remaining = load_seeds(args.seeds, args.start_at, args.limit)
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

    requests, id_map = build_sotu_batch_requests(remaining, args.model)

    client = anthropic.Anthropic()
    print(f"Submitting batch of {len(requests)} requests...")
    batch = client.messages.batches.create(requests=requests)
    print(f"Batch created: {batch.id}")

    batch = poll_batch(client, batch.id, args.poll_interval)

    print("\nProcessing results...")
    succeeded, failed = process_batch_results_sotu(
        client, batch.id, id_map, overwrite=args.overwrite
    )

    print(f"\nDone: {succeeded} succeeded, {failed} failed out of {len(remaining)}")
    print(f"Output: {SOTU_JSON_DIR.relative_to(REPO_ROOT)}/ and {SOTU_PL_DIR.relative_to(REPO_ROOT)}/")


if __name__ == "__main__":
    main()
