"""SCOPE decomposition of SOTU addresses via Claude Haiku batch API.

For each State of the Union address in sotu/sotu_manifest.json, sends a
request to extract exactly 3 institutional constraint topics. Results are
saved to sotu/sotu_seeds.json in the same schema as prolog/beta_seeds.json.

Usage:
    python3 python/sotu_scope_batch.py
    python3 python/sotu_scope_batch.py --dry-run
    python3 python/sotu_scope_batch.py --poll-interval 60
"""

import argparse
import json
import sys
import time
from pathlib import Path

import anthropic

REPO_ROOT = Path(__file__).resolve().parent.parent
SOTU_DIR = REPO_ROOT / "sotu"
MANIFEST_PATH = SOTU_DIR / "sotu_manifest.json"
SEEDS_PATH = SOTU_DIR / "sotu_seeds.json"
PROCESSED_LOG = SOTU_DIR / "sotu_scope_processed.txt"
SCOPE_PROMPT_PATH = REPO_ROOT / "prompts" / "uke_scope_v2_json.md"

DEFAULT_MODEL = "claude-haiku-4-5-20251001"
BATCH_POLL_INTERVAL = 30

_PER_ADDRESS_PROMPT = """\
Analyze this State of the Union address and identify exactly 3 institutional \
mechanisms, regulations, policies, or structural constraints that the address \
proposes, defends, or describes. For each, provide:

  - constraint_id: snake_case identifier, prefixed with year_lastname \
(e.g. "1961_kennedy_peace_corps_establishment")
  - human_readable: one-line description of the constraint
  - topic_domain: one of [economics, labor, education, military, healthcare, \
regulatory, trade, social_policy, infrastructure, governance, foreign_policy, technology]
  - summary: 2-3 sentences describing the constraint as an institutional \
mechanism — who benefits, who bears costs, and what structural role it plays

Output ONLY a JSON array of exactly 3 objects. No commentary, no markdown fences.

=== STATE OF THE UNION ADDRESS ===
President: {president}
Year: {year}

{text}
"""


def load_processed():
    try:
        return set(PROCESSED_LOG.read_text(encoding="utf-8").splitlines())
    except FileNotFoundError:
        return set()


def append_processed(entry):
    with open(PROCESSED_LOG, "a", encoding="utf-8") as f:
        f.write(entry + "\n")


def load_manifest():
    if not MANIFEST_PATH.exists():
        print(f"ERROR: {MANIFEST_PATH} not found. Run sotu_fetch.py first.")
        sys.exit(1)
    return json.loads(MANIFEST_PATH.read_text(encoding="utf-8"))


def build_requests(entries, scope_prompt_text, model):
    """Build batch requests for all unprocessed SOTU entries."""
    system = [
        {
            "type": "text",
            "text": scope_prompt_text,
            "cache_control": {"type": "ephemeral"},
        }
    ]

    requests = []
    for entry in entries:
        text_path = REPO_ROOT / entry["path"]
        full_text = text_path.read_text(encoding="utf-8")
        truncated = full_text[:8000]

        user_msg = _PER_ADDRESS_PROMPT.format(
            president=entry["president"],
            year=entry["year"],
            text=truncated,
        )

        requests.append({
            "custom_id": entry["id"],
            "params": {
                "model": model,
                "max_tokens": 2048,
                "system": system,
                "messages": [{"role": "user", "content": user_msg}],
            },
        })
    return requests


def poll_batch(client, batch_id, poll_interval):
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


def process_results(client, batch_id):
    """Stream batch results, parse JSON arrays, return list of seeds."""
    seeds = []
    failed_ids = []

    for result in client.messages.batches.results(batch_id):
        sotu_id = result.custom_id

        if result.result.type != "succeeded":
            print(f"  FAIL {sotu_id}: {result.result.type}")
            failed_ids.append(sotu_id)
            continue

        response = result.result.message
        raw_text = "".join(
            block.text for block in response.content if block.type == "text"
        ).strip()

        # Strip optional markdown fences
        if raw_text.startswith("```"):
            raw_text = raw_text.split("\n", 1)[1] if "\n" in raw_text else raw_text[3:]
            if raw_text.endswith("```"):
                raw_text = raw_text[:-3].strip()

        try:
            topics = json.loads(raw_text)
        except json.JSONDecodeError as e:
            print(f"  FAIL {sotu_id}: JSON parse error — {e}")
            failed_ids.append(sotu_id)
            continue

        if not isinstance(topics, list) or len(topics) != 3:
            print(f"  FAIL {sotu_id}: expected list of 3, got {type(topics).__name__} len={len(topics) if isinstance(topics, list) else '?'}")
            failed_ids.append(sotu_id)
            continue

        # Validate and normalise each topic
        ok = True
        for topic in topics:
            required = {"constraint_id", "human_readable", "topic_domain", "summary"}
            missing = required - set(topic.keys())
            if missing:
                print(f"  FAIL {sotu_id}: topic missing fields {missing}")
                ok = False
                break
            topic["original_id"] = sotu_id

        if not ok:
            failed_ids.append(sotu_id)
            continue

        seeds.extend(topics)
        append_processed(sotu_id)
        print(f"  OK {sotu_id} — {len(topics)} seeds")

    return seeds, failed_ids


def main():
    parser = argparse.ArgumentParser(
        description="SCOPE decomposition of SOTU addresses (Haiku batch)"
    )
    parser.add_argument("--model", default=DEFAULT_MODEL)
    parser.add_argument("--poll-interval", type=int, default=BATCH_POLL_INTERVAL)
    parser.add_argument("--dry-run", action="store_true",
                        help="Print addresses without submitting")
    args = parser.parse_args()

    manifest = load_manifest()
    processed = load_processed()
    remaining = [e for e in manifest if e["id"] not in processed]

    if not remaining:
        print("All addresses already processed.")
        return

    scope_prompt_text = SCOPE_PROMPT_PATH.read_text(encoding="utf-8")
    print(f"Processing {len(remaining)} addresses (of {len(manifest)} total)")

    if args.dry_run:
        for entry in remaining:
            print(f"  {entry['year']} {entry['president']:20s}  {entry['id']}")
        print(f"\nDRY RUN — {len(remaining)} addresses would be submitted")
        return

    requests = build_requests(remaining, scope_prompt_text, args.model)

    client = anthropic.Anthropic()
    print(f"Submitting batch of {len(requests)} requests...")
    batch = client.messages.batches.create(requests=requests)
    print(f"Batch created: {batch.id}")

    batch = poll_batch(client, batch.id, args.poll_interval)

    print("\nProcessing results...")
    new_seeds, failed_ids = process_results(client, batch.id)

    # Merge with any existing seeds
    existing_seeds = []
    if SEEDS_PATH.exists():
        existing_seeds = json.loads(SEEDS_PATH.read_text(encoding="utf-8"))

    all_seeds = existing_seeds + new_seeds
    SOTU_DIR.mkdir(parents=True, exist_ok=True)
    SEEDS_PATH.write_text(json.dumps(all_seeds, indent=2) + "\n", encoding="utf-8")

    print(f"\nDone: {len(new_seeds)} seeds from this batch, {len(all_seeds)} total in {SEEDS_PATH.relative_to(REPO_ROOT)}")
    if failed_ids:
        print(f"Failed addresses ({len(failed_ids)}): {failed_ids}")


if __name__ == "__main__":
    main()
