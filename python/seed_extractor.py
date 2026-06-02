#!/usr/bin/env python3
"""Extract topic seeds from archived .pl testset files.

Reads <archive>/*.pl (default: prolog/archives/prolog_v5) and extracts:
  - constraint_id (from filename)
  - human_readable (from human_readable/2 fact)
  - topic_domain (from topic_domain/2 fact)
  - summary (the FULL `SUMMARY:` block from the /* ... */ header comment)

Outputs a JSON seeds file for the beta corpus generator.
NO claimed_type is included — the generating model classifies from structure alone.

Note (2026-06-01): the summary capture reads the single-star `/* ... SUMMARY: ... */`
header block in full. The prior version matched only double-star `/** ... */` doc
comments and kept the first sentence (≤500 chars), which both missed the single-star
headers used by the prolog_v5 corpus and truncated multi-paragraph summaries.

Usage:
    python3 python/seed_extractor.py
    python3 python/seed_extractor.py --archive-dir prolog/archives/prolog_v5
    python3 python/seed_extractor.py --output prolog/beta_seeds.json
"""

import argparse
import json
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
DEFAULT_ARCHIVE = REPO_ROOT / "prolog" / "archives" / "prolog_v5"
DEFAULT_OUTPUT = REPO_ROOT / "prolog" / "beta_seeds.json"

# ---------------------------------------------------------------------------
# .pl file parsers
# ---------------------------------------------------------------------------

# Matches: human_readable(some_id, "Some Human Readable Name").
# Handles both quoted and single-quoted atoms
RE_HUMAN_READABLE = re.compile(
    r"""human_readable\(\s*'?[\w]+'?\s*,\s*["'](.+?)["']\s*\)\.""",
    re.DOTALL
)

# Matches: topic_domain(some_id, "Some Domain").
RE_TOPIC_DOMAIN = re.compile(
    r"""topic_domain\(\s*'?[\w]+'?\s*,\s*["'](.+?)["']\s*\)\.""",
    re.DOTALL
)

# Matches the constraint_id from a module declaration or filename
RE_MODULE = re.compile(r":- module\(constraint_(\w+)")

# Matches the SUMMARY: block inside the single-star /* ... */ header comment.
# Non-greedy capture from "SUMMARY:" to the end of that header block (first */).
RE_SUMMARY = re.compile(r"SUMMARY:\s*(.*?)\*/", re.DOTALL | re.IGNORECASE)


def normalize_constraint_id(raw_id: str) -> tuple[str, bool]:
    """Normalize a constraint_id to match schema regex ^[a-z][a-z0-9_]*$.

    Returns (normalized_id, was_changed).
    """
    normalized = raw_id.lower()

    # IDs starting with a digit: prefix with 'n' (e.g., 8k_tv → n8k_tv)
    if normalized and normalized[0].isdigit():
        normalized = "n" + normalized

    changed = normalized != raw_id
    return normalized, changed


def extract_seed(pl_path: Path) -> dict | None:
    """Extract a seed dict from a single .pl file."""
    try:
        text = pl_path.read_text(encoding="utf-8", errors="replace")
    except Exception as e:
        print(f"  WARN: Cannot read {pl_path.name}: {e}", file=sys.stderr)
        return None

    # constraint_id from filename (strip .pl)
    constraint_id = pl_path.stem

    # human_readable
    m = RE_HUMAN_READABLE.search(text)
    human_readable = m.group(1).strip() if m else None

    # topic_domain
    m = RE_TOPIC_DOMAIN.search(text)
    topic_domain = m.group(1).strip() if m else None

    # Extract the FULL SUMMARY: block from the /* ... */ header comment.
    summary = None
    m = RE_SUMMARY.search(text)
    if m:
        body = m.group(1)
        body = re.sub(r'\n\s*\*+', '\n', body)    # drop leading "* " on each line
        body = re.sub(r'={3,}', ' ', body)         # drop ==== separators
        body = re.sub(r'\s+', ' ', body).strip()   # collapse whitespace
        body = re.sub(r'^[\*\s]+', '', body)       # drop any leading comment-star
        if len(body) > 20:                         # skip trivially short blocks
            summary = body                         # full block, no truncation

    if not human_readable:
        # Fallback: humanize the filename
        human_readable = constraint_id.replace("_", " ").title()

    if not topic_domain:
        topic_domain = "General"

    # Normalize constraint_id for schema compliance
    normalized_id, was_changed = normalize_constraint_id(constraint_id)

    seed = {
        "constraint_id": normalized_id,
        "human_readable": human_readable,
        "topic_domain": topic_domain,
        "summary": summary,
    }

    # Preserve original ID for v3↔v4 mapping when normalization changed it
    if was_changed:
        seed["original_id"] = constraint_id

    return seed


def extract_all_seeds(archive_dir: Path) -> list[dict]:
    """Extract seeds from all .pl files in the archive directory."""
    pl_files = sorted(archive_dir.glob("*.pl"))
    if not pl_files:
        print(f"ERROR: No .pl files found in {archive_dir}", file=sys.stderr)
        sys.exit(1)

    seeds = []
    skipped = 0
    for pl_path in pl_files:
        seed = extract_seed(pl_path)
        if seed:
            seeds.append(seed)
        else:
            skipped += 1

    print(f"Extracted {len(seeds)} seeds from {len(pl_files)} files ({skipped} skipped)")
    return seeds


def main():
    parser = argparse.ArgumentParser(description="Extract topic seeds from v3 .pl files")
    parser.add_argument("--archive-dir", type=Path, default=DEFAULT_ARCHIVE,
                        help=f"Directory with v3 testset .pl files (default: {DEFAULT_ARCHIVE})")
    parser.add_argument("--output", "-o", type=Path, default=DEFAULT_OUTPUT,
                        help=f"Output JSON seeds file (default: {DEFAULT_OUTPUT})")
    args = parser.parse_args()

    if not args.archive_dir.is_dir():
        print(f"ERROR: Archive directory not found: {args.archive_dir}", file=sys.stderr)
        sys.exit(1)

    seeds = extract_all_seeds(args.archive_dir)

    # Write output
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(
        json.dumps(seeds, indent=2, ensure_ascii=False) + "\n",
        encoding="utf-8",
    )
    print(f"Wrote {len(seeds)} seeds to {args.output}")

    # Quick stats
    with_summary = sum(1 for s in seeds if s["summary"])
    normalized = sum(1 for s in seeds if "original_id" in s)
    domains = set(s["topic_domain"] for s in seeds)
    print(f"  With summary: {with_summary}/{len(seeds)}")
    print(f"  Normalized IDs: {normalized}/{len(seeds)}")
    if normalized:
        for s in seeds:
            if "original_id" in s:
                print(f"    {s['original_id']} → {s['constraint_id']}")
    print(f"  Unique domains: {len(domains)}")


if __name__ == "__main__":
    main()
