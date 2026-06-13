#!/usr/bin/env python3
"""Collect-only: re-fetch a retained batch by ID and extract base_properties from each
story's RAW JSON (no schema-admission gate — reading a field for an audit does not require
corpus-validity). Reuses the spend; safe to re-run. Saves raw stories + prints the table.

Usage: python3 collect.py <batch_id> <out_subdir>
"""
import json
import re
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(REPO / "agent"))
sys.path.insert(0, str(REPO / "python"))
import generate_kernel_corpus as gkc  # noqa: E402
from story_generator_base import strip_json_fences  # noqa: E402

AUDIT = Path(__file__).resolve().parent


def main():
    batch_id, sub = sys.argv[1], sys.argv[2]
    out = AUDIT / sub
    out.mkdir(parents=True, exist_ok=True)
    client = gkc.get_client()
    rows = []
    for result in client.messages.batches.results(batch_id):
        cid = result.custom_id
        if result.result.type != "succeeded":
            rows.append((cid, f"API_{result.result.type}", None)); continue
        text = "".join(b.text for b in result.result.message.content if b.type == "text")
        try:
            story = json.loads(strip_json_fences(text))
        except json.JSONDecodeError as e:
            rows.append((cid, f"PARSE_ERR:{e}", None)); continue
        bp = story.get("base_properties", {})
        ct, eps = bp.get("claimed_type"), bp.get("extractiveness")
        # save raw with a filesystem-safe lowercase name (audit artifact, not corpus)
        safe = re.sub(r"[^a-z0-9_]", "_", cid.lower())
        json.dump(story, open(out / f"{safe}.raw.json", "w"), indent=2, ensure_ascii=False)
        rows.append((cid, ct, eps))
    print(f"=== collected {len(rows)} from {batch_id} -> {sub}/ ===")
    for cid, ct, eps in sorted(rows):
        print(f"{cid:46s} claimed={ct!s:14s} eps={eps}")
    # mountain count (Spend-A discriminator)
    mtn = sum(1 for _, ct, _ in rows if ct == "mountain")
    print(f"\nmountain count: {mtn} / {len(rows)}")


if __name__ == "__main__":
    main()
