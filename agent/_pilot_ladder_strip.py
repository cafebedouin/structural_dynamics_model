#!/usr/bin/env python3
"""OQ-121 ladder-fossil strip + before/after pending witness for one chunk.

Usage: python3 agent/_pilot_ladder_strip.py <chunk.json>
Throwaway driver for the corpus-rebuild-fresh full-completion run; not pipeline-wired.
"""
import json, pathlib, sys

chunk_path = pathlib.Path(sys.argv[1])
chunk = json.loads(chunk_path.read_text())


def cid(s):
    if s.get("kernel_id") and s.get("reading_id"):
        return f"{s['kernel_id']}__{s['reading_id']}"
    return s.get("constraint_id")


ids = [cid(s) for s in chunk]
ids_set = set(ids)
bp = pathlib.Path("prolog/beta_processed.txt")
processed = set(bp.read_text().split()) if bp.exists() else set()
pre_pending = [i for i in ids if i not in processed]
print(f"PRE-STRIP : pending={len(pre_pending)} / masked(fossil)={len(ids) - len(pre_pending)} / chunk_size={len(ids)}")

lines = bp.read_text().splitlines()
before = len(lines)
kept = [l for l in lines if l.strip() not in ids_set]
bp.write_text("\n".join(kept) + ("\n" if kept else ""))

processed2 = set(pathlib.Path("prolog/beta_processed.txt").read_text().split())
post_pending = [i for i in ids if i not in processed2]
print(f"beta_processed.txt: {before} -> {len(kept)} (stripped {before - len(kept)})")
print(f"POST-STRIP: pending={len(post_pending)} / chunk_size={len(ids)}")
assert len(post_pending) == len(ids), "STRIP INCOMPLETE"
print("STRIP COMPLETE")
