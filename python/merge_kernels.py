#!/usr/bin/env python3
"""Merge kernel proposals into one deduplicated file + sample equal plain seeds.

Inputs:
  - prolog/kernels/*.json   — kernel proposals from many models (uniform schema:
    kernel_id, human_readable, topic_domain, kernel_candidate, summary). Tolerant of
    ```json fences and empty files.
  - prolog/kernel_seeds.json — the 46 hand-authored kernels.

Dedup: an item is a duplicate if its kernel_id OR its normalized title (lowercase,
alphanumeric-only) already appeared. First occurrence wins; merged-from sources recorded.

Outputs:
  - prolog/kernels_merged.json   — K deduplicated kernel seeds (each gets source_model +
    merged_from provenance).
  - prolog/toy_plain_seeds.json  — K plain seeds randomly sampled (seeded) from the archive
    seed pool (prolog/beta_seeds.json), to pair 1:1 with the kernels for Phase 2.

Usage:
    python3 python/merge_kernels.py            # seed=0
    python3 python/merge_kernels.py --seed 0
"""
import argparse
import json
import re
import glob
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
PROLOG = REPO_ROOT / "prolog"
KERNELS_DIR = PROLOG / "kernels"
KERNEL_SEEDS = PROLOG / "kernel_seeds.json"
BETA_SEEDS = PROLOG / "beta_seeds.json"
OUT_KERNELS = PROLOG / "kernels_merged.json"
OUT_PLAIN = PROLOG / "toy_plain_seeds.json"


def load_json_tolerant(path):
    raw = Path(path).read_text(encoding="utf-8", errors="replace").strip()
    if not raw:
        return []
    try:
        d = json.loads(raw)
    except json.JSONDecodeError:
        m = re.search(r"\[.*\]|\{.*\}", raw, re.DOTALL)  # strip ```json fences etc.
        if not m:
            return []
        d = json.loads(m.group(0))
    if isinstance(d, dict):
        d = d.get("kernels", [d])
    return d if isinstance(d, list) else []


def norm_title(t):
    return re.sub(r"[^a-z0-9]", "", (t or "").lower())


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--seed", type=int, default=0)
    args = ap.parse_args()

    # 1. Collect every kernel proposal, tagged by source file.
    raw = []
    for f in sorted(glob.glob(str(KERNELS_DIR / "*.json"))):
        for it in load_json_tolerant(f):
            if isinstance(it, dict) and it.get("kernel_id"):
                it = dict(it)
                it["source_model"] = Path(f).stem
                raw.append(it)
    for it in load_json_tolerant(KERNEL_SEEDS):
        if isinstance(it, dict) and it.get("kernel_id"):
            it = dict(it)
            it["source_model"] = "kernel_seeds"
            raw.append(it)
    print(f"raw combined kernels: {len(raw)} "
          f"({KERNELS_DIR.name}/*.json + {KERNEL_SEEDS.name})")

    # 2. Union dedup on (kernel_id OR normalized title); first wins, record merges.
    seen_id, seen_title, merged = {}, {}, []
    for it in raw:
        kid, nt = it["kernel_id"], norm_title(it.get("human_readable"))
        keep = seen_id.get(kid) or seen_title.get(nt)
        if keep is not None:
            keep.setdefault("merged_from", []).append(
                f"{it['source_model']}:{kid}")
            continue
        it["merged_from"] = []
        merged.append(it)
        seen_id[kid] = it
        if nt:
            seen_title[nt] = it
    K = len(merged)
    dups = len(raw) - K
    OUT_KERNELS.write_text(json.dumps(merged, indent=2, ensure_ascii=False) + "\n",
                           encoding="utf-8")
    print(f"deduplicated kernels: {K}  (removed {dups} duplicates) -> {OUT_KERNELS.name}")

    # 3. Sample K plain seeds from the archive seed pool (reproducible).
    import random
    plain_pool = json.loads(BETA_SEEDS.read_text(encoding="utf-8"))
    rng = random.Random(args.seed)
    if K > len(plain_pool):
        print(f"WARNING: K={K} > plain pool {len(plain_pool)}; taking all plain.")
        sample = plain_pool
    else:
        sample = rng.sample(plain_pool, K)
    sample = sorted(sample, key=lambda s: s["constraint_id"])
    OUT_PLAIN.write_text(json.dumps(sample, indent=2, ensure_ascii=False) + "\n",
                         encoding="utf-8")
    print(f"sampled plain seeds: {len(sample)} (seed={args.seed}) -> {OUT_PLAIN.name}")

    # 4. Provenance summary.
    from collections import Counter
    src = Counter(it["source_model"] for it in merged)
    print("\nkept kernels by source_model:")
    for s, n in src.most_common():
        print(f"  {s:42s} {n}")
    multi = [it for it in merged if it["merged_from"]]
    print(f"\nkernels that absorbed >=1 duplicate: {len(multi)}")
    for it in multi[:12]:
        print(f"  {it['kernel_id']:42s} <- {it['merged_from']}")


if __name__ == "__main__":
    main()
