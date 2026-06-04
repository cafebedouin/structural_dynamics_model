#!/usr/bin/env python3
"""Expand the depth-lineage tree spec into generation seeds (OQ-71).

Spec: audits/2026-06-04_oq71_depth_lineage/tree_spec/*.json — each file a list of kernel objects:

    {"kernel_id": str,            # atom; if this kernel is a promoted reading, its
                                  # kernel_id EQUALS that reading's id in the parent
     "parent_kernel": str|null,   # kernel_id of parent, or null for a root
     "domain": str,
     "family_id": str,
     "readings": [{"id": str, "commitment": str, "delta": str}, ...]}

Seed assembly mirrors agent/build_never_generated_seeds.py (the control arm's
builder, commit 64cc249a) field-for-field so the two arms' seed SHAPE cannot
drift: same keys, same templated summary (commitment + expected structural delta
+ "KERNEL CONTEST" sibling block), sibling_reading_ids = ALL declared siblings.

Arm symmetry is enforced structurally, not by truncation: control sibling-length
distribution is {1:6, 2:284, 3:5, 4:5} (measured 2026-06-04, OQ-71 Step 0), so a
kernel with fan outside [2,5] is a VALIDATION ERROR — depth comes from interposed
grouping kernels, never from wide fans with clipped sibling lists.

Outputs (under audits/2026-06-04_oq71_depth_lineage/):
  lineage_seeds.json   — the seed pool for agent/generate_kernel_corpus.py no-scope mode
  <run_tag>/lineage.json — tree sidecar: per-node kernel/reading/level/parent
                           (story_uid map appended post-generation, not here)
  stdout               — fan + sibling-length distribution vs control, level census
"""
import json
import sys
from collections import Counter, deque
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
SPEC_DIR = REPO_ROOT / "audits" / "2026-06-04_oq71_depth_lineage" / "tree_spec"
OUT_DIR = REPO_ROOT / "audits" / "2026-06-04_oq71_depth_lineage"
CONTROL_SIBLING_DIST = {1: 6, 2: 284, 3: 5, 4: 5}  # measured, OQ-71 Step 0
FAN_MIN, FAN_MAX = 2, 5


def load_spec():
    kernels = []
    for f in sorted(SPEC_DIR.glob("*.json")):
        kernels.extend(json.loads(f.read_text(encoding="utf-8")))
    return kernels


def validate(kernels):
    errs = []
    kids = [k["kernel_id"] for k in kernels]
    dup = [k for k, n in Counter(kids).items() if n > 1]
    if dup:
        errs.append(f"duplicate kernel_ids: {dup}")
    reading_ids = {}  # reading id -> kernel that declares it
    for k in kernels:
        fan = len(k["readings"])
        if not (FAN_MIN <= fan <= FAN_MAX):
            errs.append(f"{k['kernel_id']}: fan {fan} outside [{FAN_MIN},{FAN_MAX}]")
        for r in k["readings"]:
            if not r.get("commitment") or not r.get("delta"):
                errs.append(f"{k['kernel_id']}__{r.get('id')}: missing commitment/delta")
            if r["id"] in reading_ids:
                errs.append(f"reading id {r['id']} declared by both "
                            f"{reading_ids[r['id']]} and {k['kernel_id']}")
            reading_ids[r["id"]] = k["kernel_id"]
    kid_set = set(kids)
    for k in kernels:
        p = k.get("parent_kernel")
        if p is None:
            continue
        if p not in kid_set:
            errs.append(f"{k['kernel_id']}: parent_kernel {p} is not a kernel")
        # a non-root kernel must itself be a declared reading of its parent
        if k["kernel_id"] not in reading_ids or reading_ids[k["kernel_id"]] != p:
            errs.append(f"{k['kernel_id']}: not declared as a reading of its parent {p} "
                        f"(promoted-reading convention: child kernel_id == reading id)")
    return errs


def levels(kernels):
    """Depth of each kernel via BFS from roots; a reading's level = its kernel's depth."""
    children = {}
    roots = []
    for k in kernels:
        p = k.get("parent_kernel")
        if p is None:
            roots.append(k["kernel_id"])
        else:
            children.setdefault(p, []).append(k["kernel_id"])
    depth = {}
    q = deque((r, 0) for r in roots)
    while q:
        kid, d = q.popleft()
        depth[kid] = d
        for c in children.get(kid, []):
            q.append((c, d + 1))
    unreached = [k["kernel_id"] for k in kernels if k["kernel_id"] not in depth]
    return depth, unreached


def build_seeds(kernels, depth):
    seeds, lineage = [], []
    for k in kernels:
        kid = k["kernel_id"]
        declared = [r["id"] for r in k["readings"]]
        kernel_ctx = ("KERNEL CONTEST — all declared readings of this kernel:\n"
                      + "\n".join(f"  - {r['id']}: {r['commitment']}"
                                  for r in sorted(k["readings"], key=lambda x: x["id"])))
        for r in k["readings"]:
            rid = r["id"]
            siblings = sorted(set(declared) - {rid})
            summary = (f"{r['commitment']}\n"
                       f"Expected structural delta: {r['delta']}\n\n{kernel_ctx}")
            seeds.append({
                "constraint_id": f"{kid}__{rid}",
                "kernel_id": kid,
                "reading_id": rid,
                "human_readable": r["commitment"],
                "topic_domain": k.get("domain", ""),
                "family_id": k.get("family_id", ""),
                "sibling_reading_ids": siblings,
                "expected_structural_delta": r["delta"],
                "summary": summary,
            })
            lineage.append({
                "constraint_id": f"{kid}__{rid}",
                "kernel_id": kid,
                "reading_id": rid,
                "parent_kernel": k.get("parent_kernel"),
                "level": depth[kid],
                "promoted_to_kernel": rid if any(
                    c["kernel_id"] == rid for c in kernels) else None,
                "story_uid": None,  # appended post-generation (Step 3)
            })
    return seeds, lineage


def main():
    run_tag = sys.argv[1] if len(sys.argv) > 1 else "lineage_probe_01"
    kernels = load_spec()
    if not kernels:
        sys.exit(f"no spec files in {SPEC_DIR}")
    errs = validate(kernels)
    if errs:
        print("VALIDATION ERRORS:")
        for e in errs:
            print(f"  - {e}")
        sys.exit(1)
    depth, unreached = levels(kernels)
    if unreached:
        sys.exit(f"kernels unreachable from any root (parent cycle/typo): {unreached}")

    seeds, lineage = build_seeds(kernels, depth)
    out_seeds = OUT_DIR / "lineage_seeds.json"
    out_seeds.write_text(json.dumps(seeds, indent=2, ensure_ascii=False), encoding="utf-8")
    tag_dir = OUT_DIR / run_tag
    tag_dir.mkdir(exist_ok=True)
    (tag_dir / "lineage.json").write_text(
        json.dumps(lineage, indent=2, ensure_ascii=False), encoding="utf-8")

    sib_dist = Counter(len(s["sibling_reading_ids"]) for s in seeds)
    lvl_dist = Counter(n["level"] for n in lineage)
    sum_lens = sorted(len(s["summary"]) for s in seeds)
    print(f"kernels: {len(kernels)}   seeds: {len(seeds)}   run_tag: {run_tag}")
    print(f"sibling-length dist (depth arm): {dict(sorted(sib_dist.items()))}")
    print(f"sibling-length dist (control)  : {CONTROL_SIBLING_DIST}")
    print(f"level census (kernel depth -> stories): {dict(sorted(lvl_dist.items()))}")
    print(f"summary chars: min={sum_lens[0]} median={sum_lens[len(sum_lens)//2]} "
          f"max={sum_lens[-1]}")
    print(f"wrote {out_seeds} and {tag_dir / 'lineage.json'}")


if __name__ == "__main__":
    main()
