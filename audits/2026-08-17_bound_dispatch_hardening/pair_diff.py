#!/usr/bin/env python3
"""pair_diff.py — clean-vs-edited per_constraint diff for the six witness pairs.

Normalizes the manifest timestamp (re-stamped every run) and code_commit/code_dirty
(differ by construction across the pair); everything else must match or every
difference is enumerated per row. Refuses to compare if either half is missing.
"""
import json
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
LEGS = ["testsets", "testsets_haiku", "testsets_flash", "testsets_kimi",
        "testsets_sonnet", "kernel_v1"]
NORMALIZED_MANIFEST_KEYS = {"pipeline_run_at", "code_commit", "code_commit_short",
                            "code_dirty"}

total_rows = 0
for leg in LEGS:
    a = REPO / "outputs" / f"bdh_clean_{leg}.json"
    b = REPO / "outputs" / f"bdh_edited_{leg}.json"
    if not a.exists() or not b.exists():
        sys.exit(f"pair_diff: REFUSED — missing half for {leg}")
    da, db = json.load(open(a)), json.load(open(b))
    ma = {k: v for k, v in da["manifest"].items() if k not in NORMALIZED_MANIFEST_KEYS}
    mb = {k: v for k, v in db["manifest"].items() if k not in NORMALIZED_MANIFEST_KEYS}
    manifest_same = ma == mb
    pa = {e["id"] if isinstance(e, dict) and "id" in e else json.dumps(e, sort_keys=True)[:80]: e
          for e in da["per_constraint"]}
    pb = {e["id"] if isinstance(e, dict) and "id" in e else json.dumps(e, sort_keys=True)[:80]: e
          for e in db["per_constraint"]}
    only_a = sorted(set(pa) - set(pb))
    only_b = sorted(set(pb) - set(pa))
    changed = []
    for k in sorted(set(pa) & set(pb)):
        if pa[k] != pb[k]:
            fields = []
            if isinstance(pa[k], dict) and isinstance(pb[k], dict):
                for f in sorted(set(pa[k]) | set(pb[k])):
                    if pa[k].get(f) != pb[k].get(f):
                        fields.append(f)
            changed.append((k, fields))
    n = len(only_a) + len(only_b) + len(changed)
    total_rows += n
    status = "IDENTICAL" if n == 0 and manifest_same else f"{n} row diff(s)"
    print(f"[{leg:16}] per_constraint {status}; manifest(normalized) "
          f"{'same' if manifest_same else 'DIFFERS'}; n={len(pa)}/{len(pb)}")
    for k in only_a:
        print(f"    only-clean: {k}")
    for k in only_b:
        print(f"    only-edited: {k}")
    for k, fields in changed:
        print(f"    changed: {k} fields={fields}")
print(f"TOTAL diff rows across six pairs: {total_rows}")
