#!/usr/bin/env python3
"""C-FLOOR byte-diff vs the C-LATENT (pre-C-FLOOR) dumps: full per_constraint rows.
Diffs must be CONFINED to the census flip rows; any non-flip-row diff is a HALT.
Reports which keys differ on flip rows (purity + justified downstream)."""
import json, csv, gzip, sys
REPO = "/home/scott/bin/structural_dynamics_model"
AUD = f"{REPO}/audits/2026-07-17_oq60_purity_absence"

PAIRS = [
    ("testsets", f"{AUD}/oq60_clatent_edit_testsets.json.gz", f"{REPO}/outputs/oq60_cfloor_testsets.json", "census_testsets_v2_2026-07-23.tsv"),
    ("flash", f"{AUD}/oq60_clatent_edit_flash.json.gz", f"{REPO}/outputs/oq60_cfloor_flash.json", "census_testsets_flash.tsv"),
]
halt = False
for leg, basef, editf, tsv in PAIRS:
    base = {r["id"]: r for r in json.load(gzip.open(basef))["per_constraint"]}
    edit = {r["id"]: r for r in json.load(open(editf))["per_constraint"]}
    flips = {r["constraint"] for r in csv.DictReader(open(f"{AUD}/{tsv}"), delimiter="\t")
             if r["disposition"] == "unknown"}
    diff_rows, keyset = [], {}
    for cid in sorted(set(base) & set(edit)):
        if json.dumps(base[cid], sort_keys=True) != json.dumps(edit[cid], sort_keys=True):
            keys = sorted(k for k in set(base[cid]) | set(edit[cid])
                          if json.dumps(base[cid].get(k), sort_keys=True) != json.dumps(edit[cid].get(k), sort_keys=True))
            diff_rows.append(cid)
            keyset[cid] = keys
    off_flip = [c for c in diff_rows if c not in flips]
    missing_flip = sorted(flips - set(diff_rows))
    print(f"[{leg}] diff_rows={len(diff_rows)} flips_expected={len(flips)} "
          f"off_flip_diffs={off_flip[:5]} flips_without_diff={missing_flip[:5]}")
    from collections import Counter
    kc = Counter(tuple(v) for v in keyset.values())
    for keys, n in kc.most_common():
        print(f"  keys {list(keys)}: {n} rows")
    if off_flip:
        halt = True
        print(f"  *** [{leg}] NON-FLIP-ROW DIFF — HALT branch")
    else:
        print(f"  [{leg}] all diffs confined to census flip rows")
sys.exit(1 if halt else 0)
