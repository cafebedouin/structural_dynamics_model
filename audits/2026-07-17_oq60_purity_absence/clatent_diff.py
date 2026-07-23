#!/usr/bin/env python3
"""OQ-60 C-LATENT witness diff: per_constraint must be byte-identical base vs edit.

Compares json.dumps(sorted per_constraint) — manifest excluded (re-stamped
per run). Reports any differing constraint with the differing top-level keys.
"""
import json, sys

OUT = "/home/scott/bin/structural_dynamics_model/outputs"

def load(name):
    d = json.load(open(f"{OUT}/{name}"))
    rows = {r["id"]: r for r in d["per_constraint"]}
    return d["manifest"], rows

for leg in ("testsets", "flash"):
    mb, base = load(f"oq60_clatent_base_{leg}.json")
    me, edit = load(f"oq60_clatent_edit_{leg}.json")
    print(f"[{leg}] base n={mb['n_constraints']} edit n={me['n_constraints']} "
          f"base_commit={mb['code_commit_short']} edit_commit={me['code_commit_short']}")
    only_b = sorted(set(base) - set(edit))
    only_e = sorted(set(edit) - set(base))
    if only_b or only_e:
        print(f"  MEMBERSHIP DIFF: only_base={only_b[:5]} only_edit={only_e[:5]}")
    ndiff = 0
    for cid in sorted(set(base) & set(edit)):
        b, e = base[cid], edit[cid]
        if json.dumps(b, sort_keys=True) != json.dumps(e, sort_keys=True):
            ndiff += 1
            keys = [k for k in set(b) | set(e)
                    if json.dumps(b.get(k), sort_keys=True) != json.dumps(e.get(k), sort_keys=True)]
            if ndiff <= 10:
                print(f"  DIFF {cid}: keys={sorted(keys)}")
    verdict = "BYTE-IDENTICAL per_constraint" if ndiff == 0 and not only_b and not only_e \
              else f"*** {ndiff} differing rows — HALT branch"
    print(f"  [{leg}] {verdict}")
