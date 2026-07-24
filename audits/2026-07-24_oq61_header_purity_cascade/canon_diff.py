"""Canonicalize two pipeline outputs and diff, ignoring nondeterministic manifest
fields. Reports: (1) top-level keys added/removed, (2) per_constraint keys
added/removed, (3) any CHANGED value on a shared key (the behavior-preservation
signal). Sorts per_constraint by id; sorts list-valued fields by repr."""
import json, sys

IGNORE_MANIFEST = {"pipeline_run_at", "manifest"}

def canon(x):
    if isinstance(x, dict):
        return {k: canon(v) for k, v in sorted(x.items())}
    if isinstance(x, list):
        try:
            return sorted((canon(v) for v in x), key=lambda e: json.dumps(e, sort_keys=True))
        except Exception:
            return [canon(v) for v in x]
    return x

def load(p):
    d = json.load(open(p))
    d.pop("manifest", None)
    diag = d.get("diagnostic", {})
    return d

a = load(sys.argv[1]); b = load(sys.argv[2])
label_a, label_b = sys.argv[1].split("/")[-1], sys.argv[2].split("/")[-1]

# index per_constraint by id
def pc_index(d):
    return {pc.get("id"): pc for pc in d.get("per_constraint", [])}
pa, pb = pc_index(a), pc_index(b)

# 1. top-level diagnostic keys
da, db = a.get("diagnostic", {}), b.get("diagnostic", {})
added_diag = sorted(set(db) - set(da))
removed_diag = sorted(set(da) - set(db))
changed_diag = []
for k in sorted(set(da) & set(db)):
    if canon(da[k]) != canon(db[k]):
        changed_diag.append(k)

# 2. per_constraint key + value diffs
pc_added_keys, pc_removed_keys, pc_changed = set(), set(), []
ids = sorted(set(pa) & set(pb), key=lambda x: str(x))
for cid in ids:
    ka, kb = set(pa[cid]), set(pb[cid])
    pc_added_keys |= (kb - ka)
    pc_removed_keys |= (ka - kb)
    for k in (ka & kb):
        if canon(pa[cid][k]) != canon(pb[cid][k]):
            pc_changed.append((cid, k))

print(f"=== {label_a}  vs  {label_b} ===")
print(f"per_constraint ids: {len(pa)} vs {len(pb)}  (shared {len(ids)})")
print(f"[diagnostic] added keys:   {added_diag}")
print(f"[diagnostic] removed keys: {removed_diag}")
print(f"[diagnostic] CHANGED shared keys: {changed_diag}")
print(f"[per_constraint] added keys:   {sorted(pc_added_keys)}")
print(f"[per_constraint] removed keys: {sorted(pc_removed_keys)}")
print(f"[per_constraint] CHANGED shared (id,key): {len(pc_changed)}")
for cid, k in pc_changed[:20]:
    print(f"    {cid}.{k}: {pa[cid][k]!r} -> {pb[cid][k]!r}")
