import json, collections, sys

SCRATCH = "/tmp/claude-1000/-home-scott-bin-structural-dynamics-model/c70686a2-80ed-471d-87ec-1989dc80d08a/scratchpad"
rows = {}
for ln in open(f"{SCRATCH}/gap_sources.tsv"):
    cid, ka, kb = ln.rstrip("\n").split("\t")
    rows[cid] = (ka, kb)

pc = {e["id"]: e for e in json.load(open("outputs/pipeline_output.json"))["per_constraint"]}

def h1_bucket(cid):
    e = pc.get(cid)
    if e is None:
        return "no_entry"
    h = e.get("h1_band")
    if h is None:
        return "null"
    return "pos" if h > 0 else "zero"

# restrict to both-non-undetermined (the user's restriction: exclude undetermined under either source)
elig = {cid: (ka, kb) for cid, (ka, kb) in rows.items()
        if ka != "undetermined" and kb != "undetermined"}
print(f"eligible (both sources determinate): {len(elig)} / {len(rows)}")

# (a)/(b) agreement on the firing question within eligible
agree = sum(1 for ka, kb in elig.values() if ka == kb)
print(f"(a)/(b) agree gap-vs-no_gap: {agree}/{len(elig)}; disagree: {len(elig)-agree}")
for cid, (ka, kb) in sorted(elig.items()):
    if ka != kb:
        print(f"    DISAGREE {cid}: stakeholder={ka} canonical={kb} h1={h1_bucket(cid)}")

def crosstab(kind_index, label):
    ct = collections.Counter()
    for cid, ks in elig.items():
        fires = "fire" if ks[kind_index] == "gap" else "no_gap"
        ct[(fires, h1_bucket(cid))] += 1
    print(f"\n{label} × h1_band  (rows = source firing; cols = h1 bucket)")
    cols = ["pos", "zero", "null", "no_entry"]
    print("               " + "".join(f"{c:>10}" for c in cols))
    for r in ["fire", "no_gap"]:
        print(f"    {r:>10} " + "".join(f"{ct[(r,c)]:>10}" for c in cols))

crosstab(1, "CANONICAL (b) firing")
crosstab(0, "STAKEHOLDER (a) firing")

# Coincidence of canonical-fire with h1>0 (the "is (b) just h1_band" test)
b_fire_h1pos = sum(1 for cid, (ka, kb) in elig.items() if kb == "gap" and h1_bucket(cid) == "pos")
b_fire = sum(1 for kb in (v[1] for v in elig.values()) if kb == "gap")
h1pos = sum(1 for cid in elig if h1_bucket(cid) == "pos")
print(f"\ncanonical-fire={b_fire}  h1_pos={h1pos}  both={b_fire_h1pos}")
