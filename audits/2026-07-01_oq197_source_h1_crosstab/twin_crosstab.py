import collections, sys

SCRATCH = "/tmp/claude-1000/-home-scott-bin-structural-dynamics-model/c70686a2-80ed-471d-87ec-1989dc80d08a/scratchpad"

def h1b(h):
    if h == "null":
        return "null"
    return "pos" if int(h) > 0 else "zero"

def analyze(name, path):
    rows = {}
    for ln in open(path):
        cid, ka, kb, h1 = ln.rstrip("\n").split("\t")
        rows[cid] = (ka, kb, h1)
    elig = {c: v for c, v in rows.items()
            if v[0] != "undetermined" and v[1] != "undetermined"}
    print(f"\n===== {name} =====")
    print(f"total={len(rows)}  eligible (both sources determinate)={len(elig)}")

    agree = sum(1 for ka, kb, _ in elig.values() if ka == kb)
    print(f"(a)/(b) agree gap-vs-no_gap: {agree}/{len(elig)}  disagree: {len(elig)-agree}")

    def ct(idx, label):
        c = collections.Counter()
        for ka, kb, h1 in elig.values():
            fires = "fire" if (ka, kb)[idx] == "gap" else "no_gap"
            c[(fires, h1b(h1))] += 1
        print(f"  {label} × h1:  fire[pos={c[('fire','pos')]} zero={c[('fire','zero')]} null={c[('fire','null')]}]"
              f"  no_gap[pos={c[('no_gap','pos')]} zero={c[('no_gap','zero')]} null={c[('no_gap','null')]}]")
        return c

    cb = ct(1, "CANON (b)")
    ca = ct(0, "STAKE (a)")
    # coextension of source-fire with h1>0
    def coext(c, srcidx):
        # off-diagonal = fire&!pos  +  no_gap&pos  (ignoring null in the h1 dimension)
        off = c[("fire", "zero")] + c[("fire", "null")] + c[("no_gap", "pos")]
        tot = sum(c.values())
        return off, tot
    ob, tb = coext(cb, 1)
    oa, ta = coext(ca, 0)
    print(f"  (b)↔h1>0 off-diagonal: {ob}/{tb}   (a)↔h1>0 off-diagonal: {oa}/{ta}")

analyze("testsets (n=119, h1 from pipeline)", None) if False else None
analyze("testsets_haiku", f"{SCRATCH}/twin_testsets_haiku.tsv")
analyze("testsets_flash", f"{SCRATCH}/twin_testsets_flash.tsv")
