#!/usr/bin/env python3
"""Analysis of blind legibility codes per PREREG_legibility_coding.md.
Consumes codes.json (list of {id, channel, statable_party, hot_topic, note}).
Verdict logic fixed before coder outputs were read.
"""
import json, sys
from math import comb

SP = "/tmp/claude-1000/-home-scott-bin-structural-dynamics-model/09cd5b46-2bd8-46d6-ae9b-5ebab5543b65/scratchpad"

def fisher_two_sided(a, b, c, d):
    """2x2 [[a,b],[c,d]] two-sided Fisher exact via hypergeometric."""
    n = a + b + c + d
    r1, c1 = a + b, a + c
    def p_of(x):
        return comb(c1, x) * comb(n - c1, r1 - x) / comb(n, r1)
    p_obs = p_of(a)
    lo, hi = max(0, r1 + c1 - n), min(r1, c1)
    return sum(p_of(x) for x in range(lo, hi + 1) if p_of(x) <= p_obs + 1e-12)

def main():
    batches = json.load(open(f"{SP}/coding_batches.json"))
    group = batches["group"]
    expected = batches["controls_expected"]
    codes = {c["id"]: c for c in json.load(open(f"{SP}/codes.json"))}

    all_ids = [it["id"] for b in batches["batches"] for it in b]
    missing = [i for i in all_ids if i not in codes]
    print(f"coverage: {len(codes)}/{len(all_ids)} coded; missing: {missing}")

    # Control gate
    hits = 0
    print("\n== CONTROL GATE (need >=6/8 exact) ==")
    for cid, exp in expected.items():
        got = codes.get(cid, {}).get("channel", "ABSENT")
        ok = got == exp
        hits += ok
        print(f"  {'PASS' if ok else 'MISS'} {cid}: expected {exp}, got {got}")
    gate = hits >= 6
    print(f"controls: {hits}/8 -> gate {'PASS' if gate else 'FAIL (coding INCONCLUSIVE per prereg)'}")

    def table(field, pos, neg):
        t = {"top30": {"pos": 0, "neg": 0, "other": 0},
             "bot30": {"pos": 0, "neg": 0, "other": 0}}
        for i in all_ids:
            g = group[i]
            if g == "control":
                continue
            v = codes.get(i, {}).get(field)
            k = "pos" if v in pos else ("neg" if v in neg else "other")
            t[g][k] += 1
        return t

    print("\n== H1: channel x decile (2x2 excludes mixed/none_apparent) ==")
    t = table("channel", {"tacit"}, {"text_legible"})
    for g in ("top30", "bot30"):
        print(f"  {g}: tacit={t[g]['pos']} text_legible={t[g]['neg']} mixed/none={t[g]['other']}")
    a, b_, c, d = t["top30"]["pos"], t["top30"]["neg"], t["bot30"]["pos"], t["bot30"]["neg"]
    if min(a + b_, c + d) > 0:
        p = fisher_two_sided(a, b_, c, d)
        print(f"  Fisher two-sided p = {p:.4f}"
              f"  (H1 direction predicted: top more tacit)")
    print("  full channel counts:")
    for g in ("top30", "bot30"):
        cnt = {}
        for i in all_ids:
            if group[i] == g:
                v = codes.get(i, {}).get("channel", "ABSENT")
                cnt[v] = cnt.get(v, 0) + 1
        print(f"    {g}: {cnt}")

    print("\n== statable_party=no as tacit-proxy ==")
    t2 = table("statable_party", {"no"}, {"yes"})
    for g in ("top30", "bot30"):
        print(f"  {g}: no={t2[g]['pos']} yes={t2[g]['neg']} other={t2[g]['other']}")

    print("\n== R1: hot_topic x decile ==")
    t3 = table("hot_topic", {"yes"}, {"no"})
    for g in ("top30", "bot30"):
        print(f"  {g}: hot={t3[g]['pos']} not={t3[g]['neg']}")
    a, b_, c, d = t3["top30"]["pos"], t3["top30"]["neg"], t3["bot30"]["pos"], t3["bot30"]["neg"]
    p3 = fisher_two_sided(a, b_, c, d)
    print(f"  Fisher two-sided p = {p3:.4f}")

    print("\n== per-item join (for the record) ==")
    for i in all_ids:
        cd = codes.get(i, {})
        print(f"  {group[i]:7s} {cd.get('channel','ABSENT'):13s} sp={cd.get('statable_party','?'):7s} "
              f"hot={cd.get('hot_topic','?'):3s} {i}")

if __name__ == "__main__":
    main()
