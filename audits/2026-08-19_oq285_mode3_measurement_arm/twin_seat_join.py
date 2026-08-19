"""OQ-285 step 4 — is the twin-leg matched-seed structure usable at SEAT level?
GAP-35 pins the pairing as a filename (constraint_id) join; GAP-31 says seats are
story-local. This checks the id intersection FIRST, then whether seat NAMES join."""
import os, glob, re
legs = ['testsets_haiku','testsets_flash','testsets_kimi','testsets_sonnet','testsets']
ids = {l: set(os.path.basename(p)[:-3] for p in glob.glob(f'{l}/*.pl')) for l in legs}
for l in legs: print(f"{l:18s} {len(ids[l])}")
four = set.intersection(*[ids[l] for l in legs[:4]])
print("four-way twin id intersection:", len(four), "  (GAP-35 declares 957)")
print("live leg x four-way twin set:", len(ids['testsets'] & four))
for l in legs[:4]:
    print(f"  live x {l}: {len(ids['testsets'] & ids[l])}")
pat = re.compile(r"constraint_stakeholder\(\s*([^,]+),\s*([^,]+),")
def seats(leg, cid):
    p = f'{leg}/{cid}.pl'
    if not os.path.exists(p): return set()
    txt = open(p, encoding='utf-8', errors='replace').read()
    return set(m.group(2).strip().strip("'") for m in pat.finditer(txt))
tot = exact = anyover = 0
for cid in sorted(four):
    ss = [seats(l, cid) for l in legs[:4]]
    if not all(ss): continue          # story lacks seats in >=1 leg
    tot += 1
    if set.intersection(*ss): anyover += 1
    if len(set(map(frozenset, ss))) == 1: exact += 1
print(f"\nSEAT-level join over the {len(four)} four-way matched ids:")
print(f"  stories with stakeholder seats in ALL FOUR legs:  {tot}")
print(f"  ... with IDENTICAL seat-name sets across 4 legs:  {exact}")
print(f"  ... with a NON-EMPTY 4-way seat-name intersection: {anyover}")
