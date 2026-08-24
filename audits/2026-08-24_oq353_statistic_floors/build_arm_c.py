#!/usr/bin/env python3
"""Arm (c) — three DISJOINT random 1000-story subsamples of original_v6 (3380).

3000 used, 380 discarded. NOT overlapping samples: overlap would understate the
within-(c) draw floor, which is the denominator of B3 and B4.

Real directories under prolog/ holding SYMLINKED FILES — never symlinked
directories (classify_corpus .resolve()s the corpus path, run_pipeline.py:460).
Names avoid leg_dirs()'s `testsets*` glob and are outside LIVE_LEGS, so
_is_refusal_scope leaves them in loud-continue.
"""
import os, glob, json, random, hashlib, itertools, shutil, sys

SEED = 353                      # PINNED — recorded in PREREGISTRATION.md
N_PER_ARM, N_ARMS = 1000, 3
ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
V6   = os.path.join(ROOT, "prolog", "archives", "datasets", "original_v6")
OUT  = os.path.join(ROOT, "audits", "2026-08-24_oq353_statistic_floors")

names = sorted(os.path.basename(p) for p in glob.glob(os.path.join(V6, "*.pl")))
print(f"v6 population: {len(names)} files")
assert len(names) == 3380, f"v6 population moved: {len(names)}"

rng = random.Random(SEED)
shuffled = names[:]           # sorted input => deterministic given the seed
rng.shuffle(shuffled)
used, discarded = shuffled[:N_PER_ARM*N_ARMS], shuffled[N_PER_ARM*N_ARMS:]
arms = {f"oq353_arm_c{i+1}": sorted(used[i*N_PER_ARM:(i+1)*N_PER_ARM]) for i in range(N_ARMS)}

manifests = {}
for arm, members in arms.items():
    d = os.path.join(ROOT, "prolog", arm)
    if os.path.isdir(d) or os.path.islink(d):
        shutil.rmtree(d) if os.path.isdir(d) and not os.path.islink(d) else os.unlink(d)
    os.makedirs(d)                                     # REAL directory
    for b in members:
        os.symlink(os.path.join(V6, b), os.path.join(d, b))   # SYMLINKED FILE
    blob = "\n".join(members) + "\n"
    manifests[arm] = dict(arm=arm, dir=f"prolog/{arm}", n=len(members),
                          seed=SEED, source="prolog/archives/datasets/original_v6",
                          sha256=hashlib.sha256(blob.encode()).hexdigest(),
                          members=members)
    open(os.path.join(OUT, f"arm_c_{arm}.manifest.txt"), "w").write(blob)

print("\n=== ARM (c) MANIFESTS ===")
for a, m in manifests.items():
    print(f"  {a:<16} n={m['n']:<6} sha256={m['sha256'][:16]}…  dir={m['dir']}")
print(f"  discarded (unused of 3380): {len(discarded)}")

print("\n=== DISJOINTNESS (pairwise intersection MUST be 0) ===")
ok = True
for a, b in itertools.combinations(arms, 2):
    inter = set(arms[a]) & set(arms[b])
    print(f"  |{a} ∩ {b}| = {len(inter)}")
    ok &= (len(inter) == 0)
allm = list(itertools.chain.from_iterable(arms.values()))
print(f"  union size = {len(set(allm))} (expect {N_PER_ARM*N_ARMS}); no duplicates: {len(allm)==len(set(allm))}")
print(f"  all members are v6 members: {set(allm) <= set(names)}")
if not ok: sys.exit(2)

json.dump({a: {k: v for k, v in m.items() if k != 'members'} for a, m in manifests.items()},
          open(os.path.join(OUT, "arm_c_manifests.json"), "w"), indent=1)

# --- two-sided CONTROL: the symlinked DIRECTORY the design forbids ----------
ctrl = os.path.join(ROOT, "prolog", "oq353_ctrl_symlinked_dir")
if os.path.islink(ctrl): os.unlink(ctrl)
elif os.path.isdir(ctrl): shutil.rmtree(ctrl)
os.symlink(V6, ctrl)
print(f"\n=== CONTROL built: prolog/oq353_ctrl_symlinked_dir -> symlink TO the v6 dir ===")
print(f"  os.path.realpath -> {os.path.realpath(ctrl)}")
print(f"  files visible through it: {len(glob.glob(os.path.join(ctrl,'*.pl')))} (v6 is {len(names)})")
