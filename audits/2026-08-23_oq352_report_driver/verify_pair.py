"""Verification #3 (per-leg asserts) and #4 (pre-named cross-leg difference)."""
import sys, json, hashlib
from pathlib import Path
sys.path.insert(0, "python")
import run_pipeline as R
from corpus_hash import assert_corpus_current

LEGS = ["testsets_sonnet2", "testsets_sonnet3"]
res = json.loads(Path("/tmp/oq352_pair2.json").read_text())
OUT = R.OUTPUTS_DIR / "legs"

print("="*72); print("VERIFICATION #3 — per-leg assertions"); print("="*72)
for leg in LEGS:
    d = OUT / leg
    r = res[leg]
    cdir = R._resolve_corpus_dir(leg)
    arts = sorted(p for p in d.glob("*") if not p.name.endswith(".manifest.json")
                  and p.name != "report_corpus.result.json")
    sides = sorted(d.glob("*.manifest.json"))
    empty = [p.name for p in arts if p.stat().st_size == 0]
    bad = []
    for s in sides:
        try: assert_corpus_current(s, cdir)
        except RuntimeError as e: bad.append(f"{s.name}: {e}")
    print(f"\n--- {leg}")
    print(f"  artifacts               : {len(arts)}   empty: {empty or 'none'}")
    print(f"  sidecars                : {len(sides)}  assert_corpus_current rejects: {bad or 'none'}")
    print(f"  leg md5 across run      : {r['corpus_hash']} -> {r['corpus_hash_after']} "
          f"{'UNCHANGED' if r['corpus_hash']==r['corpus_hash_after'] else 'DRIFTED'}")
    print(f"  retry ledger            : present, {len(r['retry_ledger'])} row(s)")
    print(f"  prompt-hash token       : {r['prompt_hash']['token']} "
          f"coverage={r['prompt_hash']['coverage']:.4f} hash={sorted(r['prompt_hash']['hashes'])[0][:8]}...")
    co = Path(r['classify_output'])
    m = json.loads(co.read_text())['manifest']
    print(f"  same-commit classify    : {co.name} @ {m['code_commit'][:12]} "
          f"(HEAD {R._git_head_sha()[:12]}) {'MATCH' if m['code_commit']==R._git_head_sha() else 'MISMATCH'}")
    print(f"  transit journal at exit : {'DIRTY' if (R.REPORT_STATE_DIR/'journal.json').exists() else 'clean'}")
    print(f"  stages OK               : {sum(1 for v in r['stage_outcomes'].values() if v=='STAGE_OK')}"
          f"/{len(r['stage_outcomes'])}")

print()
print("="*72); print("VERIFICATION #4 — pre-named subset (PREREGISTRATION §1a/§1b)"); print("="*72)
c = {leg: json.loads(Path(res[leg]['classify_output']).read_text()) for leg in LEGS}
dg = {leg: c[leg]['diagnostic'] for leg in LEGS}

def med(leg, key):
    v = dg[leg].get(key)
    return v

R_ITEMS = [
 ("R1 corpus_wasserstein_fracture", lambda l: json.dumps(dg[l].get('corpus_wasserstein_fracture'), sort_keys=True)),
 ("R2 arakelov_threshold",          lambda l: json.dumps(dg[l].get('arakelov_threshold'), sort_keys=True)),
 ("R3 type_distribution",           lambda l: json.dumps(dg[l].get('type_distribution'), sort_keys=True)),
 ("R4 purity_n_scored/no_data",     lambda l: json.dumps([dg[l].get('purity_n_scored'), dg[l].get('purity_n_no_data')])),
 ("R5 drift_event_counts",          lambda l: json.dumps(dg[l].get('drift_event_counts'), sort_keys=True)),
 ("R6 orbit-class histogram",       lambda l: hashlib.sha256((OUT/l/"orbit_data.json").read_bytes()).hexdigest()[:16]),
 ("R7 giant-component size/members",lambda l: "UNAVAILABLE"),
]
ndiff = 0; navail = 0
for name, f in R_ITEMS:
    a, b = f(LEGS[0]), f(LEGS[1])
    if a == "UNAVAILABLE":
        print(f"  {name:34s} UNAVAILABLE — instrument broken on both legs (OQ-356)")
        continue
    navail += 1
    d = a != b
    ndiff += d
    print(f"  {name:34s} {'DIFFERS' if d else 'IDENTICAL'}")
print(f"\n  => {ndiff} of {navail} available REQUIRED statistics differ "
      f"(prereg verdict rule: >=5 of R1-R7; R7 unavailable, so >=5 of {navail})")
print(f"  => {'PASS' if ndiff >= 5 else 'HALT — overlay may not be taking effect'}")

print("\n  EXPECTED-SATURATED (identical is a FINDING for OQ-353, never a gate failure):")
S_ITEMS = [("S1 network_stability", 'network_stability'),
           ("S2 boltzmann_summary", 'boltzmann_summary'),
           ("S3 contextuality.by_type", 'contextuality'),
           ("S4 network_cascade_count_threshold", 'network_cascade_count_threshold')]
for name, key in S_ITEMS:
    a = json.dumps(dg[LEGS[0]].get(key), sort_keys=True)
    b = json.dumps(dg[LEGS[1]].get(key), sort_keys=True)
    print(f"    {name:36s} {'IDENTICAL (as predicted)' if a==b else 'DIFFERS (prediction wrong)'}")

print("\n  Report-stage artifacts, sonnet2 vs sonnet3 (not pre-named; recorded):")
for p in sorted((OUT/LEGS[0]).glob("*")):
    if p.name.endswith(".manifest.json") or p.name == "report_corpus.result.json": continue
    q = OUT/LEGS[1]/p.name
    if not q.exists(): continue
    same = hashlib.sha256(p.read_bytes()).hexdigest() == hashlib.sha256(q.read_bytes()).hexdigest()
    print(f"    {p.name:34s} {'IDENTICAL <- candidate saturated' if same else 'differs'}")
