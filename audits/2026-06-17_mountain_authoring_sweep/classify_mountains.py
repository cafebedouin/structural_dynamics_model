#!/usr/bin/env python3
"""
Sweep every engine-declared mountain (constraint_claim(C, mountain)) across the corpora and
content-classify each by its OWN AUTHORED PERSPECTIVES + omegas — NOT by the metric box-set
(ε/accessibility/resistance) that the Boltzmann probe already proved cannot separate physics
from naturalization.

Two content signals (a story is CONTESTED-BY-ITS-OWN-CONTENT if either fires):
  (P) perspective-diversity: the story authors a NON-mountain constraint_classification at any
      seat (the topological signature: Perspective 5 = rope). A clean converged mountain (thermo)
      authors mountain at every seat. Same-context-two-types is the strongest sub-form.
  (O) false-summit / contingency omega: an omega flagging the mountain framing as a possibly-chosen
      / negotiable / naturalized axiom (the AC signature).

POSITIVE CONTROL (pre-registered kill): the classifier MUST flag topological (via P) and AC (via O),
and MUST leave thermodynamics_entropy clean. If any of these fail, the classifier is invalid — abort.
"""
import re, sys, glob, os
from collections import defaultdict

CC = re.compile(
    r'constraint_classification\(\s*\w+\s*,\s*(\w+)\s*,\s*'
    r'context\(\s*agent_power\((\w+)\)\s*,\s*time_horizon\((\w+)\)\s*,\s*'
    r'exit_options\((\w+)\)\s*,\s*spatial_scope\((\w+)\)', re.S)
CLAIM = re.compile(r'constraint_claim\(\s*(\w+)\s*,\s*mountain\s*\)')
# contingency / false-summit omega language (scoped to omega + perspective prose)
OMEGA_KEYS = [
    'false summit', 'false_summit', 'negotiable axiom', 'chosen axiom', 'naturaliz',
    'chosen rather than', 'naturalizes', 'unchallengeable natural law vs', 'convention',
    'agreed to use', 'community has agreed', 'pure coordination', 'shift in epistemic frame',
    'alternatives require rework', 'mountain as a rope', 'actually a chosen', 'mountain_false_summit',
]

def classify(path):
    txt = open(path, encoding='utf-8', errors='replace').read()
    m = CLAIM.search(txt)
    if not m: return None
    name = m.group(1)
    persp = CC.findall(txt)                       # [(type, power, time, exit, scope), ...]
    types = [p[0] for p in persp]
    distinct = sorted(set(types))
    nonmtn = sorted(set(t for t in types if t != 'mountain'))
    # same-context-two-types
    ctxmap = defaultdict(set)
    for t,pw,ti,ex,sc in persp:
        ctxmap[(pw,ti,ex,sc)].add(t)
    contradictions = {k:sorted(v) for k,v in ctxmap.items() if len(v) > 1}
    low = txt.lower()
    omega_hits = sorted(set(k for k in OMEGA_KEYS if k in low))
    P = len(nonmtn) > 0
    O = len(omega_hits) > 0
    npersp = len(persp)
    if P or O:
        bucket = 'CONTESTED'
    elif npersp == 0:
        bucket = 'UNREAD'          # no constraint_classification facts parsed — NOT clean, just unseen
    else:
        bucket = 'CLEAN'
    return dict(name=name, npersp=npersp, distinct=distinct, nonmtn=nonmtn,
                contradictions=contradictions, omega_hits=omega_hits,
                bucket=bucket, why=('+'.join([s for s,b in [('P-persp',P),('O-omega',O)] if b]) or bucket.lower()))

def main():
    dirs = sys.argv[1:]
    rows = {}
    for d in dirs:
        for f in glob.glob(os.path.join(d, '*.pl')):
            r = classify(f)
            if r: rows.setdefault(r['name'], r)  # first occurrence per name
    # ---- POSITIVE CONTROL ----
    pc_ok = True
    def check(nm, want_bucket, want_signal=None, min_persp=None):
        nonlocal pc_ok
        r = rows.get(nm)
        if not r: print(f"  POS-CONTROL MISSING: {nm} not found"); pc_ok=False; return
        ok = (r['bucket'] == want_bucket) and (want_signal is None or want_signal in r['why']) \
             and (min_persp is None or r['npersp'] >= min_persp)
        print(f"  POS-CONTROL {'PASS' if ok else 'FAIL'}: {nm} -> bucket={r['bucket']} npersp={r['npersp']} why={r['why']} nonmtn={r['nonmtn']} omega={r['omega_hits'][:2]}")
        pc_ok = pc_ok and ok
    print("=== POSITIVE CONTROL (pre-registered) ===")
    check('topological_invariant_universality', 'CONTESTED', 'P-persp')
    check('axiom_of_choice_consequence', 'CONTESTED', 'O-omega')
    check('thermodynamics_entropy', 'CLEAN', min_persp=4)   # must actually READ its 4 seats, not default-clean
    if not pc_ok:
        print("\n!!! POSITIVE CONTROL FAILED — classifier invalid, aborting verdict !!!"); sys.exit(2)
    # ---- SWEEP ----
    from collections import Counter
    buckets = Counter(r['bucket'] for r in rows.values())
    clean = [r for r in rows.values() if r['bucket']=='CLEAN']
    contested = [r for r in rows.values() if r['bucket']=='CONTESTED']
    unread = [r for r in rows.values() if r['bucket']=='UNREAD']
    print(f"\n=== SWEEP: {len(rows)} distinct engine-declared mountains ===")
    print(f"  CLEAN (npersp>=1, mountain at EVERY authored seat, no contingency omega): {buckets['CLEAN']}")
    print(f"  CONTESTED-BY-OWN-CONTENT (non-mtn seat OR contingency omega): {buckets['CONTESTED']}")
    print(f"  UNREAD (no constraint_classification parsed — different schema, NOT classifiable here): {buckets['UNREAD']}")
    print(f"\n--- CLEAN converged mountains (the real OQ-128 test population) ---")
    for r in sorted(clean, key=lambda x:x['name']):
        print(f"  {r['name']:55} npersp={r['npersp']} types={r['distinct']}")
    print(f"\n--- CONTESTED with SAME-SEAT CONTRADICTION (the topological class — strongest mis-authoring) ---")
    for r in sorted([x for x in contested if x['contradictions']], key=lambda x:x['name']):
        print(f"  {r['name']:55} contradictions={r['contradictions']}")
    print(f"\n--- CONTESTED total={len(contested)}; with-contradiction={sum(1 for x in contested if x['contradictions'])}; omega-only={sum(1 for x in contested if not x['nonmtn'] and x['omega_hits'])} ---")

if __name__ == '__main__':
    main()
