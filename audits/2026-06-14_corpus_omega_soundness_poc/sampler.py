#!/usr/bin/env python3
"""Deterministic stratified sampler for probe-3 (soundness spot-check).
Extracts /5 omega records (name, question, approach, consequence, confidence)
joined to their /3 Ω-type, from prolog/testsets_haiku/*.pl, and draws a
fixed stratified sample of 30 (15 Ω_E / 12 Ω_C / 3 Ω_P) with a fixed seed so
the adjudicator's held sub-sample and the blind executor score the SAME omegas.
Run from prolog/ (cwd-relative glob)."""
import glob, re, json, random

def parse_args(s, i):
    assert s[i] == '('; i += 1; depth = 1; args = []; cur = []
    while i < len(s):
        c = s[i]
        if c == "'":
            cur.append(c); i += 1
            while i < len(s):
                if s[i] == "'" and i+1 < len(s) and s[i+1] == "'":
                    cur.append("''"); i += 2; continue
                if s[i] == "'":
                    cur.append("'"); i += 1; break
                cur.append(s[i]); i += 1
            continue
        if c == '(':
            depth += 1; cur.append(c); i += 1; continue
        if c == ')':
            depth -= 1
            if depth == 0:
                args.append(''.join(cur).strip()); return args, i+1
            cur.append(c); i += 1; continue
        if c == ',' and depth == 1:
            args.append(''.join(cur).strip()); cur = []; i += 1; continue
        cur.append(c); i += 1
    return args, i

def unq(a):
    a = a.strip()
    return a[1:-1].replace("''", "'") if a.startswith("'") and a.endswith("'") else a

records = []  # one per /5 omega, joined to its /3 type within the same file
for f in sorted(glob.glob('testsets_haiku/*.pl')):
    txt = open(f, encoding='utf-8').read()
    types = {}  # name -> Ω-type from /3 facts
    for m in re.finditer(r'narrative_ontology:omega_variable\s*\(', txt):
        a, _ = parse_args(txt, m.end()-1)
        if len(a) == 3:
            types[unq(a[0])] = unq(a[1])
    for m in re.finditer(r'(?<!:)\bomega_variable\s*\(', txt):
        if txt[max(0, m.start()-25):m.start()].rstrip().endswith(':'):
            continue
        a, _ = parse_args(txt, m.end()-1)
        if len(a) == 5:
            name = unq(a[0])
            records.append({
                'story': f.split('/')[-1],
                'name': name,
                'omega_type': types.get(name, 'UNKNOWN'),
                'question': unq(a[1]),
                'approach': unq(a[2]),
                'consequence': unq(a[3]),
                'confidence': unq(a[4]),
            })

by_type = {'empirical': [], 'conceptual': [], 'preference': []}
for r in records:
    by_type.setdefault(r['omega_type'], []).append(r)

print(f"/5 records extracted: {len(records)}")
for t, rs in by_type.items():
    print(f"  {t}: {len(rs)}")

rng = random.Random(20260614)  # fixed seed = reproducible sample
sample = []
for t, k in [('empirical', 15), ('conceptual', 12), ('preference', 3)]:
    pool = sorted(by_type[t], key=lambda r: (r['story'], r['name']))
    sample.extend(rng.sample(pool, k))
rng.shuffle(sample)
for idx, r in enumerate(sample):
    r['sample_id'] = idx

json.dump(sample, open('/tmp/sample_30.json', 'w'), indent=2, ensure_ascii=False)
print(f"\nsample of {len(sample)} written to /tmp/sample_30.json")
print("sample_id | type | story | name")
for r in sample:
    print(f"  {r['sample_id']:2d} | {r['omega_type'][:4]} | {r['story'][:40]:40s} | {r['name']}")
