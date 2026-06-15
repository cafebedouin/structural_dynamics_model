#!/usr/bin/env python3
"""Deterministic stratified sampler for the Ω-type diagnostic POC (OQ-130 child).

Extracts /5 omega records (name, question, approach, consequence, confidence)
joined to their /3 Ω-type and to the CONSTRAINT'S DECLARED FIELDS (the enumerated
reading set via cs_reading_relation + cs_kernel_id, victims/beneficiaries) from
prolog/testsets_haiku/*.pl. The declared-field blob is what the external-vs-restatement
locus test reads, so it must travel WITH each sampled omega and be identical for
both the adjudicator's held key and the blind executor.

Strata (N=40, fixed seed 20260614):
  - family   14  : the on-trial kernel_reading / committer family, ANY authored type
                   (full weight: it is the discriminating set, not a slice).
  - empirical 14 : authored Ω_E, NON-family (over-sampled, for the Ω_E-falsifier cell).
  - conceptual 8 : authored Ω_C, NON-family.
  - preference 4 : authored Ω_P, ANY (small population: 102 corpus-wide).

Run from prolog/ (cwd-relative glob). Writes /tmp/sample_40.json + prints the draw.
"""
import glob, re, json, random, collections

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

# focused on-trial family: kernel_reading_* / reading_kernel_* / *committer*
FAMILY = re.compile(r'^(kernel_reading|reading_kernel)|committer')

def declared_fields(txt):
    """All facts a locus test reads: enumerated readings + relations + v/b + eps."""
    out = {'cs_reading_relation': [], 'cs_kernel_id': [], 'cs_kernel_codification': [],
           'beneficiary': [], 'victim': [], 'epsilon': []}
    for m in re.finditer(r'narrative_ontology:cs_reading_relation\s*\(', txt):
        a, _ = parse_args(txt, txt.index('(', m.start()))
        if len(a) == 3:
            out['cs_reading_relation'].append([unq(a[1]), unq(a[2])])
    for m in re.finditer(r'narrative_ontology:cs_kernel_id\s*\(', txt):
        a, _ = parse_args(txt, txt.index('(', m.start()))
        if len(a) == 2:
            out['cs_kernel_id'].append([unq(a[0]), unq(a[1])])
    for m in re.finditer(r'narrative_ontology:cs_kernel_codification\s*\(', txt):
        a, _ = parse_args(txt, txt.index('(', m.start()))
        if len(a) == 2:
            out['cs_kernel_codification'].append([unq(a[0]), unq(a[1])])
    for m in re.finditer(r'narrative_ontology:constraint_beneficiary\s*\(', txt):
        a, _ = parse_args(txt, txt.index('(', m.start()))
        if len(a) == 2:
            out['beneficiary'].append(unq(a[1]))
    for m in re.finditer(r'narrative_ontology:constraint_victim\s*\(', txt):
        a, _ = parse_args(txt, txt.index('(', m.start()))
        if len(a) == 2:
            out['victim'].append(unq(a[1]))
    for m in re.finditer(r'(?:narrative_ontology:)?epsilon[a-z_]*\s*\(', txt):
        seg = txt[m.start():m.start()+200]
        nums = re.findall(r'0?\.\d+', seg)
        if nums:
            out['epsilon'].append(nums[0])
    out['epsilon'] = out['epsilon'][:3]
    return out

records = []
for f in sorted(glob.glob('testsets_haiku/*.pl')):
    txt = open(f, encoding='utf-8').read()
    types = {}
    for m in re.finditer(r'narrative_ontology:omega_variable\s*\(', txt):
        a, _ = parse_args(txt, m.end()-1)
        if len(a) == 3:
            types[unq(a[0])] = unq(a[1])
    fields = declared_fields(txt)
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
                'is_family': bool(FAMILY.search(name)),
                'question': unq(a[1]),
                'approach': unq(a[2]),
                'consequence': unq(a[3]),
                'confidence': unq(a[4]),
                'declared_fields': fields,
            })

# population report
pop = collections.Counter((r['omega_type'], r['is_family']) for r in records)
print(f"/5 records extracted: {len(records)}")
fam_total = sum(1 for r in records if r['is_family'])
print(f"family (on-trial) total: {fam_total}")
for (t, fam), c in sorted(pop.items()):
    print(f"  type={t:11s} family={fam!s:5s}: {c}")

def pool(pred):
    return sorted([r for r in records if pred(r)], key=lambda r: (r['story'], r['name']))

rng = random.Random(20260614)
sample = []
strata = [
    ('family',     lambda r: r['is_family'],                                    14),
    ('empirical',  lambda r: (not r['is_family']) and r['omega_type'] == 'empirical', 14),
    ('conceptual', lambda r: (not r['is_family']) and r['omega_type'] == 'conceptual', 8),
    ('preference', lambda r: (not r['is_family']) and r['omega_type'] == 'preference', 4),
]
for label, pred, k in strata:
    p = pool(pred)
    draw = rng.sample(p, min(k, len(p)))
    for r in draw:
        r['stratum'] = label
    sample.extend(draw)
    print(f"stratum {label:11s}: pool={len(p)} drawn={len(draw)}")

rng.shuffle(sample)
for idx, r in enumerate(sample):
    r['sample_id'] = idx

json.dump(sample, open('/tmp/sample_40.json', 'w'), indent=2, ensure_ascii=False)
print(f"\nsample of {len(sample)} -> /tmp/sample_40.json")
print("id | stratum | type | family | story | name")
for r in sample:
    print(f"  {r['sample_id']:2d} | {r['stratum'][:10]:10s} | {r['omega_type'][:4]} | "
          f"{r['is_family']!s:5s} | {r['story'][:34]:34s} | {r['name']}")
