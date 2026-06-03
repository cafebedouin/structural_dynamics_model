#!/usr/bin/env python3
"""Version-stable affects_constraint dangle-fraction across corpora (OQ-58 frontier model).

real-ID set = union of first-args of {human_readable/2, constraint_metric/3,
constraint_classification/_} + every affects_constraint SOURCE (definitionally real).
This union is robust to the later constraint_metric bridge-repair (687 files), so it does
not inflate dangle on older corpora that predate that repair.

dangle = affects_constraint targets not in the real-ID set. Identical logic per corpus.
"""
import re, sys, glob, os

# first top-level argument of a (possibly module-qualified) fact line
ARG1 = re.compile(r'^\s*(?:[a-z_][a-z0-9_]*:)?%s\s*\(\s*(.*)$')

def first_arg(rest):
    """rest = text right after the opening paren. Return first arg as bare atom."""
    rest = rest.strip()
    if rest.startswith('['):
        rest = rest[1:].strip()        # unwrap list-wrapped id
    m = re.match(r'([a-z_][a-zA-Z0-9_]*)', rest)
    return m.group(1) if m else None

def second_arg_of_affects(line):
    m = re.search(r'affects_constraint\s*\(\s*([^,]+?)\s*,\s*(.*?)\s*\)\s*\.', line)
    if not m: return (None, None)
    def clean(x):
        x = x.strip().lstrip('[').strip()
        mm = re.match(r'([a-z_][a-zA-Z0-9_]*)', x)
        return mm.group(1) if mm else None
    return (clean(m.group(1)), clean(m.group(2)))

def scan(files):
    real = set()
    edges = []  # (src, tgt)
    decl_preds = ['human_readable', 'constraint_metric', 'constraint_classification',
                  'constraint_claim']
    pats = {p: re.compile(r'^\s*(?:[a-z_][a-z0-9_]*:)?' + p + r'\s*\(\s*(.*)$') for p in decl_preds}
    for fp in files:
        with open(fp, encoding='utf-8', errors='replace') as fh:
            for line in fh:
                s = line.lstrip()
                if s.startswith('%'):
                    continue
                if '/2' in line or 'retractall' in line or 'discontiguous' in line:
                    continue
                for p, pat in pats.items():
                    m = pat.match(line)
                    if m:
                        a = first_arg(m.group(1))
                        if a: real.add(a)
                if 'affects_constraint(' in line:
                    src, tgt = second_arg_of_affects(line)
                    if src and tgt:
                        edges.append((src, tgt))
                        real.add(src)  # source is definitionally real
    return real, edges

def main():
    for label, pat in [
        ('v3 (1151)', 'prolog/archives/prolog_v3/testsets/*.pl'),
        ('v5 (3380)', 'prolog/archives/prolog_v5/*.pl'),
        ('v6 (229)',  'prolog/archives/prolog_v6/*.pl'),
        ('current (772)', 'prolog/testsets/*.pl'),
    ]:
        files = sorted(glob.glob(pat))
        if not files:
            print(f'{label:16} NO FILES at {pat}'); continue
        real, edges = scan(files)
        total = len(edges)
        dangle = sum(1 for _, t in edges if t not in real)
        frac = dangle / total if total else 0.0
        print(f'{label:16} stories={len(files):5d}  real_ids={len(real):5d}  '
              f'edges={total:5d}  dangle={dangle:5d}  frac={frac:6.3f}')

if __name__ == '__main__':
    os.chdir(os.path.join(os.path.dirname(__file__), '..'))
    main()
