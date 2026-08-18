#!/usr/bin/env python3
"""
DESIGN-FEASIBILITY PROBE — NOT A RESULT.

OQ-311 Item 2 asks whether the sign-flip Jaccard advantage tracks a GEOMETRIC
CONDITION (chi span crosses the sign-flip zero) or a TYPE LABEL (tangled_rope).
Those two cuts are only distinguishable if the OFF-DIAGONAL cells of their
cross-tab are populated: condition AND NOT tangled_rope, and tangled_rope AND
NOT condition. A design that cannot populate both is a declared UNANSWERABLE,
not a result to ship.

This script answers ONLY: "are those cells non-empty somewhere?" — i.e. is the
Item 2 design feasible at all. It is a PROXY and must not be read as an answer:

  * WRONG CORPUS. It reads the committed OQ-22 census TSVs, which cover
    testsets / haiku / flash / kernel_v1. It does NOT touch
    archives/datasets/original_v6, which is the corpus every §2.3 number came
    from and the only corpus Item 2 may be run against.
  * WRONG CODE STATE. Those TSVs were produced at commit bbbf2c6 (2026-06-28).
    The chi path has moved since (OQ-67 drained the legacy chi = eps * pi path).
  * WRONG CONDITION, POSSIBLY. §2.3 states the geometric condition two ways that
    are NOT the same set — "Hub 1 spans the snare gate" (span crosses
    snare_chi_floor) and "institutional beneficiaries below d_zero and powerless
    victims above" (span crosses ZERO). This script uses the ZERO-CROSSING.
    Resolving that conflation is the first task of PREREGISTRATION.md.
  * NO JACCARD. No transformation-variant sweep is run here. Cell occupancy is
    the entire output. Nothing about the ADVANTAGE is measured.

Output: evidence/feasibility_crosstab.tsv
"""
import csv, sys
from collections import Counter
from pathlib import Path

SRC = Path('audits/2026-06-28_oq22_hub_starvation')
LEGS = ['testsets', 'testsets_haiku', 'testsets_flash', 'kernel_v1']


def modal_type(mtype_vec):
    """Modal per-position type. Ties -> None (excluded, not silently broken)."""
    toks = [t for t in mtype_vec.split('|') if t]
    if not toks:
        return None
    c = Counter(toks).most_common()
    if len(c) > 1 and c[0][1] == c[1][1]:
        return None
    return c[0][0]


rows = []
for leg in LEGS:
    f = SRC / f'census_{leg}.tsv'
    if not f.exists():
        print(f'MISSING: {f}', file=sys.stderr)
        continue
    cells = Counter()
    skipped_tie = skipped_parse = 0
    with f.open() as fh:
        for r in csv.DictReader(fh, delimiter='\t'):
            try:
                lo, hi = float(r['chi_min']), float(r['chi_max'])
            except (ValueError, KeyError, TypeError):
                skipped_parse += 1
                continue
            mt = modal_type(r.get('mtype_vec', ''))
            if mt is None:
                skipped_tie += 1
                continue
            cond = (lo < 0.0 < hi)          # chi span crosses the sign-flip zero
            tr = (mt == 'tangled_rope')
            cells[(cond, tr)] += 1
    n = sum(cells.values())
    rows.append({
        'leg': leg,
        'n_scored': n,
        'n_skipped_modal_tie': skipped_tie,
        'n_skipped_unparseable': skipped_parse,
        'cond_and_TR': cells[(True, True)],
        'cond_and_notTR': cells[(True, False)],     # DISCRIMINATING
        'notcond_and_TR': cells[(False, True)],     # DISCRIMINATING
        'notcond_and_notTR': cells[(False, False)],
        'both_offdiag_populated': cells[(True, False)] > 0 and cells[(False, True)] > 0,
    })

# ---------------------------------------------------------------------------
# SENSITIVITY OF THE FEASIBILITY VERDICT TO THE TIE RULE AND THE TYPE COLUMN.
#
# "Modal type" needs a tie rule, and a tie rule is a free choice. If the
# feasibility verdict moved with it, the verdict would be an artefact of that
# choice rather than a fact about the corpora. So it is swept, not assumed.
#
# Rules: exclude (drop ties)  |  first (first-occurrence among the maxima)
#        any_tr (counts as tangled_rope if TR is among the maxima)
# Columns: mtype_vec (metric type) and ftype_vec (final/post-signature type).
# ---------------------------------------------------------------------------


def modal_sens(vec, tie):
    toks = [t for t in vec.split('|') if t]
    if not toks:
        return None
    c = Counter(toks).most_common()
    if len(c) > 1 and c[0][1] == c[1][1]:
        m = c[0][1]
        top = {k for k, n in c if n == m}
        if tie == 'exclude':
            return None
        if tie == 'first':
            for t in toks:
                if t in top:
                    return t
        if tie == 'any_tr':
            return 'tangled_rope' if 'tangled_rope' in top else c[0][0]
    return c[0][0]


sens = []
for tie in ('exclude', 'first', 'any_tr'):
    for leg in LEGS:
        f = SRC / f'census_{leg}.tsv'
        if not f.exists():
            continue
        for col in ('mtype_vec', 'ftype_vec'):
            cells = Counter()
            with f.open() as fh:
                for r in csv.DictReader(fh, delimiter='\t'):
                    try:
                        lo, hi = float(r['chi_min']), float(r['chi_max'])
                    except (ValueError, KeyError, TypeError):
                        continue
                    mt = modal_sens(r.get(col, ''), tie)
                    if mt is None:
                        continue
                    cells[(lo < 0.0 < hi, mt == 'tangled_rope')] += 1
            a, b = cells[(True, False)], cells[(False, True)]
            sens.append((tie, leg, col, a, b, a > 0 and b > 0))

print()
print('SENSITIVITY SWEEP — does the feasibility verdict depend on the tie rule?')
print('tie_rule\tleg\tcolumn\tcond_and_notTR\tnotcond_and_TR\tboth_offdiag')
for row in sens:
    print('\t'.join(str(x) for x in row))
print()
verdicts = {}
for tie, leg, col, a, b, both in sens:
    verdicts.setdefault(leg, set()).add(both)
print('VERDICT STABILITY per leg (across 3 tie rules x 2 type columns = 6 settings):')
for leg, v in verdicts.items():
    print(f'  {leg:16s} both_offdiag_populated = {v}  -> '
          f'{"STABLE" if len(v) == 1 else "UNSTABLE — verdict is an artefact of the tie rule"}')
print()
print('NOTE: the tie rule "first" reproduces the OQ-311 plan\'s recon figures exactly')
print('      (kernel_v1 433/155, haiku 616/48, flash notcond_and_TR=0), identifying')
print('      which rule that recon used. The counts move with the rule; the FEASIBILITY')
print('      VERDICT does not. Only the verdict is claimed.')

out = Path('audits/2026-08-18_oq311_universality_class/evidence/feasibility_crosstab.tsv')
with out.open('w', newline='') as fh:
    w = csv.DictWriter(fh, fieldnames=list(rows[0].keys()), delimiter='\t')
    w.writeheader()
    w.writerows(rows)

hdr = list(rows[0].keys())
print('\t'.join(hdr))
for r in rows:
    print('\t'.join(str(r[k]) for k in hdr))
print()
print('DESIGN-FEASIBILITY ONLY — proxy corpus, proxy code state, no Jaccard measured.')
