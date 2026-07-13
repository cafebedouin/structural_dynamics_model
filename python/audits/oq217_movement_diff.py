#!/usr/bin/env python3
"""OQ-217 movement diff: per-id before/after consensus-verdict comparison,
checked against the PRE-DERIVED prediction (derive-diff-before-run).

Inputs (all artifacts, no live engine reads):
  --before  dir with pre-tightening census_input_<leg>.json
            (the 2026-07-12 OQ-207 census artifacts)
  --after   dir with post-tightening census_input_<leg>.json AND
            predicted_movement.json (derived from the BEFORE artifacts
            before the engine change landed)

Asserts, per leg:
  1. id sets identical before/after (a membership drift would confound the
     verdict diff with corpus change);
  2. every after-verdict equals the pre-derived prediction (exact, per id);
  3. movers are enumerated BY NAME per transition (operator flag 2026-07-12:
     aggregate counts would conceal a misrouted story);
  4. no after-plural carries the unknown token (plural_has_unknown all false).

Positive control (the comparator itself is a claim): a doctored copy of one
after-record's verdict is run through the SAME comparison path and must be
FLAGGED — proving a misrouted story would not pass silently.

Exit nonzero on any assertion failure or a planted control that does not fire.
"""
import argparse
import json
import os
import sys

LEGS = ('testsets', 'testsets_haiku', 'testsets_flash', 'kernel_v1')


def load(d, leg):
    return {r['id']: r for r in json.load(open(os.path.join(d, f'census_input_{leg}.json')))}


def compare_leg(before, after, predicted):
    """Returns (movers, term_only, mismatches). movers: transition -> [ids]."""
    movers, term_only, mismatches = {}, [], []
    for cid, b in before.items():
        a = after[cid]
        want = predicted[cid]
        if a['verdict'] != want:
            mismatches.append({'id': cid, 'before': b['verdict'],
                               'predicted': want, 'actual': a['verdict']})
        if a['verdict'] != b['verdict']:
            movers.setdefault(f"{b['verdict']} -> {a['verdict']}", []).append(cid)
        elif b['verdict'] == 'plural' and b.get('plural_has_unknown') \
                and not a.get('plural_has_unknown'):
            term_only.append(cid)
    return movers, term_only, mismatches


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--before', required=True)
    ap.add_argument('--after', required=True)
    args = ap.parse_args()

    pred = json.load(open(os.path.join(args.after, 'predicted_movement.json')))
    failures = []
    report = {'before': args.before, 'after': args.after, 'legs': {}}

    for leg in LEGS:
        before, after = load(args.before, leg), load(args.after, leg)
        if set(before) != set(after):
            failures.append(f'{leg}: id-set drift '
                            f'(only-before={sorted(set(before) - set(after))[:5]}, '
                            f'only-after={sorted(set(after) - set(before))[:5]})')
            continue
        predicted = pred['legs'][leg]['predicted_verdicts']
        movers, term_only, mismatches = compare_leg(before, after, predicted)
        unknown_in_plural = [cid for cid, a in after.items()
                             if a['verdict'] == 'plural' and a.get('plural_has_unknown')]
        report['legs'][leg] = {
            'n': len(before),
            'movers': {k: sorted(v) for k, v in sorted(movers.items())},
            'mover_counts': {k: len(v) for k, v in sorted(movers.items())},
            'plural_term_drops_unknown': sorted(term_only),
            'prediction_mismatches': mismatches,
            'after_plural_with_unknown': unknown_in_plural,
        }
        if mismatches:
            failures.append(f'{leg}: {len(mismatches)} prediction mismatches')
        if unknown_in_plural:
            failures.append(f'{leg}: unknown token survives in plural: '
                            f'{unknown_in_plural[:5]}')
        print(f"== {leg} (n={len(before)}) — prediction mismatches: {len(mismatches)}")
        for k, ids in sorted(movers.items()):
            print(f"   {k}: {len(ids)}")
            for cid in sorted(ids):
                print(f"      {cid}")
        print(f"   plural term drops unknown (bucket unchanged): {len(term_only)}")

    # --- positive control: a planted misroute must be FLAGGED ---------------
    leg = 'testsets'
    before, after = load(args.before, leg), load(args.after, leg)
    predicted = pred['legs'][leg]['predicted_verdicts']
    victim = next(cid for cid, b in before.items()
                  if b['verdict'] != after[cid]['verdict'])   # a genuine mover
    doctored = dict(after)
    doctored[victim] = dict(after[victim], verdict=before[victim]['verdict'])
    _, _, planted = compare_leg(before, doctored, predicted)
    control_ok = any(m['id'] == victim for m in planted)
    report['planted_misroute_control'] = {
        'id': victim, 'planted_verdict': before[victim]['verdict'],
        'flagged': control_ok}
    print(f"\n[control] planted misroute (id={victim}, verdict reverted to "
          f"'{before[victim]['verdict']}'): "
          f"{'FLAGGED - PASS' if control_ok else 'NOT FLAGGED - FAIL'}")
    if not control_ok:
        failures.append('planted_misroute_not_flagged')

    report['verdict'] = 'PASS' if not failures else f'FAIL: {failures}'
    out = os.path.join(args.after, 'movement_diff.json')
    with open(out, 'w') as f:
        json.dump(report, f, indent=1, sort_keys=True)
    print(f"\nmovement diff -> {out}\nVERDICT: {report['verdict']}")
    return 0 if not failures else 1


if __name__ == '__main__':
    sys.exit(main())
