#!/usr/bin/env python3
"""Positive-control fixture for sheaf_audit.py's fail-closed floor (OQ-147).

Calls build_markdown / _verdict directly with hand-built `results` dicts — no file I/O,
no pipeline dependency. Two cases:

  1. empty (working_set_size == 0): build_markdown returns INSUFFICIENT-DATA markdown with
     no exception; _verdict(_, insufficient=True) == 'insufficient_data'.
  2. minimal non-empty (working_set_size == 2, one crossing): build_markdown renders a finite
     numeric verdict + finite rates; the four happy-path _verdict bands reproduce the original
     464–471 strings byte-for-byte (the verdict-string swap is NOT self-witnessed by the early
     return, so it is pinned here).

Run: python3 python/audits/tests/test_sheaf_audit.py
"""

import sys
from collections import Counter
from pathlib import Path

# Put python/audits on the path so `import sheaf_audit` resolves; importing it adds python/
# (its own sys.path.insert) for shared.loader / corpus_hash.
sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

import sheaf_audit
from sheaf_audit import build_markdown, _verdict


def _base_results(working_set_size):
    """Minimal results dict covering every key build_markdown reads."""
    return {
        'generated': '2026-01-01T00:00:00',
        'corpus_total': 80,
        'excluded_zero_contexts': 80 if working_set_size == 0 else 0,
        'excluded_one_context': 0,
        'working_set_size': working_set_size,
        'deduplication': {'total_conflicts': 0, 'constraints_with_conflicts': 0},
        'binary_preservation': {
            'n_canonical_sheaf': 1,
            'n_ten_slice_sheaf': 0,
            'n_preserved': 1,
            'n_sheaf_to_presheaf': 1,
            'n_presheaf_to_sheaf': 0,
            'crossing_rate': 0.5,
            'preservation_rate': 0.5,
            'verdict': 'NOT PRESERVED (50.00% crossing rate)',
        },
        'crossing_characterization': {
            'sheaf_to_presheaf': {
                'count': 1,
                'driving_slices': {'U_3_civ': 1},
                'signature_distribution': {'tangled_rope': 1},
                'claimed_type_distribution': {'rope': 1},
                'canonical_arakelov_mean': None,
                'u3civ_driven_count': 1,
                'organized_driven_count': 0,
            },
            'presheaf_to_sheaf': {
                'count': 0,
                'signature_distribution': {},
                'claimed_type_distribution': {},
            },
        },
        'nash_analysis': {
            'canonical_nash1_total': 0,
            'canonical_nash1_in_working_set': 0,
            'canonical_nash1_ten_stable_ge2': 0,
            'canonical_nash1_ten_still_1': 0,
            'canonical_nash1_ten_sheaf': 0,
            'stability_rate': None,
        },
    }


def test_verdict_insufficient_short_circuits():
    # insufficient=True must win regardless of crossing_rate — never a PRESERVED token.
    assert _verdict(0.0, True) == 'insufficient_data'
    assert _verdict(0.5, True) == 'insufficient_data'
    print('  ok: _verdict short-circuits to insufficient_data')


def test_verdict_bands_byte_identical():
    # These are the exact strings produced by the original 464–471 branch.
    assert _verdict(0.0, False) == 'PRESERVED (zero crossings)'
    assert _verdict(0.02, False) == 'MOSTLY PRESERVED (2.00% crossing rate)'
    assert _verdict(0.10, False) == 'PARTIAL PRESERVATION (10.00% crossing rate)'
    assert _verdict(0.20, False) == 'NOT PRESERVED (20.00% crossing rate)'
    print('  ok: _verdict happy-path bands byte-identical to pre-fix strings')


def test_empty_working_set_markdown():
    results = _base_results(0)
    md = build_markdown(results, {}, Counter(), [], [], [], [], set())
    assert isinstance(md, str)
    assert 'INSUFFICIENT DATA' in md
    assert 'not a measured-flat result' in md
    # No fabricated PRESERVED verdict, no NaN/None arithmetic artifacts.
    assert 'PRESERVED (zero crossings)' not in md
    assert 'None%' not in md and 'nan' not in md.lower()
    print('  ok: empty working set -> INSUFFICIENT-DATA markdown, no exception')


def test_nonempty_markdown_finite():
    results = _base_results(2)
    strata = {2: {'n': 2, 'canonical_sheaf': 1, 'ten_slice_sheaf': 0, 's2p': 1, 'p2s': 0}}
    driving = Counter({'U_3_civ': 1})
    md = build_markdown(results, strata, driving, [], [], [], [], set())
    assert isinstance(md, str)
    assert 'INSUFFICIENT DATA' not in md
    assert 'NOT PRESERVED (50.00% crossing rate)' in md  # finite numeric verdict
    assert 'None%' not in md and 'nan' not in md.lower()  # finite rates throughout
    print('  ok: non-empty working set -> finite verdict + finite rates')


def main():
    tests = [
        test_verdict_insufficient_short_circuits,
        test_verdict_bands_byte_identical,
        test_empty_working_set_markdown,
        test_nonempty_markdown_finite,
    ]
    print(f'Running {len(tests)} sheaf_audit fixture tests...')
    for t in tests:
        t()
    print(f'PASS: {len(tests)}/{len(tests)} sheaf_audit fixture tests')


if __name__ == '__main__':
    main()
