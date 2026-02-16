#!/usr/bin/env python3
"""
ID Normalization Audit — Bug #3 Diagnostic

Compares constraint IDs between corpus_data.json and orbit_data.json,
classifying mismatches by type: case_fold, prefix_strip, typo,
ulysses_rename, truncation, singular_plural, truly_unmatched.
"""

import json
import os
import re
from difflib import SequenceMatcher
from pathlib import Path

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
ROOT_DIR = os.path.dirname(SCRIPT_DIR)
OUTPUT_DIR = os.path.join(ROOT_DIR, 'outputs')
TESTSETS_DIR = os.path.join(ROOT_DIR, 'prolog', 'testsets')


def load_ids():
    with open(os.path.join(OUTPUT_DIR, 'corpus_data.json'), 'r') as f:
        corpus = json.load(f)
    corpus_ids = set(corpus['constraints'].keys())

    with open(os.path.join(OUTPUT_DIR, 'orbit_data.json'), 'r') as f:
        orbit = json.load(f)
    orbit_ids = set(orbit.keys())

    return corpus_ids, orbit_ids


def classify_match(corpus_id, orbit_id):
    """Classify the type of match between two IDs."""
    # Case fold
    if corpus_id.lower() == orbit_id.lower():
        return 'case_fold'

    # Prefix strip: orbit uses constraint_ prefix
    if orbit_id.startswith('constraint_') and orbit_id[len('constraint_'):] == corpus_id:
        return 'prefix_strip'
    if corpus_id.startswith('constraint_') and corpus_id[len('constraint_'):] == orbit_id:
        return 'prefix_strip'

    # Ulysses rename: ulysses_chpN <-> ulysses_*_YYYY
    ulysses_chp = re.match(r'ulysses_chp(\d+)', corpus_id) or re.match(r'ulysses_chp(\d+)', orbit_id)
    ulysses_year = re.match(r'ulysses_\w+_(\d{4})', corpus_id) or re.match(r'ulysses_\w+_(\d{4})', orbit_id)
    if ulysses_chp and ulysses_year:
        return 'ulysses_rename'

    # Truncation: one is a prefix/substring of the other
    if corpus_id in orbit_id or orbit_id in corpus_id:
        return 'truncation'

    # Singular/plural
    if corpus_id + 's' == orbit_id or orbit_id + 's' == corpus_id:
        return 'singular_plural'
    if corpus_id.rstrip('s') == orbit_id.rstrip('s'):
        return 'singular_plural'

    # Typo detection via SequenceMatcher
    ratio = SequenceMatcher(None, corpus_id, orbit_id).ratio()
    if ratio >= 0.85:
        return 'typo'

    return None


def find_matches(corpus_only, orbit_only):
    """Attempt fuzzy matching between unmatched IDs."""
    matches = []
    matched_corpus = set()
    matched_orbit = set()

    for cid in sorted(corpus_only):
        best_match = None
        best_type = None
        best_ratio = 0

        for oid in sorted(orbit_only):
            if oid in matched_orbit:
                continue

            match_type = classify_match(cid, oid)
            if match_type:
                ratio = SequenceMatcher(None, cid, oid).ratio()
                if ratio > best_ratio:
                    best_ratio = ratio
                    best_match = oid
                    best_type = match_type

        if best_match:
            matches.append((cid, best_match, best_type, best_ratio))
            matched_corpus.add(cid)
            matched_orbit.add(best_match)

    truly_unmatched_corpus = corpus_only - matched_corpus
    truly_unmatched_orbit = orbit_only - matched_orbit

    return matches, truly_unmatched_corpus, truly_unmatched_orbit


def check_pl_exists(constraint_id):
    """Check if a .pl file exists for this constraint."""
    pl_path = os.path.join(TESTSETS_DIR, f'{constraint_id}.pl')
    return os.path.exists(pl_path)


def write_report(matches, truly_unmatched_corpus, truly_unmatched_orbit,
                 corpus_ids, orbit_ids):
    """Write the audit report."""
    report_path = os.path.join(OUTPUT_DIR, 'id_normalization_audit.md')

    with open(report_path, 'w') as f:
        f.write('# ID Normalization Audit\n\n')
        f.write(f'**Corpus IDs:** {len(corpus_ids)}  \n')
        f.write(f'**Orbit IDs:** {len(orbit_ids)}  \n')
        f.write(f'**Exact matches:** {len(corpus_ids & orbit_ids)}  \n')
        f.write(f'**Corpus-only:** {len(corpus_ids - orbit_ids)}  \n')
        f.write(f'**Orbit-only:** {len(orbit_ids - corpus_ids)}  \n\n')

        # Summary by match type
        from collections import Counter
        type_counts = Counter(m[2] for m in matches)
        f.write('## Match Classification Summary\n\n')
        f.write('| Match Type | Count |\n')
        f.write('|---|---|\n')
        for mt in ['case_fold', 'prefix_strip', 'typo', 'ulysses_rename',
                    'truncation', 'singular_plural']:
            f.write(f'| {mt} | {type_counts.get(mt, 0)} |\n')
        f.write(f'| truly_unmatched (corpus-only) | {len(truly_unmatched_corpus)} |\n')
        f.write(f'| truly_unmatched (orbit-only) | {len(truly_unmatched_orbit)} |\n')
        f.write('\n')

        # Fuzzy match details
        if matches:
            f.write('## Fuzzy Matches\n\n')
            f.write('| Corpus ID | Orbit ID | Match Type | Similarity |\n')
            f.write('|---|---|---|---|\n')
            for cid, oid, mtype, ratio in sorted(matches, key=lambda x: x[2]):
                f.write(f'| `{cid}` | `{oid}` | {mtype} | {ratio:.2f} |\n')
            f.write('\n')

        # Truly unmatched corpus IDs
        if truly_unmatched_corpus:
            f.write('## Truly Unmatched — Corpus Only\n\n')
            f.write('| Constraint ID | .pl file exists? |\n')
            f.write('|---|---|\n')
            for cid in sorted(truly_unmatched_corpus):
                exists = check_pl_exists(cid)
                f.write(f'| `{cid}` | {"yes" if exists else "**NO**"} |\n')
            f.write('\n')

        # Truly unmatched orbit IDs
        if truly_unmatched_orbit:
            f.write('## Truly Unmatched — Orbit Only\n\n')
            f.write('| Constraint ID | .pl file exists? |\n')
            f.write('|---|---|\n')
            for oid in sorted(truly_unmatched_orbit):
                exists = check_pl_exists(oid)
                f.write(f'| `{oid}` | {"yes" if exists else "**NO**"} |\n')
            f.write('\n')

    print(f'Report written to {report_path}')
    print(f'  Exact matches: {len(corpus_ids & orbit_ids)}')
    print(f'  Fuzzy matches: {len(matches)}')
    print(f'  Truly unmatched (corpus): {len(truly_unmatched_corpus)}')
    print(f'  Truly unmatched (orbit): {len(truly_unmatched_orbit)}')


def main():
    corpus_ids, orbit_ids = load_ids()

    corpus_only = corpus_ids - orbit_ids
    orbit_only = orbit_ids - corpus_ids

    print(f'Corpus IDs: {len(corpus_ids)}')
    print(f'Orbit IDs: {len(orbit_ids)}')
    print(f'Exact matches: {len(corpus_ids & orbit_ids)}')
    print(f'Corpus-only: {len(corpus_only)}')
    print(f'Orbit-only: {len(orbit_only)}')
    print()

    matches, truly_unmatched_corpus, truly_unmatched_orbit = find_matches(
        corpus_only, orbit_only
    )

    write_report(matches, truly_unmatched_corpus, truly_unmatched_orbit,
                 corpus_ids, orbit_ids)


if __name__ == '__main__':
    main()
