#!/usr/bin/env python3
"""Binary Sheaf/Presheaf Boundary Audit on the 10-Slice Tier-1 Family.

Tests whether the H¹ = 0 vs H¹ > 0 binary classification is preserved
when the apparatus is run at the 10-slice Tier-1 working family, compared
to the canonical 4-point site (h1_band from pipeline_output.json).

PRIMARY FRAMING CONSTRAINT: This audit does not test "binary H¹ preservation
on the 10-slice family." It tests whether the binary classification survives
when each constraint is evaluated on whatever subset of the 10-slice contexts
its testsets happen to cover (2–7 contexts per constraint, not all 10).
Stratified analysis by n_contexts is the main diagnostic tool.

Tractability:
  - H¹ binary: TRACTABLE from pipeline_output.json (classifications field)
  - Nash distance: TRACTABLE from orbit_data.json (canonical) + same data (10-slice)
  - Arakelov fragility: DEFERRED — MaxEnt not available for 10-slice contexts

Inputs:
  outputs/pipeline_output.json  (h1_band, classifications, arakelov_height, signature)
  outputs/orbit_data.json       (canonical 4-context type vectors)

Outputs:
  outputs/sheaf_audit_results.json
  outputs/sheaf_audit_results.md
"""

import json
import sys
from collections import Counter, defaultdict
from datetime import datetime
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

from shared.loader import load_json, OUTPUT_DIR, PIPELINE_JSON, ORBIT_JSON
from corpus_hash import compute_corpus_hash  # single-source corpus fingerprint (OQ-29)

# ---------------------------------------------------------------------------
# 10-slice family definition
# ---------------------------------------------------------------------------

TEN_SLICE = {
    'U_4':      ('analytical',    'civilizational', 'analytical',  'universal'),
    'U_3_imm':  ('institutional', 'immediate',      'arbitrage',   'global'),
    'U_3_civ':  ('institutional', 'civilizational', 'arbitrage',   'global'),
    'U_1':      ('powerless',     'biographical',   'trapped',     'global'),
    'U_2':      ('moderate',      'biographical',   'constrained', 'national'),
    'organized':('organized',     'generational',   'constrained', 'global'),
    'U_1_nat':  ('powerless',     'biographical',   'trapped',     'national'),
    'U_4_glob': ('analytical',    'civilizational', 'analytical',  'global'),
    'org_nat':  ('organized',     'generational',   'constrained', 'national'),
    'U_3_nat':  ('institutional', 'immediate',      'arbitrage',   'national'),
}

# Reverse lookup: (P, T, E, S) -> label
CTX_TO_LABEL = {v: k for k, v in TEN_SLICE.items()}

# Canonical context labels (keys in orbit_data.json)
CANONICAL_LABELS = ['powerless', 'moderate', 'institutional', 'analytical']

# Piton is incomparable (extraction_rank = -1)
INCOMPARABLE_TYPES = {'scaffold', 'piton', 'naturalized', 'unknown'}

OUTPUT_PATHS = [
    OUTPUT_DIR / 'sheaf_audit_results.json',
    OUTPUT_DIR / 'sheaf_audit_results.md',
    Path('/mnt/user-data/outputs/sheaf_audit_results.json'),
    Path('/mnt/user-data/outputs/sheaf_audit_results.md'),
]


# ---------------------------------------------------------------------------
# H¹ and Nash (copied from game_theory_nash.py for self-containment)
# ---------------------------------------------------------------------------

def h1_from_vector(vec):
    """Count disagreeing context-pairs (H¹ proxy) for an N-element type vector."""
    count = 0
    n = len(vec)
    for i in range(n):
        for j in range(i + 1, n):
            if vec[i] != vec[j]:
                count += 1
    return count


def compute_nash_distance(vec, labels):
    """Structural Nash distance: minimum single-observer changes to reach H¹=0.

    Returns (distance, vulnerable_positions).
    distance=1: one position differs from majority (change it → H¹=0).
    distance=2: two positions differ (or 2-2 tie with 2 distinct types).
    distance=3+: min(unique_types, 3).
    Returns (None, []) if H¹=0 (already global section).
    """
    if len(set(vec)) == 1:
        return None, []
    type_counts = Counter(vec)
    majority_type, majority_count = type_counts.most_common(1)[0]
    minority_count = len(vec) - majority_count
    if minority_count == 1:
        vulnerable = [labels[i] for i, t in enumerate(vec) if t != majority_type]
        return 1, vulnerable
    elif minority_count == 2:
        return 2, []
    else:
        return min(len(type_counts), 3), []


# ---------------------------------------------------------------------------
# Deduplication sanity check
# ---------------------------------------------------------------------------

def deduplicate_classifications(classifications, ctx_to_label):
    """Collect one type per 10-slice context. Return (ctx_types, dupe_issues).

    ctx_types: {label: type_str}
    dupe_issues: list of (constraint_id placeholder, label, types) for disagreeing dupes
    """
    seen = {}        # label -> type
    dupe_issues = []
    for cl in classifications:
        ctx = cl.get('context', {})
        key = (
            ctx.get('agent_power'),
            ctx.get('time_horizon'),
            ctx.get('exit_options'),
            ctx.get('spatial_scope'),
        )
        if key not in ctx_to_label:
            continue
        label = ctx_to_label[key]
        ctype = cl.get('type', 'unknown')
        if label not in seen:
            seen[label] = ctype
        elif seen[label] != ctype:
            dupe_issues.append((label, seen[label], ctype))
    return seen, dupe_issues


# ---------------------------------------------------------------------------
# Main analysis
# ---------------------------------------------------------------------------

def main():
    pipeline_data = load_json(PIPELINE_JSON, 'pipeline_output')
    orbit_data = load_json(ORBIT_JSON, 'orbit_data')

    if not pipeline_data or not orbit_data:
        print('ERROR: Could not load required data files.', file=sys.stderr)
        sys.exit(1)

    constraints = pipeline_data.get('per_constraint', [])
    print(f'[sheaf_audit] Loaded {len(constraints)} constraints from pipeline_output.json')
    print(f'[sheaf_audit] Loaded {len(orbit_data)} constraints from orbit_data.json')

    # --- Deduplication sanity check -----------------------------------------
    total_dupe_conflicts = 0
    constraints_with_conflicts = []

    per_constraint_results = []

    # --- Per-constraint processing -------------------------------------------
    n_excluded_zero = 0
    n_excluded_one = 0
    n_in_working_set = 0

    for c in constraints:
        cid = c['id']
        canonical_h1 = c.get('h1_band', 0)
        canonical_arakelov = c.get('arakelov_height', None)
        signature = c.get('signature', '')
        claimed_type = c.get('claimed_type', '')

        # Collect 10-slice type vector
        classifications = c.get('classifications', [])
        ctx_types, dupe_issues = deduplicate_classifications(classifications, CTX_TO_LABEL)

        if dupe_issues:
            total_dupe_conflicts += len(dupe_issues)
            constraints_with_conflicts.append({
                'id': cid,
                'conflicts': dupe_issues
            })

        n_ten = len(ctx_types)
        if n_ten == 0:
            n_excluded_zero += 1
            continue
        if n_ten == 1:
            n_excluded_one += 1
            continue

        n_in_working_set += 1

        # Type vector and labels (sorted for stable ordering)
        sorted_labels = sorted(ctx_types.keys())
        vec = [ctx_types[l] for l in sorted_labels]

        # 10-slice H¹
        h1_ten = h1_from_vector(vec)

        # Binary classification
        canonical_sheaf = (canonical_h1 == 0)
        ten_slice_sheaf = (h1_ten == 0)
        preserved = (canonical_sheaf == ten_slice_sheaf)

        crossing_type = 'preserved'
        if canonical_sheaf and not ten_slice_sheaf:
            crossing_type = 'sheaf_to_presheaf'
        elif not canonical_sheaf and ten_slice_sheaf:
            crossing_type = 'presheaf_to_sheaf'

        # Canonical Nash from orbit_data.json
        canon_entry = orbit_data.get(cid, {})
        canon_contexts = canon_entry.get('contexts', {})
        canon_vec = [canon_contexts.get(l, 'unknown') for l in CANONICAL_LABELS]
        if 'unknown' not in canon_vec and len(set(canon_vec)) > 1:
            canon_nash, canon_vuln = compute_nash_distance(canon_vec, CANONICAL_LABELS)
        else:
            canon_nash, canon_vuln = (None, []) if len(set(canon_vec)) == 1 else (None, [])

        # 10-slice Nash
        if h1_ten > 0:
            ten_nash, ten_vuln = compute_nash_distance(vec, sorted_labels)
        else:
            ten_nash, ten_vuln = None, []

        # Which slices drive disagreement (for crossings)
        driving_slices = []
        if crossing_type == 'sheaf_to_presheaf' and h1_ten > 0:
            # Majority type in 10-slice vector
            type_count = Counter(vec)
            majority_type = type_count.most_common(1)[0][0]
            driving_slices = [l for l in sorted_labels if ctx_types[l] != majority_type]

        per_constraint_results.append({
            'id': cid,
            'canonical_h1': canonical_h1,
            'ten_slice_h1': h1_ten,
            'canonical_nash': canon_nash,
            'ten_slice_nash': ten_nash,
            'canonical_arakelov': canonical_arakelov,
            'ten_slice_arakelov': None,  # deferred: MaxEnt not available for 10-slice contexts
            'n_ten_slice_contexts': n_ten,
            'ten_slice_context_labels': sorted_labels,
            'ten_slice_types': ctx_types,
            'canonical_sheaf': canonical_sheaf,
            'ten_slice_sheaf': ten_slice_sheaf,
            'crossing_type': crossing_type,
            'driving_slices': driving_slices,
            'signature': signature,
            'claimed_type': claimed_type,
        })

    print(f'[sheaf_audit] Excluded: {n_excluded_zero} with 0 contexts, {n_excluded_one} with 1 context')
    print(f'[sheaf_audit] Working set: {n_in_working_set} constraints (≥2 10-slice contexts)')
    print(f'[sheaf_audit] Deduplication conflicts: {total_dupe_conflicts} type disagreements '
          f'across {len(constraints_with_conflicts)} constraints')

    # --- Aggregate stats ------------------------------------------------------
    n_canonical_sheaf = sum(1 for r in per_constraint_results if r['canonical_sheaf'])
    n_ten_slice_sheaf = sum(1 for r in per_constraint_results if r['ten_slice_sheaf'])
    n_preserved = sum(1 for r in per_constraint_results if r['crossing_type'] == 'preserved')
    n_s2p = sum(1 for r in per_constraint_results if r['crossing_type'] == 'sheaf_to_presheaf')
    n_p2s = sum(1 for r in per_constraint_results if r['crossing_type'] == 'presheaf_to_sheaf')
    n_total = len(per_constraint_results)

    crossing_rate = (n_s2p + n_p2s) / n_total if n_total else 0.0

    # Stratified by n_contexts
    strata = defaultdict(lambda: {'n': 0, 'canonical_sheaf': 0, 'ten_slice_sheaf': 0,
                                   's2p': 0, 'p2s': 0})
    for r in per_constraint_results:
        k = r['n_ten_slice_contexts']
        strata[k]['n'] += 1
        if r['canonical_sheaf']:
            strata[k]['canonical_sheaf'] += 1
        if r['ten_slice_sheaf']:
            strata[k]['ten_slice_sheaf'] += 1
        if r['crossing_type'] == 'sheaf_to_presheaf':
            strata[k]['s2p'] += 1
        if r['crossing_type'] == 'presheaf_to_sheaf':
            strata[k]['p2s'] += 1

    # Crossing characterization
    s2p_entries = [r for r in per_constraint_results if r['crossing_type'] == 'sheaf_to_presheaf']
    p2s_entries = [r for r in per_constraint_results if r['crossing_type'] == 'presheaf_to_sheaf']

    # Which slices drive sheaf→presheaf crossings
    driving_slice_counts = Counter()
    for r in s2p_entries:
        for sl in r['driving_slices']:
            driving_slice_counts[sl] += 1

    # Signature distribution in crossings
    s2p_sigs = Counter(r['signature'] for r in s2p_entries)
    p2s_sigs = Counter(r['signature'] for r in p2s_entries)

    # Claimed type distribution in crossings
    s2p_types = Counter(r['claimed_type'] for r in s2p_entries)
    p2s_types = Counter(r['claimed_type'] for r in p2s_entries)

    # Canonical arakelov for crossings
    s2p_arakelov = [r['canonical_arakelov'] for r in s2p_entries if r['canonical_arakelov'] is not None]

    # Nash analysis: 267 canonical Nash-distance-1 constraints
    # Identify from orbit_data: constraints with 3:1 majority:minority split in canonical vector
    canon_nash1_ids = set()
    for cid, entry in orbit_data.items():
        ctxs = entry.get('contexts', {})
        vec4 = [ctxs.get(l, 'unknown') for l in CANONICAL_LABELS]
        if 'unknown' in vec4:
            continue
        type_count = Counter(vec4)
        if len(type_count) < 2:
            continue
        majority_count = type_count.most_common(1)[0][1]
        minority_count = 4 - majority_count
        if minority_count == 1:
            canon_nash1_ids.add(cid)

    # Of those, how many become Nash-stable on 10-slice?
    nash1_in_working_set = [r for r in per_constraint_results if r['id'] in canon_nash1_ids]
    nash1_ten_stable = [r for r in nash1_in_working_set
                        if r['ten_slice_nash'] is not None and r['ten_slice_nash'] >= 2]
    nash1_ten_distance1 = [r for r in nash1_in_working_set
                           if r['ten_slice_nash'] == 1]
    nash1_ten_sheaf = [r for r in nash1_in_working_set if r['ten_slice_sheaf']]

    # --- Build JSON output ---------------------------------------------------
    results_json = {
        'corpus_hash': compute_corpus_hash(Path(__file__).resolve().parents[2] / "prolog" / "testsets"),
        'generated': datetime.now().isoformat(),
        'corpus_total': len(constraints),
        'excluded_zero_contexts': n_excluded_zero,
        'excluded_one_context': n_excluded_one,
        'working_set_size': n_total,
        'deduplication': {
            'total_conflicts': total_dupe_conflicts,
            'constraints_with_conflicts': len(constraints_with_conflicts),
            'conflict_details': constraints_with_conflicts[:20],  # first 20
        },
        'binary_preservation': {
            'n_canonical_sheaf': n_canonical_sheaf,
            'n_ten_slice_sheaf': n_ten_slice_sheaf,
            'n_preserved': n_preserved,
            'n_sheaf_to_presheaf': n_s2p,
            'n_presheaf_to_sheaf': n_p2s,
            'crossing_rate': round(crossing_rate, 6),
            'preservation_rate': round(n_preserved / n_total, 6) if n_total else 0.0,
        },
        'stratified_by_n_contexts': {
            str(k): {
                'n': v['n'],
                'canonical_sheaf': v['canonical_sheaf'],
                'ten_slice_sheaf': v['ten_slice_sheaf'],
                'sheaf_to_presheaf': v['s2p'],
                'presheaf_to_sheaf': v['p2s'],
                'crossing_rate': round((v['s2p'] + v['p2s']) / v['n'], 4) if v['n'] else 0.0,
            }
            for k, v in sorted(strata.items())
        },
        'crossing_characterization': {
            'sheaf_to_presheaf': {
                'count': n_s2p,
                'driving_slices': dict(driving_slice_counts.most_common()),
                'signature_distribution': dict(s2p_sigs.most_common(10)),
                'claimed_type_distribution': dict(s2p_types.most_common()),
                'canonical_arakelov_mean': (sum(s2p_arakelov) / len(s2p_arakelov)
                                            if s2p_arakelov else None),
                'u3civ_driven_count': driving_slice_counts.get('U_3_civ', 0),
                'organized_driven_count': (driving_slice_counts.get('organized', 0)
                                           + driving_slice_counts.get('org_nat', 0)),
            },
            'presheaf_to_sheaf': {
                'count': n_p2s,
                'signature_distribution': dict(p2s_sigs.most_common(10)),
                'claimed_type_distribution': dict(p2s_types.most_common()),
            },
        },
        'nash_analysis': {
            'canonical_nash1_total': len(canon_nash1_ids),
            'canonical_nash1_in_working_set': len(nash1_in_working_set),
            'canonical_nash1_ten_stable_ge2': len(nash1_ten_stable),
            'canonical_nash1_ten_still_1': len(nash1_ten_distance1),
            'canonical_nash1_ten_sheaf': len(nash1_ten_sheaf),
            'stability_rate': (round(len(nash1_ten_stable) / len(nash1_in_working_set), 4)
                               if nash1_in_working_set else None),
        },
        'arakelov_note': (
            'ten_slice_arakelov is null for all constraints. Arakelov height computation '
            'requires MaxEnt distributions (maxent_distribution_raw/3) at each observer '
            'context. MaxEnt is populated at pipeline-time via site_contexts/1; the '
            '10-slice contexts are not in the current pipeline run. Re-running requires '
            'adding site_contexts_ten_slice/1 to constraint_indexing.pl and a full '
            'pipeline re-run. Deferred to a subsequent pass.'
        ),
        'per_constraint': per_constraint_results,
    }

    # --- Write JSON ----------------------------------------------------------
    for path in OUTPUT_PATHS:
        if path.suffix == '.json':
            try:
                path.parent.mkdir(parents=True, exist_ok=True)
                with open(path, 'w') as f:
                    json.dump(results_json, f, indent=2)
                print(f'[sheaf_audit] Wrote {path}')
            except Exception as e:
                print(f'[sheaf_audit] Could not write {path}: {e}', file=sys.stderr)

    # --- Build markdown output -----------------------------------------------
    md = build_markdown(results_json, strata, driving_slice_counts, s2p_entries, p2s_entries,
                        nash1_in_working_set, nash1_ten_stable, canon_nash1_ids)

    for path in OUTPUT_PATHS:
        if path.suffix == '.md':
            try:
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text(md)
                print(f'[sheaf_audit] Wrote {path}')
            except Exception as e:
                print(f'[sheaf_audit] Could not write {path}: {e}', file=sys.stderr)

    # --- Console summary ------------------------------------------------------
    print()
    print('=== SHEAF AUDIT SUMMARY ===')
    print(f'Working set: {n_total} constraints (≥2 10-slice contexts)')
    print(f'Canonical sheaf (H¹=0): {n_canonical_sheaf} ({100*n_canonical_sheaf/n_total:.1f}%)')
    print(f'10-slice sheaf (H¹=0):  {n_ten_slice_sheaf} ({100*n_ten_slice_sheaf/n_total:.1f}%)')
    print(f'Binary preserved:       {n_preserved} ({100*n_preserved/n_total:.1f}%)')
    print(f'Sheaf → presheaf:       {n_s2p} ({100*n_s2p/n_total:.2f}%)')
    print(f'Presheaf → sheaf:       {n_p2s} ({100*n_p2s/n_total:.2f}%)')
    print(f'Total crossing rate:    {100*crossing_rate:.2f}%')
    print()
    print('Stratified by n_contexts:')
    for k in sorted(strata.keys()):
        v = strata[k]
        cr = (v['s2p'] + v['p2s']) / v['n'] if v['n'] else 0.0
        print(f'  n={k}: {v["n"]} constraints, crossing rate {100*cr:.2f}% (s2p={v["s2p"]}, p2s={v["p2s"]})')
    print()
    print(f'Top driving slices for sheaf→presheaf crossings:')
    for sl, cnt in driving_slice_counts.most_common(5):
        print(f'  {sl}: {cnt}')
    print()
    print(f'Nash-distance-1 canonical: {len(canon_nash1_ids)} total')
    print(f'  In working set:           {len(nash1_in_working_set)}')
    print(f'  Become Nash-stable (≥2):  {len(nash1_ten_stable)}')
    print(f'  Remain distance-1:        {len(nash1_ten_distance1)}')


def build_markdown(results, strata, driving_slice_counts, s2p_entries, p2s_entries,
                   nash1_in_working_set, nash1_ten_stable, canon_nash1_ids):
    bp = results['binary_preservation']
    n_total = results['working_set_size']
    n_s2p = bp['n_sheaf_to_presheaf']
    n_p2s = bp['n_presheaf_to_sheaf']
    crossing_rate = bp['crossing_rate']
    nash = results['nash_analysis']

    # Verdict
    if crossing_rate == 0.0:
        verdict = 'PRESERVED (zero crossings)'
    elif crossing_rate < 0.05:
        verdict = f'MOSTLY PRESERVED ({100*crossing_rate:.2f}% crossing rate)'
    elif crossing_rate < 0.15:
        verdict = f'PARTIAL PRESERVATION ({100*crossing_rate:.2f}% crossing rate)'
    else:
        verdict = f'NOT PRESERVED ({100*crossing_rate:.2f}% crossing rate)'

    # U_3_civ attribution
    u3civ_count = results['crossing_characterization']['sheaf_to_presheaf']['u3civ_driven_count']
    u3civ_fraction = u3civ_count / n_s2p if n_s2p > 0 else 0.0

    md = f"""# Sheaf/Presheaf Binary Boundary Audit: 10-Slice Tier-1 Family

Generated: {results['generated']}

## Verdict

**{verdict}**

Binary H¹ = 0 vs H¹ > 0 classification on the 10-slice Tier-1 working family
(evaluated at each constraint's testset-covered subset of the 10 contexts).

Primary framing: this audit tests whether the binary classification survives
*in testset-covered 10-slice positions*, not on the full 10-slice family.
Each constraint is evaluated at 2–7 of the 10 slices depending on testset coverage.
Stratified analysis by n_contexts is the primary diagnostic.

---

## Coverage

| Category | Count |
|---|---|
| Corpus total | {results['corpus_total']} |
| Excluded (0 contexts) | {results['excluded_zero_contexts']} |
| Excluded (1 context, no H¹ possible) | {results['excluded_one_context']} |
| **Working set (≥2 contexts)** | **{n_total}** |

**Deduplication sanity check:** {results['deduplication']['total_conflicts']} type
disagreements found across {results['deduplication']['constraints_with_conflicts']} constraints
with duplicate classification entries for the same context.
{"No conflicts — all duplicates agree on type. Deduplication assumption confirmed." if results['deduplication']['total_conflicts'] == 0 else "Conflicts present — see per_constraint data; first-occurrence rule applied."}

---

## Binary Preservation Results

| Metric | Count | Rate |
|---|---|---|
| Canonical sheaf (H¹=0) | {bp['n_canonical_sheaf']} | {100*bp['n_canonical_sheaf']/n_total:.1f}% |
| 10-slice sheaf (H¹=0) | {bp['n_ten_slice_sheaf']} | {100*bp['n_ten_slice_sheaf']/n_total:.1f}% |
| Binary preserved | {bp['n_preserved']} | {100*bp['preservation_rate']:.2f}% |
| Sheaf → presheaf crossings | {n_s2p} | {100*n_s2p/n_total:.2f}% |
| Presheaf → sheaf crossings | {n_p2s} | {100*n_p2s/n_total:.2f}% |
| **Total crossing rate** | **{n_s2p + n_p2s}** | **{100*crossing_rate:.2f}%** |

---

## Stratified Analysis by n_contexts

| n contexts | N | Canonical sheaf | 10-slice sheaf | Sheaf→presheaf | Presheaf→sheaf | Crossing rate |
|---|---|---|---|---|---|---|
"""
    for k in sorted(strata.keys()):
        v = strata[k]
        cr = (v['s2p'] + v['p2s']) / v['n'] if v['n'] else 0.0
        md += (f"| {k} | {v['n']} | {v['canonical_sheaf']} | {v['ten_slice_sheaf']} "
               f"| {v['s2p']} | {v['p2s']} | {100*cr:.2f}% |\n")

    cc = results['crossing_characterization']
    s2p_cc = cc['sheaf_to_presheaf']
    p2s_cc = cc['presheaf_to_sheaf']

    md += f"""
**Interpretation**: {"If crossing rate increases with n_contexts, the test under-detects crossings at low coverage. If rate is flat, coverage depth does not bias the result." if n_s2p + n_p2s > 0 else "Zero crossings across all n_context strata — the variable subsite does not affect the finding."}

---

## Crossing Characterization

### Sheaf → Presheaf ({n_s2p} crossings)

These constraints have canonical H¹ = 0 (global section on 4-point site) but H¹ > 0
on the 10-slice subsite (local sections fail to glue).

**Driving slices** (which slice's type disagrees with the majority in the 10-slice orbit):

| Slice | Driven crossings |
|---|---|
"""
    for sl, cnt in driving_slice_counts.most_common():
        md += f"| {sl} | {cnt} |\n"

    if n_s2p > 0:
        u3civ_note = (
            f"U_3_civ drives {u3civ_fraction*100:.1f}% of sheaf→presheaf crossings. "
            "This is the predicted piton-pattern: institutional/civilizational/arbitrage "
            "produces piton for constraints that canonical site classifies as rope. The "
            "piton gate (theater_ratio check) fires at civilizational time horizon; the "
            "canonical U3 (generational time) does not reach this gate."
            if u3civ_fraction >= 0.5
            else (
                f"U_3_civ drives {u3civ_fraction*100:.1f}% of sheaf→presheaf crossings "
                "(less than the predicted majority). Crossings are distributed across "
                "multiple slices — see table above for full distribution."
            )
        )
        md += f"\n**U_3_civ attribution**: {u3civ_note}\n"

        if s2p_cc['organized_driven_count'] > 0:
            md += (
                f"\n**Organized-driven crossings**: "
                f"{s2p_cc['organized_driven_count']} crossings driven by organized/org_nat slices. "
                "Two framings apply: (1) apparatus instability at the organized power atom, "
                "which lacks full calibration in the product-site design; "
                "(2) the product site's conservative exclusion of organized was motivated "
                "by genuine calibration uncertainty — these crossings are expected under "
                "that framing, not a contradiction of site-stability.\n"
            )

    md += f"""
**Signature distribution (sheaf→presheaf)**:

| Signature | Count |
|---|---|
"""
    for sig, cnt in sorted(s2p_cc['signature_distribution'].items(), key=lambda x: -x[1]):
        md += f"| {sig} | {cnt} |\n"

    md += f"""
**Claimed-type distribution (sheaf→presheaf)**:

| Claimed type | Count |
|---|---|
"""
    for t, cnt in sorted(s2p_cc['claimed_type_distribution'].items(), key=lambda x: -x[1]):
        md += f"| {t} | {cnt} |\n"

    md += f"""
### Presheaf → Sheaf ({n_p2s} crossings)

Constraints with canonical H¹ > 0 but 10-slice H¹ = 0. These constraints disagree
across canonical contexts but happen to agree on the specific 10-slice contexts they
appear at. Most likely: constraints classified differently at canonical contexts but
consistently at the 10-slice positions their testsets happen to cover.

**Claimed-type distribution (presheaf→sheaf)**:

| Claimed type | Count |
|---|---|
"""
    for t, cnt in sorted(p2s_cc['claimed_type_distribution'].items(), key=lambda x: -x[1]):
        md += f"| {t} | {cnt} |\n"

    md += f"""
---

## Nash Distance Analysis

| Metric | Value |
|---|---|
| Canonical Nash-distance-1 constraints | {nash['canonical_nash1_total']} |
| In working set (have ≥2 10-slice contexts) | {nash['canonical_nash1_in_working_set']} |
| Become Nash-stable on 10-slice (distance ≥ 2) | {nash['canonical_nash1_ten_stable_ge2']} |
| Remain Nash-distance-1 on 10-slice | {nash['canonical_nash1_ten_still_1']} |
| Already have 10-slice H¹ = 0 (sheaf) | {nash['canonical_nash1_ten_sheaf']} |
| Stability rate (among those in working set) | {f"{100*nash['stability_rate']:.1f}%" if nash['stability_rate'] is not None else "N/A"} |

**Interpretation**: v6.11 reports that all 267 canonical Nash-distance-1 constraints became
Nash-stable (distance ≥ 2) on the 156-point product site because the institutional position
occupies a 48-context block. The 10-slice family is not the product site — it has 1–3
institutional contexts (U_3_imm, U_3_civ, U_3_nat). Nash-stability on the 10-slice site
tests whether distance-1 constraints remain vulnerable when institutional appears at multiple
time-horizon variants rather than as a single block.

---

## Arakelov Fragility

**Deferred.** Arakelov height computation requires MaxEnt distributions
(`maxent_distribution_raw/3`) at each observer context. MaxEnt is populated at pipeline-time
for `site_contexts/1`'s current contexts; the 10-slice contexts are not in the current pipeline
run. Re-running requires:
1. Adding `site_contexts_ten_slice/1` to `constraint_indexing.pl`
2. Re-running MaxEnt for those contexts
3. Re-running the full pipeline

The per-constraint `canonical_arakelov` values (from `pipeline_output.json`) are recorded
in the JSON output but 10-slice Arakelov values are null. The fragile/genuine sub-partition
question (Q3 from the prompt) remains open.

---

## §7 Reconciliation Pointer Status

The second reconciliation pointer from `coupling_structure_evidence.md` §7:
*"A binary-boundary audit on the 10-slice family — recomputing H¹ at each slice and
checking whether the binary classification is preserved — is the test that would let
audit results bear on the framework's primary claim."*

**This audit closes the binary H¹ question (partially).** The test was run on
{n_total} constraints with ≥2 testset-covered 10-slice contexts. The framing caveat:
results describe behavior *in testset-covered 10-slice positions*, not on the 10-slice
family as a fixed site. The variable per-constraint subsite (2–7 contexts) limits
what can be inferred about the full 10-slice family.

**The Arakelov sub-question (Q3) remains open.** MaxEnt re-run required.

---

## Methodological Self-Report

**What this evidence supports:**
- Binary H¹ preservation rate in testset-covered 10-slice positions: {100*bp['preservation_rate']:.2f}%
- Crossing rate: {100*crossing_rate:.2f}% ({n_s2p} sheaf→presheaf, {n_p2s} presheaf→sheaf)
- Stratified preservation rates by n_contexts (table above)
- Nash-distance behavior for {nash['canonical_nash1_in_working_set']} constraints formerly Nash-distance-1 on canonical site

**What this evidence does not support:**
- A claim that the binary boundary is preserved on the 10-slice family as a fixed site (no constraint is classified at all 10 slices; max coverage is 7)
- A direct comparison to the 4→156 product-site expansion (different test: that fixed all contexts, this uses per-constraint variable subsites)
- Arakelov fragility claims on the 10-slice site (MaxEnt not available)
- A universal site-stability claim extending to all fine site expansions

**U_3_civ as predicted crossing source:** {"Confirmed — " + str(u3civ_count) + " of " + str(n_s2p) + " sheaf→presheaf crossings driven by U_3_civ (" + f"{100*u3civ_fraction:.0f}%)" if n_s2p > 0 and u3civ_fraction >= 0.3 else "Not the dominant source — crossings distributed differently than predicted" if n_s2p > 0 else "No sheaf→presheaf crossings to attribute"}

**Canonical site drift:** Only U_4_glob (analytical/civilizational/analytical/global) matches
a canonical context. Canonical U3 (institutional/generational/arbitrage/national) is not in
the 10-slice family; its nearest analogs are U_3_imm (immediate time) and U_3_civ
(civilizational time). Results on the 10-slice family describe a site adjacent to but distinct
from the canonical site.
"""
    return md


if __name__ == '__main__':
    main()
