#!/usr/bin/env python3
"""
Enrichment Target Identification Script

Identifies the corpus-static constraints that need structural predicate
enrichment for dynamic classification. Diffs the fingerprint-sourced
constraints (explicitly listed in fingerprint_report.md) against the full
corpus (corpus_data.json) to produce the exact file list.

For each target, audits which structural predicates are present/missing.

Usage: python3 python/enrichment_targets.py
"""

import json
import re
import os
from collections import defaultdict, Counter
from pathlib import Path


def parse_fingerprint_listed_ids(fingerprint_path):
    """Extract constraint IDs explicitly listed as bullet points in the fingerprint report.

    These are the ~194 'fingerprint-sourced' constraints. The truncated
    '...and N more' entries are NOT captured — those are the corpus-static targets.
    """
    listed_ids = set()
    current_pattern = None
    with open(fingerprint_path) as f:
        for line in f:
            m = re.match(r'### `shift\((\w+), (\w+), (\w+), (\w+)\)`', line)
            if m:
                current_pattern = {
                    'powerless': m.group(1),
                    'moderate': m.group(2),
                    'institutional': m.group(3),
                    'analytical': m.group(4),
                }
                continue
            if current_pattern and line.startswith('- `'):
                cid = line.strip().lstrip('- `').rstrip('`')
                listed_ids.add(cid)
    return listed_ids


def load_corpus_ids(corpus_path):
    """Load all constraint IDs from corpus_data.json."""
    with open(corpus_path) as f:
        data = json.load(f)
    return set(data['constraints'].keys()), data['constraints']


def find_testset_file(constraint_id, testsets_dir):
    """Find the .pl file for a constraint ID, handling case differences."""
    # Direct match first
    direct = testsets_dir / f'{constraint_id}.pl'
    if direct.exists():
        return direct

    # Case-insensitive fallback
    lower_id = constraint_id.lower()
    for f in testsets_dir.iterdir():
        if f.suffix == '.pl' and f.stem.lower() == lower_id:
            return f

    return None


def audit_file_predicates(filepath, constraint_id):
    """Audit a testset file for presence/absence of required structural predicates.

    Returns a dict of predicate categories -> bool (present or not).
    """
    content = filepath.read_text()
    cid = constraint_id
    # Also check lowercase version for matching
    cid_lower = cid.lower()

    def has_pred(pattern):
        """Check if a pattern matches anywhere in the content."""
        return bool(re.search(pattern, content))

    # Use flexible matching that handles both the exact ID and variations
    audit = {}

    # 1A. Domain priors (raw values)
    audit['domain_priors:base_extractiveness'] = has_pred(r'domain_priors:base_extractiveness\(')
    audit['domain_priors:suppression_score'] = has_pred(r'domain_priors:suppression_score\(')
    audit['domain_priors:theater_ratio'] = has_pred(r'domain_priors:theater_ratio\(')

    # 1A. Constraint metrics (engine keys)
    audit['constraint_metric:extractiveness'] = has_pred(r'constraint_metric\([^,]+,\s*extractiveness')
    audit['constraint_metric:suppression'] = has_pred(r'constraint_metric\([^,]+,\s*suppression_requirement')
    audit['constraint_metric:theater'] = has_pred(r'constraint_metric\([^,]+,\s*theater_ratio')

    # 1B. Constraint claim
    audit['constraint_claim'] = has_pred(r'constraint_claim\(')

    # 1C. Structural property flags
    audit['requires_active_enforcement'] = has_pred(r'requires_active_enforcement\(')
    audit['has_sunset_clause'] = has_pred(r'has_sunset_clause\(')
    audit['constraint_beneficiary'] = has_pred(r'constraint_beneficiary\(')
    audit['constraint_victim'] = has_pred(r'constraint_victim\(')

    # 1D. Indexed classifications
    classifications = re.findall(r'constraint_classification\(', content)
    audit['constraint_classification_count'] = len(classifications)
    audit['has_powerless_perspective'] = has_pred(r'agent_power\(powerless\)')
    audit['has_institutional_perspective'] = has_pred(r'agent_power\(institutional\)')
    audit['has_analytical_perspective'] = has_pred(r'agent_power\(analytical\)')

    # 1E. Integration hook
    audit['interval'] = has_pred(r'interval\(')

    # 1F. Omega variable
    audit['omega_variable'] = has_pred(r'omega_variable\(')

    # 1G. Temporal measurements
    measurements = re.findall(r'measurement\(', content)
    audit['measurement_count'] = len(measurements)
    audit['has_theater_measurements'] = has_pred(r'measurement\([^,]+,\s*[^,]+,\s*theater_ratio')
    audit['has_extraction_measurements'] = has_pred(r'measurement\([^,]+,\s*[^,]+,\s*base_extractiveness')

    # 1H. Coordination type
    audit['coordination_type'] = has_pred(r'coordination_type\(')

    # 1I. Multifile declarations
    audit['has_multifile_block'] = has_pred(r':- multifile')

    # Extract metric values for analysis
    m_ext = re.search(r'base_extractiveness\([^,]+,\s*([\d.]+)\)', content)
    audit['extractiveness_value'] = float(m_ext.group(1)) if m_ext else None

    m_sup = re.search(r'suppression_score\([^,]+,\s*([\d.]+)\)', content)
    audit['suppression_value'] = float(m_sup.group(1)) if m_sup else None

    m_tr = re.search(r'theater_ratio\([^,]+,\s*([\d.]+)\)', content)
    audit['theater_ratio_value'] = float(m_tr.group(1)) if m_tr else None

    # Extract claimed type
    m_claim = re.search(r'constraint_claim\([^,]+,\s*(\w+)\)', content)
    audit['claimed_type'] = m_claim.group(1) if m_claim else None

    return audit


def categorize_gaps(audit):
    """Categorize what's missing from a file into enrichment need levels."""
    gaps = []

    # Missing metrics entirely
    has_any_metrics = (audit['domain_priors:base_extractiveness'] or
                       audit['constraint_metric:extractiveness'])
    if not has_any_metrics:
        gaps.append('NO_METRICS')
    else:
        # Check for partial metrics
        if not audit['domain_priors:theater_ratio'] and not audit['constraint_metric:theater']:
            gaps.append('MISSING_THEATER_RATIO')
        if not audit['constraint_metric:extractiveness']:
            gaps.append('MISSING_CONSTRAINT_METRIC_EXT')
        if not audit['constraint_metric:suppression']:
            gaps.append('MISSING_CONSTRAINT_METRIC_SUP')
        if not audit['constraint_metric:theater']:
            gaps.append('MISSING_CONSTRAINT_METRIC_TR')

    # Missing constraint claim
    if not audit['constraint_claim']:
        gaps.append('MISSING_CLAIM')

    # Missing structural flags
    if not audit['constraint_beneficiary']:
        gaps.append('MISSING_BENEFICIARY')
    if not audit['constraint_victim']:
        gaps.append('MISSING_VICTIM')

    # Enforcement flag: only required for tangled_rope claims.
    # Mountains, ropes, scaffolds, pitons, and snares do not structurally
    # require active enforcement. Only flag if the claimed type is tangled_rope
    # or if ε is high enough that the constraint could be tangled_rope.
    claimed = audit.get('claimed_type')
    eps = audit.get('extractiveness_value')
    if not audit['requires_active_enforcement']:
        if claimed == 'tangled_rope':
            gaps.append('MISSING_ENFORCEMENT_FLAG')
        elif claimed is None and eps is not None and eps >= 0.30:
            # No claim + moderate ε = might need enforcement for tangled_rope gate
            gaps.append('MISSING_ENFORCEMENT_FLAG')

    # Missing classifications
    if audit['constraint_classification_count'] < 2:
        gaps.append('INSUFFICIENT_CLASSIFICATIONS')
    if not audit['has_analytical_perspective']:
        gaps.append('MISSING_ANALYTICAL_PERSPECTIVE')
    if not audit['has_powerless_perspective']:
        gaps.append('MISSING_POWERLESS_PERSPECTIVE')

    # Missing integration hook
    if not audit['interval']:
        gaps.append('MISSING_INTERVAL')

    # Missing omega variable
    if not audit['omega_variable']:
        gaps.append('MISSING_OMEGA')

    # Missing temporal measurements (conditional: only if ε > 0.46)
    eps = audit.get('extractiveness_value')
    if eps is not None and eps > 0.46:
        if audit['measurement_count'] < 6:
            gaps.append('MISSING_TEMPORAL_MEASUREMENTS')

    return gaps


def main():
    base = Path(__file__).resolve().parent.parent
    corpus_path = base / 'outputs' / 'corpus_data.json'
    fingerprint_path = base / 'outputs' / 'fingerprint_report.md'
    testsets_dir = base / 'prolog' / 'testsets'
    output_path = base / 'outputs' / 'enrichment_targets.md'
    output_json = base / 'outputs' / 'enrichment_targets.json'

    # Step 1: Get fingerprint-listed IDs
    fingerprint_ids = parse_fingerprint_listed_ids(fingerprint_path)
    print(f'Fingerprint-listed IDs: {len(fingerprint_ids)}')

    # Step 2: Get all corpus IDs
    all_ids, corpus_data = load_corpus_ids(corpus_path)
    print(f'Total corpus IDs: {len(all_ids)}')

    # Step 3: Compute corpus-static set
    # Need case-insensitive matching since fingerprint report may lowercase
    fingerprint_lower = {fid.lower() for fid in fingerprint_ids}

    corpus_static_ids = set()
    for cid in all_ids:
        if cid.lower() not in fingerprint_lower:
            corpus_static_ids.add(cid)

    print(f'Corpus-static IDs: {len(corpus_static_ids)}')

    # Step 4: Audit each corpus-static constraint
    results = []
    gap_counter = Counter()
    category_counts = Counter()

    for cid in sorted(corpus_static_ids):
        filepath = find_testset_file(cid, testsets_dir)
        if filepath is None:
            results.append({
                'id': cid,
                'file': None,
                'audit': None,
                'gaps': ['NO_FILE'],
                'category': 'no_file',
            })
            category_counts['no_file'] += 1
            gap_counter['NO_FILE'] += 1
            continue

        cdata = corpus_data.get(cid, {})
        domain = cdata.get('domain', 'unknown')

        audit = audit_file_predicates(filepath, cid)
        gaps = categorize_gaps(audit)

        # Determine category
        if not gaps:
            category = 'complete'
        elif 'NO_METRICS' in gaps:
            category = 'no_metrics'
        elif any(g.startswith('MISSING_CONSTRAINT_METRIC') for g in gaps) or 'MISSING_THEATER_RATIO' in gaps:
            category = 'partial_metrics'
        elif any(g in gaps for g in ['MISSING_BENEFICIARY', 'MISSING_VICTIM', 'MISSING_ENFORCEMENT_FLAG']):
            category = 'missing_structural_flags'
        elif any(g in gaps for g in ['INSUFFICIENT_CLASSIFICATIONS', 'MISSING_ANALYTICAL_PERSPECTIVE', 'MISSING_POWERLESS_PERSPECTIVE']):
            category = 'missing_classifications'
        elif 'MISSING_TEMPORAL_MEASUREMENTS' in gaps:
            category = 'missing_temporal'
        elif 'MISSING_CLAIM' in gaps:
            category = 'missing_claim'
        elif 'MISSING_OMEGA' in gaps:
            category = 'missing_omega'
        else:
            category = 'minor_gaps'

        results.append({
            'id': cid,
            'file': str(filepath),
            'domain': domain,
            'audit': audit,
            'gaps': gaps,
            'category': category,
            'extractiveness': audit.get('extractiveness_value'),
            'suppression': audit.get('suppression_value'),
            'theater_ratio': audit.get('theater_ratio_value'),
            'claimed_type': audit.get('claimed_type'),
        })

        category_counts[category] += 1
        for g in gaps:
            gap_counter[g] += 1

    # Step 5: Generate reports
    lines = []
    lines.append('# Enrichment Targets: Corpus-Static Constraints Needing Structural Predicates')
    lines.append('')
    lines.append(f'*Generated: enrichment target analysis*')
    lines.append('')
    lines.append('## Summary')
    lines.append('')
    lines.append(f'- **Fingerprint-sourced constraints**: {len(fingerprint_ids)}')
    lines.append(f'- **Total corpus constraints**: {len(all_ids)}')
    lines.append(f'- **Corpus-static (need enrichment audit)**: {len(corpus_static_ids)}')
    lines.append('')

    lines.append('## Category Breakdown')
    lines.append('')
    lines.append('| Category | Count | Description |')
    lines.append('|----------|-------|-------------|')
    cat_desc = {
        'complete': 'All required predicates present (still corpus-static due to fingerprint report truncation)',
        'no_metrics': 'Missing ALL metric predicates (base_extractiveness, suppression_score, theater_ratio)',
        'partial_metrics': 'Has some metrics but missing constraint_metric/3 or theater_ratio',
        'missing_structural_flags': 'Has metrics but missing beneficiary/victim/enforcement flags',
        'missing_classifications': 'Has metrics but fewer than 2 indexed classifications or missing key perspectives',
        'missing_temporal': 'Has metrics (ε > 0.46) but missing temporal measurement/5 data',
        'missing_claim': 'Missing constraint_claim/2',
        'missing_omega': 'Missing omega_variable/5',
        'minor_gaps': 'Only minor gaps (interval, omega, etc.)',
        'no_file': 'No testset .pl file found',
    }
    for cat in ['complete', 'no_metrics', 'partial_metrics', 'missing_structural_flags',
                'missing_classifications', 'missing_temporal', 'missing_claim',
                'missing_omega', 'minor_gaps', 'no_file']:
        if category_counts[cat] > 0:
            lines.append(f'| {cat} | {category_counts[cat]} | {cat_desc.get(cat, "")} |')
    lines.append('')

    lines.append('## Gap Frequency')
    lines.append('')
    lines.append('| Gap | Count |')
    lines.append('|-----|-------|')
    for gap, count in gap_counter.most_common():
        lines.append(f'| {gap} | {count} |')
    lines.append('')

    # Detailed listings by category
    for cat in ['no_metrics', 'partial_metrics', 'missing_structural_flags',
                'missing_classifications', 'missing_temporal', 'missing_claim',
                'missing_omega', 'minor_gaps', 'complete']:
        cat_results = [r for r in results if r['category'] == cat]
        if not cat_results:
            continue

        lines.append(f'## {cat.replace("_", " ").title()} ({len(cat_results)} constraints)')
        lines.append('')

        if cat == 'complete':
            lines.append('These constraints have all required predicates but are corpus-static due to')
            lines.append('fingerprint report truncation. They may need re-running through the engine.')
            lines.append('')

        lines.append('| ID | Domain | ε | Suppression | Theater | Claimed Type | Gaps |')
        lines.append('|----|--------|---|-------------|---------|--------------|------|')
        for r in cat_results[:50]:  # Limit to 50 per category
            eps = f'{r.get("extractiveness", "?"):.2f}' if r.get('extractiveness') is not None else '?'
            sup = f'{r.get("suppression", "?"):.2f}' if r.get('suppression') is not None else '?'
            tr = f'{r.get("theater_ratio", "?"):.2f}' if r.get('theater_ratio') is not None else '?'
            claimed = r.get('claimed_type', '?') or '?'
            gaps_str = ', '.join(r['gaps'][:3]) if r['gaps'] else 'none'
            lines.append(f'| {r["id"]} | {r.get("domain", "?")} | {eps} | {sup} | {tr} | {claimed} | {gaps_str} |')

        if len(cat_results) > 50:
            lines.append(f'| ... | ... | ... | ... | ... | ... | *{len(cat_results) - 50} more* |')
        lines.append('')

    # Representative picks for initial enrichment
    lines.append('## Recommended Initial Enrichment Targets (5 files)')
    lines.append('')
    lines.append('One from each category for quality review:')
    lines.append('')

    # 1. Low-ε constraint (mountain/rope)
    low_eps = [r for r in results if r.get('extractiveness') is not None
               and r['extractiveness'] <= 0.25 and r['gaps']]
    if low_eps:
        pick = sorted(low_eps, key=lambda x: len(x['gaps']), reverse=True)[0]
        lines.append(f'1. **Low-ε (mountain/rope)**: `{pick["id"]}` (ε={pick["extractiveness"]:.2f}, gaps: {", ".join(pick["gaps"])})')

    # 2. High-ε constraint (snare/tangled_rope)
    high_eps = [r for r in results if r.get('extractiveness') is not None
                and r['extractiveness'] >= 0.60 and r['gaps']]
    if high_eps:
        pick = sorted(high_eps, key=lambda x: len(x['gaps']), reverse=True)[0]
        lines.append(f'2. **High-ε (snare/tangled_rope)**: `{pick["id"]}` (ε={pick["extractiveness"]:.2f}, gaps: {", ".join(pick["gaps"])})')

    # 3. Missing metrics entirely
    no_metrics = [r for r in results if r['category'] == 'no_metrics']
    if no_metrics:
        pick = no_metrics[0]
        lines.append(f'3. **Missing metrics entirely**: `{pick["id"]}` (gaps: {", ".join(pick["gaps"])})')

    # 4. Has metrics but no structural flags
    no_flags = [r for r in results if r['category'] == 'missing_structural_flags']
    if no_flags:
        pick = no_flags[0]
        lines.append(f'4. **Has metrics, no structural flags**: `{pick["id"]}` (ε={pick.get("extractiveness", "?")}, gaps: {", ".join(pick["gaps"])})')

    # 5. Has structural flags but no indexed classifications
    no_class = [r for r in results if r['category'] == 'missing_classifications']
    if no_class:
        pick = no_class[0]
        lines.append(f'5. **Has flags, no classifications**: `{pick["id"]}` (gaps: {", ".join(pick["gaps"])})')

    lines.append('')

    # Write markdown report
    report = '\n'.join(lines)
    output_path.write_text(report)
    print(f'\nReport written to: {output_path}')

    # Write JSON for programmatic use
    json_data = {
        'summary': {
            'fingerprint_sourced': len(fingerprint_ids),
            'total_corpus': len(all_ids),
            'corpus_static': len(corpus_static_ids),
            'categories': dict(category_counts),
            'gap_frequency': dict(gap_counter),
        },
        'targets': [
            {
                'id': r['id'],
                'file': r['file'],
                'domain': r.get('domain'),
                'category': r['category'],
                'gaps': r['gaps'],
                'extractiveness': r.get('extractiveness'),
                'suppression': r.get('suppression'),
                'theater_ratio': r.get('theater_ratio'),
                'claimed_type': r.get('claimed_type'),
            }
            for r in results if r['category'] != 'complete'
        ],
        'complete_but_static': [r['id'] for r in results if r['category'] == 'complete'],
    }
    output_json.write_text(json.dumps(json_data, indent=2))
    print(f'JSON data written to: {output_json}')


if __name__ == '__main__':
    main()
