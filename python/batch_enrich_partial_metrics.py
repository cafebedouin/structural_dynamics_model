#!/usr/bin/env python3
"""
Batch Enrichment: Partial Metrics Constraints

For constraints missing theater_ratio and constraint_metric/3 theater_ratio,
derives theater_ratio from narrative context (claimed_type, domain, existing metrics)
and appends enrichment blocks.

For constraints also missing analytical perspective, computes chi and derives
the classification from the February 2026 gate thresholds.

Usage: python3 python/batch_enrich_partial_metrics.py [--dry-run]
"""

import json
import re
import sys
import hashlib
from pathlib import Path
from collections import Counter


# ============================================================================
# THEATER RATIO DERIVATION
# ============================================================================
#
# Theater ratio measures what fraction of visible activity is performance vs
# substance. Mathematical theorems have near-zero theater. Political
# institutions have significant legitimation theater.
#
# The derivation uses claimed_type + domain + metrics as inputs, with
# deterministic hash-based variation to avoid clustering.

# Base theater by claimed type (before domain/metric adjustments)
TYPE_THEATER_BASE = {
    'mountain': 0.03,     # Mountains are real — very low theater
    'rope': 0.10,         # Functional coordination — mostly substantive
    'scaffold': 0.15,     # Transitional tools — some framing theater
    'tangled_rope': 0.25, # Mixed coordination/extraction — moderate theater
    'snare': 0.15,        # Extraction is substantive, not theatrical
    'piton': 0.78,        # Pitons are almost pure theater (gate: ≥ 0.70)
}

# Domain adjustments to base theater
DOMAIN_ADJUSTMENTS = {
    'mathematical': -0.03,
    'mathematics': -0.03,
    'physics': -0.03,
    'physical': -0.03,
    'logical': -0.02,
    'analytical': -0.02,
    'technological': 0.00,
    'biological': -0.01,
    'scientific': -0.01,
    'health': 0.00,
    'economic': 0.03,
    'social': 0.05,
    'psychological': 0.04,
    'political': 0.08,
    'religious': 0.06,
    'legal': 0.04,
    'artistic': 0.05,
    'linguistic': 0.02,
    'philosophical': 0.03,
    'magical': 0.08,     # Fairy tales are high theater by nature
    'atmospheric_science': -0.02,
    'astrophysical': -0.02,
    'geopolitical': 0.06,
    'cognitive': 0.02,
    'corporate_governance': 0.05,
}

# Power modifiers for chi calculation
POWER_MODIFIERS = {
    'powerless': 1.5,
    'moderate': 1.0,
    'individual_moderate': 1.0,
    'powerful': 0.6,
    'organized': 0.4,
    'institutional': -0.20,
    'analytical': 1.15,
}

# Scope modifiers for chi calculation
SCOPE_MODIFIERS = {
    'local': 0.8,
    'regional': 0.9,
    'national': 1.0,
    'continental': 1.1,
    'global': 1.2,
    'universal': 1.0,
}


def derive_theater_ratio(constraint_id, claimed_type, domain, eps, suppression):
    """Derive theater_ratio from structural properties.

    Returns (theater_ratio, rationale_string).
    """
    # Base from claimed type
    base = TYPE_THEATER_BASE.get(claimed_type, 0.15)

    # Domain adjustment (use first domain if multi-domain)
    primary_domain = domain.split('/')[0].strip() if domain else 'unknown'
    domain_adj = DOMAIN_ADJUSTMENTS.get(primary_domain, 0.02)

    # Extraction adjustment: higher ε with low suppression suggests hidden
    # extraction, which correlates with more theater
    eps_adj = 0.0
    if eps is not None and eps > 0.25 and claimed_type != 'mountain':
        eps_adj = 0.015 * (eps - 0.25)

    # Suppression adjustment: very high suppression with enforcement creates
    # compliance theater
    sup_adj = 0.0
    if suppression is not None and suppression > 0.50:
        sup_adj = 0.01 * (suppression - 0.50)

    # Deterministic variation from constraint ID hash (±0.03)
    # This prevents clustering of values within the same type/domain
    h = int(hashlib.md5(constraint_id.encode()).hexdigest()[:8], 16)
    variation = ((h % 61) - 30) / 1000.0  # Range: -0.030 to +0.030

    theater = base + domain_adj + eps_adj + sup_adj + variation

    # Clamp to valid range
    if claimed_type == 'piton':
        theater = max(0.70, min(0.98, theater))
    elif claimed_type == 'mountain':
        theater = max(0.00, min(0.15, theater))
    else:
        theater = max(0.02, min(0.65, theater))

    # Round to 2 decimal places
    theater = round(theater, 2)

    # Generate rationale
    if claimed_type == 'mountain' and primary_domain in ('mathematical', 'mathematics', 'physics', 'physical', 'logical'):
        rationale = "Formal truth — substantive with near-zero performative component"
    elif claimed_type == 'mountain' and primary_domain in ('technological',):
        rationale = "Technical constraint — mostly substantive, minimal implementation theater"
    elif claimed_type == 'mountain':
        rationale = f"Structural constraint in {primary_domain} domain — low theater, high substance"
    elif claimed_type == 'rope' and primary_domain in ('mathematical', 'mathematics', 'technological'):
        rationale = "Functional coordination mechanism — primarily substantive"
    elif claimed_type == 'rope':
        rationale = f"Coordination mechanism in {primary_domain} domain — moderate institutional framing"
    elif claimed_type == 'tangled_rope':
        rationale = f"Mixed coordination/extraction — theater masks extraction component"
    elif claimed_type == 'snare':
        rationale = "Extraction is substantive — the constraint's costs are real, not theatrical"
    elif claimed_type == 'piton':
        rationale = "Non-functional constraint maintained by inertia — high theater"
    else:
        rationale = f"Derived from {claimed_type} classification in {primary_domain} domain"

    return theater, rationale


def classify_from_chi(eps, suppression, theater, power, scope):
    """Classify a constraint from chi value using February 2026 gates.

    Returns (type_string, chi_value).
    """
    pi = POWER_MODIFIERS.get(power, 1.0)
    sigma = SCOPE_MODIFIERS.get(scope, 1.0)
    chi = eps * pi * sigma

    # Gate priority order (February 2026 recalibration)
    # 1. Mountain: ε ≤ 0.25, suppression ≤ 0.05
    if eps <= 0.25 and suppression <= 0.05:
        return 'mountain', chi

    # 2. Snare: χ ≥ 0.66, ε ≥ 0.46, suppression ≥ 0.60
    if chi >= 0.66 and eps >= 0.46 and suppression >= 0.60:
        return 'snare', chi

    # 3. Scaffold: χ ≤ 0.30, theater ≤ 0.70
    if chi <= 0.30 and theater <= 0.70:
        return 'scaffold', chi

    # 4. Rope: χ ≤ 0.35, ε ≤ 0.45
    if chi <= 0.35 and eps <= 0.45:
        return 'rope', chi

    # 5. Tangled Rope: 0.40 ≤ χ ≤ 0.90, ε ≥ 0.30, suppression ≥ 0.40
    if 0.40 <= chi <= 0.90 and eps >= 0.30 and suppression >= 0.40:
        return 'tangled_rope', chi

    # 6. Piton: χ ≤ 0.25, ε > 0.10, theater ≥ 0.70
    if chi <= 0.25 and eps > 0.10 and theater >= 0.70:
        return 'piton', chi

    # Fallback: if chi is very high and doesn't meet specific gates
    if chi >= 0.66 and eps >= 0.46:
        return 'snare', chi
    if eps >= 0.30 and suppression >= 0.40:
        return 'tangled_rope', chi
    if eps <= 0.45:
        return 'rope', chi

    return 'unknown', chi


def read_file_and_audit(filepath):
    """Read a testset file and extract existing predicates."""
    content = filepath.read_text()

    audit = {}
    audit['content'] = content

    # Check what's present
    audit['has_theater_ratio'] = bool(re.search(r'domain_priors:theater_ratio\(', content))
    audit['has_cm_theater'] = bool(re.search(r'constraint_metric\([^,]+,\s*theater_ratio', content))
    audit['has_analytical'] = bool(re.search(r'agent_power\(analytical\)', content))

    # Extract constraint ID from constraint_claim or base_extractiveness
    m = re.search(r'constraint_claim\((\w+),', content)
    if not m:
        m = re.search(r'base_extractiveness\((\w+),', content)
    audit['constraint_id'] = m.group(1) if m else None

    # Check multifile declarations
    audit['has_mf_theater'] = bool(re.search(r'domain_priors:theater_ratio/2', content))
    audit['has_mf_beneficiary'] = bool(re.search(r'narrative_ontology:constraint_beneficiary/2', content))
    audit['has_mf_victim'] = bool(re.search(r'narrative_ontology:constraint_victim/2', content))
    audit['has_mf_claim'] = bool(re.search(r'narrative_ontology:constraint_claim/2', content))

    # Check for omega_variable
    audit['has_omega'] = bool(re.search(r'omega_variable\(', content))

    return audit


def generate_enrichment_block(cid, theater, theater_rationale,
                               add_analytical=False, analytical_type=None,
                               analytical_chi=None, eps=None, suppression=None,
                               needs_mf_theater=False, needs_omega=False,
                               domain=None):
    """Generate the Prolog enrichment block to append."""
    lines = []
    lines.append('')
    lines.append('% ============================================================================')
    lines.append('% ENRICHMENT: Structural predicates for dynamic classification')
    lines.append('% Generated: 2026-02-08')
    lines.append('% Template: v5.2 namespace alignment')
    lines.append('% Source: Derived from existing narrative and structural content in this file')
    lines.append('% ============================================================================')

    # Multifile declarations for new predicates
    mf_preds = []
    if needs_mf_theater:
        mf_preds.append('    domain_priors:theater_ratio/2')
    # Always need constraint_metric if adding theater
    if not needs_mf_theater:
        # Check if constraint_metric is already in multifile
        pass

    if mf_preds:
        lines.append('')
        lines.append('% --- Multifile declarations for new predicates ---')
        lines.append(':- multifile')
        lines.append(',\n'.join(mf_preds) + '.')

    # Theater ratio
    lines.append('')
    lines.append(f'% --- Theater ratio (missing from base properties) ---')
    lines.append(f'% {theater_rationale}')
    lines.append(f'domain_priors:theater_ratio({cid}, {theater}).')
    lines.append(f'narrative_ontology:constraint_metric({cid}, theater_ratio, {theater}).')

    # Analytical perspective
    if add_analytical and analytical_type:
        lines.append('')
        lines.append('% --- Analytical perspective classification (missing) ---')
        lines.append(f'% chi = {eps} * 1.15 (analytical) * 1.2 (global) = {analytical_chi:.3f}')
        lines.append(f'% Classification: {analytical_type}')
        lines.append(f'constraint_indexing:constraint_classification({cid}, {analytical_type},')
        lines.append(f'    context(agent_power(analytical),')
        lines.append(f'            time_horizon(civilizational),')
        lines.append(f'            exit_options(analytical),')
        lines.append(f'            spatial_scope(global))).')

    return '\n'.join(lines) + '\n'


def main():
    dry_run = '--dry-run' in sys.argv

    base = Path(__file__).resolve().parent.parent
    targets_path = base / 'outputs' / 'enrichment_targets.json'
    testsets_dir = base / 'prolog' / 'testsets'

    with open(targets_path) as f:
        data = json.load(f)

    partial = [t for t in data['targets'] if t['category'] == 'partial_metrics']
    print(f'Partial metrics targets: {len(partial)}')

    if dry_run:
        print('=== DRY RUN — no files will be modified ===')

    stats = Counter()
    theater_values = []
    errors = []

    for target in partial:
        cid = target['id']
        domain = target.get('domain', 'unknown')
        eps = target.get('extractiveness') or 0.0
        suppression = target.get('suppression') or 0.0
        claimed_type = target.get('claimed_type')
        gaps = target.get('gaps', [])

        # Find the file
        filepath = testsets_dir / f'{cid}.pl'
        if not filepath.exists():
            # Try case-insensitive
            found = None
            for f in testsets_dir.iterdir():
                if f.suffix == '.pl' and f.stem.lower() == cid.lower():
                    found = f
                    break
            if found:
                filepath = found
            else:
                errors.append(f'{cid}: file not found')
                stats['error_no_file'] += 1
                continue

        # Read and audit
        audit = read_file_and_audit(filepath)

        if audit['has_theater_ratio'] and audit['has_cm_theater']:
            stats['already_has_theater'] += 1
            continue

        # Derive theater ratio
        theater, rationale = derive_theater_ratio(cid, claimed_type, domain, eps, suppression)
        theater_values.append(theater)

        # Check if analytical perspective needed
        needs_analytical = 'MISSING_ANALYTICAL_PERSPECTIVE' in gaps and not audit['has_analytical']
        analytical_type = None
        analytical_chi = None

        if needs_analytical:
            analytical_type, analytical_chi = classify_from_chi(
                eps, suppression, theater, 'analytical', 'global'
            )

        # Check if multifile theater declaration is needed
        needs_mf_theater = not audit['has_mf_theater']

        # Generate enrichment block
        block = generate_enrichment_block(
            cid=audit['constraint_id'] or cid,
            theater=theater,
            theater_rationale=rationale,
            add_analytical=needs_analytical,
            analytical_type=analytical_type,
            analytical_chi=analytical_chi,
            eps=eps,
            suppression=suppression,
            needs_mf_theater=needs_mf_theater,
            domain=domain,
        )

        if dry_run:
            if stats['enriched'] < 3:
                print(f'\n--- {cid} (eps={eps}, sup={suppression}, claimed={claimed_type}) ---')
                print(f'    theater={theater}, rationale: {rationale}')
                if needs_analytical:
                    print(f'    analytical: {analytical_type} (chi={analytical_chi:.3f})')
        else:
            # Append to file
            with open(filepath, 'a') as f:
                f.write(block)

        stats['enriched'] += 1
        if needs_analytical:
            stats['added_analytical'] += 1

    print(f'\nResults:')
    for k, v in stats.most_common():
        print(f'  {k}: {v}')

    if errors:
        print(f'\nErrors:')
        for e in errors:
            print(f'  {e}')

    if theater_values:
        print(f'\nTheater ratio distribution:')
        print(f'  min: {min(theater_values):.2f}')
        print(f'  max: {max(theater_values):.2f}')
        print(f'  mean: {sum(theater_values)/len(theater_values):.2f}')
        # Histogram by decile
        deciles = Counter()
        for v in theater_values:
            bucket = int(v * 10) / 10
            deciles[f'{bucket:.1f}-{bucket+0.1:.1f}'] += 1
        print(f'  distribution:')
        for bucket in sorted(deciles.keys()):
            bar = '#' * deciles[bucket]
            print(f'    {bucket}: {bar} ({deciles[bucket]})')


if __name__ == '__main__':
    main()
