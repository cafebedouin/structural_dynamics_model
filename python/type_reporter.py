#!/usr/bin/env python3
"""
Parameterized type reporter — replaces snare/piton/scaffold/rope/true_mountain
reporters plus count_computed_classifications and high_friction.

Reads outputs/pipeline_output.json instead of regex-parsing output.txt.

Usage:
    python3 type_reporter.py --type snare
    python3 type_reporter.py --type mountain
    python3 type_reporter.py --all
    python3 type_reporter.py --summary counts
    python3 type_reporter.py --summary friction
"""

import argparse
import json
import sys
from collections import Counter
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
from orbit_utils import load_orbit_data, get_orbit_signature, format_orbit_signature

# ---------------------------------------------------------------------------
# Paths
# ---------------------------------------------------------------------------
_SCRIPT_DIR = Path(__file__).parent
_OUTPUT_DIR = _SCRIPT_DIR / '..' / 'outputs'
_PIPELINE_JSON = _OUTPUT_DIR / 'pipeline_output.json'

# ---------------------------------------------------------------------------
# Type configs
# ---------------------------------------------------------------------------
# family: "diagnostic" (A) or "validation" (B)
#   diagnostic — one entry per omega, dedup on (name, omega_question)
#   validation — one entry per constraint, dedup on name

TYPE_CONFIGS = {
    'snare': {
        'family': 'diagnostic',
        'filter_type': 'snare',
        'require_unanimity': False,
        'report_title': 'Snare Diagnostic Report',
        'entity_label': 'Snare',
        'entity_label_plural': 'Snares',
        'output_filename': 'snare_report.md',
        'found_msg': 'Found {n} unique Snares.',
        'empty_msg': 'No Snares found.',
        'sort_key': lambda e: (e['severity'] != 'critical', e['name']),
    },
    'piton': {
        'family': 'diagnostic',
        'filter_type': 'piton',
        'require_unanimity': False,
        'report_title': 'Piton Diagnostic Report',
        'entity_label': 'Piton',
        'entity_label_plural': 'Pitons',
        'output_filename': 'piton_report.md',
        'found_msg': 'Found {n} unique Pitons.',
        'empty_msg': 'No Pitons found.',
        'sort_key': lambda e: (e['severity'] != 'critical', e['name']),
    },
    'scaffold': {
        'family': 'diagnostic',
        'filter_type': 'scaffold',
        'require_unanimity': False,
        'report_title': 'Scaffold Diagnostic Report',
        'entity_label': 'Scaffold',
        'entity_label_plural': 'Scaffolds',
        'output_filename': 'scaffold_report.md',
        'found_msg': 'Found {n} unique Scaffolds.',
        'empty_msg': 'No Scaffolds found.',
        'sort_key': lambda e: (e['severity'] != 'critical', e['name']),
    },
    'rope': {
        'family': 'validation',
        'filter_type': 'rope',
        'require_unanimity': True,
        'report_title': 'Rope Validation Report',
        'entity_label': 'Rope',
        'entity_label_plural': 'Ropes',
        'output_filename': 'rope_report.md',
        'found_msg': 'Found {n} validated Ropes.',
        'empty_msg': 'No validated Ropes found.',
        'report_description': "This report lists all constraints that are consistently classified as 'rope' across all tested perspectives, indicating their functional and potentially beneficial nature within the model.",
        'sort_key': lambda e: e['name'],
        'fields': ['signature', 'orbit', 'agreement', 'gap_alert', 'omega', 'resolution'],
    },
    'mountain': {
        'family': 'validation',
        'filter_type': 'mountain',
        'require_unanimity': True,
        'report_title': 'True Mountain Validation Report',
        'entity_label': 'True Mountain',
        'entity_label_plural': 'True Mountains',
        'output_filename': 'true_mountain_report.md',
        'found_msg': 'Found {n} validated True Mountains.',
        'empty_msg': 'No validated True Mountains found.',
        'report_description': "This report lists all constraints that are consistently classified as 'mountain' across all tested perspectives, confirming their immutability within the model.",
        'sort_key': lambda e: e['name'],
        'fields': ['signature', 'orbit', 'agreement'],
    },
}

# ---------------------------------------------------------------------------
# Data loading
# ---------------------------------------------------------------------------

def load_pipeline_data():
    """Read pipeline_output.json once, return the full dict."""
    try:
        with open(_PIPELINE_JSON, 'r', encoding='utf-8') as f:
            return json.load(f)
    except FileNotFoundError:
        print(f"Error: Pipeline output not found at {_PIPELINE_JSON}", file=sys.stderr)
        sys.exit(1)


# ---------------------------------------------------------------------------
# Filtering
# ---------------------------------------------------------------------------

def filter_constraints(constraints, filter_type, require_unanimity):
    """Return constraints matching claimed_type, optionally requiring unanimity."""
    results = []
    for c in constraints:
        if c.get('claimed_type') != filter_type:
            continue
        if require_unanimity:
            perspectives = c.get('perspectives', {})
            non_unknown = {k: v for k, v in perspectives.items()
                          if v not in ('unknown', None)}
            if not non_unknown:
                continue
            if not all(v == filter_type for v in non_unknown.values()):
                continue
        results.append(c)
    return results


# ---------------------------------------------------------------------------
# Gap/alert formatting
# ---------------------------------------------------------------------------

def _format_gap_alert(gap):
    """Convert a gap dict from JSON into a readable alert string."""
    gap_type = gap.get('gap_type', '')
    parts = []
    for key in ('powerless_type', 'institutional_type', 'analytical_type',
                'moderate_type'):
        if key in gap:
            parts.append(f"{key.replace('_type', '')}: {gap[key]}")
    detail = ', '.join(parts)

    if 'alert' in gap_type.lower() or 'masked' in gap_type.lower():
        return f"! ALERT: {gap_type} ({detail})"
    return f"! GAP: {gap_type} ({detail})"


# ---------------------------------------------------------------------------
# Normalization — JSON constraint → flat report-ready dicts
# ---------------------------------------------------------------------------

def _normalize_diagnostic(constraint):
    """Family A: emit one entry per omega."""
    base = {
        'name': constraint['id'],
        'claimed_type': constraint.get('claimed_type', 'N/A'),
        'powerless_view': constraint.get('perspectives', {}).get('powerless', 'N/A'),
        'institutional_view': constraint.get('perspectives', {}).get('institutional', 'N/A'),
        'analytical_view': constraint.get('perspectives', {}).get('analytical', 'N/A'),
        'structural_signature': constraint.get('signature', 'N/A') or 'N/A',
        'related_gap_alert': 'N/A',
        'resolution_strategy': constraint.get('resolution_strategy') or 'N/A',
    }

    # Gap/alert — use first gap
    gaps = constraint.get('gaps') or []
    if gaps:
        base['related_gap_alert'] = _format_gap_alert(gaps[0])

    omegas = constraint.get('omegas') or []
    if not omegas:
        entry = dict(base)
        entry['omega_question'] = 'N/A'
        entry['severity'] = 'N/A'
        return [entry]

    entries = []
    for omega in omegas:
        entry = dict(base)
        entry['omega_question'] = omega.get('question', 'N/A')
        entry['severity'] = omega.get('severity', 'N/A') or 'N/A'
        entries.append(entry)
    return entries


def _normalize_validation(constraint):
    """Family B: emit one entry per constraint."""
    entry = {
        'name': constraint['id'],
        'claimed_type': constraint.get('claimed_type', 'N/A'),
        'structural_signature': constraint.get('signature', 'N/A') or 'N/A',
        'related_gap_alert': 'N/A',
        'omega_question': 'N/A',
        'resolution_strategy': constraint.get('resolution_strategy') or 'N/A',
    }

    gaps = constraint.get('gaps') or []
    if gaps:
        entry['related_gap_alert'] = _format_gap_alert(gaps[0])

    omegas = constraint.get('omegas') or []
    if omegas:
        entry['omega_question'] = omegas[0].get('question', 'N/A')

    return [entry]


def normalize_entries(constraints, family):
    """Dispatch to family-specific normalizer."""
    fn = _normalize_diagnostic if family == 'diagnostic' else _normalize_validation
    entries = []
    for c in constraints:
        entries.extend(fn(c))
    return entries


# ---------------------------------------------------------------------------
# Dedup + sort
# ---------------------------------------------------------------------------

def dedup_entries(entries, family):
    """Deduplicate: diagnostic on (name, omega_question), validation on name."""
    seen = set()
    unique = []
    for e in entries:
        if family == 'diagnostic':
            key = (e['name'], e.get('omega_question', ''))
        else:
            key = e['name']
        if key not in seen:
            seen.add(key)
            unique.append(e)
    return unique


def sort_entries(entries, sort_key):
    return sorted(entries, key=sort_key)


# ---------------------------------------------------------------------------
# Report writing — diagnostic (Family A)
# ---------------------------------------------------------------------------

def _write_diagnostic_report(entries, cfg, output_path, orbit_data):
    label = cfg['entity_label']
    label_plural = cfg['entity_label_plural']

    with open(output_path, 'w', encoding='utf-8') as f:
        f.write(f"# {cfg['report_title']}\n\n")
        f.write(f"**Total Unique {label_plural} Found:** {len(entries)}\n\n")
        f.write("---\n\n")

        for i, e in enumerate(entries, 1):
            f.write(f"### {i}. {label}: `{e['name']}`\n\n")
            f.write(f"*   **Claimed Type:** `{e['claimed_type']}`\n")
            f.write(f"*   **Severity:** `{e['severity']}`\n")

            f.write(f"*   **Perspectival Breakdown:**\n")
            if e['powerless_view'] not in ('N/A', 'unknown', None):
                f.write(f"    *   **Individual (Powerless) View:** `{e['powerless_view']}`\n")
            if e['institutional_view'] not in ('N/A', 'unknown', None):
                f.write(f"    *   **Institutional (Manager) View:** `{e['institutional_view']}`\n")
            if e['analytical_view'] not in ('N/A', 'unknown', None):
                f.write(f"    *   **Analytical View:** `{e['analytical_view']}`\n")

            f.write(f"*   **Structural Signature Analysis:** {e['structural_signature']}\n")
            orbit_sig = get_orbit_signature(orbit_data, e['name'])
            f.write(f"*   **Orbit Signature:** `{format_orbit_signature(orbit_sig)}`\n")

            if e['related_gap_alert'] != 'N/A':
                f.write(f"*   **Related Gap/Alert:** {e['related_gap_alert']}\n")

            f.write(f"*   **Generated Omega:** {e['omega_question']}\n")
            f.write(f"*   **Suggested Resolution Strategy:**\n")
            f.write(f"    ```\n{e['resolution_strategy']}\n    ```\n\n")
            f.write("---\n\n")


def _write_diagnostic_empty(cfg, output_path):
    with open(output_path, 'w', encoding='utf-8') as f:
        f.write(f"# {cfg['report_title']}\n\n")
        f.write(f"**Total Unique {cfg['entity_label_plural']} Found:** 0\n")


# ---------------------------------------------------------------------------
# Report writing — validation (Family B)
# ---------------------------------------------------------------------------

def _write_validation_report(entries, cfg, output_path, orbit_data):
    label = cfg['entity_label']
    filter_type = cfg['filter_type']
    fields = cfg.get('fields', [])

    with open(output_path, 'w', encoding='utf-8') as f:
        f.write(f"# {cfg['report_title']}\n\n")
        f.write(f"**Total Validated:** {len(entries)}\n\n")
        if cfg.get('report_description'):
            f.write(f"{cfg['report_description']}\n\n")
        f.write("---\n\n")

        for i, e in enumerate(entries, 1):
            f.write(f"### {i}. {label}: `{e['name']}`\n\n")
            f.write(f"*   **Claimed Type:** `{e['claimed_type']}`\n")

            if 'signature' in fields:
                f.write(f"*   **Structural Signature Analysis:** {e['structural_signature']}\n")
            if 'orbit' in fields:
                orbit_sig = get_orbit_signature(orbit_data, e['name'])
                f.write(f"*   **Orbit Signature:** `{format_orbit_signature(orbit_sig)}`\n")
            if 'agreement' in fields:
                f.write(f"*   **Perspectival Agreement:** Confirmed. All tested perspectives agree on the '{filter_type}' classification.\n")

            if 'gap_alert' in fields and e.get('related_gap_alert', 'N/A') != 'N/A':
                f.write(f"*   **Related Gap/Alert:** {e['related_gap_alert']}\n")
            if 'omega' in fields and e.get('omega_question', 'N/A') != 'N/A':
                f.write(f"*   **Generated Omega:** {e['omega_question']}\n")
            if 'resolution' in fields and e.get('resolution_strategy') not in ('N/A', '', None):
                f.write(f"*   **Suggested Resolution Strategy:**\n")
                f.write(f"    ```\n{e['resolution_strategy']}\n    ```\n")

            f.write("\n---\n\n")


def _write_validation_empty(cfg, output_path):
    with open(output_path, 'w', encoding='utf-8') as f:
        f.write(f"# {cfg['report_title']}\n\n")
        f.write(f"**Total Validated:** 0\n")


# ---------------------------------------------------------------------------
# Unified report driver
# ---------------------------------------------------------------------------

def run_type_report(type_key, pipeline_data, orbit_data):
    """Generate a report for a single type. Returns entry count."""
    cfg = TYPE_CONFIGS[type_key]
    family = cfg['family']
    constraints = pipeline_data['per_constraint']

    filtered = filter_constraints(constraints, cfg['filter_type'],
                                  cfg['require_unanimity'])
    entries = normalize_entries(filtered, family)
    entries = dedup_entries(entries, family)
    entries = sort_entries(entries, cfg['sort_key'])

    output_path = _OUTPUT_DIR / cfg['output_filename']

    if entries:
        if family == 'diagnostic':
            _write_diagnostic_report(entries, cfg, output_path, orbit_data)
        else:
            _write_validation_report(entries, cfg, output_path, orbit_data)
        print(cfg['found_msg'].format(n=len(entries)))
        print(f"Generating report at {output_path}...")
        print("Report generated successfully.")
    else:
        if family == 'diagnostic':
            _write_diagnostic_empty(cfg, output_path)
        else:
            _write_validation_empty(cfg, output_path)
        print(cfg['empty_msg'])

    return len(entries)


# ---------------------------------------------------------------------------
# Summary modes
# ---------------------------------------------------------------------------

def summary_counts(pipeline_data):
    """Replaces count_computed_classifications.py — counts by claimed_type."""
    constraints = pipeline_data['per_constraint']
    counts = Counter(c.get('claimed_type') for c in constraints)

    print("Computed Classification Counts")
    print("=" * 40)
    for ctype, count in sorted(counts.items(), key=lambda x: (x[0] or '')):
        label = ctype if ctype else '(none)'
        print(f"{label:<20} {count}")
    print("=" * 40)


def summary_friction(pipeline_data):
    """Replaces high_friction.py — top 15 by gap count."""
    constraints = pipeline_data['per_constraint']
    friction = [(c['id'], len(c.get('gaps') or [])) for c in constraints
                if c.get('gaps')]
    friction.sort(key=lambda x: x[1], reverse=True)

    print("=" * 60)
    print("TOP 15 HIGH-FRICTION CONSTRAINTS (POTENTIAL TANGLED ROPES)")
    print("=" * 60)
    print(f"{'Constraint Name':<45} | {'Gap Count':<10}")
    print("-" * 60)
    for name, count in friction[:15]:
        print(f"{name:<45} | {count:<10}")
    print("=" * 60)
    print("\n[ANALYSIS] These constraints are shifting type across indices.")
    print("If Gap Count is high but Type is 'Snare', the Power Modifier")
    print("is likely pushing the extraction score (\u03c7) past 0.66 too early.")


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(
        description='Parameterized type reporter for structural dynamics model')
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument('--type', choices=list(TYPE_CONFIGS.keys()),
                       help='Generate report for a single type')
    group.add_argument('--all', action='store_true',
                       help='Generate reports for all types')
    group.add_argument('--summary', choices=['counts', 'friction'],
                       help='Print summary to stdout (no file output)')

    args = parser.parse_args()
    pipeline_data = load_pipeline_data()

    if args.summary:
        if args.summary == 'counts':
            summary_counts(pipeline_data)
        else:
            summary_friction(pipeline_data)
        return

    orbit_data = load_orbit_data()

    if args.all:
        for type_key in TYPE_CONFIGS:
            run_type_report(type_key, pipeline_data, orbit_data)
            print()
    else:
        run_type_report(args.type, pipeline_data, orbit_data)


if __name__ == '__main__':
    main()
