"""Parameterized type reporter — query function.

Replaces snare/piton/scaffold/rope/true_mountain/tangled_rope/false_mountain
reporters plus count_computed_classifications and high_friction.

No template — query handles all file writing internally via run_type_report().
"""

from collections import Counter

from orbit_utils import load_orbit_data, get_orbit_signature, format_orbit_signature

from shared.loader import OUTPUT_DIR

# ---------------------------------------------------------------------------
# Type configs
# ---------------------------------------------------------------------------

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
    'tangled_rope': {
        'family': 'diagnostic',
        'filter_type': 'tangled_rope',
        'custom_filter': 'any_perspective',
        'require_unanimity': False,
        'report_title': 'Tangled Rope Diagnostic Report',
        'entity_label': 'Tangled Rope',
        'entity_label_plural': 'Tangled Ropes',
        'output_filename': 'tangled_rope_report.md',
        'found_msg': 'Found {n} constraints classified as Tangled Ropes.',
        'empty_msg': 'No constraints classified as Tangled Ropes found.',
        'sort_key': lambda e: (e['severity'] != 'critical', e['name']),
        'show_all_perspectives': True,
        'bold_perspectives': False,
        'always_show_gap': True,
    },
    'false_mountain': {
        'family': 'false_mountain',
        'report_title': 'False Mountain Diagnostic Report',
        'entity_label': 'False Mountain',
        'entity_label_plural': 'False Mountains',
        'output_filename': 'false_mountain_report.md',
        'found_msg': 'Found {n} unique False Mountains.',
        'empty_msg': 'No False Mountains found.',
        'sort_key': lambda e: (e['severity'] != 'critical', e['name']),
    },
}

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


def _filter_any_perspective(constraints, type_name):
    """Return constraints where ANY perspective value equals type_name."""
    results = []
    for c in constraints:
        perspectives = c.get('perspectives', {})
        if any(v == type_name for v in perspectives.values()):
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


def _format_gap_detected(gap):
    """Convert a gap dict into readable text (false_mountain variant)."""
    gap_type = gap.get('gap_type', '')
    parts = []
    for key in ('powerless_type', 'institutional_type', 'analytical_type',
                'moderate_type'):
        if key in gap:
            parts.append(f"{key.replace('_type', '')}: {gap[key]}")
    detail = ', '.join(parts)
    if 'masked' in gap_type.lower():
        return f"! ALERT: {gap_type} ({detail})"
    return f"! GAP: {gap_type} ({detail})"


# ---------------------------------------------------------------------------
# Normalization
# ---------------------------------------------------------------------------

def _normalize_diagnostic(constraint):
    """Family A: emit one entry per omega."""
    base = {
        'name': constraint['id'],
        'claimed_type': constraint.get('claimed_type') or 'N/A',
        'powerless_view': constraint.get('perspectives', {}).get('powerless') or 'N/A',
        'institutional_view': constraint.get('perspectives', {}).get('institutional') or 'N/A',
        'analytical_view': constraint.get('perspectives', {}).get('analytical') or 'N/A',
        'structural_signature': constraint.get('signature') or 'N/A',
        'related_gap_alert': 'N/A',
        'resolution_strategy': constraint.get('resolution_strategy') or 'N/A',
    }

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
        entry['omega_question'] = omega.get('question') or 'N/A'
        entry['severity'] = omega.get('severity') or 'N/A'
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


def _normalize_false_mountain(constraints):
    """False mountain: one entry per gap (gap-as-entity normalization)."""
    entries = []
    for c in constraints:
        gaps = c.get('gaps') or []
        if not gaps:
            continue

        omegas = c.get('omegas') or []
        perspectives = c.get('perspectives', {})

        for gap in gaps:
            entry = {
                'name': c['id'],
                'powerless_view': gap.get('powerless_type') or perspectives.get('powerless') or 'N/A',
                'institutional_view': gap.get('institutional_type') or perspectives.get('institutional') or 'N/A',
                'gap_detected': _format_gap_detected(gap),
                'resolution_strategy': c.get('resolution_strategy') or 'N/A',
            }

            if omegas:
                severity_order = {'critical': 0, 'high': 1, 'moderate': 2, 'unknown': 3}
                best = min(omegas, key=lambda o: severity_order.get(o.get('severity', 'unknown'), 99))
                entry['severity'] = best.get('severity') or 'N/A'
            else:
                entry['severity'] = 'N/A'

            if omegas:
                o = omegas[0]
                entry['omega_question'] = f"{o['id']} ({o.get('type', 'conceptual')})"
            else:
                entry['omega_question'] = 'N/A'

            entries.append(entry)
    return entries


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


def _dedup_false_mountain(entries):
    """Dedup on (name, gap_detected)."""
    seen = set()
    unique = []
    for e in entries:
        key = (e['name'], e['gap_detected'])
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
    show_all = cfg.get('show_all_perspectives', False)
    bold_persp = cfg.get('bold_perspectives', True)
    always_gap = cfg.get('always_show_gap', False)

    with open(output_path, 'w', encoding='utf-8') as f:
        f.write(f"# {cfg['report_title']}\n\n")
        f.write(f"**Total Unique {label_plural} Found:** {len(entries)}\n\n")
        f.write("---\n\n")

        for i, e in enumerate(entries, 1):
            f.write(f"### {i}. {label}: `{e['name']}`\n\n")
            f.write(f"*   **Claimed Type:** `{e['claimed_type']}`\n")
            f.write(f"*   **Severity:** `{e['severity']}`\n")

            f.write(f"*   **Perspectival Breakdown:**\n")
            if show_all:
                if bold_persp:
                    f.write(f"    *   **Individual (Powerless) View:** `{e['powerless_view']}`\n")
                    f.write(f"    *   **Institutional (Manager) View:** `{e['institutional_view']}`\n")
                    f.write(f"    *   **Analytical View:** `{e['analytical_view']}`\n")
                else:
                    f.write(f"    *   Individual (Powerless) View: `{e['powerless_view']}`\n")
                    f.write(f"    *   Institutional (Manager) View: `{e['institutional_view']}`\n")
                    f.write(f"    *   Analytical View: `{e['analytical_view']}`\n")
            else:
                if e['powerless_view'] not in ('N/A', 'unknown', None):
                    f.write(f"    *   **Individual (Powerless) View:** `{e['powerless_view']}`\n")
                if e['institutional_view'] not in ('N/A', 'unknown', None):
                    f.write(f"    *   **Institutional (Manager) View:** `{e['institutional_view']}`\n")
                if e['analytical_view'] not in ('N/A', 'unknown', None):
                    f.write(f"    *   **Analytical View:** `{e['analytical_view']}`\n")

            f.write(f"*   **Structural Signature Analysis:** {e['structural_signature']}\n")
            orbit_sig = get_orbit_signature(orbit_data, e['name'])
            f.write(f"*   **Orbit Signature:** `{format_orbit_signature(orbit_sig)}`\n")

            if always_gap or e['related_gap_alert'] != 'N/A':
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
# Report writing — false mountain (Family C)
# ---------------------------------------------------------------------------

def _write_false_mountain_report(entries, cfg, output_path, orbit_data):
    label = cfg['entity_label']
    label_plural = cfg['entity_label_plural']

    with open(output_path, 'w', encoding='utf-8') as f:
        f.write(f"# {cfg['report_title']}\n\n")
        f.write(f"**Total Unique {label_plural} Found:** {len(entries)}\n\n")
        f.write("---\n\n")

        for i, fm in enumerate(entries, 1):
            f.write(f"### {i}. {label}: `{fm['name']}`\n\n")
            f.write(f"*   **Severity:** `{fm['severity']}`\n")
            orbit_sig = get_orbit_signature(orbit_data, fm['name'])
            f.write(f"*   **Orbit Signature:** `{format_orbit_signature(orbit_sig)}`\n")
            f.write(f"*   **Gap Detected:** {fm['gap_detected']}\n")

            if fm['powerless_view'] != 'N/A' or fm['institutional_view'] != 'N/A':
                f.write(f"*   **Perspectival Mismatch:**\n")
                if fm['powerless_view'] != 'N/A':
                    f.write(f"    *   **Powerless View:** `{fm['powerless_view']}`\n")
                if fm['institutional_view'] != 'N/A':
                    f.write(f"    *   **Institutional View:** `{fm['institutional_view']}`\n")

            f.write(f"*   **Generated Omega:** {fm['omega_question']}\n")
            f.write(f"*   **Suggested Resolution Strategy:**\n")
            f.write(f"    ```\n{fm['resolution_strategy']}\n    ```\n\n")
            f.write("---\n\n")


def _write_false_mountain_empty(cfg, output_path):
    with open(output_path, 'w', encoding='utf-8') as f:
        f.write(f"# {cfg['report_title']}\n\n")
        f.write(f"**Total Unique {cfg['entity_label_plural']} Found:** 0\n")


# ---------------------------------------------------------------------------
# Unified report driver
# ---------------------------------------------------------------------------

def run_type_report(type_key, pipeline_data, orbit_data):
    """Generate a report for a single type. Returns entry count."""
    cfg = TYPE_CONFIGS[type_key]
    family = cfg['family']
    constraints = pipeline_data['per_constraint']

    if cfg.get('custom_filter') == 'any_perspective':
        filtered = _filter_any_perspective(constraints, cfg['filter_type'])
    elif family == 'false_mountain':
        filtered = constraints
    else:
        filtered = filter_constraints(constraints, cfg['filter_type'],
                                      cfg['require_unanimity'])

    if family == 'false_mountain':
        entries = _normalize_false_mountain(filtered)
    else:
        entries = normalize_entries(filtered, family)

    if family == 'false_mountain':
        entries = _dedup_false_mountain(entries)
    else:
        entries = dedup_entries(entries, family)

    entries = sort_entries(entries, cfg['sort_key'])

    output_path = OUTPUT_DIR / cfg['output_filename']

    if entries:
        if family == 'false_mountain':
            _write_false_mountain_report(entries, cfg, output_path, orbit_data)
        elif family == 'diagnostic':
            _write_diagnostic_report(entries, cfg, output_path, orbit_data)
        else:
            _write_validation_report(entries, cfg, output_path, orbit_data)
        print(cfg['found_msg'].format(n=len(entries)))
        print(f"Generating report at {output_path}...")
        print("Report generated successfully.")
    else:
        if family == 'false_mountain':
            _write_false_mountain_empty(cfg, output_path)
        elif family == 'diagnostic':
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
# Query entry point (for registry)
# ---------------------------------------------------------------------------

def query(data: dict) -> dict:
    """Pipeline/orbit data -> run all type reports (or single type via _type key).

    Returns empty dict since template=None — this query writes files directly.
    """
    pipeline_data = data["pipeline"]
    orbit_data = data["orbit"]

    type_key = data.get("_type")
    if type_key:
        run_type_report(type_key, pipeline_data, orbit_data)
    else:
        for key in TYPE_CONFIGS:
            run_type_report(key, pipeline_data, orbit_data)

    return {}
