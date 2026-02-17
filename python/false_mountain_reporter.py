import json
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
from orbit_utils import load_orbit_data, get_orbit_signature, format_orbit_signature

_SCRIPT_DIR = Path(__file__).parent
_PIPELINE_JSON = _SCRIPT_DIR / '..' / 'outputs' / 'pipeline_output.json'


def load_pipeline_data():
    try:
        with open(_PIPELINE_JSON, 'r', encoding='utf-8') as f:
            return json.load(f)
    except FileNotFoundError:
        print(f"Error: Pipeline output not found at {_PIPELINE_JSON}", file=sys.stderr)
        sys.exit(1)


def _format_gap_detected(gap):
    """Format a gap dict into readable alert/gap text."""
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


def normalize_entries(constraints):
    """One entry per gap (gap-as-entity). Each gap within a constraint becomes
    its own report entry."""
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

            # Severity: use highest severity among omegas, or 'N/A'
            if omegas:
                severity_order = {'critical': 0, 'high': 1, 'moderate': 2, 'unknown': 3}
                best = min(omegas, key=lambda o: severity_order.get(o.get('severity', 'unknown'), 99))
                entry['severity'] = best.get('severity') or 'N/A'
            else:
                entry['severity'] = 'N/A'

            # Omega: show id (type) format to match old report style
            if omegas:
                o = omegas[0]
                entry['omega_question'] = f"{o['id']} ({o.get('type', 'conceptual')})"
            else:
                entry['omega_question'] = 'N/A'

            entries.append(entry)
    return entries


def dedup_entries(entries):
    """Dedup on (constraint_name, gap_detected)."""
    seen = set()
    unique = []
    for e in entries:
        key = (e['name'], e['gap_detected'])
        if key not in seen:
            seen.add(key)
            unique.append(e)
    return unique


def generate_markdown_report(fm_data, output_path, orbit_data=None):
    sorted_fms = sorted(fm_data, key=lambda x: (x['severity'] != 'critical', x['name']))

    with open(output_path, 'w', encoding='utf-8') as f:
        f.write("# False Mountain Diagnostic Report\n\n")
        f.write(f"**Total Unique False Mountains Found:** {len(sorted_fms)}\n\n")
        f.write("---\n\n")

        for i, fm in enumerate(sorted_fms, 1):
            f.write(f"### {i}. False Mountain: `{fm['name']}`\n\n")
            f.write(f"*   **Severity:** `{fm['severity']}`\n")
            orbit_sig = get_orbit_signature(orbit_data or {}, fm['name'])
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


def main():
    script_dir = Path(__file__).parent
    report_file = script_dir / '../outputs/false_mountain_report.md'

    print("Parsing pipeline output to find False Mountains...")

    pipeline_data = load_pipeline_data()
    constraints = pipeline_data['per_constraint']

    entries = normalize_entries(constraints)
    entries = dedup_entries(entries)
    orbit_data = load_orbit_data()

    if entries:
        print(f"Found {len(entries)} unique False Mountains.")
        print(f"Generating report at {report_file}...")
        generate_markdown_report(entries, report_file, orbit_data)
        print("Report generated successfully.")
    else:
        print("No False Mountains found.")
        with open(report_file, 'w', encoding='utf-8') as f:
            f.write("# False Mountain Diagnostic Report\n\n**Total Unique False Mountains Found:** 0\n")


if __name__ == '__main__':
    main()
