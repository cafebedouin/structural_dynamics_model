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


def _format_gap_alert(gap):
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


def filter_tangled_ropes(constraints):
    """Return constraints where ANY non-unknown perspective == 'tangled_rope'."""
    results = []
    for c in constraints:
        perspectives = c.get('perspectives', {})
        if any(v == 'tangled_rope' for v in perspectives.values()):
            results.append(c)
    return results


def normalize_entries(constraints):
    """One entry per omega (same as Family A diagnostic pattern)."""
    entries = []
    for c in constraints:
        base = {
            'name': c['id'],
            'claimed_type': c.get('claimed_type') or 'N/A',
            'powerless_view': c.get('perspectives', {}).get('powerless') or 'N/A',
            'institutional_view': c.get('perspectives', {}).get('institutional') or 'N/A',
            'analytical_view': c.get('perspectives', {}).get('analytical') or 'N/A',
            'structural_signature': c.get('signature') or 'N/A',
            'related_gap_alert': 'N/A',
            'resolution_strategy': c.get('resolution_strategy') or 'N/A',
        }

        gaps = c.get('gaps') or []
        if gaps:
            base['related_gap_alert'] = _format_gap_alert(gaps[0])

        omegas = c.get('omegas') or []
        if not omegas:
            entry = dict(base)
            entry['omega_question'] = 'N/A'
            entry['severity'] = 'N/A'
            entries.append(entry)
        else:
            for omega in omegas:
                entry = dict(base)
                entry['omega_question'] = omega.get('question') or 'N/A'
                entry['severity'] = omega.get('severity') or 'N/A'
                entries.append(entry)
    return entries


def dedup_entries(entries):
    seen = set()
    unique = []
    for e in entries:
        key = (e['name'], e.get('omega_question', ''))
        if key not in seen:
            seen.add(key)
            unique.append(e)
    return unique


def generate_markdown_report(tr_data, output_path, orbit_data=None):
    sorted_trs = sorted(tr_data, key=lambda x: (x['severity'] != 'critical', x['name']))

    with open(output_path, 'w', encoding='utf-8') as f:
        f.write("# Tangled Rope Diagnostic Report\n\n")
        f.write(f"**Total Unique Tangled Ropes Found:** {len(sorted_trs)}\n\n")
        f.write("---\n\n")

        for i, tr in enumerate(sorted_trs, 1):
            f.write(f"### {i}. Tangled Rope: `{tr['name']}`\n\n")
            f.write(f"*   **Claimed Type:** `{tr['claimed_type']}`\n")
            f.write(f"*   **Severity:** `{tr['severity']}`\n")

            f.write(f"*   **Perspectival Breakdown:**\n")
            f.write(f"    *   Individual (Powerless) View: `{tr['powerless_view']}`\n")
            f.write(f"    *   Institutional (Manager) View: `{tr['institutional_view']}`\n")
            f.write(f"    *   Analytical View: `{tr['analytical_view']}`\n")

            f.write(f"*   **Structural Signature Analysis:** {tr['structural_signature']}\n")
            orbit_sig = get_orbit_signature(orbit_data or {}, tr['name'])
            f.write(f"*   **Orbit Signature:** `{format_orbit_signature(orbit_sig)}`\n")
            f.write(f"*   **Related Gap/Alert:** {tr['related_gap_alert']}\n")
            f.write(f"*   **Generated Omega:** {tr['omega_question']}\n")
            f.write(f"*   **Suggested Resolution Strategy:**\n")
            f.write(f"    ```\n{tr['resolution_strategy']}\n    ```\n\n")
            f.write("---\n\n")


def main():
    script_dir = Path(__file__).parent
    report_file = script_dir / '../outputs/tangled_rope_report.md'

    print("Parsing pipeline output to find Tangled Ropes (based on analyzed perspectives)...")

    pipeline_data = load_pipeline_data()
    constraints = pipeline_data['per_constraint']

    filtered = filter_tangled_ropes(constraints)
    entries = normalize_entries(filtered)
    entries = dedup_entries(entries)
    orbit_data = load_orbit_data()

    if entries:
        print(f"Found {len(entries)} constraints classified as Tangled Ropes.")
        print(f"Generating report at {report_file}...")
        generate_markdown_report(entries, report_file, orbit_data)
        print("Report generated successfully.")
    else:
        print("No constraints classified as Tangled Ropes found.")
        with open(report_file, 'w', encoding='utf-8') as f:
            f.write("# Tangled Rope Diagnostic Report\n\n**Total Unique Tangled Ropes Found:** 0\n")


if __name__ == '__main__':
    main()
