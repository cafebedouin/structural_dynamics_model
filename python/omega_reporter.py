import json
import sys
from pathlib import Path

_SCRIPT_DIR = Path(__file__).parent
_PIPELINE_JSON = _SCRIPT_DIR / '..' / 'outputs' / 'pipeline_output.json'


def load_pipeline_data():
    try:
        with open(_PIPELINE_JSON, 'r', encoding='utf-8') as f:
            return json.load(f)
    except FileNotFoundError:
        print(f"Error: Pipeline output not found at {_PIPELINE_JSON}", file=sys.stderr)
        sys.exit(1)


def _format_source_gap(gap):
    """Format gap dict to Prolog-style source_gap string:
    gap(gap_type,powerless_type,institutional_type)"""
    gap_type = gap.get('gap_type', 'unknown')
    powerless = gap.get('powerless_type', 'unknown')
    institutional = gap.get('institutional_type', 'unknown')
    return f"gap({gap_type},{powerless},{institutional})"


def extract_omegas(constraints):
    """Build omega-centric records from per-constraint data."""
    omegas = []
    for c in constraints:
        gaps = c.get('gaps') or []
        for omega in (c.get('omegas') or []):
            record = {
                'name': omega['id'],
                'severity': omega.get('severity') or 'N/A',
                'associated_constraint': c['id'],
                'source_gap': _format_source_gap(gaps[0]) if gaps else 'N/A',
                'question': omega.get('question') or 'N/A',
                'resolution_strategy': c.get('resolution_strategy') or 'N/A',
            }
            omegas.append(record)
    return omegas


def dedup_omegas(omegas):
    seen = set()
    unique = []
    for o in omegas:
        if o['name'] not in seen:
            seen.add(o['name'])
            unique.append(o)
    return unique


def generate_markdown_report(omega_data, output_path):
    sorted_omegas = sorted(omega_data, key=lambda x: (x['severity'] != 'critical', x['name']))

    with open(output_path, 'w', encoding='utf-8') as f:
        f.write("# Omega Epistemological Gap Report\n\n")
        f.write(f"**Total Unique Omegas Found:** {len(sorted_omegas)}\n\n")
        f.write("This report lists all unique Omega variables generated during the analysis. Each Omega represents a critical question, a perspectival gap, or a contradiction that requires further investigation.\n\n")
        f.write("---\n\n")

        for i, o in enumerate(sorted_omegas, 1):
            f.write(f"### {i}. Omega: `{o['name']}`\n\n")
            f.write(f"*   **Severity:** `{o['severity']}`\n")
            f.write(f"*   **Associated Constraint:** `{o['associated_constraint']}`\n")
            f.write(f"*   **Source Gap:** `{o['source_gap']}`\n")
            f.write(f"*   **Question:** {o['question']}\n")
            f.write(f"*   **Suggested Resolution Strategy:**\n")
            f.write(f"    ```\n{o['resolution_strategy']}\n    ```\n\n")
            f.write("---\n\n")


def main():
    script_dir = Path(__file__).parent
    report_file = script_dir / '../outputs/omega_report.md'
    json_file = script_dir / '../outputs/omega_data.json'

    print("Parsing pipeline output to find Omegas...")

    pipeline_data = load_pipeline_data()
    constraints = pipeline_data['per_constraint']

    omega_data = extract_omegas(constraints)
    omega_data = dedup_omegas(omega_data)

    # Write JSON sidecar for downstream enrichment
    with open(json_file, 'w', encoding='utf-8') as f:
        json.dump(omega_data, f, indent=2)
    print(f"JSON data written to {json_file}")

    if omega_data:
        print(f"Found {len(omega_data)} unique Omegas.")
        print(f"Generating report at {report_file}...")
        generate_markdown_report(omega_data, report_file)
        print("Report generated successfully.")
    else:
        print("No Omegas found.")
        with open(report_file, 'w', encoding='utf-8') as f:
            f.write("# Omega Epistemological Gap Report\n\n**Total Unique Omegas Found:** 0\n")


if __name__ == '__main__':
    main()
