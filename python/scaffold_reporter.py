import re
import sys
from pathlib import Path

def parse_log_content(content):
    """
    Parses the log content to find and extract Scaffold details.
    """
    scaffolds = []

    scenario_chunks = re.split(r'(?=\[SCENARIO MANAGER\] Clearing Knowledge Base\.\.\.)', content)

    for chunk in scenario_chunks:
        if not chunk.strip():
            continue

        name_match = re.search(r'Loading:.*?testsets/(.+?)\.pl', chunk)
        if not name_match:
            continue
        constraint_name = name_match.group(1)

        # Check if the claimed type is 'scaffold' within the INDEXICAL AUDIT section
        claimed_type_match = re.search(r'Constraint: ' + re.escape(constraint_name) + r'\s*\n\s*Claimed Type: (\w+)', chunk)
        if not claimed_type_match or claimed_type_match.group(1) != 'scaffold':
            continue

        scaffold_data = {
            'name': constraint_name,
            'claimed_type': claimed_type_match.group(1),
            'powerless_view': 'N/A',
            'institutional_view': 'N/A',
            'analytical_view': 'N/A',
            'structural_signature': 'N/A',
            'related_gap_alert': 'N/A',
            'omega_question': 'N/A',
            'severity': 'N/A',
            'resolution_strategy': ''
        }

        # Extract perspective classifications from the INDEXICAL AUDIT section
        perspectives_section = re.search(r'\[CONSTRAINT INVENTORY: INDEXICAL AUDIT\]\s*\n.*?Perspectives:(.*?)(?=\n\n|\n\[CROSS-DOMAIN ISOMORPHISM)', chunk, re.DOTALL)
        if perspectives_section:
            for line in perspectives_section.group(1).split('\n'):
                if 'Individual (Powerless):' in line:
                    scaffold_data['powerless_view'] = re.search(r': (\w+)', line).group(1)
                elif 'Institutional (Manager):' in line:
                    scaffold_data['institutional_view'] = re.search(r': (\w+)', line).group(1)
                elif 'Analytical:' in line:
                    scaffold_data['analytical_view'] = re.search(r': (\w+)', line).group(1)

        # Extract Structural Signature Analysis
        signature_match = re.search(r'\[STRUCTURAL SIGNATURE ANALYSIS\]\s*\n.*?→ (.+)', chunk, re.DOTALL)
        if signature_match:
            scaffold_data['structural_signature'] = signature_match.group(1).strip()

        # Extract related gap/alert
        gap_analysis_section = re.search(r'\[PERSPECTIVAL GAP ANALYSIS\](.*?)(?=\n\[OMEGA GENERATION|\n\[OMEGA TRIAGE)', chunk, re.DOTALL)
        if gap_analysis_section:
            gap_alert_match = re.search(r'(! (?:ALERT|GAP): .+)', gap_analysis_section.group(1))
            if gap_alert_match:
                scaffold_data['related_gap_alert'] = gap_alert_match.group(1).strip()

        # Extract Omega from OMEGA GENERATION section
        omega_section = re.search(r'\[OMEGA GENERATION FROM PERSPECTIVAL GAPS: ' + re.escape(constraint_name) + r'\](.*?)(?=\n\[OMEGA TRIAGE)', chunk, re.DOTALL)
        if omega_section:
            omega_match = re.search(r'Ω: (.+)', omega_section.group(1))
            if omega_match:
                scaffold_data['omega_question'] = omega_match.group(1).strip()

        # Extract Severity from the Triage section
        triage_section = re.search(r'\[OMEGA TRIAGE & PRIORITIZATION\](.*?)(?=\n\n|\n\[OMEGA RESOLUTION)', chunk, re.DOTALL)
        if triage_section:
            if '[critical]' in triage_section.group(1):
                scaffold_data['severity'] = 'critical'
            elif '[high]' in triage_section.group(1):
                scaffold_data['severity'] = 'high'

        # Extract Resolution Strategy
        res_match = re.search(r'RESOLUTION STRATEGY:\s*\n(.*?)(?:\n\s*└─|\Z)', chunk, re.DOTALL)
        if res_match:
            strategy = res_match.group(1).strip()
            cleaned_strategy = "\n".join(line.strip().lstrip('│').lstrip() for line in strategy.split('\n'))
            scaffold_data['resolution_strategy'] = cleaned_strategy

        if scaffold_data['claimed_type'] == 'scaffold':
            scaffolds.append(scaffold_data)

    # Deduplicate
    unique_scaffolds = []
    seen = set()
    for n in scaffolds:
        identifier = (n['name'], n['omega_question'])
        if identifier not in seen:
            unique_scaffolds.append(n)
            seen.add(identifier)

    return unique_scaffolds

def generate_markdown_report(scaffolds_data, output_path):
    """
    Generates a Markdown report from the list of Scaffold data.
    """
    sorted_scaffolds = sorted(scaffolds_data, key=lambda x: (x['severity'] != 'critical', x['name']))

    with open(output_path, 'w', encoding='utf-8') as f:
        f.write("# Scaffold Diagnostic Report\n\n")
        f.write(f"**Total Unique Scaffolds Found:** {len(sorted_scaffolds)}\n\n")
        f.write("---\n\n")

        for i, n in enumerate(sorted_scaffolds, 1):
            f.write(f"### {i}. Scaffold: `{n['name']}`\n\n")
            f.write(f"*   **Claimed Type:** `{n['claimed_type']}`\n")
            f.write(f"*   **Severity:** `{n['severity']}`\n")

            f.write(f"*   **Perspectival Breakdown:**\n")
            if n['powerless_view'] != 'N/A':
                f.write(f"    *   **Individual (Powerless) View:** `{n['powerless_view']}`\n")
            if n['institutional_view'] != 'N/A':
                f.write(f"    *   **Institutional (Manager) View:** `{n['institutional_view']}`\n")
            if n['analytical_view'] != 'N/A':
                f.write(f"    *   **Analytical View:** `{n['analytical_view']}`\n")

            f.write(f"*   **Structural Signature Analysis:** {n['structural_signature']}\n")

            if n['related_gap_alert'] != 'N/A':
                f.write(f"*   **Related Gap/Alert:** {n['related_gap_alert']}\n")

            f.write(f"*   **Generated Omega:** {n['omega_question']}\n")
            f.write(f"*   **Suggested Resolution Strategy:**\n")
            f.write(f"    ```\n{n['resolution_strategy']}\n    ```\n\n")
            f.write("---\n\n")

def main():
    """
    Main function to run the reporter.
    """
    script_dir = Path(__file__).parent
    log_file = script_dir / '../outputs/output.txt'
    report_file = script_dir / '../outputs/scaffold_report.md'

    print("Parsing log file to find Scaffolds...")

    try:
        with open(log_file, 'r', encoding='utf-8') as f:
            log_content = f.read()
    except FileNotFoundError:
        print(f"Error: Log file not found at {log_file}", file=sys.stderr)
        sys.exit(1)

    scaffolds_data = parse_log_content(log_content)

    if scaffolds_data:
        print(f"Found {len(scaffolds_data)} unique Scaffolds.")
        print(f"Generating report at {report_file}...")
        generate_markdown_report(scaffolds_data, report_file)
        print("Report generated successfully.")
    else:
        print("No Scaffolds found in the log file.")
        with open(report_file, 'w', encoding='utf-8') as f:
            f.write("# Scaffold Diagnostic Report\n\n**Total Unique Scaffolds Found:** 0\n")

if __name__ == '__main__':
    main()
