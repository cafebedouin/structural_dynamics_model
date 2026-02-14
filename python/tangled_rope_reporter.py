import re
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
from orbit_utils import load_orbit_data, get_orbit_signature, format_orbit_signature

def parse_log_content(content):
    """
    Parses the log content to find and extract details for any constraint
    that is classified as a Tangled Rope from any perspective.
    """
    tangled_ropes = []
    
    # Split the entire log into chunks, each starting with a new scenario load.
    scenario_chunks = re.split(r'(?=\[SCENARIO MANAGER\] Clearing Knowledge Base...)', content)

    for chunk in scenario_chunks:
        if not chunk.strip():
            continue

        # Get the name of the constraint being tested in this chunk
        name_match = re.search(r'Loading:.*?testsets/(.+?)\.pl', chunk)
        if not name_match:
            continue
        constraint_name = name_match.group(1)

        # The trigger: Find 'tangled_rope' in any perspective classification
        perspectives_section_match = re.search(r'\[CONSTRAINT INVENTORY: INDEXICAL AUDIT\]\s*\n(.*?)(?=\n\n\[CROSS-DOMAIN ISOMORPHISM|\Z)', chunk, re.DOTALL)
        if not perspectives_section_match:
            continue

        perspectives_section = perspectives_section_match.group(1)
        if 'tangled_rope' not in perspectives_section:
            continue # This scenario does not contain a tangled rope classification.

        tr_data = {
            'name': constraint_name,
            'claimed_type': 'N/A',
            'powerless_view': 'N/A',
            'institutional_view': 'N/A',
            'analytical_view': 'N/A',
            'structural_signature': 'N/A',
            'related_gap_alert': 'N/A',
            'omega_question': 'N/A',
            'severity': 'N/A',
            'resolution_strategy': ''
        }

        # Extract original claimed type
        claimed_type_match = re.search(r'^\s*Claimed Type: (\w+)', perspectives_section, re.MULTILINE)
        if claimed_type_match:
            tr_data['claimed_type'] = claimed_type_match.group(1)

        # FINAL FIX: More robust perspective parsing
        for line in perspectives_section.split('\n'):
            # Match the pattern like "- [context...]: classification (Match...)"
            match = re.search(r'-\s\[context\(.*?\)\]:\s*(\w+)', line)
            if not match:
                continue
            
            classification = match.group(1)
            if 'powerless' in line:
                tr_data['powerless_view'] = classification
            elif 'institutional' in line:
                tr_data['institutional_view'] = classification
            elif 'analytical' in line:
                tr_data['analytical_view'] = classification

        # Extract Structural Signature Analysis (non-greedy)
        signature_match = re.search(r'→\s*(.+)', chunk)
        if signature_match:
            tr_data['structural_signature'] = signature_match.group(1).strip()
        
        # Extract related gap/alert (if any)
        gap_alert_match = re.search(r'(!\s(?:ALERT|GAP):\s*.+)', chunk)
        if gap_alert_match:
            tr_data['related_gap_alert'] = gap_alert_match.group(1).strip()

        # Extract Omega Question specifically
        omega_question_match = re.search(r'Question:\s*(.+)', chunk)
        if omega_question_match:
            tr_data['omega_question'] = omega_question_match.group(1).strip()

        # Extract Severity from the Triage section
        triage_section_match = re.search(r'\[OMEGA TRIAGE & PRIORITIZATION\](.*?)(?=\n\n|\n\[OMEGA RESOLUTION)', chunk, re.DOTALL)
        if triage_section_match:
            if '[critical]' in triage_section_match.group(1):
                tr_data['severity'] = 'critical'
            elif '[high]' in triage_section_match.group(1):
                tr_data['severity'] = 'high'
        
        # Extract Resolution Strategy
        res_match = re.search(r'RESOLUTION STRATEGY:\s*\n(.*?)(?:\n\s*└─|\n\n### START LLM REFINEMENT MANIFEST|\Z)', chunk, re.DOTALL)
        if res_match:
            strategy = res_match.group(1).strip()
            cleaned_strategy = "\n".join(line.strip().lstrip('│').lstrip() for line in strategy.split('\n'))
            tr_data['resolution_strategy'] = cleaned_strategy.strip()

        tangled_ropes.append(tr_data)

    # Deduplicate
    unique_trs = []
    seen = set()
    for tr in tangled_ropes:
        identifier = (tr['name'], tr['omega_question']) 
        if identifier not in seen:
            unique_trs.append(tr)
            seen.add(identifier)

    return unique_trs

def generate_markdown_report(tr_data, output_path, orbit_data=None):
    """
    Generates a Markdown report from the list of Tangled Rope data.
    """
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
    """
    Main function to run the reporter.
    """
    script_dir = Path(__file__).parent
    log_file = script_dir / '../outputs/output.txt'
    report_file = script_dir / '../outputs/tangled_rope_report.md'
    
    print("Parsing log file to find Tangled Ropes (based on analyzed perspectives)...")
    
    try:
        with open(log_file, 'r', encoding='utf-8') as f:
            log_content = f.read()
    except FileNotFoundError:
        print(f"Error: Log file not found at {log_file}", file=sys.stderr)
        sys.exit(1)
        
    tr_data = parse_log_content(log_content)
    orbit_data = load_orbit_data()

    if tr_data:
        print(f"Found {len(tr_data)} constraints classified as Tangled Ropes.")
        print(f"Generating report at {report_file}...")
        generate_markdown_report(tr_data, report_file, orbit_data)
        print("Report generated successfully.")
    else:
        print("No constraints classified as Tangled Ropes found in the log file.")
        with open(report_file, 'w', encoding='utf-8') as f:
            f.write("# Tangled Rope Diagnostic Report\n\n**Total Unique Tangled Ropes Found:** 0\n")

if __name__ == '__main__':
    main()
