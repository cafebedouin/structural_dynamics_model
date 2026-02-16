import json
import re
import sys
from pathlib import Path

def parse_log_content(content):
    """
    Parses the log content to find and extract all Omega details.
    """
    omegas = []
    
    scenario_chunks = re.split(r'(?=\[SCENARIO MANAGER\] Clearing Knowledge Base...)', content)

    for chunk in scenario_chunks:
        if not chunk.strip() or 'Ω:' not in chunk:
            continue

        name_match = re.search(r'Loading:.*?testsets/(.+?)\.pl', chunk)
        if not name_match:
            continue
        constraint_name = name_match.group(1)

        # Split the scenario chunk by Omega Generation sections
        omega_gen_sections = re.split(r'(?=\[OMEGA GENERATION FROM PERSPECTIVAL GAPS:)', chunk)

        for omega_chunk in omega_gen_sections:
            if 'Ω:' not in omega_chunk:
                continue

            omega_data = {
                'name': 'N/A',
                'severity': 'N/A',
                'associated_constraint': constraint_name,
                'source_gap': 'N/A',
                'question': 'N/A',
                'resolution_strategy': ''
            }

            # Extract Omega variable name, question, and source
            omega_name_match = re.search(r'Ω:\s*(\w+)', omega_chunk)
            if omega_name_match:
                omega_data['name'] = omega_name_match.group(1)

            omega_question_match = re.search(r'Question:\s*(.+)', omega_chunk)
            if omega_question_match:
                omega_data['question'] = omega_question_match.group(1).strip()
            
            source_gap_match = re.search(r'Source:\s*(.+)', omega_chunk)
            if source_gap_match:
                omega_data['source_gap'] = source_gap_match.group(1).strip()

            # Find the corresponding Triage and Resolution sections
            triage_chunk = omega_chunk
            # To get the right triage/resolution, we need to search in the remainder of the chunk
            if omega_data['name'] != 'N/A':
                # Narrow down the search area to after the omega name
                search_area_match = re.search(re.escape(omega_data['name']) + r'(.*)', omega_chunk, re.DOTALL)
                if search_area_match:
                    search_area = search_area_match.group(1)
                    
                    triage_section_match = re.search(r'\[OMEGA TRIAGE & PRIORITIZATION\](.*?)(?=\[OMEGA RESOLUTION)', search_area, re.DOTALL)
                    if triage_section_match:
                        if '[critical]' in triage_section_match.group(1):
                            omega_data['severity'] = 'critical'
                        elif '[high]' in triage_section_match.group(1):
                            omega_data['severity'] = 'high'

                    res_match = re.search(r'RESOLUTION STRATEGY:\s*\n(.*?)(?:\n\s*└─|\Z)', search_area, re.DOTALL)
                    if res_match:
                        strategy = res_match.group(1).strip()
                        cleaned_strategy = "\n".join(line.strip().lstrip('│').lstrip() for line in strategy.split('\n'))
                        omega_data['resolution_strategy'] = cleaned_strategy.strip()

            if omega_data['name'] != 'N/A':
                omegas.append(omega_data)


    # Deduplicate based on the unique name of the Omega
    unique_omegas = []
    seen = set()
    for o in omegas:
        identifier = o['name']
        if identifier not in seen:
            unique_omegas.append(o)
            seen.add(identifier)

    return unique_omegas

def generate_markdown_report(omega_data, output_path):
    """
    Generates a Markdown report from the list of Omega data.
    """
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
    """
    Main function to run the reporter.
    """
    script_dir = Path(__file__).parent
    log_file = script_dir / '../outputs/output.txt'
    report_file = script_dir / '../outputs/omega_report.md'
    
    print("Parsing log file to find Omegas...")
    
    try:
        with open(log_file, 'r', encoding='utf-8') as f:
            log_content = f.read()
    except FileNotFoundError:
        print(f"Error: Log file not found at {log_file}", file=sys.stderr)
        sys.exit(1)
        
    omega_data = parse_log_content(log_content)
    
    # Write JSON intermediate for downstream enrichment
    json_file = script_dir / '../outputs/omega_data.json'
    with open(json_file, 'w', encoding='utf-8') as f:
        json.dump(omega_data, f, indent=2)
    print(f"JSON data written to {json_file}")

    if omega_data:
        print(f"Found {len(omega_data)} unique Omegas.")
        print(f"Generating report at {report_file}...")
        generate_markdown_report(omega_data, report_file)
        print("Report generated successfully.")
    else:
        print("No Omegas found in the log file.")
        with open(report_file, 'w', encoding='utf-8') as f:
            f.write("# Omega Epistemological Gap Report\n\n**Total Unique Omegas Found:** 0\n")

if __name__ == '__main__':
    main()
