import re
import sys
from pathlib import Path

def parse_log_content(content):
    """
    Parses the log content to find and extract False Mountain details.
    """
    false_mountains = []
    
    # Split the entire log into chunks, each starting with a new scenario load.
    # The delimiter is a lookahead assertion to keep the delimiter string.
    scenario_chunks = re.split(r'(?=\[SCENARIO MANAGER\] Clearing Knowledge Base...)', content)

    for chunk in scenario_chunks:
        if not chunk.strip():
            continue

        # Get the name of the constraint being tested in this chunk
        name_match = re.search(r'Loading:.*?testsets/(.+?)\.pl', chunk)
        if not name_match:
            continue
        constraint_name = name_match.group(1)

        # Split the scenario chunk by perspectival gaps, which are our triggers
        gap_chunks = re.split(r'(?=! ALERT:|! GAP:)', chunk)
        
        for gap_chunk in gap_chunks:
            if not gap_chunk.startswith('! ALERT:') and not gap_chunk.startswith('! GAP:'):
                continue
            
            fm_data = {
                'name': constraint_name,
                'severity': 'N/A',
                'powerless_view': 'N/A',
                'institutional_view': 'N/A',
                'resolution_strategy': ''
            }

            # Extract gap and perspectives
            gap_match = re.search(r'(! (?:ALERT|GAP): .+)', gap_chunk)
            if gap_match:
                fm_data['gap_detected'] = gap_match.group(1).strip()

            # More robust perspective extraction
            pv_match = re.search(r'(?:Individual \(Powerless\)|Powerless see): (\w+)', gap_chunk)
            if pv_match:
                fm_data['powerless_view'] = pv_match.group(1)

            iv_match = re.search(r'(?:Institutional \(Manager\)|Institutions see): (\w+)', gap_chunk)
            if iv_match:
                fm_data['institutional_view'] = iv_match.group(1)

            # Extract Omega
            omega_match = re.search(r'Ω: (.+)', gap_chunk)
            if omega_match:
                fm_data['omega_question'] = omega_match.group(1).strip()
            else:
                fm_data['omega_question'] = "N/A"

            # Extract Severity from the Triage section
            if '[critical]' in gap_chunk:
                fm_data['severity'] = 'critical'
            elif '[high]' in gap_chunk:
                fm_data['severity'] = 'high'
            
            # Extract Resolution Strategy
            res_match = re.search(r'RESOLUTION STRATEGY:\s*\n(.*?)(?:\n\s*└─|\Z)', gap_chunk, re.DOTALL)
            if res_match:
                strategy = res_match.group(1).strip()
                # Clean up the left-aligned junk from the box drawing
                cleaned_strategy = "\n".join(line.strip().lstrip('│').lstrip() for line in strategy.split('\n'))
                fm_data['resolution_strategy'] = cleaned_strategy

            # Add to list if we have the core info
            if 'gap_detected' in fm_data:
                false_mountains.append(fm_data)

    # Deduplicate entries based on constraint name and the gap detected
    unique_fms = []
    seen = set()
    for fm in false_mountains:
        identifier = (fm['name'], fm['gap_detected'])
        if identifier not in seen:
            unique_fms.append(fm)
            seen.add(identifier)

    return unique_fms

def generate_markdown_report(false_mountains, output_path):
    """
    Generates a Markdown report from the list of False Mountain data.
    """
    # Sort by severity (critical first), then by name
    sorted_fms = sorted(false_mountains, key=lambda x: (x['severity'] != 'critical', x['name']))

    with open(output_path, 'w', encoding='utf-8') as f:
        f.write("# False Mountain Diagnostic Report\n\n")
        f.write(f"**Total Unique False Mountains Found:** {len(sorted_fms)}\n\n")
        f.write("---\n\n")

        for i, fm in enumerate(sorted_fms, 1):
            f.write(f"### {i}. False Mountain: `{fm['name']}`\n\n")
            f.write(f"*   **Severity:** `{fm['severity']}`\n")
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
    """
    Main function to run the reporter.
    """
    # Using pathlib for more robust path handling
    script_dir = Path(__file__).parent
    log_file = script_dir / '../outputs/output.txt'
    report_file = script_dir / '../outputs/false_mountain_report.md'
    
    print("Parsing log file to find False Mountains...")
    
    try:
        with open(log_file, 'r', encoding='utf-8') as f:
            log_content = f.read()
    except FileNotFoundError:
        print(f"Error: Log file not found at {log_file}", file=sys.stderr)
        sys.exit(1)
        
    false_mountains_data = parse_log_content(log_content)
    
    if false_mountains_data:
        print(f"Found {len(false_mountains_data)} unique False Mountains.")
        print(f"Generating report at {report_file}...")
        generate_markdown_report(false_mountains_data, report_file)
        print("Report generated successfully.")
    else:
        print("No False Mountains found in the log file.")
        with open(report_file, 'w', encoding='utf-8') as f:
            f.write("# False Mountain Diagnostic Report\n\n**Total Unique False Mountains Found:** 0\n")

if __name__ == '__main__':
    main()
