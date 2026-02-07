import re
import sys
from pathlib import Path

def parse_log_content(content):
    """
    Parses the log content to find and extract True Mountain details.
    A True Mountain is defined as a constraint that is both claimed as a mountain
    and is classified as a mountain from all perspectives (no mismatches).
    """
    true_mountains = []
    
    scenario_chunks = re.split(r'(?=\[SCENARIO MANAGER\] Clearing Knowledge Base...)', content)

    for chunk in scenario_chunks:
        if not chunk.strip():
            continue

        name_match = re.search(r'Loading:.*?testsets/(.+?)\.pl', chunk)
        if not name_match:
            continue
        constraint_name = name_match.group(1)

        # Look for the main audit section
        audit_section_match = re.search(r'\[CONSTRAINT INVENTORY: INDEXICAL AUDIT\]\s*\n(.*?)(?=\n\n\[CROSS-DOMAIN ISOMORPHISM|\Z)', chunk, re.DOTALL)
        if not audit_section_match:
            continue
        
        audit_section = audit_section_match.group(1)

        # 1. Claimed Type must be 'mountain'
        claimed_type_match = re.search(r'^\s*Claimed Type: (mountain)', audit_section, re.MULTILINE)
        if not claimed_type_match:
            continue

        # 2. There must be at least one perspective, and NO mismatches
        if '(Mismatch)' in audit_section or 'Perspectives:' not in audit_section:
            continue
            
        # If we passed all checks, it's a True Mountain. Extract details.
        tm_data = {
            'name': constraint_name,
            'claimed_type': 'mountain',
            'structural_signature': 'N/A'
        }

        # Extract Structural Signature Analysis
        signature_match = re.search(r'→\s*(.+)', chunk)
        if signature_match:
            tm_data['structural_signature'] = signature_match.group(1).strip()
        
        true_mountains.append(tm_data)

    # Deduplicate (shouldn't be necessary with this logic, but good practice)
    unique_tms = []
    seen = set()
    for tm in true_mountains:
        identifier = tm['name']
        if identifier not in seen:
            unique_tms.append(tm)
            seen.add(identifier)

    return unique_tms

def generate_markdown_report(tm_data, output_path):
    """
    Generates a Markdown report from the list of True Mountain data.
    """
    sorted_tms = sorted(tm_data, key=lambda x: x['name'])

    with open(output_path, 'w', encoding='utf-8') as f:
        f.write("# True Mountain Validation Report\n\n")
        f.write(f"**Total Validated:** {len(sorted_tms)}\n\n")
        f.write("This report lists all constraints that are consistently classified as 'mountain' across all tested perspectives, confirming their immutability within the model.\n\n")
        f.write("---\n\n")

        for i, tm in enumerate(sorted_tms, 1):
            f.write(f"### {i}. True Mountain: `{tm['name']}`\n\n")
            f.write(f"*   **Claimed Type:** `{tm['claimed_type']}`\n")
            f.write(f"*   **Structural Signature Analysis:** {tm['structural_signature']}\n")
            f.write(f"*   **Perspectival Agreement:** Confirmed. All tested perspectives agree on the 'mountain' classification.\n\n")
            f.write("---\n\n")

def main():
    """
    Main function to run the reporter.
    """
    script_dir = Path(__file__).parent
    log_file = script_dir / '../outputs/output.txt'
    report_file = script_dir / '../outputs/true_mountain_report.md'
    
    print("Parsing log file to find True Mountains...")
    
    try:
        with open(log_file, 'r', encoding='utf-8') as f:
            log_content = f.read()
    except FileNotFoundError:
        print(f"Error: Log file not found at {log_file}", file=sys.stderr)
        sys.exit(1)
        
    tm_data = parse_log_content(log_content)
    
    if tm_data:
        print(f"Found {len(tm_data)} validated True Mountains.")
        print(f"Generating report at {report_file}...")
        generate_markdown_report(tm_data, report_file)
        print("Report generated successfully.")
    else:
        print("No validated True Mountains found in the log file.")
        with open(report_file, 'w', encoding='utf-8') as f:
            f.write("# True Mountain Validation Report\n\n**Total Validated:** 0\n")

if __name__ == '__main__':
    main()
