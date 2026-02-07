import re
import sys
from pathlib import Path

def parse_log_content(content):
    """
    Parses the log content to find and extract Rope details.
    A Rope is defined as a constraint that is claimed as a rope
    and is classified as a rope from all perspectives (no mismatches).
    """
    ropes = []
    
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

        # 1. Claimed Type must be 'rope'
        claimed_type_match = re.search(r'^\s*Claimed Type: (rope)', audit_section, re.MULTILINE)
        if not claimed_type_match:
            continue

        # 2. There must be at least one perspective, and NO mismatches
        # Also ensure no Mismatch in the whole perspectives section
        if '(Mismatch)' in audit_section or 'Perspectives:' not in audit_section:
            continue

        # And explicitly check that all perspectives are 'rope'
        all_perspectives_are_rope = True
        perspectives_lines = re.findall(r'-\s*\[context\(.*?\)\]:\s*(\w+)', audit_section)
        if not perspectives_lines: # No perspectives listed
            all_perspectives_are_rope = False
        else:
            for classification in perspectives_lines:
                if classification != 'rope':
                    all_perspectives_are_rope = False
                    break
        
        if not all_perspectives_are_rope:
            continue
            
        # If we passed all checks, it's a True Rope. Extract details.
        rope_data = {
            'name': constraint_name,
            'claimed_type': 'rope',
            'structural_signature': 'N/A',
            'related_gap_alert': 'N/A',
            'omega_question': 'N/A',
            'resolution_strategy': ''
        }

        # Extract Structural Signature Analysis
        signature_match = re.search(r'→\s*(.+)', chunk)
        if signature_match:
            rope_data['structural_signature'] = signature_match.group(1).strip()
        
        # Extract related gap/alert (if any)
        gap_alert_match = re.search(r'(!\s(?:ALERT|GAP):\s*.+)', chunk)
        if gap_alert_match:
            rope_data['related_gap_alert'] = gap_alert_match.group(1).strip()

        # Extract Omega Question specifically
        omega_question_match = re.search(r'Question:\s*(.+)', chunk)
        if omega_question_match:
            rope_data['omega_question'] = omega_question_match.group(1).strip()
        
        # Extract Resolution Strategy
        res_match = re.search(r'RESOLUTION STRATEGY:\s*\n(.*?)(?:\n\s*└─|\n\n### START LLM REFINEMENT MANIFEST|\Z)', chunk, re.DOTALL)
        if res_match:
            strategy = res_match.group(1).strip()
            cleaned_strategy = "\n".join(line.strip().lstrip('│').lstrip() for line in strategy.split('\n'))
            rope_data['resolution_strategy'] = cleaned_strategy.strip()

        ropes.append(rope_data)

    # Deduplicate
    unique_ropes = []
    seen = set()
    for r in ropes:
        identifier = r['name']
        if identifier not in seen:
            unique_ropes.append(r)
            seen.add(identifier)

    return unique_ropes

def generate_markdown_report(rope_data, output_path):
    """
    Generates a Markdown report from the list of Rope data.
    """
    sorted_ropes = sorted(rope_data, key=lambda x: x['name'])

    with open(output_path, 'w', encoding='utf-8') as f:
        f.write("# Rope Validation Report\n\n")
        f.write(f"**Total Validated:** {len(sorted_ropes)}\n\n")
        f.write("This report lists all constraints that are consistently classified as 'rope' across all tested perspectives, indicating their functional and potentially beneficial nature within the model.\n\n")
        f.write("---\n\n")

        for i, r in enumerate(sorted_ropes, 1):
            f.write(f"### {i}. Rope: `{r['name']}`\n\n")
            f.write(f"*   **Claimed Type:** `{r['claimed_type']}`\n")
            f.write(f"*   **Structural Signature Analysis:** {r['structural_signature']}\n")
            f.write(f"*   **Perspectival Agreement:** Confirmed. All tested perspectives agree on the 'rope' classification.\n")
            
            if r['related_gap_alert'] != 'N/A':
                f.write(f"*   **Related Gap/Alert:** {r['related_gap_alert']}\n")
            if r['omega_question'] != 'N/A':
                f.write(f"*   **Generated Omega:** {r['omega_question']}\n")
            if r['resolution_strategy'] != '':
                f.write(f"*   **Suggested Resolution Strategy:**\n")
                f.write(f"    ```\n{r['resolution_strategy']}\n    ```\n\n")
            else:
                f.write("\n") # Add a newline if no strategy to maintain spacing
            f.write("---\n\n")

def main():
    """
    Main function to run the reporter.
    """
    script_dir = Path(__file__).parent
    log_file = script_dir / '../outputs/output.txt'
    report_file = script_dir / '../outputs/rope_report.md'
    
    print("Parsing log file to find Ropes...")
    
    try:
        with open(log_file, 'r', encoding='utf-8') as f:
            log_content = f.read()
    except FileNotFoundError:
        print(f"Error: Log file not found at {log_file}", file=sys.stderr)
        sys.exit(1)
        
    rope_data = parse_log_content(log_content)
    
    if rope_data:
        print(f"Found {len(rope_data)} validated Ropes.")
        print(f"Generating report at {report_file}...")
        generate_markdown_report(rope_data, report_file)
        print("Report generated successfully.")
    else:
        print("No validated Ropes found in the log file.")
        with open(report_file, 'w', encoding='utf-8') as f:
            f.write("# Rope Validation Report\n\n**Total Validated:** 0\n")

if __name__ == '__main__':
    main()
