# python3 clean_log.py audit.log > signal_only.txt


import sys
import re

def extract_signal(log_content):
    # Regex patterns for key signal elements
    domain_pattern = re.compile(r'^\[\d+\] DOMAIN:.*')
    omega_pattern = re.compile(r'^\s*- Ω_.*')
    summary_delimiter = "===================================================="
    error_keywords = ["ERROR:", "[FAIL]", "Syntax error", "Warning:"]
    final_tally = re.compile(r'^DONE: \d+ Passed, \d+ Failed')

    signal = []
    in_summary_block = False

    lines = log_content.splitlines()

    for line in lines:
        clean_line = line.strip()

        # 1. Capture Domain Headers
        if domain_pattern.match(clean_line):
            signal.append(f"\n{clean_line}")
            continue

        # 2. Capture Errors and Redefinition Warnings
        if any(kw in clean_line for kw in error_keywords):
            signal.append(f"  >>> SIGNAL_ALERT: {clean_line}")
            continue

        # 3. Capture Omega Variables (The conceptual/empirical pivots)
        if omega_pattern.match(line): # use original line to preserve indentation
            signal.append(line.rstrip())
            continue

        # 4. Capture Executive Summary Blocks (The "Meat")
        if summary_delimiter in clean_line:
            in_summary_block = not in_summary_block
            signal.append(summary_delimiter)
            continue

        if in_summary_block:
            signal.append(line.rstrip())
            continue

        # 5. Capture the final status tally
        if final_tally.match(clean_line):
            signal.append(f"\n{clean_line}")

    return "\n".join(signal)

if __name__ == "__main__":
    # If you save your log to 'audit.log', run: python script.py audit.log
    if len(sys.argv) > 1:
        with open(sys.argv[1], 'r', encoding='utf-8') as f:
            content = f.read()
    else:
        # Fallback for demonstration: read from stdin
        content = sys.stdin.read()

    print(extract_signal(content))
