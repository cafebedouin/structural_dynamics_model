import os
import re
import json

TESTSETS_DIR = "../prolog/testsets/"
ANALYSIS_FILE = "../outputs/structured_analysis.json"

def analyze_gaps():
    results = {}
    files = [f for f in os.listdir(TESTSETS_DIR) if f.endswith('.pl')]

    # Regex to find the classification type used in the indexing block
    type_regex = re.compile(r"constraint_classification\(\s*[^,]+,\s*([a-zA-Z0-9_]+),")

    print(f"{'DOMAIN':<45} | {'ROPE':<5} | {'NOOSE':<5} | {'MTN':<5} | {'STATUS'}")
    print("-" * 85)

    for filename in sorted(files):
        path = os.path.join(TESTSETS_DIR, filename)
        domain_id = filename.replace('.pl', '')

        with open(path, 'r') as f:
            content = f.read()
            found_types = type_regex.findall(content)

        # Check for the presence of each pillar
        has_rope = "rope" in found_types
        has_noose = "noose" in found_types
        has_mountain = "mountain" in found_types

        status = "COMPLETE" if (has_rope and has_noose and has_mountain) else "GAP"

        # Color/Visual indicators for terminal
        r_mark = "✓" if has_rope else "✗"
        n_mark = "✓" if has_noose else "✗"
        m_mark = "✓" if has_mountain else "✗"

        print(f"{domain_id:<45} |  {r_mark}   |  {n_mark}   |  {m_mark}   | {status}")

        results[domain_id] = {
            "rope": has_rope,
            "noose": has_noose,
            "mountain": has_mountain,
            "is_complete": status == "COMPLETE"
        }

    # Save to a report for LLM evaluation
    with open("../outputs/gap_report.json", "w") as out:
        json.dump(results, out, indent=4)

    summary = sum(1 for v in results.values() if v["is_complete"])
    print(f"\n✓ Analysis Complete: {summary}/{len(files)} files are perspectivally complete.")

if __name__ == "__main__":
    analyze_gaps()
