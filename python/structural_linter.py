import os
import re
import sys

def lint_file(filepath):
    with open(filepath, 'r') as f:
        content = f.read()

    errors = []

    # 1. Modular Check: Does the file have a pillar OR a category?
    # If the file uses domain_category, we assume the pillars are in the priors.
    is_categorized = "domain_category(" in content
    has_any_pillar = re.search(r'constraint_(classification|claim)\(', content)

    if not (is_categorized or has_any_pillar):
        errors.append("MISSING_STRUCTURE: File lacks both explicit pillars and category assignment.")

    # 2. Mandatrophy Gate: Only flag high-extraction facts
    # We only care about the Noose/Rope dimension if the 'Mountain' is predatory.
    ext_match = re.search(r'base_extractiveness\(.*,\s*([\d\.]+)\)', content)
    if ext_match:
        val = float(ext_match.group(1))
        if val > 0.7 and "[RESOLVED MANDATROPHY]" not in content:
            errors.append(f"UNRESOLVED_OMEGA: High extraction ({val}) requires indexical resolution.")

    # 3. Ontological Gate: Allow standard v3.1 terms
    pillars = ['mountain', 'rope', 'noose', 'tangled_rope', 'mandatrophy']
    illegal_ontologies = re.findall(r'constraint_claim\(.*,\s*([a-z_]+)\)', content)
    for ont in illegal_ontologies:
        if ont not in pillars:
            errors.append(f"ILLEGAL_ONTOLOGY: {ont}")

    return errors

if __name__ == "__main__":
    target_dir = "../prolog/testsets"
    total_errors = 0
    for root, _, files in os.walk(target_dir):
        for file in files:
            if file.endswith('.pl'):
                errs = lint_file(os.path.join(root, file))
                if errs:
                    print(f"❌ {file} FAILED LINT:")
                    for e in errs:
                        print(f"  - {e}")
                    total_errors += len(errs)

    if total_errors > 0:
        print(f"\nFATAL: {total_errors} structural violations found.")
        sys.exit(1)
    else:
        print("✅ Indexical Relativity Gate Passed (Inheritance Mode).")
        sys.exit(0)
