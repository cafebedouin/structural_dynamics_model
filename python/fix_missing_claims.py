#!/usr/bin/env python3
"""Add missing constraint_claim/2 facts to testset files.

For each file missing constraint_claim, extracts the type from the
constraint_classification with agent_power(analytical) and adds the claim.
Also adds narrative_ontology:constraint_claim/2 to the multifile declaration
if missing.
"""

import os
import re
import sys

TESTSETS_DIR = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))),
                            "prolog", "testsets")

VALID_TYPES = {'mountain', 'rope', 'tangled_rope', 'snare', 'scaffold', 'piton'}


def fix_file(filepath):
    with open(filepath, 'r', encoding='utf-8') as f:
        content = f.read()

    # Skip if already has a valid constraint_claim
    if re.search(r'constraint_claim\(\w+,\s*(?:' + '|'.join(VALID_TYPES) + r')\)', content):
        return None, "already has valid claim"

    # Extract constraint_id from the analytical classification
    m = re.search(
        r'constraint_classification\((\w+),\s*(\w+),\s*\n?\s*context\(agent_power\(analytical\)',
        content)
    if not m:
        return None, "no analytical classification found"

    constraint_id = m.group(1)
    analytical_type = m.group(2)

    if analytical_type not in VALID_TYPES:
        return None, f"analytical type '{analytical_type}' not valid"

    claim_line = f"narrative_ontology:constraint_claim({constraint_id}, {analytical_type})."
    modified = False

    # 1. Add constraint_claim/2 to multifile declaration if missing
    if 'narrative_ontology:constraint_claim/2' not in content:
        # Insert before constraint_indexing:constraint_classification/3.
        content = content.replace(
            '    constraint_indexing:constraint_classification/3.',
            '    narrative_ontology:constraint_claim/2,\n'
            '    constraint_indexing:constraint_classification/3.',
            1
        )
        modified = True

    # 2. Add the constraint_claim fact
    # Try to insert after constraint_metric block, before beneficiary/victim
    # Look for a good insertion point
    insertion_patterns = [
        # After the last constraint_metric line
        (r'(narrative_ontology:constraint_metric\([^)]+\)\.\n)(?!narrative_ontology:constraint_metric)',
         r'\1\n% Constraint classification claim\n' + claim_line + '\n'),
        # After theater_ratio domain_priors
        (r'(domain_priors:theater_ratio\([^)]+\)\.\n)',
         r'\1\n' + claim_line + '\n'),
    ]

    inserted = False
    for pattern, replacement in insertion_patterns:
        new_content = re.sub(pattern, replacement, content, count=1)
        if new_content != content:
            content = new_content
            inserted = True
            modified = True
            break

    if not inserted:
        # Fallback: insert before first constraint_classification
        content = content.replace(
            'constraint_indexing:constraint_classification(',
            claim_line + '\n\nconstraint_indexing:constraint_classification(',
            1
        )
        modified = True

    if modified:
        with open(filepath, 'w', encoding='utf-8') as f:
            f.write(content)
        return analytical_type, "fixed"

    return None, "no changes needed"


def main():
    fixed = 0
    skipped = 0
    errors = 0

    for filename in sorted(os.listdir(TESTSETS_DIR)):
        if not filename.endswith('.pl'):
            continue
        filepath = os.path.join(TESTSETS_DIR, filename)

        # Quick check: does it need fixing?
        with open(filepath, 'r', encoding='utf-8') as f:
            content = f.read()
        if 'constraint_classification(' not in content:
            continue
        if re.search(r'constraint_claim\(\w+,\s*(?:' + '|'.join(VALID_TYPES) + r')\)', content):
            continue

        result_type, status = fix_file(filepath)
        if status == "fixed":
            print(f"FIXED: {filename} -> {result_type}")
            fixed += 1
        elif "no analytical" in status:
            print(f"ERROR: {filename} - {status}")
            errors += 1
        else:
            skipped += 1

    print(f"\n{'='*50}")
    print(f"  Fixed:   {fixed}")
    print(f"  Skipped: {skipped}")
    print(f"  Errors:  {errors}")
    print(f"{'='*50}")


if __name__ == "__main__":
    main()
