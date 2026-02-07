"""
Repair script: Add narrative_ontology:constraint_metric/3 facts to testset files
that have domain_priors:base_extractiveness/2 but lack constraint_metric/3.

This bridges the data gap so drl_core:base_extractiveness/2 (which reads from
constraint_metric via config) can find these constraints. This enables the
logical fingerprint engine to compute shift patterns for all constraints.

What it does for each file:
1. Adds narrative_ontology:constraint_metric/3 to the multifile block
2. Adds constraint_metric facts mirroring domain_priors values:
   - base_extractiveness -> extractiveness
   - suppression_score -> suppression_requirement
   - theater_ratio -> theater_ratio (if present)

Safe: only adds lines, never modifies existing content.
"""

import os
import re
import sys


def extract_metric(content, metric_name):
    """Extract value from domain_priors:metric_name(ID, Value)."""
    pattern = rf"domain_priors:{metric_name}\(([^,]+),\s*([0-9.]+)\)"
    match = re.search(pattern, content)
    if match:
        return match.group(1), float(match.group(2))
    return None, None


def needs_repair(content):
    """Check if file needs constraint_metric facts added."""
    has_base_ext = 'domain_priors:base_extractiveness(' in content
    has_metric = 'narrative_ontology:constraint_metric(' in content
    return has_base_ext and not has_metric


def cleanup_previous_repair(content):
    """Remove incorrectly-placed constraint_metric facts from a previous repair run.

    Removes:
    - Lines matching 'narrative_ontology:constraint_metric(...)'
    - The comment line '% Constraint metric facts (bridge for classification engine)'
    - The multifile declaration 'narrative_ontology:constraint_metric/3,'

    Returns cleaned content (ready for re-repair) or None if no cleanup needed.
    """
    lines = content.split('\n')
    original_count = len(lines)

    # Remove constraint_metric fact lines and our comment
    cleaned = []
    skip_blank_after_removal = False
    for line in lines:
        stripped = line.strip()
        if stripped.startswith('narrative_ontology:constraint_metric(') and 'extractiveness' in stripped:
            skip_blank_after_removal = True
            continue
        if stripped.startswith('narrative_ontology:constraint_metric(') and 'suppression_requirement' in stripped:
            skip_blank_after_removal = True
            continue
        if stripped.startswith('narrative_ontology:constraint_metric(') and 'theater_ratio' in stripped:
            skip_blank_after_removal = True
            continue
        if stripped == '% Constraint metric facts (bridge for classification engine)':
            skip_blank_after_removal = True
            continue
        # Remove the multifile declaration we added
        if stripped == 'narrative_ontology:constraint_metric/3,':
            continue
        # Skip one blank line after a removal to avoid double-spacing
        if skip_blank_after_removal and stripped == '':
            skip_blank_after_removal = False
            continue
        skip_blank_after_removal = False
        cleaned.append(line)

    if len(cleaned) == original_count:
        return None  # Nothing was cleaned

    return '\n'.join(cleaned)


def add_multifile_declaration(content):
    """Add constraint_metric/3 to multifile block if not already there."""
    if 'narrative_ontology:constraint_metric/3' in content:
        return content

    # Find the multifile block and add constraint_metric/3
    # Look for the last line of the multifile block (ends with a period on its own line
    # or the last entry before the period)

    # Strategy: find the multifile declaration, then find a good insertion point
    # Most files have patterns like:
    #   :- multifile
    #       domain_priors:base_extractiveness/2,
    #       domain_priors:suppression_score/2,
    #       ...
    #       constraint_indexing:constraint_classification/3.

    # Insert before the last entry (the one ending with period)
    # Or after narrative_ontology entries if they exist

    # Find if there's already a narrative_ontology multifile entry
    if 'narrative_ontology:' in content.split(':- multifile')[1].split('.')[0] if ':- multifile' in content else False:
        # Add after existing narrative_ontology entries in multifile
        lines = content.split('\n')
        new_lines = []
        in_multifile = False
        added = False
        for i, line in enumerate(lines):
            new_lines.append(line)
            if ':- multifile' in line:
                in_multifile = True
            if in_multifile and not added:
                # Look for last narrative_ontology line or last line before period
                stripped = line.strip()
                if stripped.startswith('narrative_ontology:') and stripped.endswith(','):
                    new_lines.append('    narrative_ontology:constraint_metric/3,')
                    added = True
        if added:
            return '\n'.join(new_lines)

    # Fallback: insert before constraint_indexing:constraint_classification/3
    # which is typically the last entry in multifile blocks
    if 'constraint_indexing:constraint_classification/3' in content:
        content = content.replace(
            'constraint_indexing:constraint_classification/3',
            'narrative_ontology:constraint_metric/3,\n    constraint_indexing:constraint_classification/3'
        )
        return content

    # Another fallback: insert before the period that closes the multifile block
    # Find the multifile block
    multifile_match = re.search(r'(:- multifile\s*\n(?:.*\n)*?)(.*?\.)\n', content)
    if multifile_match:
        last_line = multifile_match.group(2).strip()
        if last_line.endswith('.'):
            # Change the period to comma, add our line with period
            old = multifile_match.group(2)
            new = old.rstrip().rstrip('.') + ',\n    narrative_ontology:constraint_metric/3.'
            content = content.replace(old, new, 1)
        return content

    return content


def find_insertion_point(content, constraint_id):
    """Find where to insert constraint_metric facts.

    Only matches FACT DECLARATIONS — lines at the left margin that contain
    a numeric value and end with a period (possibly followed by a comment).
    Ignores references inside test clauses (which are indented and use variables).
    """
    lines = content.split('\n')
    last_dp_line = -1
    # Pattern: line starts with domain_priors:metric( and contains a numeric literal
    fact_pattern = re.compile(
        r'^domain_priors:(base_extractiveness|suppression_score|theater_ratio)\(.+,\s*[0-9]+\.?[0-9]*\)'
    )
    for i, line in enumerate(lines):
        if fact_pattern.match(line):
            last_dp_line = i

    return last_dp_line


def repair_file(filepath, force_cleanup=False):
    """Add constraint_metric facts to a single file."""
    with open(filepath, 'r', encoding='utf-8') as f:
        content = f.read()

    # If force_cleanup, remove any previous repair artifacts first
    if force_cleanup:
        cleaned = cleanup_previous_repair(content)
        if cleaned is not None:
            content = cleaned

    if not needs_repair(content):
        return False, "No repair needed"

    # Extract values
    ext_id, ext_val = extract_metric(content, 'base_extractiveness')
    supp_id, supp_val = extract_metric(content, 'suppression_score')
    theater_id, theater_val = extract_metric(content, 'theater_ratio')

    if ext_id is None:
        return False, "Could not extract base_extractiveness"

    # Use the ID from base_extractiveness (most reliable)
    constraint_id = ext_id

    # Build constraint_metric facts
    metric_lines = []
    metric_lines.append('')
    metric_lines.append('% Constraint metric facts (bridge for classification engine)')
    metric_lines.append(f'narrative_ontology:constraint_metric({constraint_id}, extractiveness, {ext_val}).')
    if supp_val is not None:
        metric_lines.append(f'narrative_ontology:constraint_metric({constraint_id}, suppression_requirement, {supp_val}).')
    if theater_val is not None:
        metric_lines.append(f'narrative_ontology:constraint_metric({constraint_id}, theater_ratio, {theater_val}).')

    # Add multifile declaration
    content = add_multifile_declaration(content)

    # Find insertion point (after last domain_priors fact)
    lines = content.split('\n')
    insert_after = find_insertion_point(content, constraint_id)

    if insert_after == -1:
        return False, f"Could not find insertion point for {constraint_id}"

    # Insert the metric lines
    new_lines = lines[:insert_after + 1] + metric_lines + lines[insert_after + 1:]
    new_content = '\n'.join(new_lines)

    with open(filepath, 'w', encoding='utf-8') as f:
        f.write(new_content)

    metrics_added = 1 + (1 if supp_val else 0) + (1 if theater_val else 0)
    return True, f"Added {metrics_added} constraint_metric facts for {constraint_id}"


def main():
    testsets_dir = os.path.join(os.path.dirname(os.path.dirname(__file__)), 'prolog', 'testsets')

    force_cleanup = '--cleanup' in sys.argv
    file_args = [a for a in sys.argv[1:] if not a.startswith('--')]

    if file_args:
        files = file_args
    else:
        files = []
        for f in sorted(os.listdir(testsets_dir)):
            if f.endswith('.pl'):
                files.append(os.path.join(testsets_dir, f))

    if force_cleanup:
        print("Running with --cleanup: removing previous repair artifacts first\n")

    repaired = 0
    skipped = 0
    errors = 0

    for filepath in files:
        if not os.path.isabs(filepath):
            filepath = os.path.join(testsets_dir, filepath)

        try:
            success, msg = repair_file(filepath, force_cleanup=force_cleanup)
            basename = os.path.basename(filepath)
            if success:
                repaired += 1
                print(f"  [OK] {basename}: {msg}")
            else:
                if "No repair needed" in msg:
                    skipped += 1
                else:
                    errors += 1
                    print(f"  [SKIP] {basename}: {msg}", file=sys.stderr)
        except Exception as e:
            errors += 1
            print(f"  [ERR] {os.path.basename(filepath)}: {e}", file=sys.stderr)

    print(f"\n{'='*50}")
    print(f"  Repaired: {repaired}")
    print(f"  Skipped:  {skipped} (already complete)")
    print(f"  Errors:   {errors}")
    print(f"{'='*50}")


if __name__ == '__main__':
    main()
