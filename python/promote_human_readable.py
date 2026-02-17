#!/usr/bin/env python3
"""promote_human_readable.py

Promotes human_readable from comment metadata to queryable Prolog facts
across all testset .pl files in prolog/testsets/.

For each testset file:
1. Extracts constraint_id and human_readable from the comment block
2. Adds narrative_ontology:human_readable/2 to the :- multifile declaration
3. Adds narrative_ontology:human_readable(ID, "Title"). fact after constraint_claim

Usage:
    python python/promote_human_readable.py [--dry-run]
"""

import re
import sys
from pathlib import Path

SCRIPT_DIR = Path(__file__).resolve().parent
PROJECT_ROOT = SCRIPT_DIR.parent
TESTSETS_DIR = PROJECT_ROOT / "prolog" / "testsets"


def extract_human_readable_from_comment(content):
    """Extract human_readable value from comment block."""
    m = re.search(r'\*\s*human_readable:\s*(.+?)\s*$', content, re.MULTILINE)
    if m:
        return m.group(1).strip()
    return None


def extract_constraint_id_from_comment(content):
    """Extract constraint_id from comment block."""
    m = re.search(r'\*\s*constraint_id:\s*(\S+)', content)
    if m:
        return m.group(1).strip()
    return None


def extract_constraint_id_from_fact(content):
    """Extract constraint_id from constraint_claim fact (fallback)."""
    m = re.search(r'narrative_ontology:constraint_claim\((\w+)\s*,', content)
    if m:
        return m.group(1)
    return None


def already_processed(content):
    """Check if file already has human_readable/2 (idempotent)."""
    return bool(re.search(r'narrative_ontology:human_readable\(', content))


def clean_for_prolog_string(value):
    """Clean human_readable value for use in a Prolog double-quoted string.

    - Strips outer wrapping double quotes if the entire value is quoted
      and contains no inner double quotes (formatting convention).
    - Escapes remaining backslashes and double quotes for Prolog syntax.
    """
    if value.startswith('"') and value.endswith('"') and '"' not in value[1:-1]:
        value = value[1:-1]
    value = value.replace('\\', '\\\\')
    value = value.replace('"', '\\"')
    return value


def add_multifile_declaration(content):
    """Add narrative_ontology:human_readable/2 to the :- multifile block.

    Strategy: Find the last entry of the multifile block (ending with '.'),
    change its '.' to ',', and append human_readable/2 as the new last entry.
    """
    lines = content.split('\n')
    in_multifile = False
    insert_idx = -1

    for i, line in enumerate(lines):
        stripped = line.strip()
        if ':- multifile' in stripped:
            in_multifile = True
            if stripped.endswith('.') and '/' in stripped:
                insert_idx = i
                break
            continue
        if in_multifile:
            if stripped.endswith('.') and stripped != '':
                insert_idx = i
                break
            elif stripped.endswith(',') or stripped == '':
                continue
            else:
                break

    if insert_idx < 0:
        return content, False

    last_line = lines[insert_idx]
    leading_spaces = len(last_line) - len(last_line.lstrip())
    indent = last_line[:leading_spaces] if leading_spaces > 0 else '    '

    # Change the closing '.' to ','
    lines[insert_idx] = last_line.rstrip().rstrip('.') + ','

    # Insert the new entry
    lines.insert(insert_idx + 1, f'{indent}narrative_ontology:human_readable/2.')

    return '\n'.join(lines), True


def add_human_readable_fact(content, constraint_id, human_readable_value):
    """Add human_readable/2 fact after the constraint_claim fact line."""
    escaped = clean_for_prolog_string(human_readable_value)
    new_fact = f'narrative_ontology:human_readable({constraint_id}, "{escaped}").'

    # Find the constraint_claim fact for this ID
    pattern = re.compile(
        r'(narrative_ontology:constraint_claim\('
        + re.escape(constraint_id)
        + r',\s*\w+\)\.[^\n]*\n)',
    )

    m = pattern.search(content)
    if m:
        insert_pos = m.end()
        return content[:insert_pos] + new_fact + '\n' + content[insert_pos:], True

    return content, False


def process_file(filepath, dry_run=False):
    """Process a single testset file. Returns (status, message)."""
    with open(filepath, 'r', encoding='utf-8') as f:
        content = f.read()

    filename = filepath.name

    # Idempotency check
    if already_processed(content):
        return 'skipped', f'{filename}: already has human_readable/2 fact'

    # Extract human_readable from comment
    human_readable = extract_human_readable_from_comment(content)
    if not human_readable:
        return 'missing', f'{filename}: no human_readable comment found'

    # Extract constraint_id (prefer fact, fallback to comment)
    cid_from_fact = extract_constraint_id_from_fact(content)
    cid_from_comment = extract_constraint_id_from_comment(content)

    constraint_id = cid_from_fact or cid_from_comment
    if not constraint_id:
        return 'error', f'{filename}: could not determine constraint_id'

    if cid_from_fact and cid_from_comment and cid_from_fact != cid_from_comment:
        print(f"  [NOTE] {filename}: comment says '{cid_from_comment}', "
              f"fact uses '{cid_from_fact}' — using fact ID")
        constraint_id = cid_from_fact

    # Add multifile declaration
    content, decl_ok = add_multifile_declaration(content)
    if not decl_ok:
        return 'error', f'{filename}: could not find multifile block'

    # Add the fact
    content, fact_ok = add_human_readable_fact(content, constraint_id, human_readable)
    if not fact_ok:
        return 'error', f'{filename}: could not insert fact (no constraint_claim found?)'

    if not dry_run:
        with open(filepath, 'w', encoding='utf-8') as f:
            f.write(content)

    title_preview = human_readable[:50] + ('...' if len(human_readable) > 50 else '')
    return 'modified', f'{filename}: human_readable({constraint_id}, "{title_preview}")'


def main():
    dry_run = '--dry-run' in sys.argv

    if dry_run:
        print("=== DRY RUN MODE (no files will be modified) ===\n")

    if not TESTSETS_DIR.is_dir():
        print(f"ERROR: Testsets directory not found: {TESTSETS_DIR}")
        sys.exit(1)

    pl_files = sorted(TESTSETS_DIR.glob('*.pl'))
    print(f"Found {len(pl_files)} testset files in {TESTSETS_DIR}\n")

    stats = {'modified': 0, 'skipped': 0, 'missing': 0, 'error': 0}
    issues = []

    for filepath in pl_files:
        status, message = process_file(filepath, dry_run)
        stats[status] += 1

        if status == 'modified':
            print(f"  [OK] {message}")
        elif status == 'skipped':
            print(f"  [SKIP] {message}")
        elif status == 'missing':
            issues.append(message)
            print(f"  [WARN] {message}")
        elif status == 'error':
            issues.append(message)
            print(f"  [ERROR] {message}")

    print(f"\n{'=' * 60}")
    print(f"Results:")
    print(f"  Modified:  {stats['modified']}")
    print(f"  Skipped:   {stats['skipped']} (already processed)")
    print(f"  Missing:   {stats['missing']} (no human_readable comment)")
    print(f"  Errors:    {stats['error']}")
    print(f"  Total:     {len(pl_files)}")

    if issues:
        print(f"\nIssues requiring attention:")
        for issue in issues:
            print(f"  - {issue}")

    if stats['error'] > 0:
        sys.exit(1)


if __name__ == '__main__':
    main()
