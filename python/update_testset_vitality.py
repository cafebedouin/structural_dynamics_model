#!/usr/bin/env python3
"""
update_testset_vitality.py — Bulk-update testset .pl files with coordination_vitality/2 declarations.

Reads the regenerated coordination_vitality_diagnostic_data.json (which now emits all 95 pitons
with their terminal/degrading/transitional classification) and edits each terminal/degrading
piton's testset .pl file to add:
  1. narrative_ontology:coordination_vitality/2 to the multifile declaration block
  2. A coordination_vitality fact after the beneficiary/victim declarations

Terminal pitons → coordination_vitality(ID, dead).
Degrading pitons → coordination_vitality(ID, degrading).
Transitional pitons → no declaration (default = active coordination).

Usage:
    python3 python/update_testset_vitality.py [--dry-run]
"""

import json
import os
import re
import sys
import glob

TESTSETS_DIR = os.path.join(os.path.dirname(__file__), "..", "prolog", "testsets")
DIAGNOSTIC_JSON = os.path.join(
    os.path.dirname(__file__), "..", "outputs", "coordination_vitality_diagnostic_data.json"
)

# Map terminal_score classification → Prolog atom
CLASSIFICATION_TO_ATOM = {
    "terminal": "dead",
    "degrading": "degrading",
}


def build_id_to_file_map(testsets_dir):
    """Scan all .pl files and extract constraint IDs.

    Uses two strategies:
      1. Module declaration: :- module(constraint_ID, []).
      2. Fact-level constraint_claim: narrative_ontology:constraint_claim(ID, ...).

    Some files use different names for the module vs the constraint ID
    (e.g., module constraint_ulysses_aeolus but facts use ulysses_aeolus_1904).
    The fact-level mapping is authoritative.
    """
    id_to_file = {}
    for fn in glob.glob(os.path.join(testsets_dir, "*.pl")):
        with open(fn) as f:
            for line in f:
                # Module-level mapping (fallback)
                m = re.match(r":- module\(constraint_(\w+)", line)
                if m:
                    mid = m.group(1)
                    if mid not in id_to_file:
                        id_to_file[mid] = fn
                # Fact-level mapping (authoritative — overrides module name)
                m2 = re.match(
                    r"narrative_ontology:constraint_claim\((\w+),", line
                )
                if m2:
                    id_to_file[m2.group(1)] = fn
    return id_to_file


def add_vitality_to_file(filepath, constraint_id, atom):
    """
    Edit a testset .pl file to add coordination_vitality/2 declaration.

    1. Add narrative_ontology:coordination_vitality/2 to the multifile block
    2. Add the fact after constraint_victim/constraint_beneficiary declarations
    """
    with open(filepath) as f:
        lines = f.readlines()

    # --- Phase 1: Add to multifile block ---
    # Find the multifile block and add coordination_vitality/2 before the closing period.
    # The multifile block looks like:
    #   :- multifile
    #       ...,
    #       narrative_ontology:topic_domain/2.
    #
    # We need to add:  narrative_ontology:coordination_vitality/2,
    # Strategy: find the last line of the multifile block (ends with period)
    # and insert our declaration before it.

    multifile_start = None
    multifile_end = None
    already_has_vitality_multifile = False

    for i, line in enumerate(lines):
        if re.match(r"^:- multifile\b", line):
            multifile_start = i
        if multifile_start is not None and "coordination_vitality/2" in line:
            already_has_vitality_multifile = True
        if multifile_start is not None and multifile_end is None:
            stripped = line.rstrip()
            if stripped.endswith(".") and i > multifile_start:
                multifile_end = i
                break

    if not already_has_vitality_multifile and multifile_end is not None:
        # Change the period on the last line to a comma, then add our line
        old_last = lines[multifile_end]
        # Replace trailing period with comma
        lines[multifile_end] = old_last.rstrip().rstrip(".") + ",\n"
        # Insert new line after
        indent = "    "  # Match existing indentation
        lines.insert(multifile_end + 1, f"{indent}narrative_ontology:coordination_vitality/2.\n")

    # --- Phase 2: Add the vitality fact ---
    # Find the best insertion point: after constraint_victim or constraint_beneficiary lines,
    # before the Indexed Classifications section.
    already_has_vitality_fact = False
    insert_after = None

    for i, line in enumerate(lines):
        if f"coordination_vitality({constraint_id}," in line:
            already_has_vitality_fact = True
            break
        # Track last beneficiary/victim line as insertion point
        if re.search(
            rf"(constraint_beneficiary|constraint_victim)\({re.escape(constraint_id)},",
            line,
        ):
            insert_after = i
        # Also track requires_active_enforcement as fallback
        if re.search(
            rf"requires_active_enforcement\({re.escape(constraint_id)}\)", line
        ):
            if insert_after is None:
                insert_after = i

    if not already_has_vitality_fact and insert_after is not None:
        fact_line = f"narrative_ontology:coordination_vitality({constraint_id}, {atom}).\n"
        lines.insert(insert_after + 1, fact_line)

    with open(filepath, "w") as f:
        f.writelines(lines)

    return not already_has_vitality_multifile, not already_has_vitality_fact


def main():
    dry_run = "--dry-run" in sys.argv

    # Load diagnostic data
    with open(DIAGNOSTIC_JSON) as f:
        data = json.load(f)

    all_pitons = data["step5_degradation_assessment"]["all_pitons"]

    # Filter to terminal and degrading only
    to_update = [
        p for p in all_pitons if p["classification"] in CLASSIFICATION_TO_ATOM
    ]
    transitional = [p for p in all_pitons if p["classification"] == "transitional"]

    print(f"[VIT] Total pitons: {len(all_pitons)}")
    print(f"[VIT] To update (terminal+degrading): {len(to_update)}")
    print(f"[VIT] Skipping (transitional): {len(transitional)}")

    # Build ID → file mapping
    testsets_dir = os.path.normpath(TESTSETS_DIR)
    id_to_file = build_id_to_file_map(testsets_dir)
    print(f"[VIT] Scanned {len(id_to_file)} testset files")

    # Process each piton
    modified = 0
    skipped_no_file = []
    skipped_already = 0

    for piton in to_update:
        cid = piton["id"]
        atom = CLASSIFICATION_TO_ATOM[piton["classification"]]

        if cid not in id_to_file:
            skipped_no_file.append(cid)
            continue

        filepath = id_to_file[cid]

        if dry_run:
            print(f"  [DRY] Would update {os.path.basename(filepath)}: "
                  f"coordination_vitality({cid}, {atom})")
            modified += 1
            continue

        added_multifile, added_fact = add_vitality_to_file(filepath, cid, atom)

        if added_multifile or added_fact:
            print(f"  [OK] {os.path.basename(filepath)}: "
                  f"coordination_vitality({cid}, {atom})")
            modified += 1
        else:
            skipped_already += 1

    print(f"\n[VIT] Summary:")
    print(f"  Modified: {modified}")
    print(f"  Already had declarations: {skipped_already}")
    print(f"  No testset file found: {len(skipped_no_file)}")
    if skipped_no_file:
        for sid in skipped_no_file:
            classification = next(
                p["classification"] for p in to_update if p["id"] == sid
            )
            print(f"    {sid} ({classification})")


if __name__ == "__main__":
    main()
