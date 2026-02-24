#!/usr/bin/env python3
"""testset_rebuild.py — Reconcile constraint_classification/3 predicates.

Compares per-perspective types (from constraint_classification/3 facts in
Prolog spec files) against engine-computed types (from drl_core:dr_type/3
perspectives in enriched_pipeline.json) and batch-updates mismatched
classifications.

Seven phases:
  1. Snapshot     — compute per-perspective mismatches, emit JSON artifact
  2. Batch        — rewrite type atoms in prolog/testsets/*.pl
  3. Verify       — swipl syntax check on modified files
  3.5 Engine      — validate engine agreement on stratified sample
  4. Dedup        — testset deduplication analysis (read-only)
  5. Report       — generate docs/testset_rebuild_dedup.md

Reads:  outputs/enriched_pipeline.json, outputs/claim_engine_mismatch_snapshot.json,
        prolog/testsets/*.pl
Writes: outputs/testset_rebuild_data.json
        docs/testset_rebuild_dedup.md
        (modifies prolog/testsets/*.pl in-place)

Usage:
    python3 python/testset_rebuild.py
    python3 python/testset_rebuild.py --dry-run
    python3 python/testset_rebuild.py --snapshot-only
    python3 python/testset_rebuild.py --dedup-only
"""

import difflib
import glob
import json
import os
import random
import re
import subprocess
import sys
from collections import Counter, defaultdict
from datetime import datetime, timezone
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))

from shared.loader import load_json, ENRICHED_PIPELINE_JSON, OUTPUT_DIR, PROLOG_DIR

# ---------------------------------------------------------------------------
# Path constants
# ---------------------------------------------------------------------------

ROOT_DIR = Path(__file__).resolve().parent.parent
TESTSETS_DIR = PROLOG_DIR / "testsets"
DOCS_DIR = ROOT_DIR / "docs"
REBUILD_DATA_PATH = OUTPUT_DIR / "testset_rebuild_data.json"
SNAPSHOT_PATH = OUTPUT_DIR / "claim_engine_mismatch_snapshot.json"
REPORT_PATH = DOCS_DIR / "testset_rebuild_dedup.md"

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

CANONICAL_PERSPECTIVES = ["powerless", "moderate", "institutional", "analytical"]
VALID_TYPES = {"mountain", "rope", "snare", "tangled_rope", "scaffold", "piton"}

# Known series prefixes — skip O(n^2) pairs within these
KNOWN_SERIES_PREFIXES = [
    "ulysses_", "ergo_", "gs1_", "kjv_", "scam_compound_",
]

# =========================================================================
# Helpers (reused from batch_claim_reconciliation.py)
# =========================================================================


def build_id_to_file_map(testsets_dir):
    """Scan testset .pl files for constraint IDs.

    Two strategies:
      1. Module declaration fallback:  :- module(constraint_ID, []).
      2. Fact-level authoritative:     narrative_ontology:constraint_claim(ID, atom).

    Returns dict mapping constraint_id -> absolute file path string.
    """
    id_to_file = {}
    for fn in sorted(glob.glob(str(testsets_dir / "*.pl"))):
        with open(fn) as f:
            for line in f:
                m = re.match(r":- module\(constraint_(\w+)", line)
                if m:
                    mid = m.group(1)
                    if mid not in id_to_file:
                        id_to_file[mid] = fn
                m2 = re.match(
                    r"narrative_ontology:constraint_claim\((\w+),", line
                )
                if m2:
                    id_to_file[m2.group(1)] = fn
    return id_to_file


def verify_swipl_syntax(filepath, timeout_sec=10):
    """Run swipl read_term on a .pl file to verify syntax."""
    abs_path = os.path.abspath(filepath)
    goal = (
        f"catch("
        f"(open('{abs_path}', read, S), "
        f"read_term(S, _, []), close(S), halt(0)), "
        f"E, (print_message(error, E), halt(1)))"
    )
    try:
        result = subprocess.run(
            ["swipl", "-g", goal],
            capture_output=True, text=True, timeout=timeout_sec,
        )
        return result.returncode == 0
    except (subprocess.TimeoutExpired, FileNotFoundError):
        return False


# =========================================================================
# Engine perspective map
# =========================================================================


def build_engine_perspective_map(pipeline_data):
    """Build dict: {constraint_id: {perspective: engine_type}}.

    Uses the 'perspectives' field from enriched_pipeline.json.
    """
    result = {}
    for c in pipeline_data.get("per_constraint", []):
        cid = c["id"]
        perspectives = c.get("perspectives", {})
        if perspectives:
            result[cid] = perspectives
    return result


# =========================================================================
# Tie-case data
# =========================================================================


def load_tie_cases():
    """Load tie-resolved constraints from claim_engine_mismatch_snapshot.json."""
    snapshot = load_json(SNAPSHOT_PATH, "claim_engine_mismatch_snapshot")
    tie_cases = snapshot.get("tie_cases", [])
    tie_by_id = {}
    for tc in tie_cases:
        tie_by_id[tc["id"]] = {
            "tie_resolved": True,
            "tie_perspectives": tc.get("counts", {}),
            "tie_winner": tc.get("resolved_to"),
            "tie_rule": "extraction_severity_precedence",
        }
    return tie_by_id


# =========================================================================
# Regex patterns
# =========================================================================

# Pattern for standalone constraint_classification/3 facts.
# Matches the full multi-line fact from opening to closing period.
# Groups: 1=prefix (up to type), 2=constraint_id, 3=type_atom,
#         4=suffix (from after type to end), 5=agent_power
#
# The conditional clause detection is done separately to skip those facts.
# Matches standalone (non-indented) constraint_classification/3 facts.
# Both unconditional (  )).  ) and conditional (  )) :- body.  ) forms.
# The (?:^|\n) anchor ensures we only match facts at the start of a line,
# not indented test body references.
# Groups: 1=prefix (up to type), 2=constraint_id, 3=type_atom,
#         4=suffix, 5=agent_power, 6=conditional body (None if unconditional)
FACT_PATTERN = re.compile(
    r"(?:^|\n)"               # anchor to start of line
    r"(constraint_indexing:constraint_classification\(\s*"
    r"'?(\w+)'?"              # constraint ID (group 2)
    r"\s*,\s*)"
    r"([a-z]\w*)"             # type atom (group 3) — excludes uppercase Variables
    r"("                      # start suffix (group 4)
    r"\s*,\s*"
    r"context\("
    r"[^)]*?"                 # non-greedy to first agent_power
    r"agent_power\((\w+)\)"   # perspective (group 5)
    r".*?"                    # rest of context
    r"\)\)"                   # close context + close classification
    r"(?:\s*:-\s*([^.]+))?"   # optional conditional body (group 6)
    r"\s*\.)",                # closing period — end suffix
    re.DOTALL
)

# Pattern for test body hardcoded types (single-line, indented)
TEST_BODY_PATTERN = re.compile(
    r"([ \t]+constraint_indexing:constraint_classification\(\s*"
    r"'?(\w+)'?"              # constraint ID (group 2)
    r"\s*,\s*)"
    r"(mountain|rope|snare|tangled_rope|scaffold|piton)"  # type (group 3)
    r"(\s*,\s*context\("
    r"agent_power\((\w+)\)"   # perspective (group 5)
    r"[^)]*\)\))"             # rest + close
)


def find_classification_facts(content, constraint_id=None):
    """Find all constraint_classification/3 facts in file content.

    Returns list of dicts with keys:
      match_obj, constraint_id, type_atom, agent_power, is_conditional,
      start, end
    """
    facts = []
    for m in FACT_PATTERN.finditer(content):
        cid = m.group(2)
        if constraint_id is not None and cid != constraint_id:
            continue

        # Group 6 is the conditional body — if present, this is a conditional fact
        is_conditional = m.group(6) is not None

        facts.append({
            "match": m,
            "constraint_id": cid,
            "type_atom": m.group(3),
            "agent_power": m.group(5),
            "is_conditional": is_conditional,
            "start": m.start(),
            "end": m.end(),
        })

    return facts


# =========================================================================
# Phase 1: Snapshot
# =========================================================================


def phase_snapshot(engine_map, id_to_file, tie_by_id):
    """Compute per-perspective mismatches between files and engine.

    Returns (snapshot_data, summary_metadata).
    """
    per_constraint = []
    total_facts = 0
    matched_facts = 0
    mismatched_facts = 0
    skipped_non_canonical = 0
    skipped_conditional = 0
    missing_perspectives = 0
    no_classification_facts = 0
    file_read_cache = {}

    # Track type transitions
    transition_table = Counter()
    # Track per-constraint for batch update
    file_edits = defaultdict(list)

    all_constraint_ids = set(engine_map.keys())
    files_scanned = set()

    for cid in sorted(all_constraint_ids):
        filepath = id_to_file.get(cid)
        if not filepath or not os.path.exists(filepath):
            per_constraint.append({
                "id": cid,
                "file_path": None,
                "facts": [],
                "status": "unmapped",
            })
            continue

        # Read file (cached)
        if filepath not in file_read_cache:
            with open(filepath, encoding="utf-8") as f:
                file_read_cache[filepath] = f.read()
        content = file_read_cache[filepath]
        files_scanned.add(filepath)

        # Find all classification facts for this constraint
        facts = find_classification_facts(content, constraint_id=cid)

        engine_perspectives = engine_map.get(cid, {})
        constraint_facts = []

        if not facts:
            no_classification_facts += 1
            per_constraint.append({
                "id": cid,
                "file_path": filepath,
                "facts": [],
                "status": "no_classification_facts",
            })
            continue

        for fact in facts:
            total_facts += 1
            power = fact["agent_power"]

            if power not in CANONICAL_PERSPECTIVES:
                skipped_non_canonical += 1
                constraint_facts.append({
                    "agent_power": power,
                    "file_type": fact["type_atom"],
                    "engine_type": None,
                    "match": None,
                    "canonical": False,
                    "conditional": fact["is_conditional"],
                    "skipped_reason": "non_canonical_power",
                })
                continue

            if fact["is_conditional"]:
                skipped_conditional += 1
                engine_type = engine_perspectives.get(power)
                constraint_facts.append({
                    "agent_power": power,
                    "file_type": fact["type_atom"],
                    "engine_type": engine_type,
                    "match": fact["type_atom"] == engine_type if engine_type else None,
                    "canonical": True,
                    "conditional": True,
                    "skipped_reason": "conditional_clause",
                })
                continue

            engine_type = engine_perspectives.get(power)
            if engine_type is None:
                missing_perspectives += 1
                constraint_facts.append({
                    "agent_power": power,
                    "file_type": fact["type_atom"],
                    "engine_type": None,
                    "match": None,
                    "canonical": True,
                    "conditional": False,
                    "skipped_reason": "missing_engine_perspective",
                })
                continue

            is_match = (fact["type_atom"] == engine_type)
            if is_match:
                matched_facts += 1
            else:
                mismatched_facts += 1
                transition_table[(fact["type_atom"], engine_type)] += 1
                file_edits[filepath].append({
                    "constraint_id": cid,
                    "agent_power": power,
                    "old_type": fact["type_atom"],
                    "new_type": engine_type,
                })

            constraint_facts.append({
                "agent_power": power,
                "file_type": fact["type_atom"],
                "engine_type": engine_type,
                "match": is_match,
                "canonical": True,
                "conditional": False,
                "skipped_reason": None,
            })

        # Add tie-case metadata
        tie_info = tie_by_id.get(cid)

        rec = {
            "id": cid,
            "file_path": filepath,
            "facts": constraint_facts,
            "status": "ok",
        }
        if tie_info:
            rec.update(tie_info)
        per_constraint.append(rec)

    # Build transition table list
    transition_list = [
        {"file_type": old, "engine_type": new, "count": cnt}
        for (old, new), cnt in sorted(transition_table.items(), key=lambda x: -x[1])
    ]

    # Pre/post distribution
    pre_dist = Counter()
    post_dist = Counter()
    for rec in per_constraint:
        for fact in rec.get("facts", []):
            if fact.get("canonical") and not fact.get("conditional"):
                ft = fact.get("file_type")
                et = fact.get("engine_type")
                if ft:
                    pre_dist[ft] += 1
                if et:
                    post_dist[et] += 1

    metadata = {
        "generated": datetime.now(timezone.utc).isoformat(),
        "total_constraints_in_engine": len(all_constraint_ids),
        "files_scanned": len(files_scanned),
        "total_facts": total_facts,
        "matched_facts": matched_facts,
        "mismatched_facts": mismatched_facts,
        "skipped_non_canonical": skipped_non_canonical,
        "skipped_conditional": skipped_conditional,
        "missing_engine_perspectives": missing_perspectives,
        "no_classification_facts": no_classification_facts,
        "transition_table": transition_list,
        "pre_distribution": dict(pre_dist),
        "post_distribution": dict(post_dist),
        "tie_resolved_count": len(tie_by_id),
    }

    snapshot = {**metadata, "per_constraint": per_constraint}
    return snapshot, file_edits


# =========================================================================
# Phase 2: Batch Update
# =========================================================================


def phase_batch_update(file_edits, dry_run=False):
    """Batch-update constraint_classification type atoms in Prolog spec files.

    Safety logic per fact:
      - on-disk type == new_type → skip (already correct)
      - on-disk type != old_type → skip (unexpected state)
      - on-disk type == old_type → replace with new_type
    """
    results = {
        "modified_facts": [],
        "modified_files": [],
        "modified_test_body_facts": [],
        "skipped_already_correct": [],
        "skipped_disk_mismatch": [],
        "skipped_no_match": [],
        "would_modify": [],
        "would_modify_test_body": [],
        "failed_swipl": [],
    }

    for filepath, edits in sorted(file_edits.items()):
        with open(filepath, encoding="utf-8") as f:
            content = f.read()
        original = content

        file_modified = False
        for edit in edits:
            cid = edit["constraint_id"]
            power = edit["agent_power"]
            old_type = edit["old_type"]
            new_type = edit["new_type"]

            # Find all classification facts for this constraint in content
            facts = find_classification_facts(content, constraint_id=cid)

            # Filter to matching perspective, non-conditional
            target_facts = [
                f for f in facts
                if f["agent_power"] == power
                and not f["is_conditional"]
                and f["type_atom"] in VALID_TYPES
            ]

            if not target_facts:
                results["skipped_no_match"].append({
                    "id": cid, "perspective": power,
                })
                continue

            for fact in target_facts:
                on_disk = fact["type_atom"]

                if on_disk == new_type:
                    results["skipped_already_correct"].append({
                        "id": cid, "perspective": power,
                    })
                    continue

                if on_disk != old_type:
                    results["skipped_disk_mismatch"].append({
                        "id": cid, "perspective": power,
                        "expected": old_type, "found": on_disk,
                    })
                    continue

                # Perform the replacement — targeted at this specific match
                # Use group start positions to handle the (?:^|\n) anchor
                m = fact["match"]
                new_content = (
                    content[:m.start(3)]
                    + new_type
                    + content[m.end(3):]
                )

                if new_content != content:
                    content = new_content
                    file_modified = True
                    entry = {"id": cid, "perspective": power,
                             "old": old_type, "new": new_type,
                             "file": os.path.basename(filepath)}
                    if dry_run:
                        results["would_modify"].append(entry)
                    else:
                        results["modified_facts"].append(entry)

                    # Re-parse facts since positions shifted
                    # (only matters if multiple edits in same file)
                    break  # Process next edit, will re-parse on next iteration

            # Also update test body hardcoded types for same (cid, power)
            for tb_m in TEST_BODY_PATTERN.finditer(content):
                tb_cid = tb_m.group(2)
                tb_type = tb_m.group(3)
                tb_power = tb_m.group(5)
                if tb_cid == cid and tb_power == power and tb_type == old_type:
                    new_content = (
                        content[:tb_m.start()]
                        + tb_m.group(1) + new_type + tb_m.group(4)
                        + content[tb_m.end():]
                    )
                    if new_content != content:
                        content = new_content
                        file_modified = True
                        entry = {"id": cid, "perspective": power,
                                 "old": old_type, "new": new_type,
                                 "file": os.path.basename(filepath),
                                 "location": "test_body"}
                        if dry_run:
                            results["would_modify_test_body"].append(entry)
                        else:
                            results["modified_test_body_facts"].append(entry)

        if not file_modified or content == original:
            continue

        if dry_run:
            continue

        with open(filepath, "w", encoding="utf-8") as f:
            f.write(content)
        results["modified_files"].append(filepath)

    return results


# =========================================================================
# Phase 3: Verify
# =========================================================================


def phase_verify(modified_files):
    """Run swipl syntax verification on all modified files.

    Returns dict with pass/fail lists and rollback info.
    """
    results = {"passed": [], "failed": [], "rolled_back": []}

    for filepath in modified_files:
        ok = verify_swipl_syntax(filepath)
        if ok:
            results["passed"].append(filepath)
        else:
            results["failed"].append(filepath)
            print(f"  [WARN] swipl verification FAILED: {filepath}",
                  file=sys.stderr)

    return results


# =========================================================================
# Phase 3.5: Engine Agreement Check
# =========================================================================


def phase_engine_check(engine_map, tie_by_id, modified_facts, sample_size=50):
    """Validate engine agreement on a stratified sample.

    Runs drl_core:dr_type/3 via swipl for sampled constraints and
    compares against the engine types from enriched_pipeline.json.
    """
    # Build stratified sample: all tie-resolved + random others
    tie_ids = set(tie_by_id.keys())
    modified_ids = set(f["id"] for f in modified_facts)

    # All tie-resolved that were also modified
    sample_ids = list(tie_ids & modified_ids)
    # Add random others
    remaining = list(modified_ids - tie_ids)
    random.seed(42)
    n_random = min(sample_size, len(remaining))
    sample_ids.extend(random.sample(remaining, n_random))
    sample_ids = sorted(set(sample_ids))

    if not sample_ids:
        return {"status": "no_sample", "sample_size": 0, "matches": 0, "mismatches": 0}

    # Standard contexts (matching logical_fingerprint:standard_context_for_power/2)
    standard_contexts = {
        "powerless": "context(agent_power(powerless),time_horizon(biographical),exit_options(trapped),spatial_scope(local))",
        "moderate": "context(agent_power(moderate),time_horizon(biographical),exit_options(mobile),spatial_scope(national))",
        "institutional": "context(agent_power(institutional),time_horizon(generational),exit_options(arbitrage),spatial_scope(national))",
        "analytical": "context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global))",
    }

    matches = 0
    mismatches = 0
    errors = 0
    mismatch_details = []

    prolog_dir = str(PROLOG_DIR)

    # Build query pairs
    query_pairs = []
    for cid in sample_ids:
        engine_perspectives = engine_map.get(cid, {})
        for power in CANONICAL_PERSPECTIVES:
            engine_type = engine_perspectives.get(power)
            if not engine_type:
                continue
            query_pairs.append((cid, power, engine_type))

    if not query_pairs:
        return {"status": "no_sample", "sample_size": 0, "matches": 0, "mismatches": 0}

    # Write queries to a temp Prolog file — each as its own directive with
    # catch/recovery so a single constraint error doesn't halt the batch
    goal_file = os.path.join(prolog_dir, "_engine_check_goal.pl")
    with open(goal_file, "w") as f:
        f.write(":- corpus_loader:ensure_corpus_loaded.\n")
        for cid, power, engine_type in query_pairs:
            ctx = standard_contexts[power]
            f.write(
                f":- catch(("
                f"(drl_core:dr_type({cid},{ctx},T) -> true ; T = unknown),"
                f"format('~w|~w|~w|~w~n', [{cid},{power},{engine_type},T])), _, "
                f"format('~w|~w|~w|error~n', [{cid},{power},{engine_type}])).\n"
            )
        f.write(":- halt.\n")

    try:
        result = subprocess.run(
            ["swipl", "-l", "stack.pl", "-l", "_engine_check_goal.pl"],
            capture_output=True, text=True, timeout=180,
            cwd=prolog_dir,
        )
        # Parse results
        output_pairs = set()
        for line in result.stdout.strip().split("\n"):
            line = line.strip()
            if not line or "|" not in line:
                continue
            parts = line.split("|")
            if len(parts) != 4:
                continue
            cid, power, expected, actual = parts
            output_pairs.add((cid, power))
            if actual == "error":
                errors += 1
                mismatch_details.append({
                    "id": cid,
                    "perspective": power,
                    "engine_type": expected,
                    "error": "prolog exception",
                    "tie_resolved": cid in tie_ids,
                })
            elif actual == expected:
                matches += 1
            else:
                mismatches += 1
                mismatch_details.append({
                    "id": cid,
                    "perspective": power,
                    "engine_type": expected,
                    "live_type": actual,
                    "tie_resolved": cid in tie_ids,
                })
        # Check for queries not in output
        for cid, power, engine_type in query_pairs:
            if (cid, power) not in output_pairs:
                errors += 1
                mismatch_details.append({
                    "id": cid,
                    "perspective": power,
                    "error": "not in output",
                })
    except subprocess.TimeoutExpired:
        errors = len(query_pairs)
        mismatch_details.append({"error": "batch query timed out"})
    except FileNotFoundError:
        errors = len(query_pairs)
        mismatch_details.append({"error": "swipl not found"})
    finally:
        if os.path.exists(goal_file):
            os.unlink(goal_file)

    total = matches + mismatches + errors
    match_rate = matches / total if total > 0 else 0

    return {
        "status": "pass" if mismatches == 0 and errors == 0 else "fail",
        "sample_size": len(sample_ids),
        "total_checks": total,
        "matches": matches,
        "mismatches": mismatches,
        "errors": errors,
        "match_rate": round(match_rate, 4),
        "mismatch_details": mismatch_details,
        "tie_resolved_in_sample": len(tie_ids & set(sample_ids)),
    }


# =========================================================================
# Phase 4: Deduplication Analysis
# =========================================================================


def levenshtein_distance(s1, s2):
    """Simple Levenshtein distance implementation."""
    if len(s1) < len(s2):
        return levenshtein_distance(s2, s1)
    if len(s2) == 0:
        return len(s1)

    prev_row = range(len(s2) + 1)
    for i, c1 in enumerate(s1):
        curr_row = [i + 1]
        for j, c2 in enumerate(s2):
            insertions = prev_row[j + 1] + 1
            deletions = curr_row[j] + 1
            substitutions = prev_row[j] + (c1 != c2)
            curr_row.append(min(insertions, deletions, substitutions))
        prev_row = curr_row

    return prev_row[-1]


def is_series_member(name):
    """Check if a filename belongs to a known series."""
    for prefix in KNOWN_SERIES_PREFIXES:
        if name.startswith(prefix):
            return True
    return False


def phase_dedup(id_to_file, pipeline_data):
    """Testset deduplication analysis (read-only).

    Returns dict with candidates, classifications, and summary.
    """
    # Build pipeline lookup
    pipeline_by_id = {}
    for c in pipeline_data.get("per_constraint", []):
        pipeline_by_id[c["id"]] = c

    # Get all filenames and constraint IDs
    file_to_id = {}
    for cid, fpath in id_to_file.items():
        basename = os.path.basename(fpath).replace(".pl", "")
        file_to_id.setdefault(fpath, []).append(cid)

    all_files = sorted(glob.glob(str(TESTSETS_DIR / "*.pl")))
    basenames = {f: os.path.basename(f).replace(".pl", "") for f in all_files}

    # --- Phase 1: Filename similarity ---
    candidates = []
    seen_pairs = set()

    for i, f1 in enumerate(all_files):
        b1 = basenames[f1]
        if is_series_member(b1):
            continue
        for f2 in all_files[i + 1:]:
            b2 = basenames[f2]
            if is_series_member(b2):
                continue

            # Skip if both would be in the same series
            pair_key = (min(b1, b2), max(b1, b2))
            if pair_key in seen_pairs:
                continue

            # Check similarity
            ratio = difflib.SequenceMatcher(None, b1, b2).ratio()
            is_substring = b1 in b2 or b2 in b1
            lev = levenshtein_distance(b1, b2)

            if ratio > 0.75 or is_substring or lev < 5:
                seen_pairs.add(pair_key)
                candidates.append({
                    "file_a": os.path.basename(f1),
                    "file_b": os.path.basename(f2),
                    "name_a": b1,
                    "name_b": b2,
                    "similarity": round(ratio, 3),
                    "is_substring": is_substring,
                    "levenshtein": lev,
                    "path_a": f1,
                    "path_b": f2,
                })

    # --- Phase 2: Constraint ID collisions ---
    # Check if multiple distinct files declare the same constraint_claim
    cid_map = defaultdict(set)  # Use set to deduplicate within same file
    for fn in all_files:
        try:
            with open(fn, encoding="utf-8") as f:
                content = f.read()
        except (OSError, UnicodeDecodeError):
            continue
        for m in re.finditer(r"narrative_ontology:constraint_claim\((\w+),", content):
            cid_map[m.group(1)].add(os.path.basename(fn))
    # Only flag where multiple distinct files declare the same ID
    id_collisions = {k: sorted(v) for k, v in cid_map.items() if len(v) > 1}

    # --- Phase 3: Classify candidates ---
    classified = []
    for cand in candidates:
        b1, b2 = cand["name_a"], cand["name_b"]
        # Find constraint IDs for each file
        ids_a = file_to_id.get(cand["path_a"], [])
        ids_b = file_to_id.get(cand["path_b"], [])

        # Get pipeline data
        p_a = pipeline_by_id.get(ids_a[0]) if ids_a else None
        p_b = pipeline_by_id.get(ids_b[0]) if ids_b else None

        classification = classify_candidate(cand, p_a, p_b, ids_a, ids_b)
        cand["classification"] = classification
        cand["ids_a"] = ids_a
        cand["ids_b"] = ids_b
        classified.append(cand)

    # --- Phase 4: Naming mismatches ---
    naming_mismatches = []
    for cid, fpath in id_to_file.items():
        basename = os.path.basename(fpath).replace(".pl", "")
        # Strip common prefix
        expected_basename = cid
        if basename != expected_basename and basename != f"constraint_{expected_basename}":
            naming_mismatches.append({
                "constraint_id": cid,
                "filename": os.path.basename(fpath),
                "expected": f"{expected_basename}.pl",
            })

    # --- Summary ---
    class_counts = Counter(c["classification"]["category"] for c in classified)

    return {
        "candidates": classified,
        "id_collisions": id_collisions,
        "naming_mismatches": naming_mismatches[:50],  # Limit output
        "total_naming_mismatches": len(naming_mismatches),
        "summary": {
            "total_candidates": len(classified),
            "hard_duplicate": class_counts.get("hard_duplicate", 0),
            "semantic_duplicate": class_counts.get("semantic_duplicate", 0),
            "intentional_variant": class_counts.get("intentional_variant", 0),
            "false_positive": class_counts.get("false_positive", 0),
            "id_collisions": len(id_collisions),
        },
    }


def classify_candidate(cand, p_a, p_b, ids_a, ids_b):
    """Classify a duplicate candidate pair."""
    if not p_a or not p_b:
        return {
            "category": "false_positive",
            "reason": "one or both not in pipeline",
        }

    hr_a = p_a.get("human_readable") or ""
    hr_b = p_b.get("human_readable") or ""
    hr_sim = difflib.SequenceMatcher(None, hr_a, hr_b).ratio() if hr_a and hr_b else 0.0

    eps_a = p_a.get("base_extractiveness")
    eps_b = p_b.get("base_extractiveness")
    eps_diff = abs(eps_a - eps_b) if eps_a is not None and eps_b is not None else 999

    domain_a = p_a.get("topic_domain") or p_a.get("domain") or ""
    domain_b = p_b.get("topic_domain") or p_b.get("domain") or ""
    same_domain = domain_a == domain_b and domain_a != ""

    persp_a = p_a.get("perspectives", {})
    persp_b = p_b.get("perspectives", {})
    persp_agree = sum(
        1 for k in CANONICAL_PERSPECTIVES
        if persp_a.get(k) == persp_b.get(k)
    )

    # Hard duplicate: nearly identical
    if hr_sim > 0.9 and eps_diff < 0.05 and same_domain and persp_agree >= 3:
        keep = "a" if len(hr_a) >= len(hr_b) else "b"
        return {
            "category": "hard_duplicate",
            "reason": f"human_readable similarity {hr_sim:.2f}, eps_diff {eps_diff:.3f}, "
                      f"{persp_agree}/4 perspectives agree",
            "recommendation": f"keep {'file_a' if keep == 'a' else 'file_b'}",
            "hr_similarity": round(hr_sim, 3),
            "eps_diff": round(eps_diff, 3),
        }

    # Semantic duplicate
    if hr_sim > 0.7 and same_domain and eps_diff < 0.15:
        return {
            "category": "semantic_duplicate",
            "reason": f"similar topic (hr_sim={hr_sim:.2f}), same domain ({domain_a}), "
                      f"eps_diff={eps_diff:.3f}",
            "recommendation": "review for merge",
            "hr_similarity": round(hr_sim, 3),
            "eps_diff": round(eps_diff, 3),
        }

    # Intentional variant: same domain area but meaningfully different
    if same_domain or hr_sim > 0.5:
        return {
            "category": "intentional_variant",
            "reason": f"related topic (hr_sim={hr_sim:.2f}) but different metrics "
                      f"(eps_diff={eps_diff:.3f}, {persp_agree}/4 perspectives agree)",
            "recommendation": "keep both",
            "hr_similarity": round(hr_sim, 3),
            "eps_diff": round(eps_diff, 3),
        }

    return {
        "category": "false_positive",
        "reason": f"similar names but unrelated (hr_sim={hr_sim:.2f})",
        "hr_similarity": round(hr_sim, 3),
    }


# =========================================================================
# Phase 5: Report
# =========================================================================


def phase_report(snapshot, batch_results, verify_results, engine_results,
                 dedup_results, dry_run):
    """Generate docs/testset_rebuild_dedup.md."""
    today = datetime.now().strftime("%Y-%m-%d")
    lines = []

    def w(s=""):
        lines.append(s)

    meta = {k: v for k, v in snapshot.items() if k != "per_constraint"}

    # --- Header ---
    w("# Test Suite Rebuild: Classification Reconciliation & Deduplication Report")
    w()
    w(f"**Generated:** {today}")
    if dry_run:
        w("**Mode:** DRY RUN — no files were modified")
    w()

    # --- 1. Executive Summary ---
    w("## 1. Executive Summary")
    w()
    n_mod = (len(batch_results.get("modified_facts", []))
             if not dry_run
             else len(batch_results.get("would_modify", [])))
    n_test = (len(batch_results.get("modified_test_body_facts", []))
              if not dry_run
              else len(batch_results.get("would_modify_test_body", [])))
    w(f"Reconciled **{n_mod}** `constraint_classification/3` facts "
      f"(+ {n_test} test body references) across "
      f"{meta['total_constraints_in_engine']} constraints to match "
      f"engine-computed perspective types.")
    w()
    w(f"- Total classification facts scanned: {meta['total_facts']}")
    w(f"- Already matching engine: {meta['matched_facts']} "
      f"({meta['matched_facts']*100/meta['total_facts']:.1f}%)"
      if meta["total_facts"] > 0 else "")
    w(f"- Mismatched (updated): {meta['mismatched_facts']}")
    w(f"- Skipped non-canonical power atoms: {meta['skipped_non_canonical']}")
    w(f"- Skipped conditional clauses: {meta['skipped_conditional']}")
    w(f"- Missing engine perspectives: {meta['missing_engine_perspectives']}")
    w(f"- Constraints with no classification facts: {meta['no_classification_facts']}")
    w(f"- Tie-resolved constraints tagged: {meta['tie_resolved_count']}")
    w()

    # --- 2. Type Transition Table ---
    w("## 2. Type Transitions")
    w()
    if meta["transition_table"]:
        w("| File Type (before) | Engine Type (after) | Count |")
        w("|---|---|---:|")
        for row in meta["transition_table"]:
            w(f"| {row['file_type']} | {row['engine_type']} | {row['count']} |")
        w()
        total_transitions = sum(r["count"] for r in meta["transition_table"])
        w(f"**Total fact-level transitions:** {total_transitions}")
    else:
        w("No transitions — all facts already matched engine output.")
    w()

    # --- 3. Distribution Shift ---
    w("## 3. Distribution Shift")
    w()
    pre = meta.get("pre_distribution", {})
    post = meta.get("post_distribution", {})
    all_types = sorted(set(list(pre.keys()) + list(post.keys())))
    if all_types:
        w("| Type | Before (file) | After (engine) | Delta |")
        w("|---|---:|---:|---:|")
        for t in all_types:
            p = pre.get(t, 0)
            a = post.get(t, 0)
            d = a - p
            sign = "+" if d > 0 else ""
            w(f"| {t} | {p} | {a} | {sign}{d} |")
    w()

    # --- 4. Batch Results ---
    w("## 4. Batch Update Results")
    w()
    if dry_run:
        w(f"- Would modify (facts): {len(batch_results.get('would_modify', []))}")
        w(f"- Would modify (test body): {len(batch_results.get('would_modify_test_body', []))}")
    else:
        w(f"- Facts modified: {len(batch_results.get('modified_facts', []))}")
        w(f"- Test body refs modified: {len(batch_results.get('modified_test_body_facts', []))}")
        w(f"- Files modified: {len(batch_results.get('modified_files', []))}")
    w(f"- Skipped (already correct): {len(batch_results.get('skipped_already_correct', []))}")
    w(f"- Skipped (disk mismatch): {len(batch_results.get('skipped_disk_mismatch', []))}")
    w(f"- Skipped (no regex match): {len(batch_results.get('skipped_no_match', []))}")
    w(f"- Failed swipl: {len(batch_results.get('failed_swipl', []))}")
    w()

    if batch_results.get("skipped_disk_mismatch"):
        w("### Disk Mismatches")
        w()
        w("| Constraint | Perspective | Expected | Found |")
        w("|---|---|---|---|")
        for dm in batch_results["skipped_disk_mismatch"][:20]:
            w(f"| {dm['id']} | {dm['perspective']} | {dm['expected']} | {dm['found']} |")
        if len(batch_results["skipped_disk_mismatch"]) > 20:
            w(f"| ... | ... | ... | ... |")
            w(f"*({len(batch_results['skipped_disk_mismatch'])} total)*")
        w()

    # --- 5. Verification ---
    w("## 5. Verification")
    w()
    if verify_results:
        w(f"- swipl syntax passed: {len(verify_results.get('passed', []))}")
        w(f"- swipl syntax failed: {len(verify_results.get('failed', []))}")
        if verify_results.get("failed"):
            w()
            w("**Failed files:**")
            for f in verify_results["failed"]:
                w(f"- `{os.path.basename(f)}`")
    else:
        w("*(skipped in dry-run mode)*")
    w()

    # --- 6. Engine Agreement Check ---
    w("## 6. Engine Agreement Check")
    w()
    if engine_results:
        w(f"- **Status:** {engine_results['status']}")
        w(f"- Sample size: {engine_results['sample_size']} constraints "
          f"({engine_results['total_checks']} perspective checks)")
        w(f"- Matches: {engine_results['matches']}")
        w(f"- Mismatches: {engine_results['mismatches']}")
        w(f"- Errors: {engine_results['errors']}")
        w(f"- Match rate: {engine_results['match_rate']:.1%}")
        w(f"- Tie-resolved in sample: {engine_results.get('tie_resolved_in_sample', 0)}")
        if engine_results.get("mismatch_details"):
            w()
            w("### Mismatch Details")
            w()
            w("| Constraint | Perspective | Pipeline Type | Live Engine Type | Tie-Resolved |")
            w("|---|---|---|---|---|")
            for md in engine_results["mismatch_details"][:20]:
                w(f"| {md.get('id', '?')} | {md.get('perspective', '?')} "
                  f"| {md.get('engine_type', '?')} | {md.get('live_type', md.get('error', '?'))} "
                  f"| {md.get('tie_resolved', False)} |")
            w()
            if engine_results["status"] == "fail":
                w("**WARNING:** Engine agreement check failed. `enriched_pipeline.json` "
                  "may be stale after the batch claim reconciliation changed 429 "
                  "`constraint_claim/2` values. Consider re-running the pipeline "
                  "before continuing.")
    else:
        w("*(skipped in dry-run mode)*")
    w()

    # --- 7. Deduplication Analysis ---
    w("## 7. Deduplication Analysis")
    w()
    if dedup_results:
        summary = dedup_results.get("summary", {})
        w(f"- Total candidate pairs examined: {summary.get('total_candidates', 0)}")
        w(f"- Hard duplicates: {summary.get('hard_duplicate', 0)}")
        w(f"- Semantic duplicates: {summary.get('semantic_duplicate', 0)}")
        w(f"- Intentional variants: {summary.get('intentional_variant', 0)}")
        w(f"- False positives: {summary.get('false_positive', 0)}")
        w(f"- Constraint ID collisions: {summary.get('id_collisions', 0)}")
        w(f"- Naming mismatches (file ≠ constraint ID): "
          f"{dedup_results.get('total_naming_mismatches', 0)}")
        w()

        # ID collisions
        collisions = dedup_results.get("id_collisions", {})
        if collisions:
            w("### Constraint ID Collisions (Action Required)")
            w()
            w("| Constraint ID | Files |")
            w("|---|---|")
            for cid, files in sorted(collisions.items()):
                w(f"| {cid} | {', '.join(files)} |")
            w()

        # Hard duplicates
        hard_dupes = [c for c in dedup_results.get("candidates", [])
                      if c["classification"]["category"] == "hard_duplicate"]
        if hard_dupes:
            w("### Hard Duplicates (Action Required)")
            w()
            w("| File A | File B | Similarity | ε Diff | Recommendation |")
            w("|---|---|---:|---:|---|")
            for c in hard_dupes:
                cl = c["classification"]
                w(f"| {c['file_a']} | {c['file_b']} | {c['similarity']:.3f} "
                  f"| {cl.get('eps_diff', '?')} | {cl.get('recommendation', '?')} |")
            w()

        # Semantic duplicates
        sem_dupes = [c for c in dedup_results.get("candidates", [])
                     if c["classification"]["category"] == "semantic_duplicate"]
        if sem_dupes:
            w("### Semantic Duplicates (Review Recommended)")
            w()
            w("| File A | File B | Similarity | ε Diff | Reason |")
            w("|---|---|---:|---:|---|")
            for c in sem_dupes:
                cl = c["classification"]
                w(f"| {c['file_a']} | {c['file_b']} | {c['similarity']:.3f} "
                  f"| {cl.get('eps_diff', '?')} | {cl.get('reason', '')} |")
            w()

        # Intentional variants
        variants = [c for c in dedup_results.get("candidates", [])
                    if c["classification"]["category"] == "intentional_variant"]
        if variants:
            w("### Intentional Variants (No Action)")
            w()
            w("| File A | File B | Similarity | Reason |")
            w("|---|---|---:|---|")
            for c in variants:
                cl = c["classification"]
                w(f"| {c['file_a']} | {c['file_b']} | {c['similarity']:.3f} "
                  f"| {cl.get('reason', '')} |")
            w()
    else:
        w("*(dedup analysis not run)*")
    w()

    # --- 8. Deferred Items ---
    w("## 8. Deferred Items")
    w()
    w("1. **`TypeVar == atom` test assertions** (~700 occurrences in ~302 files): "
      "require variable-flow analysis. Failing tests serve as flags for manual update.")
    w("2. **Conditional clause facts** (~129 files): condition and conclusion are coupled. "
      "Candidates for dead-code simplification pass.")
    w("3. **Non-canonical power atoms**: facts with power atoms outside "
      "{powerless, moderate, institutional, analytical} were not reconciled.")
    w("4. **Dedup file deletions**: recommendations above are for human review — "
      "no files were deleted.")
    w()

    # --- 9. Pipeline Re-run Readiness ---
    w("## 9. Pipeline Re-run Readiness")
    w()
    w("After accepting dedup recommendations and addressing deferred items:")
    w()
    w("```bash")
    w("python3 python/run_pipeline.py")
    w("python3 python/tangled_gradient.py")
    w("python3 python/chi_variance_decomposition.py")
    w("python3 python/rope_dominant_spot_check.py")
    w("```")
    w()

    report_text = "\n".join(lines) + "\n"
    with open(REPORT_PATH, "w", encoding="utf-8") as f:
        f.write(report_text)
    print(f"[REB] Report written to {REPORT_PATH}")


# =========================================================================
# Main
# =========================================================================


def main():
    dry_run = "--dry-run" in sys.argv
    snapshot_only = "--snapshot-only" in sys.argv
    dedup_only = "--dedup-only" in sys.argv

    if dry_run:
        print("[REB] DRY RUN — no files will be modified")

    # Load pipeline data
    pipeline = load_json(ENRICHED_PIPELINE_JSON, "enriched_pipeline")
    if not pipeline:
        print("[REB] ERROR: Could not load enriched_pipeline.json", file=sys.stderr)
        sys.exit(1)

    # Build maps
    print("[REB] Building ID-to-file map...")
    id_to_file = build_id_to_file_map(TESTSETS_DIR)
    print(f"[REB] Mapped {len(id_to_file)} constraint IDs to testset files")

    engine_map = build_engine_perspective_map(pipeline)
    print(f"[REB] Built engine perspective map for {len(engine_map)} constraints")

    tie_by_id = load_tie_cases()
    print(f"[REB] Loaded {len(tie_by_id)} tie-resolved constraints")

    # Handle dedup-only mode
    if dedup_only:
        print("[REB] Dedup-only mode — running deduplication analysis...")
        dedup_results = phase_dedup(id_to_file, pipeline)
        # Write minimal output
        output = {"dedup": dedup_results}
        with open(REBUILD_DATA_PATH, "w", encoding="utf-8") as f:
            json.dump(output, f, indent=2, default=str)
        print(f"[REB] Dedup data written to {REBUILD_DATA_PATH}")
        # Generate report with dedup only
        phase_report({
            "total_constraints_in_engine": len(engine_map),
            "total_facts": 0, "matched_facts": 0, "mismatched_facts": 0,
            "skipped_non_canonical": 0, "skipped_conditional": 0,
            "missing_engine_perspectives": 0, "no_classification_facts": 0,
            "transition_table": [], "pre_distribution": {},
            "post_distribution": {}, "tie_resolved_count": 0,
        }, {}, None, None, dedup_results, dry_run=False)
        return

    # Phase 1: Snapshot
    print("[REB] Phase 1: Computing classification snapshot...")
    snapshot, file_edits = phase_snapshot(engine_map, id_to_file, tie_by_id)
    meta = {k: v for k, v in snapshot.items() if k != "per_constraint"}
    print(f"[REB] Phase 1 complete: {meta['mismatched_facts']} mismatched facts "
          f"across {len(file_edits)} files")

    if snapshot_only:
        with open(REBUILD_DATA_PATH, "w", encoding="utf-8") as f:
            json.dump(snapshot, f, indent=2, default=str)
        print(f"[REB] Snapshot written to {REBUILD_DATA_PATH}")
        print("[REB] --snapshot-only mode, stopping after Phase 1")
        return

    # Phase 2: Batch update
    print(f"[REB] Phase 2: Batch updating classification facts...")
    batch_results = phase_batch_update(file_edits, dry_run=dry_run)
    n_mod = (len(batch_results["modified_facts"]) if not dry_run
             else len(batch_results["would_modify"]))
    n_test = (len(batch_results["modified_test_body_facts"]) if not dry_run
              else len(batch_results["would_modify_test_body"]))
    print(f"[REB] Phase 2 complete: {n_mod} facts + {n_test} test body refs "
          f"{'would be modified' if dry_run else 'modified'}")

    # Phase 3: Verify
    verify_results = None
    if not dry_run and batch_results.get("modified_files"):
        print(f"[REB] Phase 3: Verifying {len(batch_results['modified_files'])} files...")
        verify_results = phase_verify(batch_results["modified_files"])
        print(f"[REB] Phase 3 complete: {len(verify_results['passed'])} passed, "
              f"{len(verify_results['failed'])} failed")

    # Phase 3.5: Engine agreement check
    # Use modified_facts if available, otherwise use all engine constraints
    engine_results = None
    if not dry_run:
        # Build a sample-worthy fact list: modified facts, or synthesize
        # from all engine constraints if this is an idempotency re-run
        check_facts = batch_results.get("modified_facts", [])
        if not check_facts:
            # Idempotency re-run: synthesize from engine map
            check_facts = [{"id": cid} for cid in engine_map]
        print("[REB] Phase 3.5: Engine agreement check...")
        engine_results = phase_engine_check(
            engine_map, tie_by_id, check_facts
        )
        print(f"[REB] Phase 3.5 complete: {engine_results['status']} "
              f"({engine_results['matches']}/{engine_results['total_checks']} match)")

        if engine_results["status"] == "fail":
            print()
            print("=" * 60)
            print("WARNING: ENGINE AGREEMENT CHECK FAILED")
            print("=" * 60)
            print(f"  Mismatches: {engine_results['mismatches']}")
            print(f"  Errors: {engine_results['errors']}")
            print("  enriched_pipeline.json may be stale.")
            print("  Review mismatch details in the report before proceeding.")
            print("=" * 60)

    # Phase 4: Dedup
    print("[REB] Phase 4: Running deduplication analysis...")
    dedup_results = phase_dedup(id_to_file, pipeline)
    dedup_summary = dedup_results.get("summary", {})
    print(f"[REB] Phase 4 complete: {dedup_summary.get('total_candidates', 0)} pairs, "
          f"{dedup_summary.get('hard_duplicate', 0)} hard dupes, "
          f"{dedup_summary.get('semantic_duplicate', 0)} semantic dupes")

    # Write full data artifact
    output_data = {
        **snapshot,
        "batch_results": batch_results,
        "verify_results": verify_results,
        "engine_results": engine_results,
        "dedup": dedup_results,
    }
    with open(REBUILD_DATA_PATH, "w", encoding="utf-8") as f:
        json.dump(output_data, f, indent=2, default=str)
    print(f"[REB] Data written to {REBUILD_DATA_PATH}")

    # Phase 5: Report
    print("[REB] Phase 5: Generating report...")
    phase_report(snapshot, batch_results, verify_results, engine_results,
                 dedup_results, dry_run)

    # Summary
    print()
    print("=" * 60)
    print("TESTSET REBUILD SUMMARY")
    print("=" * 60)
    print(f"  Constraints in engine:     {meta['total_constraints_in_engine']}")
    print(f"  Classification facts:      {meta['total_facts']}")
    print(f"  Already matching:          {meta['matched_facts']}")
    print(f"  Mismatched:                {meta['mismatched_facts']}")
    print(f"  Facts reconciled:          {n_mod}")
    print(f"  Test body refs updated:    {n_test}")
    print(f"  Conditional (skipped):     {meta['skipped_conditional']}")
    print(f"  Non-canonical (skipped):   {meta['skipped_non_canonical']}")
    print(f"  Tie-resolved tagged:       {meta['tie_resolved_count']}")
    if engine_results:
        print(f"  Engine agreement:          {engine_results['status']} "
              f"({engine_results['match_rate']:.1%})")
    print(f"  Dedup candidates:          {dedup_summary.get('total_candidates', 0)}")
    print(f"  Hard duplicates:           {dedup_summary.get('hard_duplicate', 0)}")
    print(f"  Data:                      {REBUILD_DATA_PATH}")
    print(f"  Report:                    {REPORT_PATH}")
    if dry_run:
        print()
        print("  [DRY RUN — no files were modified]")


if __name__ == "__main__":
    main()
