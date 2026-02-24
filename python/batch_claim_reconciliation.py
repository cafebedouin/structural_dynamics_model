#!/usr/bin/env python3
"""batch_claim_reconciliation.py — Reconcile stale constraint_claim/2 predicates.

Compares claimed_type (from constraint_claim/2 in Prolog spec files) against
modal engine-computed type (from drl_core:dr_type/3 perspectives in
enriched_pipeline.json) and batch-updates mismatched claims.

Five phases:
  1. Snapshot  — compute mismatches, emit JSON artifact
  2. Batch     — rewrite constraint_claim atoms in prolog/testsets/*.pl
  3. Python    — fix 4 hardcoded threshold values in 2 Python scripts
  4. Quine     — verify quine_self_replication was reconciled
  5. Report    — generate docs/batch_claim_reconciliation.md

Reads:  outputs/enriched_pipeline.json, prolog/testsets/*.pl
Writes: outputs/claim_engine_mismatch_snapshot.json
        docs/batch_claim_reconciliation.md
        (modifies prolog/testsets/*.pl in-place)
        (modifies python/coordination_vitality_diagnostic.py lines 58-59)
        (modifies python/scaffold_piton_gate_audit.py lines 51-52)

Usage:
    python3 python/batch_claim_reconciliation.py
    python3 python/batch_claim_reconciliation.py --dry-run
    python3 python/batch_claim_reconciliation.py --snapshot-only
    python3 python/batch_claim_reconciliation.py --verify-swipl
"""

import glob
import json
import os
import re
import subprocess
import sys
from collections import Counter
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
SNAPSHOT_PATH = OUTPUT_DIR / "claim_engine_mismatch_snapshot.json"
REPORT_PATH = DOCS_DIR / "batch_claim_reconciliation.md"

# ---------------------------------------------------------------------------
# Tie-breaking precedence (leftmost wins)
# ---------------------------------------------------------------------------

TIE_PRECEDENCE = [
    "snare", "tangled_rope", "piton", "scaffold",
    "rope", "mountain", "naturalized",
]

# ---------------------------------------------------------------------------
# Python hardcoded value fixes
# ---------------------------------------------------------------------------

PYTHON_FIXES = [
    {
        "file": ROOT_DIR / "python" / "coordination_vitality_diagnostic.py",
        "old": "PITON_EXTRACTION_CEILING = 0.25",
        "new": "PITON_EXTRACTION_CEILING = 0.45",
        "param": "piton_extraction_ceiling",
    },
    {
        "file": ROOT_DIR / "python" / "coordination_vitality_diagnostic.py",
        "old": "SCAFFOLD_EXTRACTION_CEIL = 0.30",
        "new": "SCAFFOLD_EXTRACTION_CEIL = 0.45",
        "param": "scaffold_extraction_ceil",
    },
    {
        "file": ROOT_DIR / "python" / "scaffold_piton_gate_audit.py",
        "old": "SCAFFOLD_EXTRACTION_CEIL = 0.30",
        "new": "SCAFFOLD_EXTRACTION_CEIL = 0.45",
        "param": "scaffold_extraction_ceil",
    },
    {
        "file": ROOT_DIR / "python" / "scaffold_piton_gate_audit.py",
        "old": "PITON_EXTRACTION_CEILING = 0.25",
        "new": "PITON_EXTRACTION_CEILING = 0.45",
        "param": "piton_extraction_ceiling",
    },
]

# =========================================================================
# Helpers
# =========================================================================


def build_id_to_file_map(testsets_dir):
    """Scan testset .pl files for constraint IDs.

    Two strategies (adapted from update_testset_vitality.py:37-64):
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


def compute_modal_type(perspectives):
    """Compute modal type from 4 perspective values.

    Returns the most common type.  On ties, uses TIE_PRECEDENCE.
    Returns None if any perspective is 'unknown' or data is incomplete.
    """
    vals = list(perspectives.values())
    if len(vals) != 4 or "unknown" in vals:
        return None
    counter = Counter(vals)
    max_count = counter.most_common(1)[0][1]
    tied = [t for t, c in counter.items() if c == max_count]
    if len(tied) == 1:
        return tied[0]
    for pref in TIE_PRECEDENCE:
        if pref in tied:
            return pref
    return tied[0]


def make_claim_pattern(constraint_id):
    """Build regex for matching concrete constraint_claim facts.

    Matches both qualified and unqualified forms:
      narrative_ontology:constraint_claim(ID, atom_type).
      constraint_claim(ID, atom_type).

    Only matches concrete atoms ([a-z]\\w*), excluding Prolog variables
    (uppercase start), anonymous variables (_prefix), and bracket-lists.

    Returns compiled regex with 3 groups: prefix, type atom, suffix.
    """
    # Handle quoted atoms: Prolog requires quoting atoms starting with
    # a digit (e.g., '26usc469_real_estate_exemption').
    return re.compile(
        r"^("
        r"(?:narrative_ontology:)?"
        r"constraint_claim\("
        r"'?" + re.escape(constraint_id) + r"'?"
        r",\s*"
        r")"
        r"([a-z]\w*)"
        r"(\)\s*\.)",
        re.MULTILINE,
    )


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
# Phase 1: Snapshot
# =========================================================================


def phase_snapshot(pipeline_data, id_to_file):
    """Compute claim/engine mismatches and write snapshot JSON.

    Returns (per_constraint_list, metadata_dict).
    """
    constraints = pipeline_data.get("per_constraint", [])
    per_constraint = []
    claimed_dist = Counter()
    modal_dist = Counter()
    tie_cases = []
    skipped = []

    for c in constraints:
        cid = c["id"]
        claimed = c.get("claimed_type")
        perspectives = c.get("perspectives", {})
        modal = compute_modal_type(perspectives)

        # Track distributions (only for non-None)
        if claimed is not None:
            claimed_dist[claimed] += 1
        if modal is not None:
            modal_dist[modal] += 1

        # Detect ties
        vals = list(perspectives.values())
        tie_info = None
        if len(vals) == 4 and "unknown" not in vals:
            counter = Counter(vals)
            max_count = counter.most_common(1)[0][1]
            tied = [t for t, cnt in counter.items() if cnt == max_count]
            if len(tied) > 1:
                tie_info = {
                    "tied_types": sorted(tied),
                    "counts": dict(counter),
                    "resolved_to": modal,
                }
                tie_cases.append({"id": cid, **tie_info})

        if claimed is None or modal is None:
            skipped.append({
                "id": cid,
                "reason": "null_claimed" if claimed is None else "unknown_perspective",
                "claimed_type": claimed,
            })
            per_constraint.append({
                "id": cid,
                "claimed_type": claimed,
                "modal_type": modal,
                "perspectives": perspectives,
                "file_path": id_to_file.get(cid),
                "match": None,
                "tie_info": tie_info,
            })
            continue

        match = (claimed == modal)
        per_constraint.append({
            "id": cid,
            "claimed_type": claimed,
            "modal_type": modal,
            "perspectives": perspectives,
            "file_path": id_to_file.get(cid),
            "match": match,
            "tie_info": tie_info,
        })

    # Compute mismatch table
    mismatch_table = Counter()
    mismatches = []
    for rec in per_constraint:
        if rec["match"] is False:
            mismatch_table[(rec["claimed_type"], rec["modal_type"])] += 1
            mismatches.append(rec)

    matched = sum(1 for r in per_constraint if r["match"] is True)

    # Distribution delta
    all_types = sorted(set(list(claimed_dist.keys()) + list(modal_dist.keys())))
    distribution_shift = {
        "claimed": dict(claimed_dist),
        "modal": dict(modal_dist),
        "delta": {t: modal_dist[t] - claimed_dist[t] for t in all_types},
    }

    # Sum-verify mismatch table
    table_sum = sum(mismatch_table.values())
    assert table_sum == len(mismatches), (
        f"Mismatch table sum ({table_sum}) != mismatch count ({len(mismatches)})"
    )

    # Build mismatch table list sorted by count desc
    mismatch_table_list = [
        {"claimed": old, "modal": new, "count": cnt}
        for (old, new), cnt in sorted(mismatch_table.items(), key=lambda x: -x[1])
    ]

    # Unmapped constraints (no file found)
    unmapped = [r["id"] for r in per_constraint if r["file_path"] is None]

    metadata = {
        "generated": datetime.now(timezone.utc).isoformat(),
        "total_constraints": len(constraints),
        "matched": matched,
        "mismatched": len(mismatches),
        "mismatch_rate": round(len(mismatches) / len(constraints), 4)
            if constraints else 0,
        "skipped": skipped,
        "unmapped_ids": unmapped,
        "tie_cases": tie_cases,
        "distribution_shift": distribution_shift,
        "mismatch_table": mismatch_table_list,
        "tie_breaking_precedence": TIE_PRECEDENCE,
    }

    # Write snapshot
    snapshot = {**metadata, "per_constraint": per_constraint}
    with open(SNAPSHOT_PATH, "w", encoding="utf-8") as f:
        json.dump(snapshot, f, indent=2, default=str)
    print(f"[REC] Snapshot written to {SNAPSHOT_PATH}")

    return mismatches, metadata


# =========================================================================
# Phase 2: Batch Update
# =========================================================================


def phase_batch_update(mismatches, dry_run=False, do_swipl=False):
    """Batch-update constraint_claim atoms in Prolog testset files.

    Safety logic per constraint:
      - on-disk atom == modal_type  → skip (already correct)
      - on-disk atom != claimed AND != modal → skip (unexpected state)
      - on-disk atom == claimed_type → replace with modal_type
    """
    results = {
        "modified": [],
        "skipped_no_file": [],
        "skipped_already_correct": [],
        "skipped_disk_mismatch": [],
        "skipped_no_match": [],
        "would_modify": [],
        "failed_swipl": [],
    }

    # Group by file to handle multi-constraint files efficiently
    file_edits = {}  # filepath -> list of (cid, old_type, new_type)
    for mm in mismatches:
        fp = mm.get("file_path")
        if not fp or not os.path.exists(fp):
            results["skipped_no_file"].append(mm["id"])
            continue
        file_edits.setdefault(fp, []).append(
            (mm["id"], mm["claimed_type"], mm["modal_type"])
        )

    for filepath, edits in sorted(file_edits.items()):
        with open(filepath) as f:
            content = f.read()
        original = content

        file_modified = False
        for cid, old_type, new_type in edits:
            pattern = make_claim_pattern(cid)
            matches = pattern.findall(content)

            if not matches:
                results["skipped_no_match"].append(cid)
                continue

            # Extract on-disk type atoms (group index 1 = type atom)
            on_disk_types = [m[1] for m in matches]

            if new_type in on_disk_types:
                results["skipped_already_correct"].append(cid)
                continue

            if old_type not in on_disk_types:
                results["skipped_disk_mismatch"].append({
                    "id": cid,
                    "expected": old_type,
                    "found_on_disk": on_disk_types,
                })
                continue

            # Perform the replacement
            content = pattern.sub(rf"\g<1>{new_type}\g<3>", content)
            file_modified = True

            if dry_run:
                results["would_modify"].append(cid)
            else:
                results["modified"].append(cid)

        if not file_modified or content == original:
            continue

        if dry_run:
            continue

        with open(filepath, "w") as f:
            f.write(content)

        if do_swipl:
            ok = verify_swipl_syntax(filepath)
            if not ok:
                # Rollback
                with open(filepath, "w") as f:
                    f.write(original)
                # Move from modified to failed
                for cid, _, _ in edits:
                    if cid in results["modified"]:
                        results["modified"].remove(cid)
                        results["failed_swipl"].append(cid)

    return results


# =========================================================================
# Phase 3: Python Value Fixes
# =========================================================================


def phase_python_fixes(dry_run=False):
    """Fix 4 hardcoded threshold values in Python scripts."""
    results = {"modified": [], "skipped": []}
    today = datetime.now().strftime("%Y-%m-%d")

    for fix in PYTHON_FIXES:
        filepath = fix["file"]
        old_text = fix["old"]
        new_text = fix["new"]
        param = fix["param"]

        with open(filepath) as f:
            content = f.read()

        # Find the line containing old_text and replace the entire line
        lines = content.split("\n")
        found = False
        replacement = new_text + f"  # Must match config.pl param: {param} -- updated {today}"
        for i, line in enumerate(lines):
            if old_text in line:
                found = True
                target_line = i
                break

        if not found:
            results["skipped"].append({
                "file": str(filepath.relative_to(ROOT_DIR)),
                "old": old_text,
                "reason": "old text not found in file",
            })
            continue

        if dry_run:
            results["modified"].append({
                "file": str(filepath.relative_to(ROOT_DIR)),
                "old": old_text,
                "new": replacement,
            })
            continue

        # Replace the full line, preserving indentation
        indent = lines[target_line][:len(lines[target_line]) - len(lines[target_line].lstrip())]
        lines[target_line] = indent + replacement
        content = "\n".join(lines)

        with open(filepath, "w") as f:
            f.write(content)

        results["modified"].append({
            "file": str(filepath.relative_to(ROOT_DIR)),
            "old": old_text,
            "new": replacement,
        })

    return results


# =========================================================================
# Phase 4: Quine Guard
# =========================================================================


def phase_quine_guard(mismatches, batch_results):
    """Verify quine_self_replication was reconciled to rope."""
    quine_mms = [m for m in mismatches if m["id"] == "quine_self_replication"]
    if not quine_mms:
        return {
            "status": "NOT_IN_MISMATCHES",
            "warning": "quine_self_replication not found in mismatch set",
        }

    q = quine_mms[0]
    modified = batch_results.get("modified", [])
    already = batch_results.get("skipped_already_correct", [])
    would = batch_results.get("would_modify", [])

    if q["id"] in modified or q["id"] in would:
        return {
            "status": "reconciled",
            "old_type": q["claimed_type"],
            "new_type": q["modal_type"],
            "perspectives": q["perspectives"],
        }
    elif q["id"] in already:
        return {
            "status": "already_correct_on_disk",
            "modal_type": q["modal_type"],
            "perspectives": q["perspectives"],
        }
    else:
        return {
            "status": "skipped",
            "detail": "check batch results for reason",
            "modal_type": q["modal_type"],
        }


# =========================================================================
# Phase 5: Report
# =========================================================================


def phase_report(metadata, batch_results, python_results, quine_results, dry_run):
    """Generate docs/batch_claim_reconciliation.md."""
    today = datetime.now().strftime("%Y-%m-%d")
    lines = []

    def w(s=""):
        lines.append(s)

    # --- Header ---
    w("# Batch Claim Reconciliation Report")
    w()
    w(f"**Generated:** {today}")
    w()

    # --- 1. Executive Summary ---
    w("## 1. Executive Summary")
    w()
    n_mod = len(batch_results.get("modified", []))
    n_would = len(batch_results.get("would_modify", []))
    n_reconciled = n_mod if not dry_run else n_would
    w(f"Reconciled **{n_reconciled}** of {metadata['total_constraints']} "
      f"constraint claims to match engine-computed modal types. "
      f"Pre-reconciliation mismatch rate: {metadata['mismatch_rate']:.1%} "
      f"({metadata['mismatched']} constraints).")
    w()
    w("**Rationale:** `constraint_claim/2` predicates should reflect the current "
      "engine baseline so that future LLM-generated constraint stories produce "
      "meaningful diffs against an accurate reference. The mismatched claims were "
      "stale artifacts from earlier calibrations.")
    w()

    # --- 2. Pre-Reconciliation Snapshot ---
    w("## 2. Pre-Reconciliation Snapshot")
    w()
    w(f"Full mismatch data preserved in `outputs/claim_engine_mismatch_snapshot.json`.")
    w()
    w(f"- Total constraints: {metadata['total_constraints']}")
    w(f"- Matched (claim == modal): {metadata['matched']}")
    w(f"- Mismatched: {metadata['mismatched']}")
    w(f"- Skipped (null/unknown): {len(metadata['skipped'])}")
    w(f"- Unmapped (no file found): {len(metadata['unmapped_ids'])}")
    if metadata["unmapped_ids"]:
        w(f"  - IDs: {', '.join(metadata['unmapped_ids'][:10])}")
    w()

    # --- 3. Mismatch Table ---
    w("## 3. Reconciliation Log")
    w()
    w("### Mismatch Transitions")
    w()
    w("| Claimed (before) | Modal (after) | Count |")
    w("|---|---|---:|")
    for row in metadata["mismatch_table"]:
        w(f"| {row['claimed']} | {row['modal']} | {row['count']} |")
    w()
    total_table = sum(r["count"] for r in metadata["mismatch_table"])
    w(f"**Total:** {total_table}")
    w()

    # Batch results summary
    w("### Batch Update Results")
    w()
    if dry_run:
        w(f"- Would modify: {len(batch_results.get('would_modify', []))}")
    else:
        w(f"- Files modified: {len(batch_results.get('modified', []))}")
    w(f"- Skipped (no file): {len(batch_results.get('skipped_no_file', []))}")
    w(f"- Skipped (already correct on disk): "
      f"{len(batch_results.get('skipped_already_correct', []))}")
    w(f"- Skipped (disk mismatch): "
      f"{len(batch_results.get('skipped_disk_mismatch', []))}")
    w(f"- Skipped (no regex match): "
      f"{len(batch_results.get('skipped_no_match', []))}")
    w(f"- Failed swipl: {len(batch_results.get('failed_swipl', []))}")
    w()

    if batch_results.get("skipped_disk_mismatch"):
        w("#### Disk Mismatches (already reconciled or unexpected state)")
        w()
        w("| Constraint ID | Expected | Found on Disk |")
        w("|---|---|---|")
        for dm in batch_results["skipped_disk_mismatch"]:
            w(f"| {dm['id']} | {dm['expected']} | {', '.join(dm['found_on_disk'])} |")
        w()

    # --- 4. Distribution Shift ---
    w("## 4. Distribution Shift")
    w()
    ds = metadata["distribution_shift"]
    w("| Type | Claimed (before) | Modal (after) | Delta |")
    w("|---|---:|---:|---:|")
    all_types = sorted(set(list(ds["claimed"].keys()) + list(ds["modal"].keys())))
    for t in all_types:
        c = ds["claimed"].get(t, 0)
        m = ds["modal"].get(t, 0)
        d = ds["delta"].get(t, 0)
        sign = "+" if d > 0 else ""
        w(f"| {t} | {c} | {m} | {sign}{d} |")
    w()

    # --- 5. Tie-Breaking Decisions ---
    w("## 5. Tie-Breaking Decisions")
    w()
    w(f"**Precedence:** {' > '.join(TIE_PRECEDENCE)}")
    w()
    w(f"**Total ties resolved:** {len(metadata['tie_cases'])}")
    w()
    if metadata["tie_cases"]:
        # Summarize by pattern
        patterns = Counter(
            tuple(sorted(tc["tied_types"])) for tc in metadata["tie_cases"]
        )
        w("| Tied Types | Count | Resolved To |")
        w("|---|---:|---|")
        for pat, cnt in patterns.most_common():
            # Find the resolution for this pattern
            resolved = next(
                tc["resolved_to"] for tc in metadata["tie_cases"]
                if tuple(sorted(tc["tied_types"])) == pat
            )
            w(f"| {' vs '.join(pat)} | {cnt} | {resolved} |")
        w()

    # --- 6. Python Fixes ---
    w("## 6. Python Value Fixes")
    w()
    if python_results["modified"]:
        w("| File | Old | New |")
        w("|---|---|---|")
        for fix in python_results["modified"]:
            w(f"| `{fix['file']}` | `{fix['old']}` | `{fix['new']}` |")
        w()
    if python_results["skipped"]:
        w("**Skipped:**")
        for s in python_results["skipped"]:
            w(f"- `{s['file']}`: {s['reason']}")
        w()

    # --- 7. Quine Resolution ---
    w("## 7. Quine Self-Replication Resolution")
    w()
    w(f"- **Status:** {quine_results['status']}")
    if "old_type" in quine_results:
        w(f"- **Transition:** {quine_results['old_type']} → {quine_results['new_type']}")
    if "perspectives" in quine_results:
        p = quine_results["perspectives"]
        w(f"- **Perspectives:** powerless={p.get('powerless')}, "
          f"moderate={p.get('moderate')}, institutional={p.get('institutional')}, "
          f"analytical={p.get('analytical')}")
        w("- **Tie resolution:** mountain vs rope → rope (rope > mountain in precedence)")
    w()

    # --- 8. Pipeline Re-run Sequence ---
    w("## 8. Pipeline Re-run Sequence")
    w()
    w("After reconciliation, regenerate downstream artifacts:")
    w()
    w("```bash")
    w("make pipeline_output")
    w("make enriched_pipeline")
    w("python3 python/tangled_gradient.py")
    w("python3 python/chi_variance_decomposition.py")
    w("python3 python/rope_dominant_spot_check.py")
    w("```")
    w()
    w("**Expected changes:**")
    w("- `tangled_rope` pool shrinks significantly (claimed distribution shifts)")
    w("- `snare` pool grows correspondingly")
    w("- Tangled gradient subtypes may redistribute")
    w("- The 88% genuinely-perspectival finding should be re-verified against "
      "the reconciled population")
    w("- Scaffold/piton gate audit results will change due to threshold corrections "
      "(0.25/0.30 → 0.45)")
    w()

    # --- 9. Remaining Deferred Items ---
    w("## 9. Remaining Deferred Items")
    w()
    w("This reconciliation does **not** address:")
    w()
    w("1. **Dynamic config reading** — Python files still hardcode thresholds "
      "(now correct values). Refactoring to use `shared.loader.read_config()` "
      "is a separate task.")
    w("2. **27 dead/docs-only config parameters** — flagged in followup Part 4c, "
      "not cleaned up.")
    w("3. **`contamination_strength_*` config/code bypass** — "
      "`drl_purity_network.pl` ignores these config values.")
    w("4. **`decentralized_infrastructure_rope` missing testsets copy** — "
      "constraint exists in `prolog/testsets/new_civilizational_rope.pl` but "
      "has no dedicated testset file.")
    w("5. **`constraint_classification/3` reconciliation** — indexed "
      "classifications may also diverge from engine output; out of scope for "
      "this claim-only reconciliation.")
    w()

    report_text = "\n".join(lines) + "\n"
    with open(REPORT_PATH, "w", encoding="utf-8") as f:
        f.write(report_text)
    print(f"[REC] Report written to {REPORT_PATH}")


# =========================================================================
# Main
# =========================================================================


def main():
    dry_run = "--dry-run" in sys.argv
    snapshot_only = "--snapshot-only" in sys.argv
    do_swipl = "--verify-swipl" in sys.argv

    if dry_run:
        print("[REC] DRY RUN — no files will be modified")

    # Load pipeline data
    pipeline = load_json(ENRICHED_PIPELINE_JSON, "enriched_pipeline")
    if not pipeline:
        print("[REC] ERROR: Could not load enriched_pipeline.json", file=sys.stderr)
        sys.exit(1)

    # Build ID -> file mapping
    id_to_file = build_id_to_file_map(TESTSETS_DIR)
    print(f"[REC] Mapped {len(id_to_file)} constraint IDs to testset files")

    # Phase 1: Snapshot
    print("[REC] Phase 1: Computing mismatch snapshot...")
    mismatches, metadata = phase_snapshot(pipeline, id_to_file)
    print(f"[REC] Phase 1 complete: {metadata['mismatched']} mismatches, "
          f"{len(metadata['tie_cases'])} ties")

    if snapshot_only:
        print("[REC] --snapshot-only mode, stopping after Phase 1")
        return

    # Phase 2: Batch update
    print(f"[REC] Phase 2: Batch updating {len(mismatches)} constraint claims...")
    batch_results = phase_batch_update(mismatches, dry_run=dry_run, do_swipl=do_swipl)
    n_mod = len(batch_results["modified"]) if not dry_run else len(batch_results["would_modify"])
    print(f"[REC] Phase 2 complete: {n_mod} constraints "
          f"{'would be modified' if dry_run else 'modified'}")

    # Phase 3: Python fixes
    print("[REC] Phase 3: Fixing Python hardcoded values...")
    python_results = phase_python_fixes(dry_run=dry_run)
    print(f"[REC] Phase 3 complete: {len(python_results['modified'])} values fixed")

    # Phase 4: Quine guard
    print("[REC] Phase 4: Quine guard check...")
    quine_results = phase_quine_guard(mismatches, batch_results)
    print(f"[REC] Phase 4 complete: quine status = {quine_results['status']}")

    # Phase 5: Report
    print("[REC] Phase 5: Generating report...")
    phase_report(metadata, batch_results, python_results, quine_results, dry_run)

    # Summary
    print()
    print("=" * 60)
    print("RECONCILIATION SUMMARY")
    print("=" * 60)
    print(f"  Total constraints:     {metadata['total_constraints']}")
    print(f"  Mismatches found:      {metadata['mismatched']}")
    print(f"  Claims reconciled:     {n_mod}")
    print(f"  Skipped (no file):     {len(batch_results['skipped_no_file'])}")
    print(f"  Skipped (already ok):  {len(batch_results['skipped_already_correct'])}")
    print(f"  Skipped (disk diff):   {len(batch_results['skipped_disk_mismatch'])}")
    print(f"  Skipped (no match):    {len(batch_results['skipped_no_match'])}")
    print(f"  Failed swipl:          {len(batch_results['failed_swipl'])}")
    print(f"  Python fixes:          {len(python_results['modified'])}")
    print(f"  Quine:                 {quine_results['status']}")
    print(f"  Snapshot:              {SNAPSHOT_PATH}")
    print(f"  Report:                {REPORT_PATH}")
    if dry_run:
        print()
        print("  [DRY RUN — no files were modified]")


if __name__ == "__main__":
    main()
