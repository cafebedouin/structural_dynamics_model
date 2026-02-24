#!/usr/bin/env python3
"""Type Count Reconciliation Audit

Reconciles four different tangled_rope population counts (773, 677, 606, 752)
across different pipeline stages and counting methods.

Steps:
  1.1  Raw field census — cross-tabulation of 7 types × 6 counting columns
  1.2  TYPE_CONFIGS filter logic replication (standard, unanimity, any_perspective)
  1.3  Historical count reconciliation
  1.4  Double-counting check (any_perspective overlap between types)
  1.5  Canonical count establishment (structural vs report regimes)
  1.6  Output generation

Reads:  outputs/enriched_pipeline.json
Writes: docs/type_count_reconciliation.md
        outputs/type_count_reconciliation.json

Usage:  python3 python/type_count_reconciliation.py
"""

import json
import sys
from collections import Counter, defaultdict
from datetime import datetime
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))

from shared.loader import load_json, ENRICHED_PIPELINE_JSON, OUTPUT_DIR
from shared.constants import MAXENT_TYPES
from shared.schemas import validate_enriched_pipeline

# ---------------------------------------------------------------------------
# Path constants
# ---------------------------------------------------------------------------

ROOT_DIR = Path(__file__).resolve().parent.parent
DOCS_DIR = ROOT_DIR / "docs"
REPORT_PATH = DOCS_DIR / "type_count_reconciliation.md"
DATA_PATH = OUTPUT_DIR / "type_count_reconciliation.json"

# Perspective keys
PERSPECTIVE_KEYS = ["powerless", "moderate", "institutional", "analytical"]

# ---------------------------------------------------------------------------
# TYPE_CONFIGS filter methods (replicated from type_reporter.py)
# ---------------------------------------------------------------------------
# Maps each type to its filter method:
#   standard       — claimed_type == type_name
#   unanimity      — claimed_type == type_name AND all non-unknown perspectives match
#   any_perspective — ANY perspective value == type_name (ignores claimed_type)

TYPE_FILTER_MAP = {
    "snare":        "standard",
    "scaffold":     "standard",
    "rope":         "unanimity",
    "mountain":     "unanimity",
    "tangled_rope": "any_perspective",
    "piton":        "any_perspective",
}

TYPE_FAMILY_MAP = {
    "snare":        "diagnostic",
    "scaffold":     "diagnostic",
    "rope":         "validation",
    "mountain":     "validation",
    "tangled_rope": "diagnostic",
    "piton":        "diagnostic",
}

# ---------------------------------------------------------------------------
# Step 1.1 — Raw field census
# ---------------------------------------------------------------------------

def raw_field_census(constraints):
    """Cross-tabulate types across 6 counting columns."""
    # Counting columns:
    #   claimed_type, maxent_top_type, p.powerless, p.moderate, p.institutional, p.analytical
    columns = ["claimed_type", "maxent_top_type",
               "p.powerless", "p.moderate", "p.institutional", "p.analytical"]

    tallies = {col: Counter() for col in columns}

    for c in constraints:
        ct = c.get("claimed_type")
        if ct:
            tallies["claimed_type"][ct] += 1

        mt = c.get("maxent_top_type")
        if mt:
            tallies["maxent_top_type"][mt] += 1

        persp = c.get("perspectives", {})
        for pkey in PERSPECTIVE_KEYS:
            pval = persp.get(pkey)
            if pval and pval != "unknown":
                tallies[f"p.{pkey}"][pval] += 1

    return tallies


# ---------------------------------------------------------------------------
# Step 1.2 — Replicate TYPE_CONFIGS filter logic
# ---------------------------------------------------------------------------

def filter_standard(constraints, type_name):
    """claimed_type == type_name."""
    return [c for c in constraints if c.get("claimed_type") == type_name]


def filter_unanimity(constraints, type_name):
    """claimed_type == type_name AND all non-unknown perspectives match."""
    results = []
    for c in constraints:
        if c.get("claimed_type") != type_name:
            continue
        persp = c.get("perspectives", {})
        non_unknown = {k: v for k, v in persp.items()
                       if v not in ("unknown", None)}
        if not non_unknown:
            continue
        if all(v == type_name for v in non_unknown.values()):
            results.append(c)
    return results


def filter_any_perspective(constraints, type_name):
    """ANY perspective value == type_name (ignores claimed_type)."""
    results = []
    for c in constraints:
        persp = c.get("perspectives", {})
        if any(v == type_name for v in persp.values()):
            results.append(c)
    return results


def apply_filter(constraints, type_name):
    """Apply the appropriate filter for a type."""
    method = TYPE_FILTER_MAP.get(type_name, "standard")
    if method == "unanimity":
        return filter_unanimity(constraints, type_name)
    elif method == "any_perspective":
        return filter_any_perspective(constraints, type_name)
    else:
        return filter_standard(constraints, type_name)


# ---------------------------------------------------------------------------
# Step 1.2 continued — Diagnostic normalization + dedup
# ---------------------------------------------------------------------------

def normalize_diagnostic(constraint):
    """Emit one entry per omega (diagnostic family normalization)."""
    omegas = constraint.get("omegas") or []
    if not omegas:
        return [(constraint["id"], "N/A")]
    return [(constraint["id"], o.get("question", "N/A")) for o in omegas]


def normalize_validation(constraint):
    """Emit one entry per constraint (validation family normalization)."""
    return [(constraint["id"], None)]


def normalize_and_dedup(filtered, type_name):
    """Apply family-specific normalization and dedup."""
    family = TYPE_FAMILY_MAP.get(type_name, "diagnostic")

    entries = []
    for c in filtered:
        if family == "diagnostic":
            entries.extend(normalize_diagnostic(c))
        else:
            entries.extend(normalize_validation(c))

    # Dedup
    if family == "diagnostic":
        seen = set()
        unique = []
        for name, omega_q in entries:
            key = (name, omega_q)
            if key not in seen:
                seen.add(key)
                unique.append(key)
        return unique
    else:
        seen = set()
        unique = []
        for name, _ in entries:
            if name not in seen:
                seen.add(name)
                unique.append((name, None))
        return unique


# ---------------------------------------------------------------------------
# Step 1.3 — Historical count reconciliation
# ---------------------------------------------------------------------------

HISTORICAL_COUNTS = [
    {
        "count": 773,
        "method": "any_perspective",
        "pipeline_state": "Pre-piton-gate-changes",
        "reproducible": False,
        "notes": "Historical count before coordination_vitality gate additions. "
                 "Not reproducible from current data — represents earlier pipeline state."
    },
    {
        "count": 677,
        "method": "any_perspective",
        "pipeline_state": "Post-gate-changes, pre-FCR-fix",
        "reproducible": False,
        "notes": "After piton gate changes but before the false_ci_rope override fix. "
                 "Not reproducible from current data — represents intermediate pipeline state."
    },
    {
        "count": 606,
        "method": "any_perspective + diagnostic normalization + dedup",
        "pipeline_state": "Current pipeline",
        "reproducible": True,
        "notes": "Current tangled_rope report entry count. Uses any_perspective filter, "
                 "then diagnostic normalization (expand one-per-omega), then dedup on "
                 "(constraint_id, omega_question)."
    },
    {
        "count": 752,
        "method": "claimed_type == 'tangled_rope'",
        "pipeline_state": "Current enriched_pipeline.json",
        "reproducible": True,
        "notes": "Structural count: constraints where the LLM's claimed_type label is "
                 "'tangled_rope'. One type per constraint, sums to 1151."
    },
]


# ---------------------------------------------------------------------------
# Step 1.4 — Double-counting check
# ---------------------------------------------------------------------------

def double_counting_check(constraints):
    """Check overlap between types using any_perspective filter."""
    type_sets = {}
    for type_name in MAXENT_TYPES:
        if TYPE_FILTER_MAP.get(type_name) == "any_perspective":
            filtered = filter_any_perspective(constraints, type_name)
            type_sets[type_name] = {c["id"] for c in filtered}

    # Check all pairwise overlaps among any_perspective types
    overlaps = {}
    ap_types = sorted(type_sets.keys())
    for i, t1 in enumerate(ap_types):
        for t2 in ap_types[i+1:]:
            shared = type_sets[t1] & type_sets[t2]
            if shared:
                overlaps[f"{t1} ∩ {t2}"] = sorted(shared)

    # Also check any_perspective types vs claimed_type types
    all_any_perspective_ids = set()
    for ids in type_sets.values():
        all_any_perspective_ids |= ids

    # Constraints appearing in multiple any_perspective type reports
    id_to_types = defaultdict(list)
    for type_name, ids in type_sets.items():
        for cid in ids:
            id_to_types[cid].append(type_name)

    multi_type = {cid: types for cid, types in id_to_types.items() if len(types) > 1}

    return type_sets, overlaps, multi_type


# ---------------------------------------------------------------------------
# Step 1.5 — Canonical counts
# ---------------------------------------------------------------------------

def canonical_counts(constraints):
    """Compute both counting regimes."""
    # Structural: claimed_type (one per constraint)
    structural = Counter(c.get("claimed_type") for c in constraints)

    # Report: per TYPE_CONFIGS filter, with normalization + dedup for diagnostics
    report_raw = {}
    report_normalized = {}
    for type_name in MAXENT_TYPES:
        filtered = apply_filter(constraints, type_name)
        report_raw[type_name] = len(filtered)
        norm_entries = normalize_and_dedup(filtered, type_name)
        report_normalized[type_name] = len(norm_entries)

    return structural, report_raw, report_normalized


# ---------------------------------------------------------------------------
# Step 1.6 — Output generation
# ---------------------------------------------------------------------------

def write_report(tallies, structural, report_raw, report_normalized,
                 overlaps, multi_type, constraints):
    """Write the markdown reconciliation report."""
    now = datetime.now().strftime("%Y-%m-%d %H:%M")

    with open(REPORT_PATH, "w", encoding="utf-8") as f:
        f.write("# Type Count Reconciliation Report\n\n")
        f.write(f"**Generated:** {now}\n\n")
        f.write(f"**Corpus size:** {len(constraints)} constraints\n\n")
        f.write("---\n\n")

        # --- Section 1: Raw Field Census ---
        f.write("## 1. Raw Field Census\n\n")
        f.write("Cross-tabulation of constraint types across 6 counting columns.\n\n")

        columns = ["claimed_type", "maxent_top_type",
                    "p.powerless", "p.moderate", "p.institutional", "p.analytical"]
        all_types = sorted(set().union(*(t.keys() for t in tallies.values())))

        # Table header
        f.write("| Type |")
        for col in columns:
            f.write(f" {col} |")
        f.write("\n")
        f.write("| :--- |")
        for _ in columns:
            f.write(" ---: |")
        f.write("\n")

        # Table rows
        for t in all_types:
            f.write(f"| {t} |")
            for col in columns:
                count = tallies[col].get(t, 0)
                f.write(f" {count} |")
            f.write("\n")

        # Totals row
        f.write("| **Total** |")
        for col in columns:
            total = sum(tallies[col].values())
            f.write(f" **{total}** |")
        f.write("\n\n")

        f.write("**Note:** `claimed_type` sums to corpus size (one per constraint). "
                "Perspective columns may differ because constraints can have `unknown` "
                "or `null` perspectives.\n\n")

        # --- Section 2: Filter Methods ---
        f.write("## 2. Filter Methods and Report Counts\n\n")
        f.write("Each type uses a specific filter method in `TYPE_CONFIGS` "
                "(from `type_reporter.py`).\n\n")

        f.write("| Type | Filter Method | Family | Raw Filtered | "
                "Normalized+Deduped |\n")
        f.write("| :--- | :--- | :--- | ---: | ---: |\n")
        for type_name in MAXENT_TYPES:
            method = TYPE_FILTER_MAP.get(type_name, "standard")
            family = TYPE_FAMILY_MAP.get(type_name, "diagnostic")
            raw = report_raw[type_name]
            norm = report_normalized[type_name]
            f.write(f"| {type_name} | `{method}` | {family} | {raw} | {norm} |\n")
        f.write("\n")

        f.write("**Filter method definitions:**\n\n")
        f.write("- **`standard`**: `claimed_type == type_name`\n")
        f.write("- **`unanimity`**: `claimed_type == type_name` AND all "
                "non-unknown perspectives match\n")
        f.write("- **`any_perspective`**: ANY perspective value == type_name "
                "(ignores claimed_type)\n\n")
        f.write("For diagnostic family reports, normalization expands "
                "one-per-omega, then dedups on `(constraint_id, omega_question)`.\n\n")

        # --- Section 3: Historical Reconciliation ---
        f.write("## 3. Historical Count Reconciliation\n\n")
        f.write("Four tangled_rope counts from different sources:\n\n")

        f.write("| Count | Method | Pipeline State | Reproducible? |\n")
        f.write("| ---: | :--- | :--- | :---: |\n")
        for hc in HISTORICAL_COUNTS:
            repro = "Yes" if hc["reproducible"] else "No"
            f.write(f"| {hc['count']} | `{hc['method']}` | "
                    f"{hc['pipeline_state']} | {repro} |\n")
        f.write("\n")

        for hc in HISTORICAL_COUNTS:
            f.write(f"**{hc['count']}:** {hc['notes']}\n\n")

        # Verify reproducible counts
        f.write("### Verification of reproducible counts\n\n")
        tangled_any_persp = report_raw.get("tangled_rope", 0)
        tangled_norm = report_normalized.get("tangled_rope", 0)
        tangled_claimed = structural.get("tangled_rope", 0)

        f.write(f"- `any_perspective` raw filtered: **{tangled_any_persp}** constraints\n")
        f.write(f"- `any_perspective` + normalization + dedup: "
                f"**{tangled_norm}** report entries\n")
        f.write(f"- `claimed_type == 'tangled_rope'`: **{tangled_claimed}** constraints\n\n")

        # Check against 606 and 752
        if tangled_norm == 606:
            f.write("606 count **confirmed** — matches current tangled_rope "
                    "report entry count.\n\n")
        else:
            f.write(f"**WARNING:** Expected 606, got {tangled_norm}.\n\n")

        if tangled_claimed == 752:
            f.write("752 count **confirmed** — matches claimed_type count.\n\n")
        else:
            f.write(f"**NOTE:** claimed_type count is {tangled_claimed} "
                    f"(plan expected 752).\n\n")

        # --- Section 4: Double-Counting ---
        f.write("## 4. Double-Counting Analysis\n\n")
        f.write("With `any_perspective` filter, a constraint can appear in "
                "multiple type reports if different perspectives compute "
                "different types.\n\n")

        if overlaps:
            for pair, ids in sorted(overlaps.items()):
                f.write(f"### {pair}\n\n")
                f.write(f"**{len(ids)} constraints** appear in both reports:\n\n")
                for cid in ids[:20]:
                    # Find this constraint and show its perspectives
                    c = next((x for x in constraints if x["id"] == cid), None)
                    if c:
                        persp = c.get("perspectives", {})
                        persp_str = ", ".join(f"{k}: {v}" for k, v in
                                              sorted(persp.items()))
                        f.write(f"- `{cid}` — {persp_str}\n")
                if len(ids) > 20:
                    f.write(f"- ... and {len(ids) - 20} more\n")
                f.write("\n")
        else:
            f.write("No overlap found between any_perspective type reports.\n\n")

        if multi_type:
            f.write(f"### Multi-type constraints\n\n")
            f.write(f"**{len(multi_type)} constraints** appear in multiple "
                    f"any_perspective type reports:\n\n")
            for cid, types in sorted(multi_type.items()):
                f.write(f"- `{cid}`: {', '.join(types)}\n")
            f.write("\n")
        else:
            f.write("No constraints appear in multiple any_perspective "
                    "type reports.\n\n")

        # --- Section 5: Canonical Counts ---
        f.write("## 5. Canonical Count Regimes\n\n")

        f.write("### Structural regime (`claimed_type`)\n\n")
        f.write("One type per constraint. Sums to corpus size.\n\n")
        f.write("| Type | Count |\n")
        f.write("| :--- | ---: |\n")
        total = 0
        for type_name in MAXENT_TYPES:
            count = structural.get(type_name, 0)
            total += count
            f.write(f"| {type_name} | {count} |\n")
        # Count None/missing
        none_count = structural.get(None, 0)
        if none_count:
            total += none_count
            f.write(f"| *(none/null)* | {none_count} |\n")
        f.write(f"| **Total** | **{total}** |\n\n")

        f.write("### Report regime (per TYPE_CONFIGS filter)\n\n")
        f.write("Perspectival; may not sum to corpus size due to "
                "filter method differences and double-counting.\n\n")
        f.write("| Type | Filter | Raw Filtered | Report Entries |\n")
        f.write("| :--- | :--- | ---: | ---: |\n")
        for type_name in MAXENT_TYPES:
            method = TYPE_FILTER_MAP.get(type_name, "standard")
            f.write(f"| {type_name} | `{method}` | "
                    f"{report_raw[type_name]} | {report_normalized[type_name]} |\n")
        f.write("\n")

        f.write("**The gradient analysis (Part 3) will use `claimed_type` "
                "as its population definition.**\n\n")

        # --- Section 6: Summary ---
        f.write("## 6. Summary\n\n")
        f.write("The four tangled_rope counts reflect four different operations "
                "on the pipeline data:\n\n")
        f.write("1. **773** and **677** are historical snapshots from earlier "
                "pipeline states (pre/post piton gate changes). They are not "
                "reproducible from current data and exist only in documentation.\n\n")
        f.write("2. **606** is the current tangled_rope *report entry count*, "
                "computed via `any_perspective` filter + diagnostic normalization "
                "+ dedup. This is what the tangled_rope diagnostic report shows.\n\n")
        f.write(f"3. **{tangled_claimed}** is the *structural count* — constraints "
                f"where `claimed_type == 'tangled_rope'`. This is the "
                f"observer-invariant population definition.\n\n")
        f.write("Both reproducible counts (606 and the structural count) are "
                "correct for their respective methodologies. They measure "
                "different things: report entries (perspectival, normalized) vs. "
                "structural population (one label per constraint).\n")


def write_data(tallies, structural, report_raw, report_normalized,
               overlaps, multi_type, constraints):
    """Write the JSON data output."""
    # Build tally table
    columns = ["claimed_type", "maxent_top_type",
                "p.powerless", "p.moderate", "p.institutional", "p.analytical"]
    all_types = sorted(set().union(*(t.keys() for t in tallies.values())))
    census_table = {}
    for t in all_types:
        census_table[t] = {col: tallies[col].get(t, 0) for col in columns}

    data = {
        "generated": datetime.now().isoformat(),
        "corpus_size": len(constraints),
        "raw_field_census": census_table,
        "filter_methods": {
            type_name: {
                "method": TYPE_FILTER_MAP.get(type_name, "standard"),
                "family": TYPE_FAMILY_MAP.get(type_name, "diagnostic"),
                "raw_filtered_count": report_raw[type_name],
                "report_entry_count": report_normalized[type_name],
            }
            for type_name in MAXENT_TYPES
        },
        "structural_counts": {t: structural.get(t, 0) for t in MAXENT_TYPES},
        "structural_total": sum(structural.values()),
        "historical_counts": HISTORICAL_COUNTS,
        "double_counting": {
            "overlaps": {pair: ids for pair, ids in overlaps.items()},
            "multi_type_constraints": {
                cid: types for cid, types in multi_type.items()
            },
            "multi_type_count": len(multi_type),
        },
    }

    with open(DATA_PATH, "w", encoding="utf-8") as f:
        json.dump(data, f, indent=2)


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    print("[RECONCILIATION] Loading enriched_pipeline.json...")
    data = load_json(ENRICHED_PIPELINE_JSON, "enriched_pipeline",
                     schema=validate_enriched_pipeline)
    constraints = data["per_constraint"]
    print(f"[RECONCILIATION] Loaded {len(constraints)} constraints.")

    # Step 1.1
    print("[RECONCILIATION] Step 1.1 — Raw field census...")
    tallies = raw_field_census(constraints)

    # Step 1.2 + 1.5
    print("[RECONCILIATION] Step 1.2 — Filter methods and report counts...")
    structural, report_raw, report_normalized = canonical_counts(constraints)

    # Step 1.3
    print("[RECONCILIATION] Step 1.3 — Historical reconciliation...")
    tangled_norm = report_normalized.get("tangled_rope", 0)
    tangled_claimed = structural.get("tangled_rope", 0)
    print(f"  any_perspective + norm + dedup: {tangled_norm} "
          f"(expected 606)")
    print(f"  claimed_type count: {tangled_claimed}")

    # Step 1.4
    print("[RECONCILIATION] Step 1.4 — Double-counting check...")
    type_sets, overlaps, multi_type = double_counting_check(constraints)
    for pair, ids in overlaps.items():
        print(f"  {pair}: {len(ids)} shared constraints")
    if not overlaps:
        print("  No overlap found.")
    print(f"  Multi-type constraints: {len(multi_type)}")

    # Step 1.5
    print("[RECONCILIATION] Step 1.5 — Canonical counts...")
    total = sum(structural.values())
    print(f"  Structural total: {total} (expected {len(constraints)})")

    # Step 1.6
    print("[RECONCILIATION] Step 1.6 — Writing outputs...")
    DOCS_DIR.mkdir(parents=True, exist_ok=True)
    write_report(tallies, structural, report_raw, report_normalized,
                 overlaps, multi_type, constraints)
    write_data(tallies, structural, report_raw, report_normalized,
               overlaps, multi_type, constraints)

    print(f"[RECONCILIATION] Report: {REPORT_PATH}")
    print(f"[RECONCILIATION] Data:   {DATA_PATH}")
    print("[RECONCILIATION] Done.")


if __name__ == "__main__":
    main()
