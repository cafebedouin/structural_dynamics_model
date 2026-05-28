#!/usr/bin/env python3
"""
sotu_reconnaissance.py

Determine whether the SOTU corpus (prolog/testsets_sotu, ~189 constraints)
provides observer-slice coverage that extends the idea_site exploration's
10-slice working family.

Steps:
  1. Locate and count SOTU .pl files
  2. Parse constraint_classification/3 facts → (P, T, E, S, type) tuples
  3. Aggregate slice coverage
  4. Compare against idea_site 10-slice working family
  5. Enumerate new pure-axis pairs enabled by SOTU slices
  6. Write outputs/sotu_reconnaissance.{md,json}
"""

import json
import re
import time
from collections import Counter, defaultdict
from itertools import combinations
from pathlib import Path

SOTU_DIR = Path("prolog/testsets_sotu")
IDEA_JSON = Path("outputs/idea_site_exploration.json")
OUT_MD = Path("outputs/sotu_reconnaissance.md")
OUT_JSON = Path("outputs/sotu_reconnaissance.json")

AXES = ["agent_power", "time_horizon", "exit_options", "spatial_scope"]

# Regex to capture full constraint_classification/3 facts (handles whitespace / newlines)
CLS_PATTERN = re.compile(
    r"constraint_indexing:constraint_classification\s*\(\s*"
    r"(\w+)\s*,\s*"                        # constraint id
    r"(\w+)\s*,\s*"                        # type
    r"context\s*\(\s*"
    r"agent_power\s*\(\s*(\w+)\s*\)\s*,\s*"
    r"time_horizon\s*\(\s*(\w+)\s*\)\s*,\s*"
    r"exit_options\s*\(\s*(\w+)\s*\)\s*,\s*"
    r"spatial_scope\s*\(\s*(\w+)\s*\)\s*"
    r"\)\s*\)",
    re.DOTALL,
)

# ── axis indices for pure-pair detection ──────────────────────────────────────
AXIS_NAMES = ["P", "T", "E", "S"]
AXIS_FULL  = ["agent_power", "time_horizon", "exit_options", "spatial_scope"]


# ─────────────────────────────────────────────────────────────────────────────
# Step 1-2: parse SOTU files
# ─────────────────────────────────────────────────────────────────────────────

def parse_sotu():
    """Return list of dicts with constraint_id, type, P, T, E, S."""
    files = sorted(SOTU_DIR.glob("*.pl"))
    records = []
    for path in files:
        text = path.read_text(encoding="utf-8", errors="replace")
        for m in CLS_PATTERN.finditer(text):
            cid, typ, P, T, E, S = m.groups()
            # Skip internal test clauses with wildcard '_' values
            if any(v == "_" for v in (P, T, E, S)):
                continue
            records.append({
                "file": path.name,
                "constraint_id": cid,
                "type": typ,
                "P": P, "T": T, "E": E, "S": S,
                "slice": (P, T, E, S),
            })
    return files, records


# ─────────────────────────────────────────────────────────────────────────────
# Step 3: aggregate slice coverage
# ─────────────────────────────────────────────────────────────────────────────

def aggregate_slices(records):
    """Return {slice_tuple: {"constraint_ids": set, "type_counts": Counter}}."""
    agg = defaultdict(lambda: {"constraint_ids": set(), "type_counts": Counter()})
    for r in records:
        key = r["slice"]
        agg[key]["constraint_ids"].add(r["constraint_id"])
        agg[key]["type_counts"][r["type"]] += 1
    return dict(agg)


# ─────────────────────────────────────────────────────────────────────────────
# Step 4: compare to idea_site working slices
# ─────────────────────────────────────────────────────────────────────────────

def load_working_slices():
    with open(IDEA_JSON) as f:
        d = json.load(f)
    return {tuple(s["key"]): s["label"] for s in d["working_slices"]}


def classify_slices(sotu_agg, working_slice_map):
    """
    Partition SOTU slices into:
      - shared: present in working family
      - new: not present in working family
    Returns (shared, new_slices) each as {slice_key: data}.
    """
    shared = {}
    new_slices = {}
    for key, data in sotu_agg.items():
        if key in working_slice_map:
            shared[key] = data
        else:
            new_slices[key] = data
    return shared, new_slices


# ─────────────────────────────────────────────────────────────────────────────
# Step 5: pure-axis pairs
# ─────────────────────────────────────────────────────────────────────────────

def find_pure_axis_pairs(all_slices):
    """
    Given a set of slice tuples, return all pure-axis pairs:
    {axis_index: [(slice_a, slice_b), ...]}
    A pure-axis pair differs in exactly one axis component.
    """
    slices = list(all_slices)
    pairs = {i: [] for i in range(4)}
    for a, b in combinations(slices, 2):
        diffs = [i for i in range(4) if a[i] != b[i]]
        if len(diffs) == 1:
            pairs[diffs[0]].append((a, b))
    return pairs


def delta_pure_pairs(base_slices, extended_slices):
    """
    Compute pure-axis pairs available in base and in base∪extended.
    Returns (base_pairs, extended_pairs, new_pairs) each as {axis: list}.
    """
    base_pairs = find_pure_axis_pairs(base_slices)
    extended_pairs = find_pure_axis_pairs(extended_slices)
    new_pairs = {}
    for axis in range(4):
        base_set = set(tuple(sorted([a, b])) for a, b in base_pairs[axis])
        ext_set  = set(tuple(sorted([a, b])) for a, b in extended_pairs[axis])
        new_pairs[axis] = [
            list(pair) for pair in sorted(ext_set - base_set)
        ]
    return base_pairs, extended_pairs, new_pairs


# ─────────────────────────────────────────────────────────────────────────────
# Formatting helpers
# ─────────────────────────────────────────────────────────────────────────────

def fmt_slice(s):
    return f"({s[0]}, {s[1]}, {s[2]}, {s[3]})"


def dominant_type(type_counts):
    if not type_counts:
        return "?"
    return type_counts.most_common(1)[0][0]


# ─────────────────────────────────────────────────────────────────────────────
# Output
# ─────────────────────────────────────────────────────────────────────────────

def write_json(payload):
    with open(OUT_JSON, "w") as f:
        json.dump(payload, f, indent=2)
    print(f"  Written: {OUT_JSON}")


def write_md(files, records, sotu_agg, working_slice_map,
             shared, new_slices, base_pairs, extended_pairs, new_pairs,
             recommendation):
    lines = []
    lines.append("# SOTU Corpus Reconnaissance")
    lines.append("")
    lines.append(f"Generated: {time.strftime('%Y-%m-%d %H:%M:%S')}")
    lines.append("")

    # ── Inventory
    lines.append("## 1. Corpus Inventory")
    lines.append("")
    lines.append(f"- Files: **{len(files)}** `.pl` files in `{SOTU_DIR}`")
    n_cids = len({r['constraint_id'] for r in records})
    lines.append(f"- Unique constraint IDs: **{n_cids}**")
    lines.append(f"- Total classification records: **{len(records)}**")
    lines.append(f"- Distinct (P,T,E,S) slices: **{len(sotu_agg)}**")
    lines.append("")

    # ── Slice coverage table
    lines.append("## 2. SOTU Slice Coverage")
    lines.append("")
    lines.append("| P | T | E | S | n_constraints | n_classifications | dominant | In working set? |")
    lines.append("|---|---|---|---|---|---|---|---|")
    for key in sorted(sotu_agg.keys(), key=lambda k: -len(sotu_agg[k]["constraint_ids"])):
        data = sotu_agg[key]
        label = working_slice_map.get(key, "—")
        in_ws = f"yes ({label})" if key in working_slice_map else "no"
        lines.append(
            f"| {key[0]} | {key[1]} | {key[2]} | {key[3]} "
            f"| {len(data['constraint_ids'])} "
            f"| {sum(data['type_counts'].values())} "
            f"| {dominant_type(data['type_counts'])} "
            f"| {in_ws} |"
        )
    lines.append("")

    # ── Comparison
    lines.append("## 3. Comparison to Idea-Site Working Family")
    lines.append("")
    lines.append(f"Working family has **{len(working_slice_map)}** slices.")
    lines.append(f"SOTU slices matching working family: **{len(shared)}**")
    lines.append(f"SOTU slices NOT in working family: **{len(new_slices)}**")
    lines.append("")

    if shared:
        lines.append("**Shared slices** (SOTU provides additional data within existing working slices):")
        lines.append("")
        for key in sorted(shared.keys()):
            label = working_slice_map[key]
            n = len(shared[key]["constraint_ids"])
            lines.append(f"- `{label}` {fmt_slice(key)} — {n} SOTU constraints")
        lines.append("")

    if new_slices:
        lines.append("**New slices** (not in working family):")
        lines.append("")
        for key in sorted(new_slices.keys(), key=lambda k: -len(new_slices[k]["constraint_ids"])):
            n = len(new_slices[key]["constraint_ids"])
            lines.append(f"- {fmt_slice(key)} — {n} SOTU constraints")
        lines.append("")
    else:
        lines.append("*No new slices — all SOTU slices are already in the working family.*")
        lines.append("")

    # ── Pure-axis pairs
    lines.append("## 4. Pure-Axis Pair Analysis")
    lines.append("")

    axis_labels = ["P (agent_power)", "T (time_horizon)", "E (exit_options)", "S (spatial_scope)"]
    for i, label in enumerate(axis_labels):
        base_n  = len(base_pairs[i])
        ext_n   = len(extended_pairs[i])
        new_n   = len(new_pairs[i])
        lines.append(f"### Pure-{AXIS_NAMES[i]} pairs ({label})")
        lines.append("")
        lines.append(f"- Working family alone: **{base_n}** pairs")
        lines.append(f"- Working family + SOTU: **{ext_n}** pairs")
        lines.append(f"- Net new pairs: **{new_n}**")
        if new_pairs[i]:
            lines.append("")
            lines.append("New pairs enabled:")
            for a, b in new_pairs[i]:
                lines.append(f"  - {fmt_slice(a)}  ↔  {fmt_slice(b)}")
        lines.append("")

    # ── Determination
    lines.append("## 5. Determination")
    lines.append("")
    total_new_pairs = sum(len(v) for v in new_pairs.values())
    if total_new_pairs == 0:
        lines.append(
            "**Case (a) / (c)**: SOTU slices do not introduce new pure-axis pairs. "
            "The corpus extends sample sizes within existing slices (or matches them exactly) "
            "but does not help with axis coverage. The position-geometry audit should proceed "
            "with its existing 10-slice working family."
        )
    else:
        lines.append(
            f"**Case (b)**: SOTU introduces **{total_new_pairs}** new pure-axis pairs "
            f"across the following axes:"
        )
        lines.append("")
        for i in range(4):
            if new_pairs[i]:
                lines.append(f"- {len(new_pairs[i])} new pure-{AXIS_NAMES[i]} pairs")
        lines.append("")
        lines.append(
            "These new pairs extend axis coverage. The position-geometry audit should "
            "incorporate the relevant SOTU slices into its working family."
        )
    lines.append("")

    # ── Recommendation
    lines.append("## 6. Recommendation")
    lines.append("")
    lines.append(recommendation)
    lines.append("")

    with open(OUT_MD, "w") as f:
        f.write("\n".join(lines) + "\n")
    print(f"  Written: {OUT_MD}")


# ─────────────────────────────────────────────────────────────────────────────
# Main
# ─────────────────────────────────────────────────────────────────────────────

def main():
    t0 = time.time()

    # Step 1-2
    print("Parsing SOTU files...")
    files, records = parse_sotu()
    print(f"  {len(files)} files, {len(records)} classification records parsed.")

    # Step 3
    sotu_agg = aggregate_slices(records)
    print(f"  {len(sotu_agg)} distinct slices found.")

    # Step 4
    print("Loading idea_site working family...")
    working_slice_map = load_working_slices()
    print(f"  {len(working_slice_map)} working slices.")
    shared, new_slices = classify_slices(sotu_agg, working_slice_map)
    print(f"  Shared with working family: {len(shared)}, new slices: {len(new_slices)}")

    # Step 5
    base_slices     = set(working_slice_map.keys())
    extended_slices = base_slices | set(sotu_agg.keys())
    base_pairs, extended_pairs, new_pairs = delta_pure_pairs(base_slices, extended_slices)

    total_new_axis_pairs = sum(len(v) for v in new_pairs.values())
    print(f"  New pure-axis pairs from SOTU: {total_new_axis_pairs}")
    for i, name in enumerate(AXIS_NAMES):
        if new_pairs[i]:
            print(f"    pure-{name}: {len(new_pairs[i])} new pairs")

    # Build recommendation
    if total_new_axis_pairs == 0:
        rec = (
            "**Do not extend the working family.** The SOTU corpus does not provide "
            "any new pure-axis pairs. Incorporating its slices adds sample size within "
            "already-covered slices but does not improve axis isolation capability. "
            "Proceed with the position-geometry audit using the existing 10-slice family."
        )
    else:
        axes_helped = [AXIS_NAMES[i] for i in range(4) if new_pairs[i]]
        rec = (
            f"**Extend the working family.** The SOTU corpus enables {total_new_axis_pairs} "
            f"new pure-axis pair(s) for axis/axes: {', '.join(axes_helped)}. "
            f"Add the SOTU slices that participate in new pairs to the working family "
            f"before running the position-geometry audit."
        )

    # Serialisable pure-pair data
    def serialisable_pairs(pair_dict):
        return {
            AXIS_NAMES[i]: [[list(a), list(b)] for a, b in v]
            for i, v in pair_dict.items()
        }

    payload = {
        "metadata": {
            "timestamp": time.strftime("%Y-%m-%dT%H:%M:%S"),
            "n_files": len(files),
            "n_unique_constraint_ids": len({r["constraint_id"] for r in records}),
            "n_classification_records": len(records),
            "n_distinct_slices": len(sotu_agg),
            "n_working_slices": len(working_slice_map),
            "n_shared_slices": len(shared),
            "n_new_slices": len(new_slices),
            "n_total_new_pure_axis_pairs": total_new_axis_pairs,
        },
        "sotu_slices": {
            str(k): {
                "n_constraints": len(v["constraint_ids"]),
                "n_classifications": sum(v["type_counts"].values()),
                "type_counts": dict(v["type_counts"]),
                "in_working_family": k in working_slice_map,
                "working_family_label": working_slice_map.get(k),
            }
            for k, v in sorted(sotu_agg.items(), key=lambda x: -len(x[1]["constraint_ids"]))
        },
        "working_family_slices": {str(k): label for k, label in working_slice_map.items()},
        "shared_slices": [str(k) for k in sorted(shared.keys())],
        "new_slices": [str(k) for k in sorted(new_slices.keys())],
        "pure_axis_pairs": {
            "base_working_family": serialisable_pairs(base_pairs),
            "base_plus_sotu": serialisable_pairs(extended_pairs),
            "new_from_sotu": serialisable_pairs(new_pairs),
        },
        "recommendation": rec,
    }

    print("\nWriting outputs...")
    OUT_MD.parent.mkdir(parents=True, exist_ok=True)
    write_json(payload)
    write_md(files, records, sotu_agg, working_slice_map,
             shared, new_slices, base_pairs, extended_pairs, new_pairs,
             rec)

    print(f"\nDone in {time.time() - t0:.1f}s")


if __name__ == "__main__":
    main()
