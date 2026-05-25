"""B2 — Coherence eyeball at scale.

Parses outputs/kernel_manifests/run_01/coherence_eyeball.md systematically.
For each kernel: tabulates per-reading emitted types, determines distinct vs collapse verdict.

Output: outputs/kernel_manifests/run_01/harvest_b2_coherence.md
"""
import re
import sys
from collections import Counter
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
IN_PATH = REPO_ROOT / "outputs" / "kernel_manifests" / "run_01" / "coherence_eyeball.md"
OUT_PATH = REPO_ROOT / "outputs" / "kernel_manifests" / "run_01" / "harvest_b2_coherence.md"


def parse_coherence_eyeball(text: str) -> list[dict]:
    """Parse the coherence_eyeball.md into structured records per kernel."""
    kernels = []
    current = None

    for line in text.splitlines():
        # New kernel section
        m = re.match(r"^## (.+)$", line)
        if m:
            if current:
                kernels.append(finalize_kernel(current))
            current = {
                "kernel_id": m.group(1).strip(),
                "description": "",
                "readings": [],
                "raw_verdict": "",
            }
            continue

        if current is None:
            continue

        # Kernel description
        m = re.match(r"^kernel: (.+)$", line)
        if m:
            current["description"] = m.group(1).strip()
            continue

        # Reading line: "  - reading_id: emitted type = TYPE  (description)"
        m = re.match(r"^\s+- (.+?):\s+emitted type = (\S+)\s*(.*)", line)
        if m:
            reading_id = m.group(1).strip()
            raw_type = m.group(2).strip()
            desc = m.group(3).strip().lstrip("(").rstrip(")")
            current["readings"].append({
                "reading_id": reading_id,
                "emitted_type": raw_type,  # may be "?" for thin data
                "description": desc,
            })
            continue

        # Verdict line
        m = re.match(r"^\s*-> distinct: (.+)", line)
        if m:
            current["raw_verdict"] = f"distinct: {m.group(1).strip()}"
            continue

        m = re.match(r"^\s*\*\* COLLAPSE SIGNAL: (.+)", line)
        if m:
            current["raw_verdict"] = f"collapse: {m.group(1).strip()}"
            continue

    if current:
        kernels.append(finalize_kernel(current))

    return kernels


def finalize_kernel(raw: dict) -> dict:
    readings = raw["readings"]
    known_types = [r["emitted_type"] for r in readings if r["emitted_type"] != "?"]
    unknown_count = sum(1 for r in readings if r["emitted_type"] == "?")

    type_set = set(known_types)

    # Determine verdict
    if len(readings) < 2:
        verdict = "thin_data"
        type_distribution = {}
    elif all(r["emitted_type"] == "?" for r in readings):
        verdict = "thin_data"
        type_distribution = {}
    elif len(type_set) == 0:
        verdict = "thin_data"
        type_distribution = {}
    elif len(type_set) == 1 and unknown_count == 0:
        # All known readings same type
        verdict = "collapse"
        type_distribution = Counter(known_types)
    elif len(type_set) >= 2:
        verdict = "distinct"
        type_distribution = Counter(known_types)
    else:
        # Some unknown, some known — one known type
        verdict = "partial"
        type_distribution = Counter(known_types)

    return {
        "kernel_id": raw["kernel_id"],
        "description": raw["description"],
        "readings": readings,
        "total_readings": len(readings),
        "unknown_readings": unknown_count,
        "known_types": list(type_set),
        "verdict": verdict,
        "type_distribution": dict(type_distribution),
        "raw_verdict": raw["raw_verdict"],
    }


def main():
    if not IN_PATH.exists():
        print(f"ERROR: {IN_PATH} not found", file=sys.stderr)
        sys.exit(1)

    text = IN_PATH.read_text(encoding="utf-8")
    kernels = parse_coherence_eyeball(text)

    distinct = [k for k in kernels if k["verdict"] == "distinct"]
    collapse = [k for k in kernels if k["verdict"] == "collapse"]
    partial = [k for k in kernels if k["verdict"] == "partial"]
    thin = [k for k in kernels if k["verdict"] == "thin_data"]

    lines = [
        "# B2 — Coherence Eyeball at Scale (run_01 Kernel Corpus)",
        "",
        f"**Kernels parsed**: {len(kernels)}  |  "
        f"**Distinct**: {len(distinct)}  |  "
        f"**Collapse**: {len(collapse)}  |  "
        f"**Partial** (some ?): {len(partial)}  |  "
        f"**Thin data** (all ?): {len(thin)}",
        "",
        "---",
        "",
        "## Per-Kernel Table",
        "",
        "| Kernel ID | Readings | Unknown | Verdict | Type split |",
        "|---|---|---|---|---|",
    ]

    for k in kernels:
        td = k["type_distribution"]
        split_str = ", ".join(f"{t}×{n}" for t, n in sorted(td.items())) if td else "—"
        if k["unknown_readings"] > 0:
            split_str += f" (+{k['unknown_readings']}×?)"
        lines.append(
            f"| `{k['kernel_id']}` | {k['total_readings']} | "
            f"{k['unknown_readings']} | **{k['verdict']}** | {split_str} |"
        )

    lines += [
        "",
        "---",
        "",
        "## Distinct Kernels",
        "",
        f"{len(distinct)} kernels where readings differentiate into ≥2 types.",
        "",
    ]
    for k in distinct:
        td = k["type_distribution"]
        split = ", ".join(f"`{t}` ×{n}" for t, n in sorted(td.items()))
        if k["unknown_readings"]:
            split += f", `?` ×{k['unknown_readings']}"
        lines.append(f"### `{k['kernel_id']}`")
        lines.append(f"*{k['description'][:120]}*")
        lines.append(f"Type split: {split}")
        lines.append("")
        for r in k["readings"]:
            marker = "❓" if r["emitted_type"] == "?" else ""
            lines.append(f"- **{r['reading_id']}**: `{r['emitted_type']}` {marker} — {r['description'][:100]}")
        lines.append("")

    lines += [
        "---",
        "",
        "## Collapse Kernels",
        "",
        f"{len(collapse)} kernels where all readings emit the same type.",
        "",
    ]
    for k in collapse:
        collapse_type = list(k["type_distribution"].keys())[0] if k["type_distribution"] else "?"
        lines.append(f"### `{k['kernel_id']}` — collapsed to `{collapse_type}`")
        lines.append(f"*{k['description'][:120]}*")
        lines.append("")
        for r in k["readings"]:
            lines.append(f"- **{r['reading_id']}**: `{r['emitted_type']}`")
        lines.append("")

    if partial:
        lines += [
            "---",
            "",
            "## Partial Kernels (some readings unknown)",
            "",
            f"{len(partial)} kernels with ≥1 known type and ≥1 unknown (`?`) reading.",
            "",
        ]
        for k in partial:
            td = k["type_distribution"]
            split = ", ".join(f"`{t}` ×{n}" for t, n in sorted(td.items()))
            split += f", `?` ×{k['unknown_readings']}"
            lines.append(f"### `{k['kernel_id']}`")
            lines.append(f"*{k['description'][:120]}*")
            lines.append(f"Known types: {split}")
            lines.append("")
            for r in k["readings"]:
                marker = "❓" if r["emitted_type"] == "?" else ""
                lines.append(f"- **{r['reading_id']}**: `{r['emitted_type']}` {marker}")
            lines.append("")

    if thin:
        lines += [
            "---",
            "",
            "## Thin-Data Kernels (all readings unknown)",
            "",
            f"{len(thin)} kernels where all readings returned `?` — below threshold for verdict.",
            "",
        ]
        for k in thin:
            lines.append(f"- `{k['kernel_id']}` ({k['total_readings']} readings, all rejected)")

    lines += [
        "",
        "---",
        "",
        "## Summary Counts",
        "",
        f"| Verdict | Count |",
        f"|---|---|",
        f"| distinct | {len(distinct)} |",
        f"| collapse | {len(collapse)} |",
        f"| partial | {len(partial)} |",
        f"| thin_data | {len(thin)} |",
        f"| **total** | **{len(kernels)}** |",
        "",
        "### Collapse inventory",
        "",
        "| Kernel | Collapsed to |",
        "|---|---|",
    ]
    for k in collapse:
        t = list(k["type_distribution"].keys())[0] if k["type_distribution"] else "?"
        lines.append(f"| `{k['kernel_id']}` | `{t}` |")

    md = "\n".join(lines) + "\n"
    OUT_PATH.write_text(md, encoding="utf-8")

    print(f"B2 complete.")
    print(f"  Kernels: {len(kernels)}")
    print(f"  Distinct: {len(distinct)}, Collapse: {len(collapse)}, Partial: {len(partial)}, Thin: {len(thin)}")
    print(f"  → {OUT_PATH}")

    # Return structured data for B3/B4 use
    return kernels


if __name__ == "__main__":
    main()
