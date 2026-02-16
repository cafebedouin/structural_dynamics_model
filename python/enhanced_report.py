#!/usr/bin/env python3
"""
Enhanced Constraint Report — Runs the Prolog per-constraint report, then
appends four JSON-sourced sections drawn from the Python pipeline outputs.

Inputs:
  prolog/testsets/{ID}.pl          — constraint testset (must exist)
  outputs/orbit_data.json          — orbit signatures per constraint
  outputs/enriched_omega_data.json — triaged omega violations
  outputs/corpus_data.json         — corpus metrics and analysis
  outputs/maxent_report.md         — MaxEnt shadow classifier tables
  outputs/pattern_mining.md        — structural twin groups
  outputs/covering_analysis.md     — Erdos-Selfridge transition detail

Outputs:
  outputs/constraint_reports/{ID}_report.md

Usage:
  python3 python/enhanced_report.py columbia_2026_elections
"""

import json
import os
import re
import subprocess
import sys
from pathlib import Path

# --- Path Setup ---

SCRIPT_DIR = Path(__file__).parent
PROJECT_ROOT = SCRIPT_DIR.parent
PROLOG_DIR = PROJECT_ROOT / "prolog"
OUTPUTS_DIR = PROJECT_ROOT / "outputs"
REPORTS_DIR = OUTPUTS_DIR / "constraint_reports"


# --- JSON Loaders (graceful fallback) ---

def load_json(path, label):
    """Load a JSON file, returning None with a stderr warning on failure."""
    if not path.exists():
        print(f"[WARN] {label} not found: {path}", file=sys.stderr)
        return None
    try:
        with open(path, "r", encoding="utf-8") as f:
            return json.load(f)
    except (json.JSONDecodeError, OSError) as e:
        print(f"[WARN] Failed to load {label}: {e}", file=sys.stderr)
        return None


def load_text(path, label):
    """Load a text/markdown file, returning None with a stderr warning on failure."""
    if not path.exists():
        print(f"[WARN] {label} not found: {path}", file=sys.stderr)
        return None
    try:
        with open(path, "r", encoding="utf-8") as f:
            return f.read()
    except OSError as e:
        print(f"[WARN] Failed to load {label}: {e}", file=sys.stderr)
        return None


# --- Prolog Report Runner ---

def run_prolog_report(constraint_id):
    """Run the Prolog report generator and return stdout text."""
    testset_path = PROLOG_DIR / "testsets" / f"{constraint_id}.pl"
    if not testset_path.exists():
        print(f"[ERROR] Testset not found: {testset_path}", file=sys.stderr)
        sys.exit(1)

    cmd = [
        "swipl", "-l", "stack.pl", "-l", "report_generator.pl",
        "-g", f"run_scenario('testsets/{constraint_id}.pl', '{constraint_id}'), halt."
    ]
    try:
        result = subprocess.run(
            cmd, cwd=str(PROLOG_DIR),
            capture_output=True, text=True, timeout=120
        )
        if result.returncode != 0:
            print(f"[WARN] Prolog exited with code {result.returncode}", file=sys.stderr)
            if result.stderr:
                print(f"[WARN] Prolog stderr: {result.stderr[:500]}", file=sys.stderr)
        return result.stdout
    except FileNotFoundError:
        print("[ERROR] swipl not found on PATH", file=sys.stderr)
        sys.exit(1)
    except subprocess.TimeoutExpired:
        print("[ERROR] Prolog report timed out after 120s", file=sys.stderr)
        sys.exit(1)


# --- Section Builders ---

def build_orbit_section(constraint_id, orbit_data, omega_data):
    """Section 1: ORBIT CONTEXT from orbit_data.json + omega family lookup."""
    lines = ["", "--- ORBIT CONTEXT ---", ""]

    if orbit_data is None:
        lines.append("  [orbit_data.json not available]")
        return "\n".join(lines)

    key = constraint_id.lower()
    entry = orbit_data.get(key)
    if entry is None:
        lines.append(
            "  Not found in orbit analysis — constraint may not have been\n"
            "  included in batch run or may lack sufficient index configurations."
        )
        return "\n".join(lines)

    sig = entry.get("orbit_signature", [])
    contexts = entry.get("contexts", {})

    # Gauge status: invariant if all context values are identical
    context_vals = list(contexts.values())
    gauge = "Gauge-Invariant" if len(set(context_vals)) <= 1 else "Gauge-Variant"

    lines.append(f"  Orbit Signature:    [{', '.join(sig)}]")
    lines.append(f"  Orbit Span:         {len(sig)}")
    lines.append(f"  Number of Contexts: {len(contexts)}")
    lines.append(f"  Gauge Status:       {gauge}")
    lines.append("")
    lines.append("  Per-context types:")
    for ctx in ["powerless", "moderate", "institutional", "analytical"]:
        val = contexts.get(ctx, "N/A")
        lines.append(f"    {ctx:15s} -> {val}")

    # Family ID from enriched omega data
    family = None
    if omega_data and "omegas" in omega_data:
        for omega in omega_data["omegas"]:
            if omega.get("associated_constraint", "").lower() == key:
                family = omega.get("family")
                break
    if family:
        lines.append(f"\n  Orbit Family ID:    {family}")
    else:
        lines.append(f"\n  Orbit Family ID:    No family assignment (no omega generated)")

    return "\n".join(lines)


def parse_maxent_tables(maxent_text):
    """Parse high-uncertainty and hard-disagreement tables from maxent_report.md."""
    high_uncertainty = {}
    hard_disagreements = {}

    if not maxent_text:
        return high_uncertainty, hard_disagreements

    # High Uncertainty table:
    # | Constraint | Det Type | Shadow Top | H_norm | Confidence | Top P |
    hu_pattern = re.compile(
        r'^\| (\S+) \| (\S+) \| (\S+) \| ([0-9.]+) \| ([0-9.]+) \| ([0-9.]+) \|',
        re.MULTILINE
    )
    # Hard Disagreements table:
    # | Constraint | Det Type | Shadow Top | Distribution |
    hd_pattern = re.compile(
        r'^\| (\S+) \| (\S+) \| (\S+) \| (.+?) \|',
        re.MULTILINE
    )

    # Split into sections to avoid cross-matching
    hu_section = ""
    hd_section = ""

    hu_marker = "## High Uncertainty Constraints"
    hd_marker = "### Hard Disagreements"

    if hu_marker in maxent_text:
        start = maxent_text.index(hu_marker)
        # Find next ## header
        rest = maxent_text[start + len(hu_marker):]
        end = rest.find("\n## ")
        hu_section = rest[:end] if end != -1 else rest

    if hd_marker in maxent_text:
        start = maxent_text.index(hd_marker)
        rest = maxent_text[start + len(hd_marker):]
        end = rest.find("\n## ")
        if end == -1:
            end = rest.find("\n### ")
            if end != -1 and rest[end+5:end+20].strip().startswith("Soft"):
                pass  # include soft disagreements section boundary
            else:
                end = -1
        hd_section = rest[:end] if end != -1 else rest

    for m in hu_pattern.finditer(hu_section):
        cid = m.group(1).lower()
        high_uncertainty[cid] = {
            "det_type": m.group(2),
            "shadow_top": m.group(3),
            "h_norm": m.group(4),
            "confidence": m.group(5),
            "top_p": m.group(6),
        }

    for m in hd_pattern.finditer(hd_section):
        name = m.group(1)
        if name.lower() in ("constraint", "------------", ""):
            continue
        hard_disagreements[name.lower()] = {
            "det_type": m.group(2),
            "shadow_top": m.group(3),
            "distribution": m.group(4).strip(),
        }

    return high_uncertainty, hard_disagreements


def build_maxent_section(constraint_id, maxent_text):
    """Section 2: MAXENT SHADOW CLASSIFICATION from maxent_report.md."""
    lines = ["", "--- MAXENT SHADOW CLASSIFICATION ---", ""]

    if maxent_text is None:
        lines.append("  [maxent_report.md not available]")
        return "\n".join(lines)

    high_unc, hard_dis = parse_maxent_tables(maxent_text)
    key = constraint_id.lower()

    in_hard = key in hard_dis
    in_hu = key in high_unc

    if in_hard:
        hd = hard_dis[key]
        h_norm = high_unc[key]["h_norm"] if in_hu else "below threshold"
        lines.append(f"  HARD DISAGREEMENT: Pipeline says {hd['det_type']}, MaxEnt says {hd['shadow_top']}")
        lines.append(f"  H_norm:        {h_norm}")
        lines.append(f"  Det Type:      {hd['det_type']}")
        lines.append(f"  Shadow Top:    {hd['shadow_top']}")
        lines.append(f"  Distribution:  {hd['distribution']}")
    elif in_hu:
        hu = high_unc[key]
        lines.append(f"  High Uncertainty (but types agree)")
        lines.append(f"  H_norm:        {hu['h_norm']}")
        lines.append(f"  Confidence:    {hu['confidence']}")
        lines.append(f"  Top P:         {hu['top_p']}")
        lines.append(f"  Det Type:      {hu['det_type']}")
        lines.append(f"  Shadow Top:    {hu['shadow_top']}")
    else:
        lines.append("  No MaxEnt flags — classification is stable (low entropy, types agree)")

    return "\n".join(lines)


def build_omega_section(constraint_id, omega_data):
    """Section 3: ENRICHED OMEGA CONTEXT from enriched_omega_data.json."""
    lines = ["", "--- ENRICHED OMEGA CONTEXT ---", ""]

    if omega_data is None:
        lines.append("  [enriched_omega_data.json not available]")
        return "\n".join(lines)

    key = constraint_id.lower()
    matches = [
        o for o in omega_data.get("omegas", [])
        if o.get("associated_constraint", "").lower() == key
    ]

    if not matches:
        lines.append(
            "  No omega violations detected — perspectives agree on\n"
            "  classification for this constraint."
        )
        return "\n".join(lines)

    for i, omega in enumerate(matches):
        if i > 0:
            lines.append("")
        lines.append(f"  Omega Name:          {omega.get('name', 'N/A')}")
        lines.append(f"  Severity Score:      {omega.get('severity_score', 'N/A')}")
        lines.append(f"  Severity Category:   {omega.get('severity', 'N/A')}")
        lines.append(f"  Gap Class:           {omega.get('gap_class', 'N/A')}")
        lines.append(f"  Gap Pattern:         {omega.get('gap_pattern', 'N/A')}")
        lines.append(f"  Epsilon:             {omega.get('epsilon', 'N/A')}")
        lines.append(f"  Suppression:         {omega.get('suppression', 'N/A')}")
        lines.append(f"  Family ID:           {omega.get('family', 'N/A')}")
        lines.append(f"  Resolution Strategy: {omega.get('resolution_strategy', 'N/A')}")

    return "\n".join(lines)


def build_structural_section(constraint_id, corpus_data, pattern_text, covering_text):
    """Section 4: STRUCTURAL CONTEXT from corpus_data.json + markdown reports."""
    lines = ["", "--- STRUCTURAL CONTEXT ---", ""]

    key = constraint_id.lower()

    # --- Corpus data: analysis sub-object ---
    if corpus_data and "constraints" in corpus_data:
        # Case-insensitive lookup
        cdata = None
        for cid, val in corpus_data["constraints"].items():
            if cid.lower() == key:
                cdata = val
                break

        if cdata and "analysis" in cdata:
            a = cdata["analysis"]
            vr = a.get("variance_ratio")
            ic = a.get("index_configs")
            tp = a.get("types_produced")

            if vr is not None:
                if vr == 1.0:
                    interp = "stable"
                elif vr > 0.5:
                    interp = "high variance"
                else:
                    interp = "low variance"
                lines.append(f"  Variance Ratio:      {vr} ({interp})")
            if ic is not None:
                lines.append(f"  Index Configs:       {ic}")
            if tp is not None:
                lines.append(f"  Types Produced:      {tp}")
        elif cdata:
            lines.append("  [No analysis sub-object in corpus_data for this constraint]")
        else:
            lines.append("  [Constraint not found in corpus_data.json]")
    else:
        lines.append("  [corpus_data.json not available]")

    # --- Pattern mining: structural twins ---
    lines.append("")
    if pattern_text:
        # Search the twin group table for rows containing the constraint ID
        twin_found = False
        # Table rows: | Signature | Count | Types Present | Domains | Examples |
        twin_row_re = re.compile(
            r'^\| ([^|]+)\| *(\d+) \| ([^|]+)\| ([^|]+)\| ([^|]+)\|',
            re.MULTILINE
        )
        for m in twin_row_re.finditer(pattern_text):
            examples = m.group(5).strip()
            if key in examples.lower():
                sig = m.group(1).strip()
                count = m.group(2).strip()
                types = m.group(3).strip()
                twin_found = True
                lines.append(f"  Structural Twin Group:")
                lines.append(f"    Signature:   {sig}")
                lines.append(f"    Group Size:  {count}")
                lines.append(f"    Types:       {types}")
                break
        if not twin_found:
            lines.append("  Structural Twins: Not in any structural twin group")
    else:
        lines.append("  [pattern_mining.md not available]")

    # --- Covering analysis: transition detail ---
    lines.append("")
    if covering_text:
        # Find rows matching this constraint in the transition detail table
        # | Constraint | D1 | Sigma | Type1 | D2 | Sigma2 | Type2 | Axis |
        transitions = []
        for line in covering_text.splitlines():
            if line.startswith("| ") and key in line.lower():
                parts = [p.strip() for p in line.split("|")[1:-1]]
                if len(parts) >= 7 and parts[0].lower() == key:
                    type1 = parts[3]
                    type2 = parts[6]
                    transitions.append(f"{type1} -> {type2}")

        if transitions:
            unique_transitions = sorted(set(transitions))
            lines.append(f"  Covering Analysis:")
            lines.append(f"    Missed Transitions: {len(transitions)}")
            lines.append(f"    Unique Type Shifts:  {', '.join(unique_transitions)}")
        else:
            lines.append("  Covering Analysis: No missed transitions on expanded grid")
    else:
        lines.append("  [covering_analysis.md not available]")

    return "\n".join(lines)


# --- Report Assembly ---

def assemble_report(prolog_output, sections):
    """Insert the four sections between LOGICAL FINGERPRINT and SYSTEM INSIGHTS."""
    marker_fp = "--- LOGICAL FINGERPRINT ---"
    marker_si = "--- SYSTEM INSIGHTS ---"

    insertion = "\n".join(sections)

    if marker_fp in prolog_output and marker_si in prolog_output:
        # Split at SYSTEM INSIGHTS marker — insert before it
        before_si, after_si = prolog_output.split(marker_si, 1)
        return before_si + insertion + "\n\n" + marker_si + after_si
    else:
        # Markers not found — append at end
        print("[WARN] Section markers not found in Prolog output; appending sections at end",
              file=sys.stderr)
        return prolog_output + "\n" + insertion


# --- Main ---

def main():
    if len(sys.argv) < 2:
        print("Usage: python3 python/enhanced_report.py <constraint_id>", file=sys.stderr)
        sys.exit(1)

    constraint_id = sys.argv[1]
    print(f"Generating enhanced report for: {constraint_id}")

    # Step 1: Run Prolog report
    prolog_output = run_prolog_report(constraint_id)

    # Step 2: Load data sources
    orbit_data = load_json(OUTPUTS_DIR / "orbit_data.json", "orbit_data.json")
    omega_data = load_json(OUTPUTS_DIR / "enriched_omega_data.json", "enriched_omega_data.json")
    corpus_data = load_json(OUTPUTS_DIR / "corpus_data.json", "corpus_data.json")
    maxent_text = load_text(OUTPUTS_DIR / "maxent_report.md", "maxent_report.md")
    pattern_text = load_text(OUTPUTS_DIR / "pattern_mining.md", "pattern_mining.md")
    covering_text = load_text(OUTPUTS_DIR / "covering_analysis.md", "covering_analysis.md")

    # Step 3: Build the four sections
    sec_orbit = build_orbit_section(constraint_id, orbit_data, omega_data)
    sec_maxent = build_maxent_section(constraint_id, maxent_text)
    sec_omega = build_omega_section(constraint_id, omega_data)
    sec_structural = build_structural_section(constraint_id, corpus_data, pattern_text, covering_text)

    # Step 4: Assemble
    full_report = assemble_report(prolog_output, [sec_orbit, sec_maxent, sec_omega, sec_structural])

    # Step 5: Write output
    REPORTS_DIR.mkdir(parents=True, exist_ok=True)
    out_path = REPORTS_DIR / f"{constraint_id}_report.md"
    with open(out_path, "w", encoding="utf-8") as f:
        f.write(full_report)

    print(f"Report written to: {out_path}")


if __name__ == "__main__":
    main()
