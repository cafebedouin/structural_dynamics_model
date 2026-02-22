#!/usr/bin/env python3
"""
Enhanced Constraint Report — Three-Level Feedback Model

Runs the Prolog per-constraint report (Stream 1: Live Diagnosis), then
inserts Python-built analysis sections (Stream 2) between the LOGICAL
FINGERPRINT and DR EXECUTIVE SUMMARY sections, organized into three
analytical levels:

  Verdict Banner — traffic-light summary (GREEN/YELLOW/RED)

  Level 1: SELF-CONSISTENCY
    - Constraint Identity (type, signature, purity, coupling, Boltzmann, drift, tangled)
    - Orbit Context (signature, span, gauge status)
    - Enriched Omega Context (severity, gap class/pattern, family)

  Level 2: DIAGNOSTIC CONVERGENCE
    - Classification Convergence (batch agreement, confidence, rival, margin, boundary, H^1)
    - MaxEnt Shadow Classification (entropy, probability distribution)
    - Abductive Flags (cross-subsystem anomaly triggers)
    - Diagnostic Verdict (subsystems, agreements, conflicts, tensions)

  Level 3: CORPUS POSITIONING
    - Corpus Distribution (type, purity, coupling, signature, confidence distributions)
    - Constraint Positioning (percentiles, boundary zone, orbit family)
    - Structural Context (variance, twins, covering analysis)

  Post-Synthesis (T12 divergence flags, only if present)

Inputs:
  prolog/testsets/{ID}.pl          — constraint testset (must exist)
  outputs/enriched_pipeline.json   — enriched pipeline results + diagnostic
  outputs/orbit_data.json          — orbit signatures per constraint
  outputs/enriched_omega_data.json — triaged omega violations
  outputs/corpus_data.json         — corpus metrics and analysis
  outputs/maxent_report.md         — MaxEnt shadow classifier tables
  outputs/pattern_mining.md        — structural twin groups
  outputs/covering_analysis.md     — Erdos-Selfridge transition detail

Outputs:
  outputs/constraint_reports/{ID}_report.md

Usage:
  python3 python/enhanced_report.py columbia_2026_elections   # specific constraint
  python3 python/enhanced_report.py foo bar baz               # multiple constraints
  python3 python/enhanced_report.py                           # auto: testsets modified in last hour
"""

import json
import os
import re
import subprocess
import sys
import time
from pathlib import Path

# --- Path Setup ---

SCRIPT_DIR = Path(__file__).parent
PROJECT_ROOT = SCRIPT_DIR.parent
PROLOG_DIR = PROJECT_ROOT / "prolog"
OUTPUTS_DIR = PROJECT_ROOT / "outputs"
REPORTS_DIR = OUTPUTS_DIR / "constraint_reports"

# --- Splice markers ---

MARKER_FP = "--- LOGICAL FINGERPRINT ---"
MARKER_EXEC = "===================================================="


# --- JSON/Text Loaders (graceful fallback) ---

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


# --- Helpers ---

def _compact_types(perspectives):
    """Summarize perspectives as 'type1 (ctx1), type2 (ctx2)' — one ctx per unique type."""
    type_to_ctx = {}
    for ctx in ["powerless", "moderate", "institutional", "analytical"]:
        t = perspectives.get(ctx)
        if t and t not in type_to_ctx:
            type_to_ctx[t] = ctx
    return ", ".join(f"{t} ({ctx})" for t, ctx in type_to_ctx.items())


def find_constraint_entry(pipeline_data, constraint_id):
    """Find a constraint in enriched_pipeline.json per_constraint array."""
    if pipeline_data is None:
        return None
    key = constraint_id.lower()
    for pc in pipeline_data.get("per_constraint", []):
        if pc.get("id", "").lower() == key:
            return pc
    return None


# --- Header Builder ---

def build_header(pipeline_data):
    """Build 3-line corpus summary header from diagnostic + validation sections."""
    if pipeline_data is None:
        return ""

    diag = pipeline_data.get("diagnostic")
    val = pipeline_data.get("validation")
    if not diag:
        return ""

    corpus_size = diag.get("corpus_size", "?")
    type_dist = diag.get("type_distribution", {})
    network = diag.get("network_stability", "unknown")
    omega_count = val.get("omega_count", 0) if val else 0
    critical = val.get("omega_by_severity", {}).get("critical", 0) if val else 0

    # Format type distribution in standard order
    type_order = ["mountain", "rope", "tangled_rope", "snare", "piton", "scaffold"]
    type_parts = [f"{type_dist[t]} {t}" for t in type_order if t in type_dist]
    for t, count in sorted(type_dist.items()):
        if t not in type_order:
            type_parts.append(f"{count} {t}")

    lines = [
        f"CORPUS CONTEXT: {corpus_size} constraints",
        f"  Types: {', '.join(type_parts)}",
        f"  Network stability: {network} | {omega_count} omegas ({critical} critical)",
    ]

    # Confidence distribution from per_constraint
    per_constraint = pipeline_data.get("per_constraint", [])
    band_counts = {}
    for pc in per_constraint:
        b = pc.get("confidence_band")
        if b:
            band_counts[b] = band_counts.get(b, 0) + 1
    if band_counts:
        parts = []
        for band in ["deep", "moderate", "borderline"]:
            n = band_counts.get(band, 0)
            total = sum(band_counts.values())
            pct = round(n / total * 100) if total else 0
            parts.append(f"{n} {band} ({pct}%)")
        lines.append(f"  Confidence: {' | '.join(parts)}")

    lines.append("")
    return "\n".join(lines)


# --- Live Prolog Extraction (minimal regex) ---

def extract_live_perspectives(prolog_output):
    """Extract claimed type and perspective types from Prolog INDEXICAL AUDIT block.

    Uses two regex patterns only:
      1. 'Claimed Type: X' line
      2. perspective lines: '[context(agent_power(X),...))]: type'
    """
    claimed = None
    perspectives = {}

    m = re.search(r'Claimed Type:\s+(\S+)', prolog_output)
    if m:
        claimed = m.group(1)

    for m in re.finditer(
        r'agent_power\((\w+)\)[^\n]+\]:\s+(\w+)',
        prolog_output
    ):
        power = m.group(1)
        ctype = m.group(2)
        if power in ("powerless", "moderate", "institutional", "analytical"):
            perspectives[power] = ctype

    return claimed, perspectives


# --- Level Header ---

def build_level_header(level_num, title):
    """Return a level separator: ═══ LEVEL N: TITLE ═══"""
    return f"\n═══ LEVEL {level_num}: {title} ═══\n"


# --- Verdict Banner ---

def build_verdict_banner(constraint_id, pipeline_data):
    """Top-of-report traffic-light banner extracted from diagnostic_verdict."""
    if pipeline_data is None:
        return "\n  [Verdict unavailable — run full pipeline to include]\n"

    entry = find_constraint_entry(pipeline_data, constraint_id)
    if entry is None:
        return "\n  [Verdict unavailable — constraint not yet in batch]\n"

    dv = entry.get("diagnostic_verdict")
    if dv is None:
        return "\n  [Verdict unavailable — run full pipeline to include]\n"

    verdict = dv.get("verdict", "unknown")
    verdict_upper = verdict.upper() if isinstance(verdict, str) else "UNKNOWN"
    n_avail = dv.get("subsystems_available", 0)
    unavail = dv.get("subsystems_unavailable", [])
    total = n_avail + len(unavail)
    tensions = dv.get("tensions", [])

    if tensions:
        tension_parts = [t.get("subsystem", "?") for t in tensions]
        detail = f"{n_avail}/{total} subsystems — {len(tensions)} tension(s) ({', '.join(tension_parts)})"
    else:
        detail = f"{n_avail}/{total} subsystems checked — no tensions"

    return (
        "\n"
        "╔═══════════════════════════════════════════════════╗\n"
        f"║  VERDICT: {verdict_upper:<41}║\n"
        f"║  {detail:<49}║\n"
        "╚═══════════════════════════════════════════════════╝\n"
    )


# --- Level 1: CONSTRAINT IDENTITY (from old Section A "This Constraint" L1 fields) ---

def build_level1_identity(constraint_id, pipeline_data, prolog_output):
    """L1: Self-consistency identity — claimed/live type, signature, purity,
    coupling, Boltzmann, drift events, tangled fields."""
    lines = ["", "--- CONSTRAINT IDENTITY ---", ""]

    if pipeline_data is None:
        lines.append("  [enriched_pipeline.json not available]")
        return "\n".join(lines)

    entry = find_constraint_entry(pipeline_data, constraint_id)
    live_claimed, live_perspectives = extract_live_perspectives(prolog_output)
    in_batch = entry is not None

    if in_batch:
        claimed = entry.get("claimed_type", "N/A")
        signature = entry.get("signature", "N/A")
        purity = entry.get("purity_score")
        purity_band = entry.get("purity_band", "N/A")
        coupling = entry.get("coupling", {})
        coupling_cat = coupling.get("category", "N/A")
        coupling_score = coupling.get("score")
        boltzmann = coupling.get("boltzmann", "unknown")

        lines.append(f"    Claimed Type:     {claimed}")

        if live_perspectives:
            live_str = _compact_types(live_perspectives)
            if live_str:
                lines.append(f"    Live Type:        {live_str}")

        lines.append(f"    Signature:        {signature}")

        if purity is not None:
            lines.append(f"    Purity:           {purity} ({purity_band})")
        else:
            lines.append(f"    Purity:           N/A ({purity_band})")

        if coupling_score is not None:
            lines.append(f"    Coupling:         {coupling_cat} (score: {coupling_score})")
        else:
            lines.append(f"    Coupling:         {coupling_cat}")

        lines.append(f"    Boltzmann:        {boltzmann}")

        # Drift events
        drift = entry.get("drift_events", [])
        if drift:
            drift_types = ", ".join(d.get("type", "?") for d in drift)
            lines.append(f"    Drift events:     {len(drift)} — {drift_types}")

        # Tangled rope fields
        t_psi = entry.get("tangled_psi")
        if t_psi is not None:
            t_band = entry.get("tangled_band", "N/A")
            coalition = entry.get("coalition_type", "N/A")
            lines.append(f"    Tangled psi:      {t_psi:.4f} ({t_band})")
            lines.append(f"    Coalition:        {coalition}")

    else:
        if live_claimed:
            lines.append(f"    Claimed Type:     {live_claimed}")
        if live_perspectives:
            live_str = _compact_types(live_perspectives)
            if live_str:
                lines.append(f"    Live Type:        {live_str}")
        lines.append("    Signature:        [from Prolog output above]")
        lines.append("    Purity:           [not yet in batch]")
        lines.append("    Coupling:         [not yet in batch]")

    return "\n".join(lines)


# --- Level 2: CLASSIFICATION CONVERGENCE (from old Section A L2 fields) ---

def build_level2_convergence(constraint_id, pipeline_data):
    """L2: Diagnostic convergence — batch agreement, confidence, rival,
    margin, boundary, H^1 band."""
    lines = ["", "--- CLASSIFICATION CONVERGENCE ---", ""]

    if pipeline_data is None:
        lines.append("  [enriched_pipeline.json not available]")
        return "\n".join(lines)

    entry = find_constraint_entry(pipeline_data, constraint_id)
    if entry is None:
        lines.append("  Not yet in batch — run full pipeline to include.")
        return "\n".join(lines)

    # Batch type + agreement with live
    batch_persp = entry.get("perspectives", {})
    batch_str = _compact_types(batch_persp)
    if batch_str:
        lines.append(f"    Batch Type:       {batch_str}")

    # Confidence fields
    conf = entry.get("confidence")
    conf_band = entry.get("confidence_band")
    if conf is not None:
        lines.append(f"    Confidence:       {conf:.4f} ({conf_band})")
        rival = entry.get("rival_type")
        rival_p = entry.get("rival_prob")
        if rival and rival_p is not None:
            lines.append(f"    Rival Type:       {rival} (P={rival_p:.4f})")
        margin = entry.get("confidence_margin")
        if margin is not None:
            lines.append(f"    Margin:           {margin:+.4f}")
        boundary = entry.get("boundary")
        if boundary:
            lines.append(f"    Boundary:         {boundary}")

    # H^1 band (cohomological obstruction)
    h1 = entry.get("h1_band")
    if h1 is not None:
        h1_desc = {0: "gauge-invariant (all observers agree)",
                   1: "minimal fracture", 2: "moderate fracture",
                   3: "power-scaling driven", 4: "hub-conflict driven",
                   5: "high fracture", 6: "maximally fractured"}
        lines.append(f"    H^1 band:         {h1} — {h1_desc.get(h1, 'unknown')}")

    return "\n".join(lines)


# --- Level 3: CORPUS DISTRIBUTION + POSITIONING (from old Section A) ---

def build_level3_distribution(constraint_id, pipeline_data, orbit_data, omega_data):
    """L3: Corpus distributions, constraint positioning, and orbit family."""
    lines = ["", "--- CORPUS DISTRIBUTION ---", ""]

    if pipeline_data is None:
        lines.append("  [enriched_pipeline.json not available]")
        return "\n".join(lines)

    diag = pipeline_data.get("diagnostic", {})
    val = pipeline_data.get("validation", {})
    per_constraint = pipeline_data.get("per_constraint", [])
    entry = find_constraint_entry(pipeline_data, constraint_id)
    in_batch = entry is not None

    type_dist = diag.get("type_distribution", {})
    purity_dist = diag.get("purity_summary", {})
    coupling_dist = diag.get("coupling_summary", {})
    sig_dist = val.get("signature_distribution", {}) if val else {}

    type_order = ["mountain", "rope", "tangled_rope", "snare", "piton", "scaffold"]
    type_parts = [f"{type_dist[t]} {t}" for t in type_order if t in type_dist]
    for t, count in sorted(type_dist.items()):
        if t not in type_order:
            type_parts.append(f"{count} {t}")
    lines.append(f"    Type:      {' | '.join(type_parts)}")

    purity_order = ["pristine", "sound", "borderline", "contaminated", "degraded"]
    purity_parts = [f"{purity_dist[p]} {p}" for p in purity_order if p in purity_dist]
    lines.append(f"    Purity:    {' | '.join(purity_parts)}")

    coupling_order = ["strongly_coupled", "weakly_coupled", "independent", "inconclusive"]
    coupling_parts = []
    for c in coupling_order:
        if c in coupling_dist:
            label = c.replace("_coupled", "")
            coupling_parts.append(f"{coupling_dist[c]} {label}")
    lines.append(f"    Coupling:  {' | '.join(coupling_parts)}")

    if sig_dist:
        sig_parts = [f"{count} {sig}" for sig, count in
                     sorted(sig_dist.items(), key=lambda x: -x[1])]
        if len(sig_parts) > 5:
            sig_parts = sig_parts[:5] + ["..."]
        lines.append(f"    Signature: {' | '.join(sig_parts)}")

    # Confidence distribution across corpus
    conf_bands = {}
    conf_sum = 0.0
    conf_n = 0
    for pc in per_constraint:
        b = pc.get("confidence_band")
        if b:
            conf_bands[b] = conf_bands.get(b, 0) + 1
        c = pc.get("confidence")
        if c is not None:
            conf_sum += c
            conf_n += 1
    if conf_bands:
        band_parts = [f"{conf_bands.get(b, 0)} {b}" for b in ["deep", "moderate", "borderline"] if b in conf_bands]
        mean_str = f" (mean: {conf_sum / conf_n:.3f})" if conf_n else ""
        lines.append(f"    Confidence: {' | '.join(band_parts)}{mean_str}")

    # --- Positioning block (batch constraints only) ---
    if in_batch:
        lines.append("")
        lines.append("  --- CONSTRAINT POSITIONING ---")
        signature = entry.get("signature", "unknown")
        if sig_dist and signature in sig_dist:
            sig_count = sig_dist[signature]
            corpus_size = diag.get("corpus_size", 1)
            sig_pct = (sig_count / corpus_size) * 100
            lines.append(f"    This constraint is a {signature} ({sig_pct:.1f}% of corpus shares this signature)")

        purity_band = entry.get("purity_band", "unknown")
        if purity_dist and purity_band in purity_dist:
            band_count = purity_dist[purity_band]
            corpus_size = diag.get("corpus_size", 1)
            band_pct = (band_count / corpus_size) * 100
            lines.append(f"    Purity band: {purity_band} ({band_pct:.1f}% of corpus in this band)")

        conf_band = entry.get("confidence_band")
        if conf_band and conf_bands:
            cb_count = conf_bands.get(conf_band, 0)
            cb_total = sum(conf_bands.values())
            cb_pct = (cb_count / cb_total) * 100 if cb_total else 0
            lines.append(f"    Confidence band: {conf_band} ({cb_pct:.1f}% of corpus in this band)")
        boundary = entry.get("boundary")
        if boundary:
            boundary_count = sum(1 for pc in per_constraint if pc.get("boundary") == boundary)
            lines.append(f"    Boundary zone: {boundary} ({boundary_count} constraints share this boundary)")

    # Orbit Family ID (from orbit/omega data, positioned at L3)
    key = constraint_id.lower()
    family = None
    if omega_data and "omegas" in omega_data:
        for omega in omega_data["omegas"]:
            if omega.get("associated_constraint", "").lower() == key:
                family = omega.get("family")
                break
    if family:
        lines.append(f"    Orbit Family ID:  {family}")

    return "\n".join(lines)


# --- Level 1: ORBIT CONTEXT (signature, span, gauge — no family) ---

def build_level1_orbit(constraint_id, orbit_data):
    """L1: Orbit self-consistency — signature, span, gauge status."""
    lines = ["", "--- ORBIT CONTEXT ---", ""]

    if orbit_data is None:
        lines.append("  [orbit_data.json not available]")
        return "\n".join(lines)

    key = constraint_id.lower()
    entry = orbit_data.get(key)
    if entry is None:
        lines.append(
            "  Not yet in orbit analysis — run full pipeline to include."
        )
        return "\n".join(lines)

    sig = entry.get("orbit_signature", [])
    contexts = entry.get("contexts", {})

    context_vals = list(contexts.values())
    gauge = "Gauge-Invariant" if len(set(context_vals)) <= 1 else "Gauge-Variant"

    lines.append(f"  Orbit Signature:    [{', '.join(sig)}]")
    lines.append(f"  Orbit Span:         {len(sig)}")
    lines.append(f"  Gauge Status:       {gauge}")

    return "\n".join(lines)


# --- Section C: MAXENT SHADOW CLASSIFICATION ---

def build_maxent_section(constraint_id, pipeline_data):
    """Section C: MAXENT SHADOW CLASSIFICATION from enriched_pipeline.json."""
    lines = ["", "--- MAXENT SHADOW CLASSIFICATION ---", ""]

    if pipeline_data is None:
        lines.append("  [enriched_pipeline.json not available]")
        return "\n".join(lines)

    entry = find_constraint_entry(pipeline_data, constraint_id)
    if entry is None:
        lines.append(
            "  Not yet in MaxEnt batch — run full pipeline to include.\n"
            "  (MaxEnt validates classification stability across the full corpus.)"
        )
        return "\n".join(lines)

    claimed = entry.get("claimed_type", "unknown")
    top_type = entry.get("maxent_top_type")
    conf = entry.get("confidence")
    conf_band = entry.get("confidence_band")
    rival = entry.get("rival_type")
    rival_p = entry.get("rival_prob")
    margin = entry.get("confidence_margin")
    entropy = entry.get("confidence_entropy")
    probs = entry.get("maxent_probs", {})

    if conf is None:
        lines.append("  [confidence fields not yet enriched — re-run pipeline]")
        return "\n".join(lines)

    # Determine classification status
    hard_disagreement = top_type is not None and top_type != claimed
    high_uncertainty = entropy is not None and entropy > 0.5

    if hard_disagreement:
        lines.append(f"  HARD DISAGREEMENT: Pipeline says {claimed}, MaxEnt says {top_type}")
    elif high_uncertainty:
        lines.append("  High Uncertainty (types agree but entropy is elevated)")
    else:
        lines.append("  Classification is stable (low entropy, types agree)")

    lines.append(f"  Confidence:    {conf:.4f} ({conf_band})")
    if rival:
        lines.append(f"  Rival Type:    {rival} (P={rival_p:.4f})" if rival_p is not None else f"  Rival Type:    {rival}")
    if margin is not None:
        lines.append(f"  Margin:        {margin:+.4f}")
    if entropy is not None:
        lines.append(f"  Entropy:       {entropy:.4f}")

    # Top-3 probability distribution
    if probs:
        sorted_probs = sorted(probs.items(), key=lambda x: -x[1])[:3]
        dist_parts = [f"{t}: {p:.3f}" for t, p in sorted_probs if p > 0]
        if dist_parts:
            lines.append(f"  Distribution:  {', '.join(dist_parts)}")

    return "\n".join(lines)


# --- Section D: ENRICHED OMEGA CONTEXT ---

def build_omega_section(constraint_id, omega_data):
    """Section D: ENRICHED OMEGA CONTEXT from enriched_omega_data.json.

    Only shows enrichment-unique fields: severity_score, gap_class, gap_pattern, family.
    """
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
            "  Not yet enriched — see live omega results in report sections below.\n"
            "  (Run full pipeline to include in severity scoring and family grouping.)"
        )
        return "\n".join(lines)

    for i, omega in enumerate(matches):
        if i > 0:
            lines.append("")
        lines.append(f"  Omega: {omega.get('name', 'N/A')}")
        lines.append(f"    Severity Score:    {omega.get('severity_score', 'N/A')}")
        lines.append(f"    Gap Class:         {omega.get('gap_class', 'N/A')}")
        lines.append(f"    Gap Pattern:       {omega.get('gap_pattern', 'N/A')}")
        lines.append(f"    Family ID:         {omega.get('family', 'N/A')}")

    return "\n".join(lines)


# --- Section E: STRUCTURAL CONTEXT ---

def build_structural_section(constraint_id, corpus_data, pattern_text, covering_text):
    """Section E: STRUCTURAL CONTEXT from corpus_data.json + markdown reports."""
    lines = ["", "--- STRUCTURAL CONTEXT ---", ""]

    key = constraint_id.lower()

    # --- Corpus data: analysis sub-object ---
    if corpus_data and "constraints" in corpus_data:
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
            lines.append(
                "  Not yet in corpus — run full pipeline to include.\n"
                "  (Variance, twin group, and covering analysis require batch corpus data.)"
            )
    else:
        lines.append("  [corpus_data.json not available]")

    # --- Pattern mining: structural twins ---
    lines.append("")
    if pattern_text:
        twin_found = False
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
                lines.append("  Structural Twin Group:")
                lines.append(f"    Signature:   {sig}")
                lines.append(f"    Group Size:  {count}")
                lines.append(f"    Types:       {types}")
                break
        if not twin_found:
            lines.append("  Structural Twins:     [not found in batch twin analysis]")
    else:
        lines.append("  [pattern_mining.md not available]")

    # --- Covering analysis: transition detail ---
    lines.append("")
    if covering_text:
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
            lines.append("  Covering Analysis:")
            lines.append(f"    Missed Transitions: {len(transitions)}")
            lines.append(f"    Unique Type Shifts:  {', '.join(unique_transitions)}")
        else:
            lines.append("  Covering Analysis:    [not found in batch covering analysis]")
    else:
        lines.append("  [covering_analysis.md not available]")

    return "\n".join(lines)


# --- Section F: ABDUCTIVE FLAGS ---

def build_abductive_section(constraint_id, pipeline_data):
    """Section F: ABDUCTIVE FLAGS — cross-subsystem anomaly synthesis."""
    lines = ["", "--- ABDUCTIVE FLAGS ---", ""]

    if pipeline_data is None:
        lines.append("  [enriched_pipeline.json not available]")
        return "\n".join(lines)

    entry = find_constraint_entry(pipeline_data, constraint_id)
    if entry is None:
        lines.append("  Not yet in batch — run full pipeline to include.")
        return "\n".join(lines)

    triggers = entry.get("abductive_triggers", [])
    if not triggers:
        lines.append("  No abductive triggers fired. All diagnostic paths agree.")
        return "\n".join(lines)

    lines.append(f"  **{len(triggers)} trigger(s) fired:**")
    lines.append("")
    lines.append("  | Trigger Class | Confidence | Anomaly | Category |")
    lines.append("  |---|---|---|---|")
    for t in sorted(triggers, key=lambda x: x.get("confidence", 0), reverse=True):
        tc = t.get("trigger_class", "—")
        conf = t.get("confidence", 0)
        anom = t.get("anomaly_type", "—")
        cat = t.get("category", "—")
        lines.append(f"  | {tc} | {conf:.2f} | {anom} | {cat} |")

    return "\n".join(lines)


# --- Level 2: DIAGNOSTIC VERDICT body (without verdict line and T12) ---

def build_level2_verdict_body(constraint_id, pipeline_data):
    """L2: Diagnostic convergence — subsystems, agreements, conflicts, tensions."""
    lines = ["", "--- DIAGNOSTIC VERDICT ---", ""]

    if pipeline_data is None:
        lines.append("  [enriched_pipeline.json not available]")
        return "\n".join(lines)

    entry = find_constraint_entry(pipeline_data, constraint_id)
    if entry is None:
        lines.append("  Not yet in batch — run full pipeline to include.")
        return "\n".join(lines)

    dv = entry.get("diagnostic_verdict")
    if dv is None:
        lines.append("  [diagnostic_verdict not computed for this constraint]")
        return "\n".join(lines)

    agreements = dv.get("agreements", [])
    expected_conflicts = dv.get("expected_conflicts", [])
    convergent_rejections = dv.get("convergent_rejections", [])
    tensions = dv.get("tensions", [])
    n_avail = dv.get("subsystems_available", 0)
    unavail = dv.get("subsystems_unavailable", [])

    total_subsystems = n_avail + len(unavail)

    # Subsystems checked
    if unavail:
        unavail_str = ", ".join(str(u) for u in unavail)
        lines.append(
            f"  Subsystems Checked: {n_avail}/{total_subsystems} "
            f"({unavail_str} unavailable)"
        )
    else:
        lines.append(f"  Subsystems Checked: {n_avail}/{total_subsystems}")

    # Agreements
    lines.append("")
    if agreements:
        lines.append(f"  Agreements ({len(agreements)} subsystems):")
        lines.append(f"    {', '.join(str(a) for a in agreements)}")
    else:
        lines.append("  Agreements: none")

    # Expected Conflicts
    lines.append("")
    if expected_conflicts:
        lines.append(f"  Expected Conflicts ({len(expected_conflicts)}):")
        for ec in expected_conflicts:
            sub = ec.get("subsystem", "?")
            pat = ec.get("pattern", "?")
            expl = ec.get("explanation", "")
            lines.append(f"    {sub}: {pat}")
            if expl:
                lines.append(f"      {expl}")
    else:
        lines.append("  Expected Conflicts: none")

    # Convergent Rejections
    lines.append("")
    if convergent_rejections:
        lines.append(f"  Convergent Rejections ({len(convergent_rejections)}):")
        for cr in convergent_rejections:
            subs = cr.get("subsystems", [])
            alt = cr.get("alternative_type", "?")
            evidence = cr.get("evidence", "")
            lines.append(f"    -> {alt} (suggested by: {', '.join(str(s) for s in subs)})")
            if evidence:
                lines.append(f"       {evidence}")
    else:
        lines.append("  Convergent Rejections: none")

    # Tensions
    lines.append("")
    if tensions:
        lines.append(f"  Tensions ({len(tensions)}):")
        for t in tensions:
            sub = t.get("subsystem", "?")
            signal = t.get("signal", "?")
            lines.append(f"    {sub}: {signal}")
    else:
        lines.append("  Tensions: none")

    return "\n".join(lines)


# --- Post-Synthesis (T12 flags, from old Section G tail) ---

def build_post_synthesis(constraint_id, pipeline_data):
    """Post-synthesis divergence flags (T12). Returns empty string if none."""
    if pipeline_data is None:
        return ""

    entry = find_constraint_entry(pipeline_data, constraint_id)
    if entry is None:
        return ""

    ps_flags = entry.get("post_synthesis_flags", [])
    if not ps_flags:
        return ""

    lines = ["", "--- POST-SYNTHESIS DIVERGENCE ---", ""]
    lines.append(f"  {len(ps_flags)} flag(s):")
    for flag in ps_flags:
        ft = flag.get("flag_type", "?")
        lines.append(f"    Flag: {ft}")
        details = flag.get("details", {})
        for k, v in sorted(details.items()):
            lines.append(f"      {k}: {v}")

    return "\n".join(lines)


# --- Report Assembly ---

def assemble_report(header, prolog_output, sections):
    """Insert corpus context sections between LOGICAL FINGERPRINT and DR EXECUTIVE SUMMARY.

    Splits Prolog output at the first ==== line after --- LOGICAL FINGERPRINT ---.
    """
    insertion = "\n".join(sections)

    fp_idx = prolog_output.find(MARKER_FP)
    if fp_idx == -1:
        # Fallback: append at end
        return header + prolog_output + "\n" + insertion

    # Find the ==== delimiter that starts DR EXECUTIVE SUMMARY
    after_fp = prolog_output[fp_idx:]
    exec_offset = after_fp.find("\n" + MARKER_EXEC)
    if exec_offset == -1:
        return header + prolog_output + "\n" + insertion

    split_point = fp_idx + exec_offset
    before = prolog_output[:split_point]
    after = prolog_output[split_point:]
    return header + before + "\n" + insertion + "\n" + after


# --- Pipeline Dashboard ---

def run_dashboard():
    """Run pipeline_dashboard.sh to show corpus health before report generation."""
    dashboard = PROJECT_ROOT / "scripts" / "pipeline_dashboard.sh"
    if not dashboard.exists():
        print("[WARN] pipeline_dashboard.sh not found, skipping", file=sys.stderr)
        return
    try:
        subprocess.run(
            ["bash", str(dashboard)],
            cwd=str(PROJECT_ROOT), timeout=30
        )
    except subprocess.TimeoutExpired:
        print("[WARN] Dashboard timed out after 30s, continuing", file=sys.stderr)
    except OSError as e:
        print(f"[WARN] Dashboard failed: {e}", file=sys.stderr)


# --- Recent Testset Discovery ---

def find_recent_testsets(hours=1):
    """Find .pl testset files modified within the last `hours` hours."""
    testsets_dir = PROLOG_DIR / "testsets"
    if not testsets_dir.exists():
        return []
    cutoff = time.time() - (hours * 3600)
    recent = []
    for pl_file in sorted(testsets_dir.glob("*.pl")):
        if pl_file.stat().st_mtime >= cutoff:
            constraint_id = pl_file.stem
            recent.append(constraint_id)
    return recent


# --- Per-Constraint Report Generation ---

def generate_report(constraint_id, data):
    """Generate a single constraint report. `data` is the shared loaded data dict."""
    print(f"\nGenerating enhanced report for: {constraint_id}")

    prolog_output = run_prolog_report(constraint_id)

    header = build_header(data["pipeline"])

    # Verdict banner — first thing the analyst sees
    banner = build_verdict_banner(constraint_id, data["pipeline"])

    # Level 1: Self-Consistency
    l1_identity = build_level1_identity(constraint_id, data["pipeline"], prolog_output)
    l1_orbit = build_level1_orbit(constraint_id, data["orbit"])
    l1_omega = build_omega_section(constraint_id, data["omega"])

    # Level 2: Diagnostic Convergence
    l2_convergence = build_level2_convergence(constraint_id, data["pipeline"])
    l2_maxent = build_maxent_section(constraint_id, data["pipeline"])
    l2_abductive = build_abductive_section(constraint_id, data["pipeline"])
    l2_verdict = build_level2_verdict_body(constraint_id, data["pipeline"])

    # Level 3: Corpus Positioning
    l3_distribution = build_level3_distribution(
        constraint_id, data["pipeline"], data["orbit"], data["omega"]
    )
    l3_structural = build_structural_section(
        constraint_id, data["corpus"], data["pattern"], data["covering"]
    )

    # Post-synthesis (only if T12 flags exist)
    post = build_post_synthesis(constraint_id, data["pipeline"])

    sections = [
        banner,
        build_level_header(1, "SELF-CONSISTENCY"),
        l1_identity, l1_orbit, l1_omega,
        build_level_header(2, "DIAGNOSTIC CONVERGENCE"),
        l2_convergence, l2_maxent, l2_abductive, l2_verdict,
        build_level_header(3, "CORPUS POSITIONING"),
        l3_distribution, l3_structural,
    ]
    if post.strip():
        sections.extend(["\n═══ POST-SYNTHESIS ═══", post])

    full_report = assemble_report(header, prolog_output, sections)

    REPORTS_DIR.mkdir(parents=True, exist_ok=True)
    out_path = REPORTS_DIR / f"{constraint_id}_report.md"
    with open(out_path, "w", encoding="utf-8") as f:
        f.write(full_report)

    print(f"Report written to: {out_path}")


# --- Main ---

def main():
    # Determine which constraints to process
    if len(sys.argv) >= 2:
        constraint_ids = sys.argv[1:]
    else:
        # Auto-discover testsets modified in the last hour
        constraint_ids = find_recent_testsets(hours=1)
        if not constraint_ids:
            print("No arguments given and no testsets modified in the last hour.", file=sys.stderr)
            print("Usage: python3 python/enhanced_report.py [constraint_id ...]", file=sys.stderr)
            print("       (or modify a testset in prolog/testsets/ and re-run)", file=sys.stderr)
            sys.exit(1)
        print(f"Auto-detected {len(constraint_ids)} recently modified testset(s):")
        for cid in constraint_ids:
            print(f"  {cid}")

    # Run dashboard first
    run_dashboard()

    # Load all data sources once
    data = {
        "pipeline": load_json(OUTPUTS_DIR / "enriched_pipeline.json", "enriched_pipeline.json"),
        "orbit":    load_json(OUTPUTS_DIR / "orbit_data.json", "orbit_data.json"),
        "omega":    load_json(OUTPUTS_DIR / "enriched_omega_data.json", "enriched_omega_data.json"),
        "corpus":   load_json(OUTPUTS_DIR / "corpus_data.json", "corpus_data.json"),
        "maxent":   load_text(OUTPUTS_DIR / "maxent_report.md", "maxent_report.md"),
        "pattern":  load_text(OUTPUTS_DIR / "pattern_mining.md", "pattern_mining.md"),
        "covering": load_text(OUTPUTS_DIR / "covering_analysis.md", "covering_analysis.md"),
    }

    # Generate reports
    for constraint_id in constraint_ids:
        generate_report(constraint_id, data)

    if len(constraint_ids) > 1:
        print(f"\nDone: {len(constraint_ids)} reports generated.")


if __name__ == "__main__":
    main()
