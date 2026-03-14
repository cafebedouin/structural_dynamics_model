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

import argparse
import json
import logging
import os
import re
import subprocess
import sys
import time
from collections import Counter
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


def _explain_h1_band(h1, perspectives):
    """Dynamic H^1 band explanation using observer perspective data."""
    if h1 is None:
        return "unknown"
    if not perspectives or not isinstance(perspectives, dict):
        # Fallback to static labels when perspectives unavailable
        static = {0: "gauge-invariant (all observers agree)",
                  1: "minimal fracture", 2: "moderate fracture",
                  3: "power-scaling driven", 4: "hub-conflict driven",
                  5: "high fracture", 6: "maximally fractured"}
        return static.get(h1, "unknown")

    observers = ["powerless", "moderate", "institutional", "analytical"]
    obs_types = {o: perspectives.get(o) for o in observers if perspectives.get(o)}

    if h1 == 0:
        return "All observers agree. Neither hub produces classification divergence."

    if h1 == 3 and len(obs_types) >= 4:
        type_counts = Counter(obs_types.values())
        majority_type = type_counts.most_common(1)[0][0]
        dissenters = [o for o, t in obs_types.items() if t != majority_type]
        agreers = [o for o, t in obs_types.items() if t == majority_type]
        if len(dissenters) == 1:
            return (f"Hub 1 (power-scaled extraction) drives a 3+1 split: "
                    f"{dissenters[0]} sees {obs_types[dissenters[0]]} while "
                    f"{', '.join(agreers)} see {majority_type}.")

    if h1 == 4 and len(obs_types) >= 4:
        type_counts = Counter(obs_types.values())
        if len(type_counts) == 2:
            types = list(type_counts.keys())
            bloc_a = [o for o, t in obs_types.items() if t == types[0]]
            bloc_b = [o for o, t in obs_types.items() if t == types[1]]
            return (f"Hub 2 (effective immutability) drives a 2+2 split: "
                    f"{', '.join(bloc_a)} see {types[0]}; "
                    f"{', '.join(bloc_b)} see {types[1]}.")

    if h1 == 5 and len(obs_types) >= 4:
        type_counts = Counter(obs_types.values())
        parts = []
        for t, count in type_counts.most_common():
            who = [o for o, ot in obs_types.items() if ot == t]
            parts.append(f"{', '.join(who)} → {t}")
        return f"Both hubs contribute — 3 types across 4 observers: {'; '.join(parts)}."

    if h1 == 6 and len(obs_types) >= 4:
        parts = [f"{o} → {t}" for o, t in obs_types.items()]
        return f"Maximally fractured — all 4 observers disagree: {'; '.join(parts)}."

    # Fallback for edge cases (h1=1,2, or insufficient observer data)
    static = {0: "gauge-invariant (all observers agree)",
              1: "minimal fracture", 2: "moderate fracture",
              3: "power-scaling driven", 4: "hub-conflict driven",
              5: "high fracture", 6: "maximally fractured"}
    return static.get(h1, "unknown")


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


def extract_mandatrophy_gap(prolog_output):
    """Extract mandatrophy gap from Prolog output, if present.

    Returns dict {delta_chi: float, severity: str} or None.
    """
    m = re.search(
        r'MANDATROPHY GAP: delta_chi = ([\d.]+)\s+\((\w+)\)',
        prolog_output,
    )
    if m:
        return {"delta_chi": float(m.group(1)), "severity": m.group(2)}
    return None


# --- Sidecar Builder ---

logger = logging.getLogger(__name__)


def build_sidecar_data(constraint_id, entry, prolog_output, iteration_round=None):
    """Build structured sidecar dict from pre-markdown data sources.

    Args:
        constraint_id: The constraint ID.
        entry: The enriched pipeline per_constraint dict (or None).
        prolog_output: Raw Prolog output text.
        iteration_round: Which iteration produced this (None if initial).

    Returns:
        dict matching the report sidecar schema.
    """
    sidecar = {
        "constraint_id": constraint_id,
        "iteration_round": iteration_round,
    }

    # --- Verdict, subsystems, tensions from diagnostic_verdict ---
    dv = entry.get("diagnostic_verdict") if entry else None
    if dv:
        verdict_raw = dv.get("verdict", "unknown")
        sidecar["verdict"] = verdict_raw.upper() if isinstance(verdict_raw, str) else "UNKNOWN"
        n_avail = dv.get("subsystems_available", 0)
        unavail = dv.get("subsystems_unavailable", [])
        total = n_avail + len(unavail)
        sidecar["subsystems_checked"] = [n_avail, total]

        raw_tensions = dv.get("tensions", [])
        tensions = []
        for t in raw_tensions:
            subsystem = t.get("subsystem", "unknown")
            signal = t.get("signal", "")
            # Extract functor name (word before first '(') to match _parse_report
            paren_idx = signal.find("(")
            code = signal[:paren_idx].strip() if paren_idx > 0 else signal
            tensions.append({
                "subsystem": subsystem,
                "code": code,
                "detail": signal,
            })
        sidecar["tensions"] = tensions
        sidecar["tension_count"] = len(tensions)

        # Expected conflicts
        raw_ec = dv.get("expected_conflicts", [])
        expected_conflicts = []
        for ec in raw_ec:
            expected_conflicts.append({
                "subsystem": ec.get("subsystem", "unknown"),
                "code": ec.get("pattern", ""),
            })
        sidecar["expected_conflicts"] = expected_conflicts

        # Convergent rejections
        raw_cr = dv.get("convergent_rejections", [])
        if raw_cr:
            parts = []
            for cr in raw_cr:
                alt = cr.get("alternative_type", "?")
                subs = cr.get("subsystems", [])
                parts.append(f"{alt} (suggested by: {', '.join(str(s) for s in subs)})")
            sidecar["convergent_rejections"] = "; ".join(parts)
        else:
            sidecar["convergent_rejections"] = "none"
    else:
        sidecar["verdict"] = "UNKNOWN"
        sidecar["subsystems_checked"] = [0, 0]
        sidecar["tensions"] = []
        sidecar["tension_count"] = 0
        sidecar["expected_conflicts"] = []
        sidecar["convergent_rejections"] = "none"

    # --- Classification ---
    claimed_type = entry.get("claimed_type") if entry else None
    classified_type = entry.get("maxent_top_type") if entry else None
    mismatch = None
    if claimed_type is not None and classified_type is not None and classified_type != "":
        mismatch = (claimed_type != classified_type)

    classification = {
        "claimed_type": claimed_type,
        "classified_type": classified_type if classified_type else None,
        "mismatch": mismatch,
        "confidence": entry.get("confidence") if entry else None,
        "confidence_band": entry.get("confidence_band") if entry else None,
        "rival_type": entry.get("rival_type") if entry else None,
        "rival_p": entry.get("rival_prob") if entry else None,
        "boundary": entry.get("boundary") if entry else None,
        "psi": entry.get("tangled_psi") if entry else None,
        "psi_label": entry.get("tangled_band") if entry else None,
    }
    sidecar["classification"] = classification

    # --- Hard disagreement ---
    if mismatch:
        sidecar["hard_disagreement"] = {
            "pipeline": claimed_type,
            "maxent": classified_type,
        }
    else:
        sidecar["hard_disagreement"] = None

    # --- Drift events ---
    sidecar["drift_events"] = [
        {"severity": d.get("severity", "unknown"), "type": d.get("type", "unknown")}
        for d in (entry.get("drift_events", []) if entry else [])
    ]

    # --- Mandatrophy gap (from Prolog output) ---
    sidecar["mandatrophy_gap"] = extract_mandatrophy_gap(prolog_output)

    # --- Structural signature ---
    sidecar["structural_signature"] = entry.get("signature") if entry else None

    # --- Purity ---
    purity_score = entry.get("purity_score") if entry else None
    purity_band = entry.get("purity_band") if entry else None
    if purity_score is not None:
        sidecar["purity"] = {"value": purity_score, "band": purity_band or "unknown"}
    else:
        sidecar["purity"] = None

    # --- Post-synthesis flags ---
    sidecar["post_synthesis_flags"] = list(entry.get("post_synthesis_flags", [])) if entry else []

    return sidecar


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


# --- Level 1 (cont): CONTAMINATION NETWORK (FPN topology) ---


def build_contamination_network(constraint_id, pipeline_data):
    """L1: FPN contamination topology — intrinsic vs effective purity
    and neighbor network."""
    lines = ["", "--- CONTAMINATION NETWORK ---", ""]

    if pipeline_data is None:
        lines.append("  [enriched_pipeline.json not available]")
        return "\n".join(lines)

    entry = find_constraint_entry(pipeline_data, constraint_id)
    if entry is None:
        lines.append("  [constraint not in batch]")
        return "\n".join(lines)

    cn = entry.get("contamination_network")
    if cn is None:
        # Graceful fallback for older pipeline output
        lines.append("  No contamination network — purity is intrinsic.")
        return "\n".join(lines)

    ip = cn.get("intrinsic_purity")
    ep = cn.get("effective_purity")
    delta = cn.get("propagation_delta")
    neighbors = cn.get("neighbors", [])

    # Purity comparison
    if ip is not None and ep is not None:
        lines.append(f"    Intrinsic purity:   {ip:.4f}")
        lines.append(f"    Effective purity:   {ep:.4f}")
        if delta is not None:
            lines.append(f"    Propagation delta:  {delta:+.4f}")
    else:
        lines.append("    Purity metrics:     N/A")

    # Neighbor table
    if not neighbors:
        lines.append("")
        lines.append("  No contamination network — purity is intrinsic.")
    else:
        lines.append("")
        lines.append(f"    Network neighbors ({len(neighbors)}):")
        lines.append("")
        lines.append("    | Neighbor | Type | Edge | Strength | Purity |")
        lines.append("    |----------|------|------|----------|--------|")
        for n in neighbors:
            nid = n.get("constraint_id", "?")
            ntype = n.get("neighbor_type") or "?"
            etype = n.get("edge_type") or "?"
            strength = n.get("edge_strength")
            npurity = n.get("neighbor_purity")
            s_str = f"{strength:.2f}" if strength is not None else "N/A"
            p_str = f"{npurity:.4f}" if npurity is not None else "N/A"
            lines.append(
                f"    | {nid} | {ntype} | {etype} | {s_str} | {p_str} |"
            )

        # One-sentence interpretation
        lines.append("")
        if delta is not None and delta < -0.0001:
            ranked = sorted(
                [n for n in neighbors if n.get("neighbor_purity") is not None],
                key=lambda n: n["neighbor_purity"],
            )
            if ranked:
                worst = ranked[0]
                lines.append(
                    f"  Purity degraded from {ip:.4f} to {ep:.4f} "
                    f"by contamination from {len(neighbors)} neighbor(s), "
                    f"primarily {worst['constraint_id']} "
                    f"({worst.get('edge_type', '?')}, "
                    f"purity {worst['neighbor_purity']:.4f})."
                )
            else:
                lines.append(
                    f"  Purity degraded from {ip:.4f} to {ep:.4f} "
                    f"by contamination from {len(neighbors)} neighbor(s)."
                )
        elif delta is not None and delta > 0.0001:
            lines.append(
                f"  Purity improved from {ip:.4f} to {ep:.4f} — "
                f"cleaned by {len(neighbors)} neighbor(s)."
            )
        else:
            lines.append(
                f"  No significant contamination — "
                f"purity unchanged across {len(neighbors)} neighbor(s)."
            )

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
        perspectives = entry.get("perspectives", {})
        h1_explanation = _explain_h1_band(h1, perspectives)
        lines.append(f"    H^1 band:         {h1} — {h1_explanation}")

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

    # --- Indexed-mode MaxEnt (power-scaled χ) ---
    indexed = entry.get("maxent_indexed")
    divergence = entry.get("maxent_divergence")

    if indexed is not None:
        idx_dist = indexed.get("distribution", {})
        idx_entropy = indexed.get("entropy")
        idx_top = indexed.get("top_type")
        idx_top_p = indexed.get("top_prob")

        lines.append("")
        lines.append("  Indexed MaxEnt (\u03c7-scaled, analytical context):")
        if idx_top and idx_top_p is not None:
            lines.append(f"  Top Type:      {idx_top} (P={idx_top_p:.4f})")
        if idx_entropy is not None:
            lines.append(f"  Entropy:       {idx_entropy:.4f}")
        if idx_dist:
            sorted_idx = sorted(idx_dist.items(), key=lambda x: -x[1])[:3]
            idx_parts = [f"{t}: {p:.3f}" for t, p in sorted_idx if p > 0]
            if idx_parts:
                lines.append(f"  Distribution:  {', '.join(idx_parts)}")

    if divergence is not None:
        tv = divergence.get("total_variation")
        interp = divergence.get("interpretation")

        if tv is not None:
            lines.append("")
            lines.append(
                f"  Classical/Indexed TV Distance: {tv:.4f} ({interp})"
            )

            if interp == "near_zero":
                lines.append(
                    "  Classical and indexed MaxEnt agree — observer-dependence "
                    "does not alter probabilistic classification."
                )
            elif interp == "moderate":
                lines.append(
                    "  Moderate divergence — observer-dependence shifts "
                    "probabilistic weights without changing the top "
                    "classification."
                )
            elif interp == "large":
                lines.append(
                    "  Significant divergence — observer-dependence changes "
                    "the probabilistic landscape. Classical Oracle Gap "
                    "(Theorem 4): single-position analysis misses this "
                    "structure."
                )

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


# --- Trigger Glosses ---

_TRIGGER_GLOSSES = {
    "signature_override_artifact": (
        "Metric disagreement explained by a known signature override "
        "— architectural artifact, not a genuine anomaly."),
    "deep_deception": (
        "Claims naturality but fails Boltzmann, yet metrics predict mountain "
        "— metrically deep deception."),
    "metric_structural_divergence": (
        "MaxEnt sees ambiguity (high entropy) but Dirac orbit is unambiguous "
        "— metric uncertainty without structural uncertainty."),
    "confirmed_liminal": (
        "Triple confirmation of genuine liminality: high entropy, "
        "multi-type orbit, and active drift."),
    "coverage_gap": (
        "Multi-type orbit detected but dr_mismatch didn't flag it "
        "— known diagnostic blind spot at analytical context."),
    "accelerating_pathology": (
        "FPN zone migration plus purity drift — static contamination "
        "AND temporal degradation both active."),
    "contamination_cascade": (
        "FPN equilibrium divergence plus network drift — contamination "
        "actively propagating, not just latent."),
    "dormant_extraction": (
        "Metrically clean appearance but extractive structural fingerprint "
        "— hidden or naturalized extraction."),
    "maxent_shadow_divergence": (
        "MaxEnt strongly favors a type different from signature override "
        "target — override may mask metric-preferred classification."),
    "convergent_structural_stress": (
        "3+ stress indicators converge with a rare anomaly signal "
        "— metrically confident but structurally stressed."),
    "snare_leaning_tangled": (
        "Classified tangled_rope but snare-lean ratio exceeds threshold "
        "— behaves more like snare than classification suggests."),
    "maxent_divergence": (
        "Indexed and classical MaxEnt disagree: observer-dependence has "
        "probabilistic consequences beyond categorical shifts (Theorem 4)."),
    "hub_conflict": (
        "Hub 1 and Hub 2 produce conflicting classification signals "
        "at this constraint."),
    "epistemic_trap": (
        "Powerless observer's restricted classification diverges from "
        "full-data view — trapped in gauge-fixed frame."),
    "classical_oracle_failure": (
        "MaxEnt is confident but H^1>0: looking carefully from one position "
        "misses what comparing across positions reveals (Theorem 4)."),
}


# --- Section E2: WASSERSTEIN TRANSPORT ---


def build_wasserstein_section(constraint_id, pipeline_data):
    """Section E2: WASSERSTEIN TRANSPORT — continuous perspectival fracture."""
    lines = ["", "--- WASSERSTEIN TRANSPORT ---", ""]

    if pipeline_data is None:
        lines.append("  [enriched_pipeline.json not available]")
        return "\n".join(lines)

    entry = find_constraint_entry(pipeline_data, constraint_id)
    if entry is None:
        lines.append("  Not yet in batch — run full pipeline to include.")
        return "\n".join(lines)

    profile = entry.get("wasserstein_profile")
    total = entry.get("wasserstein_total_fracture")
    incomp = entry.get("wasserstein_incomparable_mass")

    if profile is None:
        lines.append("  [MaxEnt multi-context data not available]")
        return "\n".join(lines)

    h1 = entry.get("h1_band")
    w12 = profile.get("u1_u2", 0)
    w23 = profile.get("u2_u3", 0)
    w34 = profile.get("u3_u4", 0)

    # Incomparable mass warnings FIRST — reader must know W1 reliability
    # before interpreting the numbers
    if incomp:
        for ctx, label in [("u1", "U1"), ("u2", "U2"), ("u3", "U3"), ("u4", "U4")]:
            mass = incomp.get(ctx, 0)
            if mass > 0.4:
                lines.append(
                    f"  \u26a0 High incomparable mass at {label}: "
                    f"{mass:.4f} \u2014 W\u2081 estimate partial"
                )

    lines.append(
        f"  Edge U1\u2192U2: {w12:.4f} | "
        f"U2\u2192U3: {w23:.4f} | "
        f"U3\u2192U4: {w34:.4f} | "
        f"Total: {total:.4f}"
    )

    # Identify highest-transport edge
    edges = {"U1\u2192U2": w12, "U2\u2192U3": w23, "U3\u2192U4": w34}
    if total > 0.001:
        max_edge = max(edges, key=edges.get)
        lines.append(f"  Peak transport:  {max_edge} ({edges[max_edge]:.4f})")

    # H1 vs W1 diagnostic
    if h1 is not None:
        if h1 >= 3 and total < 0.001:
            lines.append(
                f"  H\u00b9={h1} but W\u2081\u22480 \u2014 discrete type-switching "
                "invisible to continuous distributions"
            )
        elif h1 == 0 and total > 0.1:
            lines.append(
                f"  H\u00b9=0 but W\u2081={total:.3f} \u2014 sub-threshold "
                "distributional shift despite unanimous discrete classification"
            )

    return "\n".join(lines)


# --- Section E3: CONTEXTUALITY & MONOTONICITY ---

# Map position integers to edge labels
_BOUNDARY_EDGE = {1: "U1\u2192U2", 2: "U2\u2192U3", 3: "U3\u2192U4"}

_MONO_GLOSS = {
    "constant": "all contexts agree (global section)",
    "monotone_ascending": "extraction increases with observer power",
    "monotone_descending": "extraction decreases with observer power",
    "non_monotone": "extraction reverses along power axis",
    "incomparable": "orbit includes non-chain types (piton/naturalized/scaffold)",
}


def build_cohomology_section(constraint_id, pipeline_data):
    """Section E3: CONTEXTUALITY & MONOTONICITY \u2014 classification geometry."""
    lines = ["", "--- CONTEXTUALITY & MONOTONICITY ---", ""]

    if pipeline_data is None:
        lines.append("  [enriched_pipeline.json not available]")
        return "\n".join(lines)

    entry = find_constraint_entry(pipeline_data, constraint_id)
    if entry is None:
        lines.append("  Not yet in batch \u2014 run full pipeline to include.")
        return "\n".join(lines)

    cf = entry.get("contextuality_fraction")
    mono = entry.get("orbit_monotonicity")
    bounds = entry.get("transition_boundaries", [])
    h1 = entry.get("h1_band")

    if cf is None and mono is None:
        lines.append("  [cohomology data not available]")
        return "\n".join(lines)

    # Contextuality fraction
    if cf is not None:
        h1_str = f" (H\u00b9={h1}, {h1} of 6 context-pairs disagree)" if h1 is not None else ""
        lines.append(f"  Contextuality:   {cf:.3f}{h1_str}")

    # Orbit monotonicity
    if mono is not None:
        gloss = _MONO_GLOSS.get(mono, "")
        lines.append(f"  Monotonicity:    {mono}" + (f" \u2014 {gloss}" if gloss else ""))

    # Transition boundaries
    if bounds:
        parts = []
        for b in bounds:
            pos = b.get("position", "?")
            edge = _BOUNDARY_EDGE.get(pos, f"pos{pos}")
            parts.append(f"{edge} ({b.get('from', '?')}\u2192{b.get('to', '?')})")
        lines.append(f"  Boundaries:      {', '.join(parts)}")

    return "\n".join(lines)


# --- Section: GAME-THEORETIC STRUCTURE ---

_STABILITY_GLOSS = {
    "vulnerable": "orbit flips under single-agent power perturbation",
    "latent_vulnerable": "orbit stable now but structurally fragile",
    "resistant": "orbit survives all tested perturbations",
    "not_applicable": "constant orbit, stability undefined",
}

_COVER_GLOSS = {
    "no_cover": "no FCR effect \u2014 classification is transparent",
    "nash_forced": "FCR forces a Nash equilibrium shift",
    "type_relabeled": "FCR changes type label but structural disagreement persists",
    "fcr_no_structural_effect": "FCR active but orbit structure unchanged",
}


def build_game_theory_section(constraint_id, pipeline_data):
    """GAME-THEORETIC STRUCTURE \u2014 Nash equilibrium, stability, and cover story analysis."""
    lines = ["", "--- GAME-THEORETIC STRUCTURE ---", ""]

    if pipeline_data is None:
        lines.append("  [enriched_pipeline.json not available]")
        return "\n".join(lines)

    entry = find_constraint_entry(pipeline_data, constraint_id)
    if entry is None:
        lines.append("  Not yet in batch \u2014 run full pipeline to include.")
        return "\n".join(lines)

    nds = entry.get("nash_distance_structural")
    ss = entry.get("strategic_stability")
    meq = entry.get("mixed_equilibrium_quality")
    cst = entry.get("cover_story_type")

    if all(v is None for v in [nds, ss, meq, cst]):
        lines.append("  [game-theory data not available \u2014 run game_theory_*.py scripts]")
        return "\n".join(lines)

    # Nash distance
    if nds is not None:
        stable = entry.get("nash_stable_structural")
        stable_str = "stable" if stable else "resolvable"
        lines.append(f"  Nash distance:   {nds} ({stable_str})")

        # Flag maximally entrenched
        h1 = entry.get("h1_band")
        if nds == 3 and h1 is not None:
            lines.append(f"    \u26a0 Maximally entrenched (H\u00b9={h1})")

        vuln = entry.get("vulnerable_positions") or []
        if vuln:
            vuln_str = ", ".join(f"{v}" for v in vuln)
            lines.append(f"  Vulnerable at:   {vuln_str}")

    # Strategic stability
    if ss is not None:
        gloss = _STABILITY_GLOSS.get(ss, "")
        hpm = entry.get("h1_persistence_max")
        persist_str = f" (H\u00b9 persistence: {hpm:.3f})" if hpm is not None else ""
        lines.append(f"  Stability:       {ss}{persist_str}" + (f" \u2014 {gloss}" if gloss else ""))

    # Mixed equilibrium
    if meq is not None:
        md = entry.get("max_deviation")
        md_str = f" (max deviation: {md:.4f})" if md is not None else ""
        lines.append(f"  Equilibrium:     {meq}{md_str}")
        if meq == "loose":
            lines.append("    2-vs-2 split: loose mixed equilibrium exists")

    # Cover story
    if cst is not None:
        gloss = _COVER_GLOSS.get(cst, "")
        lines.append(f"  Cover story:     {cst}" + (f" \u2014 {gloss}" if gloss else ""))

    return "\n".join(lines)


# --- Section E4: PARAMETRIC PERSISTENCE ---

def build_persistence_section(constraint_id, persistence_data):
    """Section E4: PARAMETRIC PERSISTENCE -- bar durations from grid sweep."""
    lines = ["", "--- PARAMETRIC PERSISTENCE ---", ""]

    if persistence_data is None:
        lines.append("  [persistence_results.json not available — run persistence_sweep.py]")
        return "\n".join(lines)

    results = persistence_data.get("results", {})
    if not results:
        lines.append("  [no persistence sweep results]")
        return "\n".join(lines)

    key = constraint_id.lower()
    has_any = False

    for param_name, param_result in sorted(results.items()):
        bars = param_result.get("bars", [])
        sweep_range = param_result.get("sweep_range", 0)
        grid_range = param_result.get("grid_range", [])
        original = param_result.get("original")

        # Filter bars for this constraint
        c_bars = [b for b in bars if b.get("constraint", "").lower() == key]
        if not c_bars:
            continue
        has_any = True

        range_str = (f"[{grid_range[0]:.4f}, {grid_range[1]:.4f}]"
                     if len(grid_range) == 2 else "?")
        lines.append(f"  Swept: {param_name} (baseline={original}, range={range_str})")
        lines.append("")

        # H1 bars
        h1_bars = [b for b in c_bars if b["dimension"] == "h1"]
        if h1_bars:
            lines.append("  H1 bars:")
            for b in sorted(h1_bars, key=lambda x: x.get("birth", 0)):
                death_str = "end" if b["death"] is None else f"{b['death']:.4f}"
                dur = b["duration"]
                frac = dur / sweep_range if sweep_range > 0 else 0
                tag = _persistence_tag(frac)
                lines.append(
                    f"    H1={b['value']}: [{b['birth']:.4f}, {death_str}] "
                    f"duration={dur:.4f} ({frac*100:.0f}% of range) — {tag}"
                )
            lines.append("")

        # W1 bars (per edge)
        edge_labels = {
            "w1_u1_u2": "U1→U2",
            "w1_u2_u3": "U2→U3",
            "w1_u3_u4": "U3→U4",
        }
        w1_bars = [b for b in c_bars if b["dimension"].startswith("w1_")]
        if w1_bars:
            lines.append("  W1 bars:")
            for b in sorted(w1_bars, key=lambda x: (x["dimension"], x.get("birth", 0))):
                edge = edge_labels.get(b["dimension"], b["dimension"])
                death_str = "end" if b["death"] is None else f"{b['death']:.4f}"
                dur = b["duration"]
                frac = dur / sweep_range if sweep_range > 0 else 0
                tag = _persistence_tag(frac)
                lines.append(
                    f"    {edge}: [{b['birth']:.4f}, {death_str}] "
                    f"duration={dur:.4f} ({frac*100:.0f}% of range) — {tag}"
                )
            lines.append("")

        # Type bars
        type_bars = [b for b in c_bars if b["dimension"].startswith("type:")]
        if type_bars:
            lines.append("  Type bars:")
            for b in sorted(type_bars, key=lambda x: (x["dimension"], x.get("birth", 0))):
                ctx = b["dimension"].split(":", 1)[1] if ":" in b["dimension"] else "?"
                death_str = "end" if b["death"] is None else f"{b['death']:.4f}"
                dur = b["duration"]
                frac = dur / sweep_range if sweep_range > 0 else 0
                lines.append(
                    f"    {ctx}: {b['value']} [{b['birth']:.4f}, {death_str}] "
                    f"duration={dur:.4f} ({frac*100:.0f}%)"
                )
            lines.append("")

        # H1/W1 divergence check
        h1_total = sum(b["duration"] for b in h1_bars)
        w1_u3u4 = [b for b in w1_bars if b["dimension"] == "w1_u3_u4"]
        w1_total = sum(b["duration"] for b in w1_u3u4)
        if sweep_range > 0:
            h1_frac = h1_total / sweep_range
            w1_frac = w1_total / sweep_range
            if abs(h1_frac - w1_frac) > 0.3:
                lines.append(
                    f"  ** H1/W1 DIVERGENCE: H1 {h1_frac*100:.0f}% vs "
                    f"W1(U3→U4) {w1_frac*100:.0f}% — "
                    f"topological and transport persistence disagree **"
                )
                lines.append("")

        # Diagnosis from snare_chi_floor special section
        if param_name == "snare_chi_floor":
            diag = param_result.get("snare_chi_floor_diagnosis", [])
            for d in diag:
                if d.get("constraint", "").lower() == key:
                    lines.append(f"  Diagnosis: {d.get('interpretation', '')}")
                    if d.get("h1_drops_to_zero"):
                        lines.append(
                            "  WARNING: H1 reaches 0 — fracture disappears "
                            "in part of sweep range"
                        )
                    break

    if not has_any:
        lines.append("  No persistence bars for this constraint across swept parameters.")

    return "\n".join(lines)


def _persistence_tag(frac):
    """Return a human-readable persistence tag."""
    if frac >= 0.80:
        return "FULL RANGE"
    elif frac >= 0.20:
        return "ROBUST"
    elif frac >= 0.05:
        return "MODERATE"
    else:
        return "FRAGILE"


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
    lines.append("  | Trigger Class | Confidence | Anomaly | Category | Interpretation |")
    lines.append("  |---|---|---|---|---|")
    for t in sorted(triggers, key=lambda x: x.get("confidence", 0), reverse=True):
        tc = t.get("trigger_class", "—")
        conf = t.get("confidence", 0)
        anom = t.get("anomaly_type", "—")
        cat = t.get("category", "—")
        gloss = _TRIGGER_GLOSSES.get(tc, "—")
        lines.append(f"  | {tc} | {conf:.2f} | {anom} | {cat} | {gloss} |")

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


# --- Theorem Instantiation ---

_THEOREM_TEXTS = {
    "T1": (
        "T1 (Cover Story): At least one observer sees this constraint as "
        "benign (rope/tangled_rope) while another sees it as extractive "
        "(snare). The constraint functions as a cover story — its apparent "
        "type depends on observer position."
    ),
    "T2": (
        "T2 (Discrete Blocs): H^1 >= 3 means observer classifications "
        "cluster into discrete blocs that cannot be smoothly deformed into "
        "each other. The constraint lives in a topologically non-trivial "
        "region of the classification sheaf."
    ),
    "T3": (
        "T3 (Spectral Dominance): The institutional observer's classification "
        "diverges from the majority of other observers. The power-scaled "
        "extraction metric (chi) produces a qualitatively different result "
        "at the institutional index — the spectrum is dominated by a single "
        "observer position."
    ),
    "T4_positive": (
        "T4 (Oracle Gap): A classical oracle (single-position MaxEnt) is "
        "confident, but cross-position comparison (H^1 > 0) reveals "
        "structure invisible from any single vantage point. Looking carefully "
        "from one position misses what comparing across positions reveals."
    ),
    "T5_compliant": (
        "T5 (Functor Axiom — satisfied): Classification across index "
        "dimensions factors through a single Boltzmann distribution. "
        "The constraint's type assignments are thermodynamically consistent "
        "— no hidden coupling between observer positions."
    ),
    "T5_non_compliant": (
        "T5 (Functor Axiom — violated): Classification does NOT factor "
        "through a single Boltzmann distribution. Observer positions are "
        "thermodynamically coupled — the constraint's type depends on which "
        "observers you condition on, not just their individual measurements."
    ),
    "T6_hub1": (
        "T6 (Hub Correspondence — Hub 1): H^1 = 3 maps to Hub 1 "
        "(power-scaled extraction). A single observer's chi-value diverges "
        "from the other three, producing a 3+1 classification split."
    ),
    "T6_hub2": (
        "T6 (Hub Correspondence — Hub 2): H^1 = 4 maps to Hub 2 "
        "(effective immutability). Two pairs of observers disagree, producing "
        "a 2+2 classification split driven by the immutability axis."
    ),
    "T6_both": (
        "T6 (Hub Correspondence — Both Hubs): H^1 >= 5 means both Hub 1 "
        "(power-scaled extraction) and Hub 2 (effective immutability) "
        "contribute to classification fracture. Three or more distinct types "
        "appear across observers."
    ),
}


def build_theorem_instantiation(constraint_id, pipeline_data, orbit_data):
    """Theorem instantiation section — maps diagnostics to formal theorems."""
    lines = ["", "--- THEOREM INSTANTIATION ---", ""]

    if pipeline_data is None:
        lines.append("  [enriched_pipeline.json not available]")
        return "\n".join(lines)

    entry = find_constraint_entry(pipeline_data, constraint_id)
    if entry is None:
        lines.append("  Not yet in batch — run full pipeline to include.")
        return "\n".join(lines)

    active_theorems = []

    perspectives = entry.get("perspectives", {})
    h1 = entry.get("h1_band")
    triggers = entry.get("abductive_triggers", [])
    coupling = entry.get("coupling", {})
    boltzmann = coupling.get("boltzmann") if isinstance(coupling, dict) else None

    # T1 (Cover Story): any observer sees rope/tangled_rope AND another sees snare
    if perspectives:
        types_seen = set(perspectives.values())
        benign = types_seen & {"rope", "tangled_rope"}
        extractive = types_seen & {"snare"}
        if benign and extractive:
            active_theorems.append(_THEOREM_TEXTS["T1"])

    # T2 (Discrete Blocs): h1_band in {3, 4, 5, 6}
    if h1 is not None and h1 >= 3:
        active_theorems.append(_THEOREM_TEXTS["T2"])

    # T3 (Spectral Dominance): institutional != majority of other 3
    if perspectives:
        inst_type = perspectives.get("institutional")
        others = [perspectives.get(o) for o in ["powerless", "moderate", "analytical"]
                  if perspectives.get(o)]
        if inst_type and len(others) >= 3:
            other_counts = Counter(others)
            majority_type = other_counts.most_common(1)[0][0]
            if inst_type != majority_type:
                active_theorems.append(_THEOREM_TEXTS["T3"])

    # T4 (Oracle Gap): triggers contain maxent_divergence or classical_oracle_failure
    trigger_classes = {t.get("trigger_class") for t in triggers}
    if trigger_classes & {"maxent_divergence", "classical_oracle_failure"}:
        active_theorems.append(_THEOREM_TEXTS["T4_positive"])

    # T5 (Functor Axiom): boltzmann compliant or non_compliant
    if boltzmann == "compliant":
        active_theorems.append(_THEOREM_TEXTS["T5_compliant"])
    elif boltzmann == "non_compliant":
        active_theorems.append(_THEOREM_TEXTS["T5_non_compliant"])

    # T6 (Hub Correspondence): H^1=3→Hub1, H^1=4→Hub2, H^1>=5→Both
    if h1 == 3:
        active_theorems.append(_THEOREM_TEXTS["T6_hub1"])
    elif h1 == 4:
        active_theorems.append(_THEOREM_TEXTS["T6_hub2"])
    elif h1 is not None and h1 >= 5:
        active_theorems.append(_THEOREM_TEXTS["T6_both"])

    if not active_theorems:
        lines.append("  No theorems active at this constraint.")
        return "\n".join(lines)

    for i, text in enumerate(active_theorems):
        lines.append(f"  {text}")
        if i < len(active_theorems) - 1:
            lines.append("")

    lines.append("")
    lines.append(f"  **{len(active_theorems)} of 6 theorems active.**")

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

def generate_report(constraint_id, data, iteration_round=None):
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
    # Indexed-mode MaxEnt now embedded in build_maxent_section (Gap analysis Change 5 — resolved)
    l2_wasserstein = build_wasserstein_section(constraint_id, data["pipeline"])
    l2_cohomology = build_cohomology_section(constraint_id, data["pipeline"])
    l2_game_theory = build_game_theory_section(constraint_id, data["pipeline"])
    l2_persistence = build_persistence_section(constraint_id, data["persistence"])
    l2_abductive = build_abductive_section(constraint_id, data["pipeline"])
    l2_verdict = build_level2_verdict_body(constraint_id, data["pipeline"])
    l2_theorems = build_theorem_instantiation(
        constraint_id, data["pipeline"], data["orbit"]
    )
    # Level 1: FPN contamination topology (Gap analysis Change 4 — resolved)
    l1_contamination = build_contamination_network(constraint_id, data["pipeline"])

    # Level 3: Corpus Positioning
    l3_distribution = build_level3_distribution(
        constraint_id, data["pipeline"], data["orbit"], data["omega"]
    )
    l3_structural = build_structural_section(
        constraint_id, data["corpus"], data["pattern"], data["covering"]
    )

    # Post-synthesis (only if T12 flags exist)
    post = build_post_synthesis(constraint_id, data["pipeline"])

    # Axiom 2: Chi/Epsilon decomposition
    try:
        from chi_variance_decomposition import build_axiom2_section
        l2_axiom2 = build_axiom2_section(constraint_id, data["pipeline"])
    except ImportError:
        l2_axiom2 = ""

    sections = [
        banner,
        build_level_header(1, "SELF-CONSISTENCY"),
        l1_identity, l1_contamination, l1_orbit, l1_omega,
        build_level_header(2, "DIAGNOSTIC CONVERGENCE"),
        l2_convergence, l2_maxent, l2_wasserstein, l2_cohomology, l2_game_theory, l2_persistence, l2_abductive, l2_axiom2, l2_verdict, l2_theorems,
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

    # --- Emit JSON sidecar ---
    entry = find_constraint_entry(data["pipeline"], constraint_id)
    sidecar = build_sidecar_data(constraint_id, entry, prolog_output, iteration_round)

    # Validate (warn but don't block)
    try:
        from shared.schemas import validate_report_sidecar
        validation_errors = validate_report_sidecar(sidecar)
        if validation_errors:
            for err in validation_errors:
                logger.warning("Sidecar validation: %s", err)
    except ImportError:
        pass

    sidecar_path = REPORTS_DIR / f"{constraint_id}_report.json"
    try:
        with open(sidecar_path, "w", encoding="utf-8") as f:
            json.dump(sidecar, f, indent=2)
        print(f"Sidecar written to: {sidecar_path}")
    except OSError as e:
        logger.warning("Failed to write sidecar: %s", e)


# --- Main ---

def main():
    parser = argparse.ArgumentParser(
        description="Enhanced Constraint Report — Three-Level Feedback Model",
    )
    parser.add_argument(
        "constraint_ids", nargs="*",
        help="Constraint IDs to generate reports for (auto-detects recent if omitted)",
    )
    parser.add_argument(
        "--iteration-round", type=int, default=None,
        help="Iteration round number (passed by orchestrators during iteration)",
    )
    args = parser.parse_args()

    # Determine which constraints to process
    constraint_ids = args.constraint_ids
    if not constraint_ids:
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
        "persistence": load_json(SCRIPT_DIR / "persistence_results.json", "persistence_results.json"),
        "maxent":   load_text(OUTPUTS_DIR / "maxent_report.md", "maxent_report.md"),
        "pattern":  load_text(OUTPUTS_DIR / "pattern_mining.md", "pattern_mining.md"),
        "covering": load_text(OUTPUTS_DIR / "covering_analysis.md", "covering_analysis.md"),
    }

    # Generate reports
    for constraint_id in constraint_ids:
        generate_report(constraint_id, data, iteration_round=args.iteration_round)

    if len(constraint_ids) > 1:
        print(f"\nDone: {len(constraint_ids)} reports generated.")


if __name__ == "__main__":
    main()
