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

    # CS pattern distribution (when present)
    cs_dist = val.get("cs_pattern_distribution") if val else None
    if cs_dist:
        cs_total = cs_dist.get("total_with_cs_fields", 0)
        if cs_total:
            pat_counts = cs_dist.get("pattern_counts", {})
            cs_parts = [f"{v} {k}" for k, v in sorted(pat_counts.items()) if k != "no_pattern_match"]
            verdicts_fired = sum(cs_dist.get("cs_verdicts_fired", {}).values())
            lines.append(
                f"  CS patterns: {cs_total} classified | {', '.join(cs_parts)}"
                + (f" | {verdicts_fired} verdicts fired" if verdicts_fired else "")
            )
    # CS grounding mismatch count (corpus sanity signal — non-zero means at least one
    # asserted authority grounding contradicts the computed structural signature)
    gm_count = val.get("cs_grounding_mismatch_count") if val else None
    if gm_count is not None:
        label = "grounding-metric conflicts" if gm_count != 1 else "grounding-metric conflict"
        lines.append(f"  CS grounding mismatches: {gm_count} {label}")

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


# --- Level 1 (cont): TEMPORAL TRAJECTORY ---

def build_drift_trajectory_section(constraint_id, pipeline_data):
    """L1: Temporal shape — only fires for non-trivial series (48/190 constraints).

    Triggers (any one sufficient; corrected dominant-direction logic):
      A: reversal >= 0.04 in any metric (magnitude-weighted dominant direction)
      B: cross-metric divergence — both metrics move >= 0.06 in opposite directions
      C: plateau/ceiling — monotone, all accelerations negative, last rate < 20% of first

    Silent for the 142/190 constraints with fully monotone, non-divergent series.
    Source data: drift_trajectory field in pipeline JSON (raw measurement/5 series).
    """
    if pipeline_data is None:
        return ""
    entry = find_constraint_entry(pipeline_data, constraint_id)
    if entry is None or not entry.get("drift_trajectory"):
        return ""

    dt = entry["drift_trajectory"]

    def get_vals(m):   return [pt["v"] for pt in dt[m]["series"]]
    def get_ts(m):     return [pt["t"] for pt in dt[m]["series"]]
    def get_rates(m):  return [r["rate"]  for r in dt[m]["per_interval_rate"]]
    def get_accels(m): return [a["acc"]   for a in dt[m]["per_interval_acceleration"]]

    def reversal_mag(vals):
        if len(vals) < 3:
            return 0.0
        deltas = [vals[i + 1] - vals[i] for i in range(len(vals) - 1)]
        pos_total = sum(d for d in deltas if d > 0)
        neg_total = abs(sum(d for d in deltas if d < 0))
        if pos_total == 0 or neg_total == 0:
            return 0.0
        if pos_total >= neg_total:
            return max(abs(d) for d in deltas if d < 0)
        else:
            return max(abs(d) for d in deltas if d > 0)

    # --- Trigger A: non-monotone with magnitude floor ---
    trigger_a_metrics = [m for m in dt if reversal_mag(get_vals(m)) >= 0.04]

    # --- Trigger B: cross-metric divergence ---
    trigger_b = False
    be_d = tr_d = 0.0
    if "base_extractiveness" in dt and "theater_ratio" in dt:
        be_vals = get_vals("base_extractiveness")
        tr_vals = get_vals("theater_ratio")
        be_d = be_vals[-1] - be_vals[0]
        tr_d = tr_vals[-1] - tr_vals[0]
        if be_d * tr_d < 0 and abs(be_d) >= 0.06 and abs(tr_d) >= 0.06:
            trigger_b = True

    # --- Trigger C: plateau/ceiling (monotone, sustained deceleration) ---
    trigger_c_metrics = []
    for m in dt:
        vals   = get_vals(m)
        rates  = get_rates(m)
        accels = get_accels(m)
        if len(rates) < 2:
            continue
        if reversal_mag(vals) >= 0.025:
            continue  # non-monotone: covered by A
        r_first, r_last = rates[0], rates[-1]
        if r_first <= 0.001:
            continue
        if not accels or not all(a <= 0 for a in accels):
            continue
        if r_last < 0.20 * r_first and (max(vals) - min(vals)) >= 0.05:
            trigger_c_metrics.append(m)

    if not trigger_a_metrics and not trigger_b and not trigger_c_metrics:
        return ""

    lines = ["", "--- TEMPORAL TRAJECTORY ---", ""]

    # Trigger A: non-monotone shapes
    for m in trigger_a_metrics:
        vals = get_vals(m)
        ts   = get_ts(m)
        rev  = reversal_mag(vals)
        peak_idx   = max(range(len(vals)), key=lambda i: vals[i])
        trough_idx = min(range(len(vals)), key=lambda i: vals[i])
        start_v, end_v = vals[0], vals[-1]

        if 0 < peak_idx < len(vals) - 1 and vals[peak_idx] > end_v:
            direction = "recovers to" if end_v > start_v else "falls to"
            lines.append(
                f"    {m}: peaks at T={ts[peak_idx]} ({vals[peak_idx]:.2f})"
                f" then {direction} {end_v:.2f} by T={ts[-1]}"
            )
        elif 0 < trough_idx < len(vals) - 1 and vals[trough_idx] < end_v:
            lines.append(
                f"    {m}: troughs at T={ts[trough_idx]} ({vals[trough_idx]:.2f})"
                f" then recovers to {end_v:.2f} by T={ts[-1]}"
            )
        else:
            net = end_v - start_v
            lines.append(
                f"    {m}: non-monotone (net {'+' if net >= 0 else ''}{net:.2f},"
                f" reversal {rev:.2f}) over T={ts[0]}–{ts[-1]}"
            )

    # Trigger B: cross-metric divergence
    if trigger_b:
        be_vals = get_vals("base_extractiveness")
        tr_vals = get_vals("theater_ratio")
        be_start, be_end = be_vals[0], be_vals[-1]
        tr_start, tr_end = tr_vals[0], tr_vals[-1]
        phrase = (
            "theater withdraws as extraction escalates" if be_d > 0
            else "theater substitutes as extraction decays"
        )
        lines.append(
            f"    Cross-metric: epsilon {be_start:.2f}→{be_end:.2f}"
            f" ({'+' if be_d > 0 else ''}{be_d:.2f}),"
            f" theater {tr_start:.2f}→{tr_end:.2f}"
            f" ({'+' if tr_d > 0 else ''}{tr_d:.2f})"
            f" — {phrase}"
        )

    # Trigger C: plateau/ceiling (skip metrics already described by A)
    for m in trigger_c_metrics:
        if m in trigger_a_metrics:
            continue
        vals  = get_vals(m)
        ts    = get_ts(m)
        rates = get_rates(m)
        lines.append(
            f"    {m}: ceiling approach — rate {rates[0]:.4f}→{rates[-1]:.4f}"
            f" over T={ts[0]}–{ts[-1]}, flattening near {vals[-1]:.2f}"
        )

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


# --- Section E5: PARAMETRIC STABILITY BAND ---

# Governing params confirmed by empirical witness (coverage>0, fold_survival<1.0 in ≥1 kernel
# context). Keyed by kernel_id. Only kernels with at least one confirmed governing param are
# listed — all others render "not yet witnessed."
# Witness record (2026-05-29):
#   snare_epsilon_floor × end_of_life_decision_authority: boundary at +8.7%, 39 flips.
#   tangled_rope_chi_floor: rejected (signature-locked across all tested kernels).
_WITNESSED_PARAMS: dict[str, list[tuple[str, list[float]]]] = {
    # witness_backlog.py ±10% batch 2026-05-29 (outputs/witness_backlog_results.json)
    # Convention per entry: (param_name, sweep_values_bracketing_boundary)
    # Only params with coverage > 0.03 OR fold_survival < 0.97 on any kernel are listed.
    # The sigmoid family (lower/midpoint/steepness/upper) affects many kernels through D_eff.

    "end_of_life_decision_authority": [
        # snare_epsilon_floor: upward only — lowering below rope_epsilon_ceiling=0.45 is a
        # config violation (relationship: rope_epsilon_ceiling < snare_epsilon_floor).
        # Boundary confirmed at +8.7% (0.46→0.50): 39 flips. No coverage at +4.3% (0.48).
        ("snare_epsilon_floor", [0.46, 0.48, 0.50, 0.52]),
        # snare_suppression_floor: boundary at +10% (0.6→0.66).
        ("snare_suppression_floor", [0.54, 0.60, 0.66]),
        # critical_mass_threshold: boundary at −1 (3→2); 87 flips, cov=0.186.
        ("critical_mass_threshold", [2, 3, 4]),
    ],
    "ai_risk_governance_priority": [
        # snare_chi_floor: boundary in BOTH directions. Coverage 0.173. (↓0.594, ↑0.726)
        ("snare_chi_floor", [0.594, 0.627, 0.66, 0.693, 0.726]),
        # sigmoid_midpoint: coverage 0.094. (↓0.45 → flips)
        ("sigmoid_midpoint", [0.45, 0.50, 0.55]),
        # sigmoid_steepness: coverage 0.053. (↓5.4 → flips)
        ("sigmoid_steepness", [5.4, 6.0, 6.6]),
        # sigmoid_upper: coverage 0.053. (↓1.35 → flips)
        ("sigmoid_upper", [1.35, 1.5, 1.65]),
        # sigmoid_lower: coverage 0.028.
        ("sigmoid_lower", [-0.22, -0.20, -0.18]),
        # scope_modifier_national: coverage 0.034.
        ("scope_modifier_national", [0.9, 1.0, 1.1]),
    ],
    "equal_protection_clause": [
        # snare_chi_floor: both directions. Coverage 0.144.
        ("snare_chi_floor", [0.594, 0.627, 0.66, 0.693, 0.726]),
        # sigmoid_midpoint: coverage 0.094 (shared with ai_risk_governance_priority).
        ("sigmoid_midpoint", [0.45, 0.50, 0.55]),
        # sigmoid_steepness: coverage 0.053.
        ("sigmoid_steepness", [5.4, 6.0, 6.6]),
        # sigmoid_upper: coverage 0.053.
        ("sigmoid_upper", [1.35, 1.5, 1.65]),
        # sigmoid_lower: coverage 0.028.
        ("sigmoid_lower", [-0.22, -0.20, -0.18]),
        # boltzmann_coupling_threshold — Surface-2 lock lever (witness:
        # outputs/surface2_lock_sweep_results.json; OQ-30 / OQ-37). Original 0.25; the FNL
        # lock breaks corpus-wide at threshold in (~0.83, 1.0] (≈+268%). Post-OQ-37
        # override-removal, this kernel's only load-bearing reading (diversity_reading, a
        # band-gap metric=unknown) SURFACES unknown when FNL breaks rather than flipping —
        # coverage>0, held. Co-lever coordination_type_offset (additive, same boundary) is
        # PER-CONSTRAINT (boltzmann_compliance.pl:388), not a flat config param, so it is
        # documented here + in ISSUES OQ-30 rather than perturb-swept.
        ("boltzmann_coupling_threshold", [0.25, 0.5, 0.75, 0.85, 0.95]),
    ],
    "honor_settlement_legitimacy": [
        # snare_chi_floor: coverage 0.538 — highest coverage in corpus for this param.
        ("snare_chi_floor", [0.594, 0.627, 0.66, 0.693, 0.726]),
        # sigmoid_midpoint: coverage wide.
        ("sigmoid_midpoint", [0.45, 0.50, 0.55]),
        # sigmoid_steepness: coverage 0.053.
        ("sigmoid_steepness", [5.4, 6.0, 6.6]),
        # sigmoid_upper: coverage.
        ("sigmoid_upper", [1.35, 1.5, 1.65]),
        # prh_powerless___true: boundary -10% (0.85→0.765). Coverage 0.064.
        ("prh_powerless___true", [0.765, 0.85, 0.935]),
        # critical_mass_threshold: boundary −1 (3→2) → tangled_rope/snare→naturalized, cov=0.250.
        ("critical_mass_threshold", [2, 3, 4]),
    ],
    "jurisprudential_method_kernel": [
        # snare_chi_floor: coverage 0.090 (both directions).
        ("snare_chi_floor", [0.594, 0.627, 0.66, 0.693, 0.726]),
        # sigmoid_midpoint: coverage.
        ("sigmoid_midpoint", [0.45, 0.50, 0.55]),
        # sigmoid_steepness + upper: coverage 0.053.
        ("sigmoid_steepness", [5.4, 6.0, 6.6]),
        ("sigmoid_upper", [1.35, 1.5, 1.65]),
    ],
    "latin_correctness": [
        # tangled_rope_chi_ceil: boundary -10% (0.9→0.81). Coverage 0.167.
        ("tangled_rope_chi_ceil", [0.81, 0.855, 0.9, 0.945, 0.99]),
        # piton_theater_floor: boundary -10% (0.7→0.63). Coverage 0.083.
        ("piton_theater_floor", [0.63, 0.70, 0.77]),
        # snare_suppression_floor: boundary -10% (0.6→0.54). Coverage 0.083.
        ("snare_suppression_floor", [0.54, 0.60, 0.66]),
        # sigmoid_upper: both directions.
        ("sigmoid_upper", [1.35, 1.5, 1.65]),
        # fcr_override_enabled: disable (1→0) → tangled_rope→scaffold, cov=0.333, 156 flips.
        # NOTE: some contexts flip to 'unknown' — fcr_override is load-bearing for classification path.
        ("fcr_override_enabled", [0, 1]),
    ],
    "legitimacy_of_imposed_practice": [
        # sigmoid_midpoint + steepness + upper + lower: broad sensitivity.
        ("sigmoid_midpoint", [0.45, 0.50, 0.55]),
        ("sigmoid_steepness", [5.4, 6.0, 6.6]),
        ("sigmoid_upper", [1.35, 1.5, 1.65]),
        ("sigmoid_lower", [-0.22, -0.20, -0.18]),
    ],
    "nuclear_impossibility_kernel": [
        # snare_chi_floor: coverage 0.286 (both directions).
        ("snare_chi_floor", [0.594, 0.627, 0.66, 0.693, 0.726]),
        # sigmoid_midpoint.
        ("sigmoid_midpoint", [0.45, 0.50, 0.55]),
        ("sigmoid_steepness", [5.4, 6.0, 6.6]),
        ("sigmoid_upper", [1.35, 1.5, 1.65]),
    ],
    "second_amendment_text": [
        # snare_chi_floor: coverage 0.385 (both directions).
        ("snare_chi_floor", [0.594, 0.627, 0.66, 0.693, 0.726]),
        # sigmoid params.
        ("sigmoid_midpoint", [0.45, 0.50, 0.55]),
        ("sigmoid_steepness", [5.4, 6.0, 6.6]),
        ("sigmoid_upper", [1.35, 1.5, 1.65]),
        ("sigmoid_lower", [-0.22, -0.20, -0.18]),
    ],
    "sovereign_legitimacy": [
        # rope_chi_ceiling: boundary in BOTH directions. Coverage 0.197 — highest in batch.
        # Lowering ceiling pushes more constraints to tangled_rope; raising narrows rope.
        ("rope_chi_ceiling", [0.315, 0.35, 0.385]),
        # snare_chi_floor: coverage 0.120.
        ("snare_chi_floor", [0.594, 0.627, 0.66, 0.693, 0.726]),
        # sigmoid params.
        ("sigmoid_midpoint", [0.45, 0.50, 0.55]),
        ("sigmoid_steepness", [5.4, 6.0, 6.6]),
        ("sigmoid_upper", [1.35, 1.5, 1.65]),
        # boltzmann_coupling_threshold — Surface-2 lock lever (witness:
        # outputs/surface2_lock_sweep_results.json). FNL lock breaks at ~0.83–1.0 (≈+268%);
        # post-OQ-37 the holdout republican_reading (band-gap metric=unknown) surfaces
        # unknown when FNL breaks. See equal_protection_clause entry re: the per-constraint
        # coordination_type_offset co-lever.
        ("boltzmann_coupling_threshold", [0.25, 0.5, 0.75, 0.85, 0.95]),
    ],
    "vaccine_mandate_balance": [
        # snare_chi_floor: coverage 0.126 (both directions). Maximum flips=32.
        ("snare_chi_floor", [0.594, 0.627, 0.66, 0.693, 0.726]),
        # sigmoid params.
        ("sigmoid_midpoint", [0.45, 0.50, 0.55]),
        ("sigmoid_steepness", [5.4, 6.0, 6.6]),
        ("sigmoid_upper", [1.35, 1.5, 1.65]),
        ("sigmoid_lower", [-0.22, -0.20, -0.18]),
    ],
    "woman_female_category": [
        # snare_chi_floor: coverage 0.263 (both directions).
        ("snare_chi_floor", [0.594, 0.627, 0.66, 0.693, 0.726]),
        # sigmoid params.
        ("sigmoid_midpoint", [0.45, 0.50, 0.55]),
        ("sigmoid_steepness", [5.4, 6.0, 6.6]),
        ("sigmoid_upper", [1.35, 1.5, 1.65]),
        ("sigmoid_lower", [-0.22, -0.20, -0.18]),
    ],
    # Kernels where sigmoid_midpoint alone produced minor flips (coverage < 0.03) —
    # included for completeness but these are low-signal entries.
    "animal_moral_status": [
        ("sigmoid_midpoint", [0.45, 0.50, 0.55]),
    ],
    "competence_exercise_validity": [
        ("sigmoid_midpoint", [0.45, 0.50, 0.55]),
    ],
    "kodashim_corpus": [
        ("sigmoid_midpoint", [0.45, 0.50, 0.55]),
        # boltzmann_min_classifications: +1 (3→4) → rope→scaffold, cov=0.333, 156 flips.
        ("boltzmann_min_classifications", [2, 3, 4]),
    ],
    "market_as_natural_default": [
        ("sigmoid_midpoint", [0.45, 0.50, 0.55]),
    ],
    # Kernels newly discovered in integer-step batch 2026-05-29:
    "reformation_event_boundary": [
        # fcr_override_enabled: disable → tangled_rope→scaffold, cov=0.500, 156 flips.
        ("fcr_override_enabled", [0, 1]),
    ],
    "statute_of_anne_ip_foundation": [
        # fcr_override_enabled: disable → tangled_rope→scaffold, cov=0.500, 156 flips.
        ("fcr_override_enabled", [0, 1]),
    ],
}


def _get_kernel_id_for_constraint(constraint_id: str) -> str | None:
    """Return kernel_id from cs_kernel_id fact in testset, or None."""
    import re
    pl = PROLOG_DIR / "testsets" / f"{constraint_id}.pl"
    if not pl.exists():
        return None
    m = re.search(r"cs_kernel_id\(\s*\w+\s*,\s*(\w+)\s*\)", pl.read_text(encoding="utf-8"))
    return m.group(1) if m else None


def _run_stability_band(kernel_id: str) -> dict:
    """Run perturb for witnessed governing params of kernel_id. Returns raw data dict."""
    try:
        from sweeps.perturb import perturb as _perturb
    except ImportError:
        return {"error": "sweeps.perturb not importable"}

    params_config = _WITNESSED_PARAMS.get(kernel_id, [])
    if not params_config:
        return {"not_witnessed": True, "kernel_id": kernel_id}

    param_results = []
    baseline_hash = None

    for param_name, sweep_values in params_config:
        try:
            result = _perturb(param_name, sweep_values, kernels=[kernel_id])
        except Exception as exc:
            param_results.append({"param": param_name, "error": str(exc)})
            continue

        baseline_hash = result.get("baseline_hash", "?")
        original = result.get("original", 0)
        kr_by_val = result.get("results", {})

        up_entries = {}  # abs_pct → entry dict (upward displacements)
        dn_entries = {}  # abs_pct → entry dict (downward displacements)

        for val, kr in sorted(kr_by_val.items()):
            if isinstance(kr, dict) and "error" not in kr:
                k_data = kr.get(kernel_id, {})
                if k_data and isinstance(k_data, dict):
                    fold = k_data.get("fold_survival", 1.0)
                    cov = k_data.get("coverage", 0.0)
                    touched = k_data.get("touched", 0)
                    flipped = k_data.get("flipped", 0)
                    pct = round((val - original) / original * 100, 1) if original else 0.0
                    entry = {
                        "value": val,
                        "pct": pct,
                        "fold_survival": fold,
                        "coverage": cov,
                        "touched": touched,
                        "flipped": flipped,
                    }
                    if pct > 0:
                        up_entries[pct] = entry
                    elif pct < 0:
                        dn_entries[abs(pct)] = entry

        # Find boundary (first displacement with coverage>0 AND fold_survival<1.0)
        # and floor (largest displacement with coverage>0 AND fold_survival==1.0, before boundary)
        def _parse_direction(entries):
            boundary = None
            floor_e = None
            for abs_pct in sorted(entries):
                e = entries[abs_pct]
                if e["coverage"] > 0:
                    if e["fold_survival"] < 1.0 and boundary is None:
                        boundary = e
                    elif e["fold_survival"] >= 1.0 and boundary is None:
                        floor_e = e
            return boundary, floor_e

        up_boundary, up_floor = _parse_direction(up_entries)
        dn_boundary, dn_floor = _parse_direction(dn_entries)

        param_results.append({
            "param": param_name,
            "original": original,
            "up_boundary": up_boundary,
            "up_floor": up_floor,
            "dn_boundary": dn_boundary,
            "dn_floor": dn_floor,
        })

    return {
        "kernel_id": kernel_id,
        "baseline_hash": baseline_hash,
        "params": param_results,
    }


_FISHER_RESULTS_PATH = PROJECT_ROOT / "outputs" / "epsilon_sensitivity_results.json"
_fisher_cache: dict | None = None


def _load_fisher_results() -> dict:
    """Load epsilon_sensitivity_results.json keyed by constraint id; cache on first call."""
    global _fisher_cache
    if _fisher_cache is not None:
        return _fisher_cache
    if not _FISHER_RESULTS_PATH.exists():
        _fisher_cache = {}
        return _fisher_cache
    import json as _json
    data = _json.loads(_FISHER_RESULTS_PATH.read_text())
    _fisher_cache = {e["id"]: e for e in data.get("per_constraint", [])}
    return _fisher_cache


def _fisher_probe_lines(constraint_id: str) -> list[str]:
    """Fisher ε-sensitivity sub-section (fires on every E5 path)."""
    fisher_data = _load_fisher_results()
    if constraint_id in fisher_data:
        fish = fisher_data[constraint_id].get("fisher_analytical_raw")
        fish_str = f"{fish:.3f}" if fish is not None else "n/a"
        return [
            "",
            f"  Fisher ε-sensitivity (MaxEnt): {fish_str}"
            "  [r=-0.29 vs confidence_margin — non-redundant with type classification]",
        ]
    return [
        "",
        "  Fisher ε-sensitivity (MaxEnt): not computed",
        "  (run python3 python/sweeps/epsilon_sensitivity.py to compute)",
    ]


def build_stability_band(constraint_id: str, stability_data: dict | None) -> str:
    """Render the parametric stability band section (E5)."""
    lines = ["", "--- PARAMETRIC STABILITY BAND ---", ""]

    if stability_data is None:
        lines.append("  [stability not computed]")
        lines.extend(_fisher_probe_lines(constraint_id))
        return "\n".join(lines)

    if stability_data.get("no_kernel"):
        lines.append("  stability not assessed — no kernel linkage (cs_kernel_id absent from testset)")
        lines.extend(_fisher_probe_lines(constraint_id))
        return "\n".join(lines)

    if stability_data.get("not_witnessed"):
        kid = stability_data.get("kernel_id", "?")
        lines.append(f"  stability not assessed — kernel '{kid}' has no confirmed governing params yet")
        lines.append("  (witness required: coverage>0 AND fold_survival<1.0 in ≥1 context)")
        lines.extend(_fisher_probe_lines(constraint_id))
        return "\n".join(lines)

    if stability_data.get("error"):
        lines.append(f"  [error: {stability_data['error']}]")
        lines.extend(_fisher_probe_lines(constraint_id))
        return "\n".join(lines)

    kid = stability_data.get("kernel_id", "?")
    bh = stability_data.get("baseline_hash") or "?"
    lines.append(f"  Kernel: {kid}  (baseline: {bh[:12] if bh else '?'})")
    lines.append("")

    for pr in stability_data.get("params", []):
        param = pr["param"]
        if "error" in pr:
            lines.append(f"  {param}: error — {pr['error']}")
            continue

        original = pr.get("original", "?")
        lines.append(f"  {param} (baseline={original}):")

        for arrow, boundary, floor_e in [
            ("↑", pr.get("up_boundary"), pr.get("up_floor")),
            ("↓", pr.get("dn_boundary"), pr.get("dn_floor")),
        ]:
            sign = "+" if arrow == "↑" else "-"
            if boundary:
                pct = abs(boundary["pct"])
                flipped = boundary["flipped"]
                touched = boundary["touched"]
                lines.append(
                    f"    {arrow} boundary at {sign}{pct}%"
                    f" → {flipped} contexts flip (touched={touched})"
                )
                if floor_e:
                    fp = abs(floor_e["pct"])
                    lines.append(
                        f"       stable ≥{sign}{fp}% (no flip at {floor_e['value']})"
                    )
            elif floor_e:
                pct = abs(floor_e["pct"])
                touched = floor_e.get("touched", 0)
                suffix = f"  touched={touched}" if touched else ""
                lines.append(
                    f"    {arrow} stable ≥{sign}{pct}% (max tested, no boundary in range){suffix}"
                )

    lines.extend(_fisher_probe_lines(constraint_id))
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


# --- CS Pattern Detection ---

_CS_PATTERN_PROSE = {
    "marked_revision": (
        "Marked Revision: The kernel is precisely specified; authority is voluntary "
        "and grounded in expertise. Drift is formalized as a proposal-check-absorb "
        "cycle — acknowledgment is marked and legible rather than silently absorbed. "
        "Mathematics works this way; healthy long-term relationships and programming "
        "languages with formal deprecation procedures work this way. The pattern is "
        "stable when acknowledgment capacity matches environmental change rate. Failure "
        "mode: authority structure develops extraction stakes in kernel preservation "
        "that did not exist at founding."
    ),
    "interpretive_accretion": (
        "Interpretive Accretion: The text is fixed; authority grounds itself in "
        "continuity with the founding text. The formal mechanism for changing the text "
        "does not function or does not exist. Drift migrates entirely into interpretation "
        "— everyone insists the kernel controls while operational meaning shifts "
        "substantially. Brahmanical commentary on the Vedas works this way; Catholic "
        "doctrinal development and common law jurisprudence work this way. The pattern "
        "is durable across millennia when the interpretive layer can absorb the "
        "operational drift the environment produces."
    ),
    "diffuse_reconstruction": (
        "Diffuse Reconstruction: The kernel is under-specified or intentionally "
        "ambiguous. No centralized authority structure exists to adjudicate. Many "
        "parties produce mutually incompatible readings claiming the same source. "
        "The pattern persists indefinitely but lacks operational coherence — it often "
        "serves strategic purposes for parties who benefit from operational ambiguity. "
        "The failure condition is the persistent state rather than an event."
    ),
    "implicit_practice": (
        "Implicit Practice: There is no codified kernel — the kernel is whatever the "
        "system does. Authority is grounded in practice itself; drift is the mechanism "
        "rather than a failure of it. The UK constitution works this way; craft "
        "traditions transmitting tacit knowledge through apprenticeship work this way. "
        "The pattern is stable as long as practice remains coherent. Breakdown is "
        "severe when practice loses coherence because there is no fixed referent to "
        "reconstruct from."
    ),
    "anchored_fixity_with_accretion": (
        "Anchored Fixity (with interpretive buffer): The kernel is formalized; the "
        "authority structure grounds its legitimacy in the kernel's unchangeability "
        "and extracts substantial benefit from preventing revision. Paired with a "
        "functioning interpretive substructure below the kernel that absorbs drift "
        "without surfacing revision. The Hindu Vedic-Brahmanical system, post-development "
        "Catholic doctrine, and the Confucian commentary tradition operate this way. "
        "This configuration can persist millennia: the unrevisable kernel is preserved "
        "while the interpretive layer does the acknowledgment work the kernel cannot do."
    ),
    "anchored_fixity_brittle": (
        "Anchored Fixity (brittle): The kernel is formalized; the authority structure "
        "grounds its legitimacy in the kernel's unchangeability and extracts substantial "
        "benefit from preventing revision. No interpretive buffer exists below the kernel "
        "— the kernel is supposed to govern operational practice directly. This "
        "configuration is structurally brittle: it produces accumulating gap and "
        "catastrophic breakdown when environmental change exceeds kernel-governance "
        "capacity. The Spartan Lycurgan system is the canonical civilizational instance; "
        "certain forms of religious fundamentalism and trauma responses that treat the "
        "precipitating event as unrevisable kernel instantiate the same pattern at "
        "smaller scales."
    ),
}

_CS_VERDICT_PROSE = {
    "false_marked_revision": (
        "Signals conflict with Marked Revision claim: suppression, theater, or "
        "enforcement patterns suggest the revision mechanism is not functioning or "
        "is not voluntary."
    ),
    "false_interpretive_accretion": (
        "Signals conflict with Interpretive Accretion claim: coordination type or "
        "theater/suppression levels are inconsistent with lineage-grounded authority "
        "operating through interpretation."
    ),
    "false_diffuse_reconstruction": (
        "Signals conflict with Diffuse Reconstruction claim: suppression or coordination "
        "type suggests a concentrated enforcer rather than truly distributed authority."
    ),
    "false_implicit_practice": (
        "Signals conflict with Implicit Practice claim: natural-law emergence flag, "
        "high theater, or high suppression are inconsistent with practice-grounded "
        "authority."
    ),
    "false_anchored_fixity_accretion": (
        "Signals conflict with Anchored Fixity (with buffer) claim: enforcement "
        "coordination type or high suppression suggests the interpretive layer may "
        "not be functioning."
    ),
    "false_anchored_fixity_brittle": (
        "Signals conflict with Anchored Fixity (brittle) claim: identity coordination "
        "type and moderate suppression suggest a possible informal interpretive buffer "
        "— consider interpretation_layer_present: true."
    ),
}


def build_cs_pattern_section(constraint_id, pipeline_data):
    """CS pattern classification section — L2, after theorem instantiation.

    Returns None when CS fields are absent (legacy constraint or non-CS constraint).
    Returns a string section when CS fields are present.
    """
    if pipeline_data is None:
        return None

    entry = find_constraint_entry(pipeline_data, constraint_id)
    if entry is None:
        return None

    cs_pattern = entry.get("cs_pattern")
    cs_signals = entry.get("cs_pattern_signals", [])
    cs_verdicts = entry.get("cs_verdicts", [])

    # Absent when fields missing (legacy constraint)
    if cs_pattern is None or "cs_fields_absent" in cs_signals:
        return None

    lines = ["", "--- COMMITMENT SYSTEM PATTERN ---", ""]

    if cs_pattern == "no_pattern_match":
        lines.append("  No CS pattern detected — signals ambiguous or field combination unrecognized.")
        if cs_signals:
            lines.append(f"  (Declared signals: {', '.join(str(s) for s in cs_signals)})")
        return "\n".join(lines)

    prose = _CS_PATTERN_PROSE.get(cs_pattern)
    if prose:
        lines.append(f"  Pattern: {cs_pattern}")
        lines.append("")
        lines.append(f"  {prose}")
        lines.append("")
        lines.append(f"  Structural signals: {', '.join(str(s) for s in cs_signals)}")

    for verdict in cs_verdicts:
        verdict_prose = _CS_VERDICT_PROSE.get(verdict, "")
        lines.append(f"\n  ⚠ {verdict}: {verdict_prose}")

    lines.append("")
    lines.append("  See: docs/commitment_systems/commitment_systems_sketch_v5_2.md")
    return "\n".join(lines)


# --- CS Temporal Status (drift terminal, axiom foreclosed, unacknowledged drift) ---

def build_cs_extended_section(constraint_id, pipeline_data):
    """Render CS temporal status section. Returns empty string for legacy constraints."""
    if pipeline_data is None:
        return ""
    entry = find_constraint_entry(pipeline_data, constraint_id)
    if entry is None:
        return ""

    terminal = entry.get("cs_drift_terminal")
    foreclosed = entry.get("cs_axiom_foreclosed")
    unacknowledged = entry.get("cs_drift_unacknowledged", False)

    if terminal is None and foreclosed is None and not unacknowledged:
        return ""

    lines = ["--- COMMITMENT SYSTEM TEMPORAL STATUS ---"]

    if terminal is not None:
        lines.append(f"  Drift terminal attractor: {terminal}")
    if foreclosed is not None:
        lines.append(f"  Axiom foreclosed: {foreclosed}")
    if unacknowledged:
        lines.append("  Drift state: unacknowledged")

    return "\n".join(lines)


# --- CS Kernel Reading Comparison (kernel membership + cross-reading findings) ---

def build_kernel_reading_section(constraint_id, pipeline_data):
    """Render cross-reading comparison if this constraint belongs to a kernel."""
    if pipeline_data is None:
        return ""
    kc = pipeline_data.get("validation", {}).get("cs_kernel_comparison", [])
    if not kc:
        return ""

    # Load this story's story_uid for precise instance matching (prefer UID over name)
    json_path = PROJECT_ROOT / "json" / f"{constraint_id}.json"
    story_uid = None
    try:
        import json as _json
        with open(json_path, "r", encoding="utf-8") as f:
            story_uid = _json.load(f).get("header", {}).get("story_uid")
    except (OSError, ValueError):
        pass

    kernel_entry = None
    this_reading = None
    for ke in kc:
        for r in ke.get("readings", []):
            # Match by story_uid if available; fall back to reading_id (name)
            if story_uid and r.get("story_uid") == story_uid:
                kernel_entry = ke
                this_reading = r
                break
            elif not story_uid and r.get("reading_id") == constraint_id:
                kernel_entry = ke
                this_reading = r
                break
        if kernel_entry:
            break
    if kernel_entry is None:
        return ""

    lines = [f"--- KERNEL: {kernel_entry['kernel_id']} ---"]
    lines.append(
        f"  {kernel_entry['reading_count']} readings | "
        f"{kernel_entry['diverging_pair_count']} diverging pairs | "
        f"{kernel_entry['axiom_conflict_count']} axiom conflicts"
    )
    for r in kernel_entry["readings"]:
        rid = r["reading_id"]
        r_uid = r.get("story_uid")
        is_this = (story_uid and r_uid == story_uid) or (not story_uid and rid == constraint_id)
        marker = " *" if is_this else "  "
        parts = []
        if r.get("cs_drift_terminal"):
            parts.append(f"terminal={r['cs_drift_terminal']}")
        if r.get("cs_axiom_foreclosed"):
            parts.append(f"foreclosed")
        if r.get("cs_drift_unacknowledged"):
            parts.append(f"unacknowledged")
        if r.get("cs_drift_mismatch"):
            parts.append(f"mismatch")
        suffix = f" [{', '.join(parts)}]" if parts else ""
        lines.append(f"{marker}{rid}{suffix}")

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


# --- XCON Synthesis Section ---

def build_xcon_synthesis_section(
    constraint_id, evaluative_data, scenario_data, omega_xcon_data, pipeline_data
):
    """Render ## XCON SYNTHESIS block when cross-corpus convergence threshold is met.

    Threshold (all three required):
    1. Cover story topology confirmed: constraint appears as face or extractive member
       in any cover_story_topology pattern across all evaluative constraint sets.
    2. Evaluative convergence group representing ≥10% of corpus, with at least one
       directional convergence pattern in that group.
    3. Omega narrowings ≥50 with directional consistency across all beneficiary groups.

    Returns empty string when threshold is not met (no placeholder emitted).
    """
    if not evaluative_data:
        return ""

    key = constraint_id.lower()
    all_sets = evaluative_data.get("constraint_sets", [])

    # --- Condition 1: Cover story topology ---
    cover_confirmed = False
    cover_role = None
    for cset in all_sets:
        for p in cset.get("convergence_patterns", []):
            if p.get("pattern") != "cover_story_topology":
                continue
            ev = p.get("evidence", {})
            face = (ev.get("face_constraint") or "").lower()
            extractive = [m.lower() for m in ev.get("extractive_members", [])]
            involved = [c.lower() for c in p.get("constraints_involved", [])]
            if key == face or key in extractive or key in involved:
                cover_confirmed = True
                cover_role = "face" if key == face else "extractive member"
                break
        if cover_confirmed:
            break

    if not cover_confirmed:
        return ""

    # --- Condition 2: Evaluative convergence ≥10% of corpus with directional pattern ---
    total_corpus = len(pipeline_data.get("per_constraint", [])) if pipeline_data else 0
    matched_sets = [
        s for s in all_sets
        if any(c.lower() == key for c in s.get("constraints", []))
    ]

    convergence_met = False
    qualifying_patterns = []  # collect for plain-language translation
    if total_corpus > 0:
        for cset in matched_sets:
            group_size = len(cset.get("constraints", []))
            if group_size / total_corpus < 0.10:
                continue
            directional = [
                p for p in cset.get("convergence_patterns", [])
                if p.get("pattern") in {
                    "convergent_signature", "convergent_institutional", "convergent_drift"
                } and p.get("evidence")
            ]
            if directional:
                convergence_met = True
                qualifying_patterns.extend(directional)
                break

    if not convergence_met:
        return ""

    # --- Condition 3: Omega narrowings ≥50 with directional consistency ---
    omega_count = 0
    omega_types_seen = set()
    if omega_xcon_data:
        for beneficiary_group in omega_xcon_data.values():
            constraints_map = beneficiary_group.get("constraints", {})
            # omega_cross_constraint.json keys constraints by their original ID
            entries = None
            for cid, narrows in constraints_map.items():
                if cid.lower() == key:
                    entries = narrows
                    break
            if entries:
                omega_count += len(entries)
                for e in entries:
                    omega_types_seen.add(e.get("omega_type", ""))

    # Directional consistency: single omega_type, or at most one empty/unknown entry mixed in
    directionally_consistent = (
        omega_count >= 50
        and len(omega_types_seen - {""}) <= 1
    )

    if not directionally_consistent:
        return ""

    # --- All conditions met — build block ---
    entry = find_constraint_entry(pipeline_data, constraint_id) if pipeline_data else None
    h1 = entry.get("h1_band") if entry else None

    lines = ["", "## XCON SYNTHESIS", ""]
    lines.append(f"Cross-constraint convergence confirmed for {constraint_id}.")
    lines.append("")
    lines.append("Elevated findings (treat as primary claims, not perspectival readings):")

    # Cover story topology finding
    lines.append(
        f"- Cover story topology confirmed: this constraint serves as {cover_role} "
        f"in a cross-corpus pattern."
    )

    # Directional convergence patterns
    for p in qualifying_patterns:
        pname = p.get("pattern", "")
        ev = p.get("evidence", {})
        if pname == "convergent_signature":
            sig = ev.get("shared_signature", "?")
            n = len(ev.get("constraints", []))
            lines.append(
                f"- Consistent {sig} structural signature confirmed across {n} constraints "
                f"in the corpus."
            )
        elif pname == "convergent_institutional":
            inst = ev.get("institutional_type", "?")
            anal = ev.get("analytical_type", "?")
            n = len(ev.get("constraints_with_split", []))
            lines.append(
                f"- Institutional observers classify this as {inst}; analytical observers "
                f"classify it as {anal} — systematic divergence confirmed across {n} constraints "
                f"cross-corpus."
            )
        elif pname == "convergent_drift":
            drift = ev.get("shared_drift_type", "?")
            sev = ev.get("severity", "?")
            n = len(ev.get("constraints", []))
            lines.append(
                f"- Convergent {drift} drift pattern confirmed at {sev} severity "
                f"across {n} constraints."
            )

    # Omega narrowings finding
    omega_type_label = next(iter(omega_types_seen - {""}), "directional") if omega_types_seen else "directional"
    lines.append(
        f"- {omega_count} cross-corpus omega narrowings with consistent {omega_type_label} "
        f"direction confirmed."
    )

    lines.append("")
    if h1 is not None:
        lines.append(
            f"Indexed divergence (H\u00b9={h1}) is explanatory context for the above, "
            f"not epistemic caution: it shows why these mechanisms were structurally "
            f"hard to see from single observer positions."
        )
    else:
        lines.append(
            "Indexed divergence (H\u00b9 data unavailable) — see CONTEXTUALITY & MONOTONICITY "
            "section. The divergence score is explanatory context for the above, not epistemic "
            "caution: it shows why these mechanisms were structurally hard to see from single "
            "observer positions."
        )
    lines.append("")
    lines.append(
        "NOTE: F-UNSUPPORTED-TRANSLATION applies. Each elevated finding requires "
        "independent Tier 1 evidence before inclusion in essay."
    )
    lines.append("")

    return "\n".join(lines)


# --- Cross-Constraint Convergence Section ---

def build_cross_constraint_section(constraint_id, evaluative_data):
    """Render ═══ CROSS-CONSTRAINT CONVERGENCE ═══ for this constraint.

    Loads from evaluative_convergence.json (evaluative_data).
    Returns empty string if the constraint does not appear in any constraint_set,
    or if evaluative_data is None/empty.
    """
    if not evaluative_data:
        return ""

    key = constraint_id.lower()
    matched_sets = [
        s for s in evaluative_data.get("constraint_sets", [])
        if any(c.lower() == key for c in s.get("constraints", []))
    ]
    if not matched_sets:
        return ""

    lines = ["\n═══ CROSS-CONSTRAINT CONVERGENCE ═══\n"]

    for cset in matched_sets:
        set_id = cset.get("set_id", "?")
        members = cset.get("constraints", [])
        beneficiary = cset.get("shared_beneficiary")

        if beneficiary:
            lines.append(f"  Set: {set_id} (beneficiary: {beneficiary}, n={len(members)})")
        else:
            lines.append(f"  Set: {set_id} (network adjacency, n={len(members)})")
        lines.append(f"  Members: {', '.join(members)}")
        lines.append("")

        patterns = cset.get("convergence_patterns", [])

        # --- CONVERGENCE PATTERNS ---
        lines.append("  --- CONVERGENCE PATTERNS ---")
        lines.append("")
        if not patterns:
            lines.append("  No convergence patterns detected for this set.")
        for p in patterns:
            pname = p.get("pattern", "?")
            ev = p.get("evidence", {})
            involved = p.get("constraints_involved", [])
            lines.append(f"  [{pname}]")

            if pname == "convergent_signature":
                lines.append(f"    Shared signature:  {ev.get('shared_signature', '?')}")
                lines.append(f"    Constraints:       {', '.join(ev.get('constraints', []))}")

            elif pname == "convergent_institutional":
                lines.append(f"    Institutional type: {ev.get('institutional_type', '?')}")
                lines.append(f"    Analytical type:    {ev.get('analytical_type', '?')}")
                lines.append(f"    Constraints:        {', '.join(ev.get('constraints_with_split', []))}")

            elif pname == "convergent_drift":
                lines.append(f"    Drift type:   {ev.get('shared_drift_type', '?')}")
                lines.append(f"    Severity:     {ev.get('severity', '?')}")
                lines.append(f"    Constraints:  {', '.join(ev.get('constraints', []))}")

            elif pname == "cover_story_topology":
                face = ev.get("face_constraint", "?")
                extractive = ev.get("extractive_members", [])
                lines.append(f"    Face constraint:      {face}")
                lines.append(f"    Extractive members:   {', '.join(extractive)}")
                lines.append(f"    Face ε:               {ev.get('face_base_extractiveness', '?')}")
                lines.append(f"    Intrinsic purity:     {ev.get('face_intrinsic_purity', '?')}")
                lines.append(f"    Propagation delta:    {ev.get('face_propagation_delta', '?')}")
                for qn in ev.get("qualifying_neighbors", []):
                    lines.append(
                        f"    Qualifying neighbor:  {qn['constraint_id']} "
                        f"(type={qn['neighbor_type']}, "
                        f"purity={qn['neighbor_purity']}, "
                        f"shared_sig={qn['shared_signature']})"
                    )
            lines.append("")

        # --- DEFENSIBILITY ASSESSMENT ---
        defensibility = cset.get("defensibility", {})
        constrained = defensibility.get("constrained_positions", [])
        indefensible = defensibility.get("indefensible_positions", [])

        if constrained or indefensible:
            lines.append("  --- DEFENSIBILITY ASSESSMENT ---")
            lines.append("")
            if constrained:
                lines.append("  Constrained positions:")
                for pos in constrained:
                    lines.append(f"    - {pos}")
                lines.append("")
            if indefensible:
                lines.append("  Indefensible positions:")
                for ip in indefensible:
                    lines.append(f"    Position: {ip.get('position', '?')}")
                    lines.append(f"    Ruled out by: {ip.get('ruled_out_by', '?')}")
                    lines.append("")

        # --- COVER STORY TOPOLOGY ---
        cover_pattern = next(
            (p for p in patterns if p.get("pattern") == "cover_story_topology"), None
        )
        if cover_pattern:
            ev = cover_pattern.get("evidence", {})
            face = ev.get("face_constraint", "?")
            extractive_members = ev.get("extractive_members", [])

            lines.append("  --- COVER STORY TOPOLOGY ---")
            lines.append("")
            if key == face.lower():
                lines.append(f"  Role: COVER STORY FACE")
                lines.append(
                    f"  This constraint ({constraint_id}) presents with low extractiveness "
                    f"(ε={ev.get('face_base_extractiveness', '?')}) and high intrinsic purity "
                    f"({ev.get('face_intrinsic_purity', '?')}), but is contaminated by "
                    f"extractive neighbors ({', '.join(extractive_members)}) via the FPN network "
                    f"(propagation delta={ev.get('face_propagation_delta', '?')})."
                )
            else:
                lines.append(f"  Role: EXTRACTIVE MEMBER")
                lines.append(
                    f"  This constraint ({constraint_id}) is an extractive member of a "
                    f"cover story group. The face constraint ({face}) provides structural "
                    f"legitimation that conceals the extraction."
                )
            lines.append("")

    return "\n".join(lines)


# --- Report Assembly ---

def assemble_report(header, prolog_output, sections):
    """Insert corpus context sections between LOGICAL FINGERPRINT and DR EXECUTIVE SUMMARY.

    Splits Prolog output at the first ==== line after --- LOGICAL FINGERPRINT ---.
    """
    insertion = "\n".join(s for s in sections if s is not None)

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
    l1_trajectory = build_drift_trajectory_section(constraint_id, data["pipeline"])
    l1_orbit = build_level1_orbit(constraint_id, data["orbit"])
    l1_omega = build_omega_section(constraint_id, data["omega"])

    # Level 2: Diagnostic Convergence
    l2_convergence = build_level2_convergence(constraint_id, data["pipeline"])
    l2_maxent = build_maxent_section(constraint_id, data["pipeline"])
    l2_persistence = build_persistence_section(constraint_id, data["persistence"])

    # Stability band — compute before section assembly; result also goes into sidecar
    _kernel_id = _get_kernel_id_for_constraint(constraint_id)
    if _kernel_id is None:
        stability_data = {"no_kernel": True}
    elif _kernel_id in _WITNESSED_PARAMS:
        stability_data = _run_stability_band(_kernel_id)
    else:
        stability_data = {"not_witnessed": True, "kernel_id": _kernel_id}
    l2_stability = build_stability_band(constraint_id, stability_data)

    l2_abductive = build_abductive_section(constraint_id, data["pipeline"])
    l2_verdict = build_level2_verdict_body(constraint_id, data["pipeline"])
    l2_theorems = build_theorem_instantiation(
        constraint_id, data["pipeline"], data["orbit"]
    )
    l2_cs_pattern = build_cs_pattern_section(constraint_id, data["pipeline"])
    l2_cs_extended = build_cs_extended_section(constraint_id, data["pipeline"])
    l2_cs_kernel = build_kernel_reading_section(constraint_id, data["pipeline"])
    # Level 1: FPN contamination topology (Gap analysis Change 4 — resolved)
    l1_contamination = build_contamination_network(constraint_id, data["pipeline"])

    # Post-synthesis (only if T12 flags exist)
    post = build_post_synthesis(constraint_id, data["pipeline"])

    # Axiom 2: Chi/Epsilon decomposition
    try:
        from chi_variance_decomposition import build_axiom2_section
        l2_axiom2 = build_axiom2_section(constraint_id, data["pipeline"])
    except ImportError:
        l2_axiom2 = ""

    xcon_synthesis = build_xcon_synthesis_section(
        constraint_id,
        data["evaluative"],
        data.get("scenario"),
        data.get("omega_xcon"),
        data["pipeline"],
    )

    sections = [
        banner,
        l2_cs_kernel,    # kernel cross-reading panel — first (Phase 2: kernel-terminal)
        xcon_synthesis,
        build_level_header(1, "SELF-CONSISTENCY"),
        l1_identity, l1_trajectory, l1_contamination, l1_orbit, l1_omega,
        build_level_header(2, "DIAGNOSTIC CONVERGENCE"),
        l2_convergence, l2_maxent, l2_persistence, l2_stability, l2_abductive,
        l2_axiom2, l2_verdict, l2_theorems, l2_cs_pattern, l2_cs_extended,
    ]
    if post.strip():
        sections.extend(["\n═══ POST-SYNTHESIS ═══", post])

    full_report = assemble_report(header, prolog_output, sections)

    # Append cross-constraint convergence section (after Prolog remainder + POST-SYNTHESIS)
    cross = build_cross_constraint_section(constraint_id, data["evaluative"])
    if cross.strip():
        full_report += "\n" + cross

    REPORTS_DIR.mkdir(parents=True, exist_ok=True)
    out_path = REPORTS_DIR / f"{constraint_id}_report.md"
    with open(out_path, "w", encoding="utf-8") as f:
        f.write(full_report)

    print(f"Report written to: {out_path}")

    # --- Emit JSON sidecar ---
    entry = find_constraint_entry(data["pipeline"], constraint_id)
    sidecar = build_sidecar_data(constraint_id, entry, prolog_output, iteration_round)

    # Add evaluative convergence data for this constraint
    evaluative_data = data.get("evaluative")
    if evaluative_data:
        key = constraint_id.lower()
        matched_sets = [
            s for s in evaluative_data.get("constraint_sets", [])
            if any(c.lower() == key for c in s.get("constraints", []))
        ]
        sidecar["evaluative_convergence"] = {
            "in_sets": [s["set_id"] for s in matched_sets],
            "pattern_count": sum(
                len(s.get("convergence_patterns", [])) for s in matched_sets
            ),
            "cover_story_role": None,
        }
        for s in matched_sets:
            for p in s.get("convergence_patterns", []):
                if p.get("pattern") == "cover_story_topology":
                    face = p.get("evidence", {}).get("face_constraint", "")
                    if face.lower() == key:
                        sidecar["evaluative_convergence"]["cover_story_role"] = "face"
                    else:
                        sidecar["evaluative_convergence"]["cover_story_role"] = "extractive_member"
    else:
        sidecar["evaluative_convergence"] = None

    # Add stability band data to sidecar (additive field; validator ignores extra fields)
    def _sidecar_stability(sd):
        if sd is None:
            return {"assessed": False, "kernel_id": None, "baseline_hash": None, "params": []}
        if sd.get("no_kernel"):
            return {"assessed": False, "kernel_id": None, "baseline_hash": None, "params": []}
        if sd.get("not_witnessed") or sd.get("error"):
            return {
                "assessed": False,
                "kernel_id": sd.get("kernel_id"),
                "baseline_hash": None,
                "params": [],
            }
        return {
            "assessed": True,
            "kernel_id": sd.get("kernel_id"),
            "baseline_hash": sd.get("baseline_hash"),
            "params": [
                {
                    "param": pr["param"],
                    "original": pr.get("original"),
                    "up_boundary_pct": pr.get("up_boundary", {}).get("pct") if pr.get("up_boundary") else None,
                    "up_floor_pct": pr.get("up_floor", {}).get("pct") if pr.get("up_floor") else None,
                    "dn_boundary_pct": pr.get("dn_boundary", {}).get("pct") if pr.get("dn_boundary") else None,
                    "dn_floor_pct": pr.get("dn_floor", {}).get("pct") if pr.get("dn_floor") else None,
                    "coverage": (pr.get("up_boundary") or pr.get("up_floor") or {}).get("coverage"),
                    "flipped": (pr.get("up_boundary") or {}).get("flipped"),
                }
                for pr in sd.get("params", [])
                if "error" not in pr
            ],
        }
    sidecar["stability_band"] = _sidecar_stability(stability_data)

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
        "pipeline":    load_json(OUTPUTS_DIR / "enriched_pipeline.json", "enriched_pipeline.json"),
        "orbit":       load_json(OUTPUTS_DIR / "orbit_data.json", "orbit_data.json"),
        "omega":       load_json(OUTPUTS_DIR / "enriched_omega_data.json", "enriched_omega_data.json"),
        "persistence": load_json(SCRIPT_DIR / "persistence_results.json", "persistence_results.json"),
        "maxent":      load_text(OUTPUTS_DIR / "maxent_report.md", "maxent_report.md"),
        "evaluative":  load_json(OUTPUTS_DIR / "evaluative_convergence.json",
                                 "evaluative_convergence.json"),
        "scenario":    load_json(OUTPUTS_DIR / "scenario_convergence.json",
                                 "scenario_convergence.json"),
        "omega_xcon":  load_json(OUTPUTS_DIR / "omega_cross_constraint.json",
                                 "omega_cross_constraint.json"),
    }

    # Generate reports
    for constraint_id in constraint_ids:
        generate_report(constraint_id, data, iteration_round=args.iteration_round)

    if len(constraint_ids) > 1:
        print(f"\nDone: {len(constraint_ids)} reports generated.")


if __name__ == "__main__":
    main()
