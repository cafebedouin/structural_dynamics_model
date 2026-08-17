#!/usr/bin/env python3
"""
FCR Override Ablation — Convergence Test

Runs each corpus (Flash, Haiku) with FCR override enabled and disabled,
collecting per-constraint orbit data, H1 cohomology, and FCR detection status.
Produces a comparison table answering whether the ~62% tangled_rope convergence
is driven by the FCR override or reflects a structural tendency.

Reports TWO type distributions per condition:
  - Analytical perspective: the type at the highest-power context
  - Modal (orbit majority): majority type across all 4 perspectives,
    which is what the paper's §5.2 convergence table uses

Uses the Prolog overlay pattern from config_sensitivity_sweep.py.

Usage:
    python3 python/fcr_ablation.py [--timeout SECONDS]
"""

import argparse
import os
import re
import subprocess
import sys
import tempfile
from collections import Counter
from pathlib import Path

PROLOG_DIR = Path(__file__).resolve().parent.parent / "prolog"

CORPORA = {
    "Flash (B)": "archives/prolog_v4/flashsets",
    "Haiku (A)": "archives/prolog_v4/testsets_haiku_chi_anchored",
}

FCR_STATES = {
    "enabled": 1,
    "disabled": 0,
}

# Tie-breaking precedence for 2-2 orbit splits.
# Matches batch_claim_reconciliation.py:TIE_PRECEDENCE.
TIE_PRECEDENCE = [
    "snare", "tangled_rope", "piton", "scaffold",
    "rope", "mountain", "naturalized",
]

# Prolog overlay template: sets fcr_override_enabled and corpus_path,
# then collects per-constraint orbit + H1 + FCR detection data.
# Uses findall+sort for deduplication (known_constraint/1 source fix
# is also applied, but belt+suspenders).
OVERLAY_TEMPLATE = """\
%% FCR ablation overlay — auto-generated
:- use_module(config).

:- (retract(config:param(fcr_override_enabled, _)) -> true ; true),
   asserta(config:param(fcr_override_enabled, {fcr_flag})).
:- (retract(config:param(corpus_path, _)) -> true ; true),
   asserta(config:param(corpus_path, '{corpus_dir}')).

:- [stack].
:- use_module(grothendieck_cohomology).
:- use_module(logical_fingerprint).

:- corpus_loader:ensure_corpus_loaded,
   grothendieck_cohomology:cohomology_cleanup,
   findall(C0, logical_fingerprint:known_constraint(C0), Cs0),
   sort(Cs0, Cs),
   forall(
     member(C, Cs),
     (   catch(
           (   grothendieck_cohomology:orbit_vector(C, [T1, T2, T3, T4]),
               grothendieck_cohomology:cohomological_obstruction(C, H0, H1),
               %% Unbound + post-filter per the bound-selector rule (fixed 2026-08-17;
               %% build_discipline.md -> Bound-probe bypasses clause-order). The former
               %% form OVER-COUNTED the FCR column by admitting constraints the engine
               %% assigns false_natural_law. Pre-fix vs post-fix, measured at
               %% dcde9591: flash 354->350, haiku 240->234, kimi 147->144,
               %% sonnet 400->395, testsets 86->85, kernel_v1 273->273.
               %% ANY RECORDED FINDING FROM A PRE-2026-08-17 RUN OF THIS SCRIPT RODE
               %% THE WRONG COLUMN on the flash/haiku legs — see OQ-298 before citing
               %% commit 7ca977c4's asymmetry result. The figures above are
               %% SUPERSEDED-PENDING-RE-RUN: pre-fix measurements kept for audit,
               %% not present-tense facts. They become current only when the
               %% post-fix re-run lands (OQ-298 kill condition).
               (   once(signature_detection:constraint_signature(C, Sig0)),
                   Sig0 == false_ci_rope
               ->  FCR = 1
               ;   FCR = 0
               ),
               format('DATA\\t~w\\t~w\\t~w\\t~w\\t~w\\t~w\\t~w\\t~w~n',
                      [C, T1, T2, T3, T4, H0, H1, FCR])
           ),
           Error,
           format(user_error, 'ERROR on ~w: ~w~n', [C, Error])
         )
     )
   ),
   halt.
"""


def run_ablation_query(corpus_name, corpus_dir, fcr_flag, timeout_sec):
    """Run a single ablation query and return parsed rows."""
    overlay = OVERLAY_TEMPLATE.format(
        fcr_flag=fcr_flag,
        corpus_dir=corpus_dir,
    )
    fd, overlay_path = tempfile.mkstemp(suffix=".pl", prefix="fcr_ablation_")
    try:
        with os.fdopen(fd, "w") as f:
            f.write(overlay)

        cmd = ["swipl", "-g", f"consult('{overlay_path}'), halt(0)."]
        proc = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            timeout=timeout_sec,
            cwd=str(PROLOG_DIR),
        )
        output = proc.stdout + proc.stderr

        # Parse DATA lines
        rows = []
        for line in output.split("\n"):
            if line.startswith("DATA\t"):
                parts = line.split("\t")
                if len(parts) == 9:
                    rows.append({
                        "id": parts[1],
                        "t_powerless": parts[2],
                        "t_moderate": parts[3],
                        "t_institutional": parts[4],
                        "t_analytical": parts[5],
                        "h0": int(parts[6]),
                        "h1": int(parts[7]),
                        "fcr": int(parts[8]),
                    })

        if not rows:
            print(f"  WARNING: No data rows parsed for {corpus_name} "
                  f"(fcr={fcr_flag})", file=sys.stderr)
            # Dump last 20 lines for debugging
            for line in output.split("\n")[-20:]:
                print(f"  | {line}", file=sys.stderr)

        return rows

    except subprocess.TimeoutExpired:
        print(f"  TIMEOUT after {timeout_sec}s for {corpus_name} "
              f"(fcr={fcr_flag})", file=sys.stderr)
        return []
    finally:
        try:
            os.unlink(overlay_path)
        except OSError:
            pass


def resolve_modal_type(types):
    """Compute majority type with principled tie-breaking.

    For 4 perspectives, ties occur on 2-2 splits. Uses TIE_PRECEDENCE
    (matching batch_claim_reconciliation.py) to resolve deterministically.

    Returns (modal_type, is_tie).
    """
    counter = Counter(types)
    max_count = counter.most_common(1)[0][1]
    tied = [t for t, c in counter.items() if c == max_count]
    if len(tied) == 1:
        return tied[0], False
    # Tie: resolve by precedence
    for pref in TIE_PRECEDENCE:
        if pref in tied:
            return pref, True
    return tied[0], True


def compute_metrics(rows):
    """Compute all metrics from parsed row data."""
    n = len(rows)
    if n == 0:
        return None

    # Post-override type distribution (analytical perspective as reference)
    type_counts = Counter(r["t_analytical"] for r in rows)

    # Modal type (orbit majority) with tie tracking
    orbit_types = Counter()
    tie_count = 0
    for r in rows:
        types = [r["t_powerless"], r["t_moderate"],
                 r["t_institutional"], r["t_analytical"]]
        mc, is_tie = resolve_modal_type(types)
        if is_tie:
            tie_count += 1
        orbit_types[mc] += 1

    # H1 distribution
    h1_dist = Counter(r["h1"] for r in rows)

    # Descent rate (H0=1 means all perspectives agree)
    descent_count = sum(1 for r in rows if r["h0"] == 1)
    descent_rate = descent_count / n * 100

    # Gauge-variance rate
    gauge_variant = sum(1 for r in rows if r["h1"] > 0)
    gauge_variance_rate = gauge_variant / n * 100

    # Superselection gap: H1=1 and H1=2 empty?
    h1_1_empty = h1_dist.get(1, 0) == 0
    h1_2_empty = h1_dist.get(2, 0) == 0

    # FCR detection rate
    fcr_detected = sum(r["fcr"] for r in rows)

    return {
        "n": n,
        "type_dist_analytical": dict(type_counts.most_common()),
        "type_dist_orbit_majority": dict(orbit_types.most_common()),
        "tie_count": tie_count,
        "h1_dist": {k: h1_dist[k] for k in sorted(h1_dist)},
        "descent_rate": round(descent_rate, 1),
        "gauge_variance_rate": round(gauge_variance_rate, 1),
        "h1_1_empty": h1_1_empty,
        "h1_2_empty": h1_2_empty,
        "superselection_gap": h1_1_empty and h1_2_empty,
        "fcr_detected": fcr_detected,
    }


def format_type_dist(dist, n):
    """Format type distribution as aligned table rows."""
    types_order = ["tangled_rope", "snare", "mountain", "rope",
                   "scaffold", "piton", "naturalized", "unknown"]
    lines = []
    for t in types_order:
        cnt = dist.get(t, 0)
        if cnt > 0:
            pct = cnt / n * 100
            lines.append(f"| {t:<15} | {cnt:>5} | {pct:>5.1f}% |")
    return "\n".join(lines)


def main():
    parser = argparse.ArgumentParser(description="FCR Override Ablation")
    parser.add_argument("--timeout", type=int, default=600,
                        help="Timeout per Prolog run in seconds")
    args = parser.parse_args()

    results = {}

    for corpus_name, corpus_dir in CORPORA.items():
        for fcr_name, fcr_flag in FCR_STATES.items():
            label = f"{corpus_name} / FCR {fcr_name}"
            print(f"\n{'='*60}", file=sys.stderr)
            print(f"Running: {label}", file=sys.stderr)
            print(f"{'='*60}", file=sys.stderr)

            rows = run_ablation_query(
                corpus_name, corpus_dir, fcr_flag, args.timeout)
            metrics = compute_metrics(rows)
            results[label] = metrics

            if metrics:
                print(f"  {metrics['n']} constraints, "
                      f"FCR detected: {metrics['fcr_detected']}, "
                      f"descent rate: {metrics['descent_rate']}%",
                      file=sys.stderr)

    # -----------------------------------------------------------------------
    # Output report
    # -----------------------------------------------------------------------
    print("\n# FCR Override Ablation Results\n")

    for label, m in results.items():
        if m is None:
            print(f"\n## {label}\n\nNo data collected.\n")
            continue

        print(f"\n## {label}\n")
        print(f"- **Constraints**: {m['n']}")
        print(f"- **FCR detected**: {m['fcr_detected']} "
              f"({m['fcr_detected']/m['n']*100:.1f}%)")
        print(f"- **Descent rate**: {m['descent_rate']}% "
              f"(H\u00b9=0)")
        print(f"- **Gauge-variance rate**: {m['gauge_variance_rate']}%")
        print(f"- **Superselection gap**: "
              f"H\u00b9=1 {'empty' if m['h1_1_empty'] else 'OCCUPIED'}, "
              f"H\u00b9=2 {'empty' if m['h1_2_empty'] else 'OCCUPIED'} "
              f"\u2014 {'HOLDS' if m['superselection_gap'] else 'BROKEN'}")

        print(f"\n### Type distribution — analytical perspective\n")
        print(f"| {'Type':<15} | {'Count':>5} |   {'%':>5} |")
        print(f"|{'-'*17}|{'-'*7}|{'-'*8}|")
        print(format_type_dist(m["type_dist_analytical"], m["n"]))

        print(f"\n### Type distribution — modal (orbit majority)\n")
        if m["tie_count"] > 0:
            print(f"*{m['tie_count']} constraints "
                  f"({m['tie_count']/m['n']*100:.1f}%) had 2-2 ties, "
                  f"resolved by TIE_PRECEDENCE "
                  f"(snare > tangled_rope > ...)*\n")
        print(f"| {'Type':<15} | {'Count':>5} |   {'%':>5} |")
        print(f"|{'-'*17}|{'-'*7}|{'-'*8}|")
        print(format_type_dist(m["type_dist_orbit_majority"], m["n"]))

        print(f"\n### H\u00b9 distribution\n")
        print(f"| H\u00b9 | Count | % |")
        print(f"|---:|------:|--:|")
        for h1_val in sorted(m["h1_dist"]):
            cnt = m["h1_dist"][h1_val]
            pct = cnt / m["n"] * 100
            print(f"| {h1_val} | {cnt} | {pct:.1f}% |")

    # -----------------------------------------------------------------------
    # Convergence comparison (uses modal type — matches paper §5.2)
    # -----------------------------------------------------------------------
    print("\n\n## Convergence Comparison (modal type)\n")

    for fcr_name in FCR_STATES:
        print(f"\n### FCR {fcr_name}\n")
        print(f"| {'Type':<15} | {'Flash %':>8} | {'Haiku %':>8} | {'Delta':>6} |")
        print(f"|{'-'*17}|{'-'*10}|{'-'*10}|{'-'*8}|")

        flash_key = f"Flash (B) / FCR {fcr_name}"
        haiku_key = f"Haiku (A) / FCR {fcr_name}"
        flash_m = results.get(flash_key)
        haiku_m = results.get(haiku_key)

        if flash_m and haiku_m:
            all_types = sorted(set(
                list(flash_m["type_dist_orbit_majority"]) +
                list(haiku_m["type_dist_orbit_majority"])
            ))
            for t in all_types:
                f_cnt = flash_m["type_dist_orbit_majority"].get(t, 0)
                h_cnt = haiku_m["type_dist_orbit_majority"].get(t, 0)
                f_pct = f_cnt / flash_m["n"] * 100
                h_pct = h_cnt / haiku_m["n"] * 100
                delta = h_pct - f_pct
                print(f"| {t:<15} | {f_pct:>7.1f}% | {h_pct:>7.1f}% | "
                      f"{delta:>+5.1f}% |")

    # -----------------------------------------------------------------------
    # Convergence comparison (analytical perspective — for reference)
    # -----------------------------------------------------------------------
    print("\n\n## Convergence Comparison (analytical perspective)\n")

    for fcr_name in FCR_STATES:
        print(f"\n### FCR {fcr_name}\n")
        print(f"| {'Type':<15} | {'Flash %':>8} | {'Haiku %':>8} | {'Delta':>6} |")
        print(f"|{'-'*17}|{'-'*10}|{'-'*10}|{'-'*8}|")

        flash_key = f"Flash (B) / FCR {fcr_name}"
        haiku_key = f"Haiku (A) / FCR {fcr_name}"
        flash_m = results.get(flash_key)
        haiku_m = results.get(haiku_key)

        if flash_m and haiku_m:
            all_types = sorted(set(
                list(flash_m["type_dist_analytical"]) +
                list(haiku_m["type_dist_analytical"])
            ))
            for t in all_types:
                f_cnt = flash_m["type_dist_analytical"].get(t, 0)
                h_cnt = haiku_m["type_dist_analytical"].get(t, 0)
                f_pct = f_cnt / flash_m["n"] * 100
                h_pct = h_cnt / haiku_m["n"] * 100
                delta = h_pct - f_pct
                print(f"| {t:<15} | {f_pct:>7.1f}% | {h_pct:>7.1f}% | "
                      f"{delta:>+5.1f}% |")

    # Key finding
    print("\n\n## Key Finding\n")
    flash_en = results.get("Flash (B) / FCR enabled")
    flash_dis = results.get("Flash (B) / FCR disabled")
    haiku_en = results.get("Haiku (A) / FCR enabled")
    haiku_dis = results.get("Haiku (A) / FCR disabled")

    if all(m is not None for m in [flash_en, flash_dis, haiku_en, haiku_dis]):
        def tr_pct_modal(m):
            return m["type_dist_orbit_majority"].get(
                "tangled_rope", 0) / m["n"] * 100

        def tr_pct_analytical(m):
            return m["type_dist_analytical"].get(
                "tangled_rope", 0) / m["n"] * 100

        print("### Modal type (orbit majority — paper §5.2 unit)\n")
        print(f"- Flash tangled_rope: "
              f"{tr_pct_modal(flash_en):.1f}% (enabled) -> "
              f"{tr_pct_modal(flash_dis):.1f}% (disabled)")
        print(f"- Haiku tangled_rope: "
              f"{tr_pct_modal(haiku_en):.1f}% (enabled) -> "
              f"{tr_pct_modal(haiku_dis):.1f}% (disabled)")

        en_delta = abs(tr_pct_modal(flash_en) - tr_pct_modal(haiku_en))
        dis_delta = abs(tr_pct_modal(flash_dis) - tr_pct_modal(haiku_dis))

        print(f"- Convergence gap (enabled): {en_delta:.1f}pp")
        print(f"- Convergence gap (disabled): {dis_delta:.1f}pp")

        if dis_delta < 10:
            print(f"\n**Convergence HOLDS without FCR** (gap < 10pp). "
                  f"The ~62% attractor reflects structural tendency, "
                  f"not FCR manufacturing.")
        elif dis_delta > 20:
            print(f"\n**Convergence BREAKS without FCR** (gap > 20pp). "
                  f"The FCR override is the primary attractor mechanism.")
        else:
            print(f"\n**Partial convergence** (gap {dis_delta:.1f}pp). "
                  f"FCR accelerates convergence but does not fully "
                  f"manufacture it.")

        print(f"\n### Analytical perspective (for reference)\n")
        print(f"- Flash tangled_rope: "
              f"{tr_pct_analytical(flash_en):.1f}% (enabled) -> "
              f"{tr_pct_analytical(flash_dis):.1f}% (disabled)")
        print(f"- Haiku tangled_rope: "
              f"{tr_pct_analytical(haiku_en):.1f}% (enabled) -> "
              f"{tr_pct_analytical(haiku_dis):.1f}% (disabled)")

        en_delta_a = abs(
            tr_pct_analytical(flash_en) - tr_pct_analytical(haiku_en))
        dis_delta_a = abs(
            tr_pct_analytical(flash_dis) - tr_pct_analytical(haiku_dis))

        print(f"- Convergence gap (enabled): {en_delta_a:.1f}pp")
        print(f"- Convergence gap (disabled): {dis_delta_a:.1f}pp")


if __name__ == "__main__":
    main()
