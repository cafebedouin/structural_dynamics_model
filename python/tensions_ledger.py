#!/usr/bin/env python3
"""Tensions ledger — deterministic, NON-GENERATIVE extractor (OQ-101).

Replaces the orchestrator's auto-essay (step 6). The essay FORM collapses
plurality regardless of synthesizer or prompt (operator ruling 2026-06-10,
`audits/2026-06-10_external_review_xprize/`); this ledger cannot over-state
by construction: no LLM call, no thesis, no cross-constraint narrative —
one bulleted block per constraint, every line traceable to a field in
`outputs/pipeline_output.json` or a line in the constraint's
`outputs/constraint_reports/<id>_report.md`. The operator synthesizes live
using the checklist at `audits/2026-06-10_external_review_xprize/README.md`.

Usage (from repo root):
    python3 python/tensions_ledger.py [constraint_id ...] [--output PATH]

With no ids: every constraint in pipeline_output.json (manifest order).
"""

import argparse
import json
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
OUTPUTS = ROOT / "outputs"
REPORTS = OUTPUTS / "constraint_reports"

POSITIONS = ("powerless", "moderate", "institutional", "analytical")
# OQ-108: the full 6-atom authoring vocabulary (docs/logic.md:293), distinct
# from the 4-position observer fingerprint above. Witness coverage is reported
# over all six — powerful/organized have no perspective column but are staffed.
POWER_ATOMS = ("powerless", "moderate", "powerful", "organized",
               "institutional", "analytical")


def _fmt_alert(a):
    return f"{a.get('type', '?')} [{a.get('severity', '?')}; {a.get('source', '?')}]"


def _drift_confidence_lines(report_path):
    """Pull the joined '[severity | confidence: X]' drift lines (OQ-102(b))
    out of the per-constraint report, verbatim — extraction, not synthesis."""
    if not report_path.exists():
        return []
    txt = report_path.read_text(errors="replace")
    return [ln.strip() for ln in txt.splitlines()
            if re.match(r"^\s*\[(critical|warning|watch)( \| confidence: \w+)?\]", ln)]


def build_block(entry, report_dir=REPORTS):
    cid = entry.get("id", "?")
    lines = [f"## {cid} — {entry.get('human_readable') or '(no display name)'}"]

    # verdict_join: headline verdict + alerts + provenance (OQ-98 source)
    vj = entry.get("verdict_join") or {}
    if vj:
        lines.append(
            f"- headline verdict: {vj.get('verdict', '?')} "
            f"(base {vj.get('base_verdict', '?')}, cap {vj.get('cap_applied', '?')})")
        alerts = vj.get("alerts") or []
        lines.append("- alerts: " + ("; ".join(_fmt_alert(a) for a in alerts)
                                     if alerts else "none"))
        mp = vj.get("measurement_provenance") or {}
        if mp:
            lines.append(
                f"- series provenance: authored {mp.get('authored', '?')}"
                f"/{mp.get('total', '?')}, injected {mp.get('injected', '?')}, "
                f"imputed {mp.get('imputed', '?')}, projected "
                f"{mp.get('projected', '?')} (basis=projected = authored guesses, "
                f"OQ-102(a))")
        gp = vj.get("grid_provenance")
        if isinstance(gp, dict):
            # Only surface grid coverage when SOMETHING is present (authored/
            # injected/imputed). A fully-absent grid (the corpus-wide default —
            # the optional coercion_grid block is rarely authored) would print
            # "authored 0/32" on every constraint as noise. The machine-readable
            # grid_provenance stays in pipeline_output.json regardless; this only
            # trims the human-facing ledger. no_interval is a distinct state and
            # is kept visible.
            present = sum(int(gp.get(k, 0) or 0)
                          for k in ("authored", "injected", "imputed"))
            if present > 0:
                lines.append(
                    f"- grid coverage: authored {gp.get('authored', '?')}"
                    f"/{gp.get('total', '?')} (injected {gp.get('injected', '?')}, "
                    f"imputed {gp.get('imputed', '?')}, absent {gp.get('absent', '?')})")
        elif gp == "no_interval":
            lines.append("- grid coverage: no interval")
    else:
        lines.append("- headline verdict: ABSENT (no verdict_join in pipeline entry)")

    # per-position types + index mismatches
    persp = entry.get("perspectives") or {}
    if persp:
        lines.append("- per-position types: " +
                     " ".join(f"{p}={persp.get(p, '?')}" for p in POSITIONS))

    # witness coverage (OQ-108): authored stakeholders per power atom. A 0 means
    # any perspective computed at that power is inference-only, not measured-
    # absent — so zeros are SHOWN (unlike the all-absent grid line). The authoring
    # axis (6 atoms) is distinct from the 4-position observer fingerprint above.
    pw = entry.get("perspective_witness")
    if isinstance(pw, dict):
        total = sum(int(pw.get(p, 0) or 0) for p in POWER_ATOMS)
        if total > 0:
            cells = " ".join(f"{p}={pw.get(p, 0)}" for p in POWER_ATOMS)
            lines.append("- witness coverage (authored stakeholders per power; "
                         f"0 = perspective inference-only): {cells}")
        else:
            lines.append("- witness coverage: no authored stakeholders "
                         "(all perspectives inference-only)")
    gaps = entry.get("gaps") or []
    if gaps:
        lines.append("- index mismatches: " + "; ".join(
            f"{g.get('gap_type', '?')} (powerless={g.get('powerless_type', '?')}, "
            f"institutional={g.get('institutional_type', '?')})" for g in gaps))
    else:
        mism = (persp and len({v for v in persp.values() if v}) > 1)
        lines.append("- index mismatches: "
                     + ("perspectives diverge (no gap pattern matched)" if mism
                        else "none"))

    # signature + grade
    sig = entry.get("signature") or "none"
    grade = vj.get("signature_grade") if vj else None
    lines.append(f"- signature: {sig} (grade: {grade or 'none'})")

    # omegas
    omegas = entry.get("omegas") or []
    lines.append("- omegas: " + ("; ".join(
        f"{o.get('id', '?')} [{o.get('type', '?')}; severity "
        f"{o.get('severity', '?')}]" for o in omegas) if omegas else "none"))

    # drift: events + the report's joined severity|confidence lines (OQ-102(b));
    # the basis provenance rides the drift line itself (rider (a)): a drift
    # verdict over projected points is a verdict over authored guesses.
    drift = entry.get("drift_events") or []
    mp_d = (vj.get("measurement_provenance") or {}) if vj else {}
    proj_tail = (f" [series {mp_d.get('projected')}/{mp_d.get('total')} "
                 f"authored-as-projected — OQ-102(a)]"
                 if mp_d.get("projected") else "")
    lines.append("- drift events: " + (("; ".join(
        f"{d.get('type', '?')} [{d.get('severity', '?')}]" for d in drift)
        + proj_tail) if drift else "none" + proj_tail))
    rpt = report_dir / f"{cid}_report.md"
    joined = _drift_confidence_lines(rpt)
    if joined:
        lines.append("- drift severity|confidence (report read-site, OQ-102(b)): "
                     + " || ".join(joined[:4]))
    if entry.get("drift_trajectory"):
        lines.append("- drift trajectory: series present for "
                     + ", ".join(sorted(entry["drift_trajectory"].keys())))

    # contamination edges — provenance gap labeled, never absorbed (OQ-103)
    cn = entry.get("contamination_network") or {}
    nbrs = cn.get("neighbors") or []
    if nbrs:
        edges = "; ".join(
            f"{n.get('id', n.get('neighbor', '?'))} "
            f"[{n.get('edge_type', n.get('relation', '?'))}; "
            f"strength {n.get('edge_contamination', n.get('strength', '?'))}]"
            for n in nbrs[:6])
        more = f" (+{len(nbrs) - 6} more)" if len(nbrs) > 6 else ""
        lines.append(f"- contamination edges: {edges}{more}")
        lines.append("  - edge provenance (story-authored vs corpus-topology): "
                     "NOT CARRIED — OQ-103 open; treat every edge as possibly "
                     "corpus-topology, not this story's claim")
    else:
        lines.append("- contamination edges: none")

    lines.append(f"- report: {rpt.relative_to(ROOT) if rpt.exists() else 'ABSENT'}")
    return "\n".join(lines)


def build_ledger(constraint_ids=None, pipeline_path=None, output_path=None,
                 report_dir=REPORTS):
    pipeline_path = pipeline_path or OUTPUTS / "pipeline_output.json"
    data = json.loads(Path(pipeline_path).read_text())
    manifest = data.get("manifest", {})
    entries = data.get("per_constraint", [])
    if constraint_ids:
        wanted = {c.lower() for c in constraint_ids}
        entries = [e for e in entries if e.get("id", "").lower() in wanted]
        missing = wanted - {e.get("id", "").lower() for e in entries}
    else:
        missing = set()

    head = [
        "# Tensions ledger (deterministic extraction — no synthesis; OQ-101)",
        f"pipeline_run_at: {manifest.get('pipeline_run_at', '?')} | "
        f"n_constraints: {manifest.get('n_constraints', '?')} | "
        f"code: {manifest.get('code_commit_short', '?')}"
        f"{' DIRTY' if manifest.get('code_dirty') else ''}",
        f"constraints in this ledger: {len(entries)}"
        + (f" | NOT FOUND in pipeline output: {sorted(missing)}" if missing else ""),
        "",
    ]
    blocks = [build_block(e, report_dir) for e in entries]
    text = "\n".join(head) + "\n\n".join(blocks) + "\n"

    output_path = Path(output_path) if output_path else OUTPUTS / "tensions_ledger.md"
    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_text(text)
    return output_path, len(entries)


def main():
    ap = argparse.ArgumentParser(description="OQ-101 deterministic tensions ledger")
    ap.add_argument("constraint_ids", nargs="*")
    ap.add_argument("--output", default=None)
    ap.add_argument("--pipeline", default=None)
    args = ap.parse_args()
    path, n = build_ledger(args.constraint_ids or None,
                           pipeline_path=args.pipeline, output_path=args.output)
    print(f"[ledger] {n} constraint blocks -> {path}")


if __name__ == "__main__":
    main()
