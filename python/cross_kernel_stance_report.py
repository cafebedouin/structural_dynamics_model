#!/usr/bin/env python3
"""cross_kernel_stance_report.py — the cross-kernel reading-stance transpose consumer
(GAP-04 / OQ-53, first increment).

The within-kernel half (cs_kernel_registry: cs_readings_for_kernel/2,
cs_kernel_divergence/4, compare_kernel_readings/3) holds a KERNEL fixed and sweeps
its readings. This is the consumer for the TRANSPOSE: hold a reading STANCE fixed
and sweep it ACROSS kernels, then report whether the stance has a kernel-independent
structural signature (convergent) or is kernel-dependent (divergent — the finding).

It runs the Prolog transpose (cs_kernel_registry:cross_kernel_stance_export/1) over
each LIVE TWIN corpus (testsets_haiku, testsets_flash) and reads the COMPUTED
fingerprint_shift vectors back. It does NOT recompute classify_at_power in Python —
the engine computes the shifts; this consumer only reads, combines, and renders
(Build-Discipline Pattern 1: wire the consumer to the producer's output).

The verdict carries cohort PROVENANCE (morphology-suggested vs hand-declared per
member): the cohort is a DECLARED seat (declared_stance/2), not a morphology rule, so
where a cohort is partly curated, "convergent" is partly a finding about which
readings were admitted, not purely about structure (Seat-Theorem Cor 2b / GAP-04).

Outputs:
  outputs/cross_kernel_stance.json  — combined per-corpus stance profiles (machine)
  outputs/cross_kernel_stance.md    — short markdown summary per stance per corpus

Usage:
    python3 python/cross_kernel_stance_report.py
    python3 python/cross_kernel_stance_report.py --corpora testsets_haiku testsets_flash
"""
import argparse
import json
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
PROLOG = ROOT / "prolog"
OUTPUTS = ROOT / "outputs"

DEFAULT_CORPORA = ["testsets_haiku", "testsets_flash"]


def run_transpose(corpus, out_path):
    """Run the Prolog transpose over `corpus`, writing the export JSON to out_path.

    [stack] first, THEN overlay corpus_path with retractall-then-assertz so the
    default param(corpus_path, testsets) is replaced rather than shadowed
    (CLAUDE.md Corpus Loading: a plain assertz appends after the default and is
    silently ignored). cross_kernel_stance_export/1 reads the COMPUTED shifts.
    """
    out_posix = out_path.as_posix()
    goal = (
        "[stack], "
        "retractall(config:param(corpus_path,_)), "
        f"assertz(config:param(corpus_path, {corpus})), "
        "corpus_loader:load_all_testsets, "
        f"cs_kernel_registry:cross_kernel_stance_export('{out_posix}'), "
        "halt"
    )
    proc = subprocess.run(
        ["swipl", "-g", goal, "-t", "halt(1)"],
        cwd=PROLOG, capture_output=True, text=True, timeout=600,
    )
    if proc.returncode != 0 or not out_path.exists():
        sys.stderr.write(proc.stderr)
        raise RuntimeError(
            f"transpose export failed for {corpus} (exit {proc.returncode})"
        )
    with out_path.open() as fh:
        data = json.load(fh)
    return data.get("stances", [])


def fmt_shift(shift):
    """Render a shift vector (list of 4 type strings) or null."""
    if not shift:
        return "—"
    return "/".join(shift)


def render_markdown(combined):
    lines = []
    lines.append("# Cross-kernel reading-stance transpose (GAP-04 / OQ-53)")
    lines.append("")
    lines.append(
        "Each stance is held fixed and swept across kernels. The signature is the "
        "kernel-independent `fingerprint_shift` 4-seat vector "
        "`[powerless, moderate, institutional, analytical]`, computed by the engine "
        "(this consumer does not recompute it). The cohort is a **declared seat** "
        "(`declared_stance/2`); morphology only suggested candidates, so each member "
        "carries its provenance — *morphology-suggested* vs *hand-declared*. Where a "
        "cohort is partly curated, a convergent verdict is partly a finding about "
        "which readings were admitted."
    )
    lines.append("")
    lines.append(
        "`consensus` is the per-position majority signature (`*` = no majority at "
        "that position). **convergent** = a majority of the cohort shares the "
        "consensus; **divergent** = it does not (the stance is kernel-dependent — a "
        "finding)."
    )
    lines.append("")
    for corpus, stances in combined["corpora"].items():
        lines.append(f"## {corpus}")
        lines.append("")
        for s in stances:
            prov = s["provenance"]
            lines.append(
                f"### {s['stance']}  —  **{s['verdict']}** "
                f"({s['verdict_reason']})"
            )
            lines.append("")
            lines.append(
                f"- N = {s['n']} across kernels; consensus "
                f"`{fmt_shift(s['consensus'])}` ({s['n_fixed']} fixed positions); "
                f"convergent {s['n_convergent']} / divergent {s['n_divergent']}"
            )
            lines.append(
                f"- cohort provenance: {prov['morphology_suggested']} "
                f"morphology-suggested, {prov['hand_declared']} hand-declared"
            )
            lines.append("")
            lines.append("| reading | kernel | shift | provenance |")
            lines.append("|---|---|---|---|")
            for m in s["members"]:
                lines.append(
                    f"| `{m['reading']}` | {m['kernel']} | "
                    f"`{fmt_shift(m['shift'])}` | {m['provenance']} |"
                )
            lines.append("")
            if s["outliers"]:
                outs = ", ".join(
                    f"`{o['reading']}` ({fmt_shift(o['shift'])})"
                    for o in s["outliers"]
                )
                lines.append(f"- **divergent members:** {outs}")
                lines.append("")
    return "\n".join(lines) + "\n"


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument(
        "--corpora", nargs="+", default=DEFAULT_CORPORA,
        help="corpus_path values to run the transpose over (default: the live twins)",
    )
    args = ap.parse_args()

    OUTPUTS.mkdir(exist_ok=True)
    combined = {"generated_over": args.corpora, "corpora": {}}
    for corpus in args.corpora:
        per_corpus_out = OUTPUTS / f"cross_kernel_stance_{corpus}.json"
        stances = run_transpose(corpus, per_corpus_out)
        combined["corpora"][corpus] = stances
        n_conv = sum(1 for s in stances if s["verdict"] == "convergent")
        print(
            f"[{corpus}] {len(stances)} stances "
            f"({n_conv} convergent, {len(stances) - n_conv} not)"
        )

    json_out = OUTPUTS / "cross_kernel_stance.json"
    with json_out.open("w") as fh:
        json.dump(combined, fh, indent=2, sort_keys=True)
    md_out = OUTPUTS / "cross_kernel_stance.md"
    md_out.write_text(render_markdown(combined))
    print(f"wrote {json_out.relative_to(ROOT)} and {md_out.relative_to(ROOT)}")


if __name__ == "__main__":
    main()
