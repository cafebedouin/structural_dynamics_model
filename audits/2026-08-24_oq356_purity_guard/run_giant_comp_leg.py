#!/usr/bin/env python3
"""OQ-356 — run giant_component_analysis on one leg and PERSIST its report.

WHY THIS EXISTS RATHER THAN `report_legs.py --stages giant_comp`. The OQ-352
driver refuses the report stages when a leg's `pipeline_output.<leg>.json` was
produced at a code_commit that differs from HEAD by engine-relevant files
(MISSING_CLASSIFY_OUTPUT). That refusal is correct and is NOT bypassed here:
`giant_comp` reads the corpus and the engine directly and consumes no classify
artifact (the OQ-352 census proves it — it ran all 20 legs this exact way), so
re-running classify on every leg would be a large spend to satisfy a guard over
an input this stage does not read.

The invocation below is the OQ-352 census's invocation VERBATIM
(giant_comp_leg_census.py), with one difference: the census captured stdout to a
pipe and dropped it, which is precisely why no Phase-3 contamination number has
ever been persisted. Here stdout goes to a named file.

Being invocation-stable across commits matters: criterion 4's discrimination
record is `fires at N, declines at N-1` on the SAME invocation, so this script
must not change between the pre-fix and post-fix halves.
"""
import subprocess, sys, time
from pathlib import Path

sys.path.insert(0, "python")
import run_pipeline as R


def main():
    if len(sys.argv) < 3:
        print("usage: run_giant_comp_leg.py <leg> <out.md> [timeout_s]")
        return 2
    leg, out = sys.argv[1], Path(sys.argv[2])
    timeout = int(sys.argv[3]) if len(sys.argv) > 3 else 2400

    d = R._resolve_corpus_dir(leg)
    n = len(list(d.glob("*.pl"))) if d.exists() else 0
    if n == 0:
        print("ABORT: leg %s resolves to %s with 0 .pl files" % (leg, d))
        return 2

    goal = (f"{R._leg_overlay(leg)}"
            "catch_with_backtrace(run_giant_component_analysis, E, "
            "(print_message(error,E), halt(2)))")
    t0 = time.time()
    p = subprocess.run(["swipl", "-l", "stack.pl", "-l", "giant_component_analysis.pl",
                        "-g", goal + ", halt."], cwd=str(R.PROLOG_DIR),
                       capture_output=True, text=True, timeout=timeout)
    dt = time.time() - t0
    out.parent.mkdir(parents=True, exist_ok=True)
    out.write_text(p.stdout, encoding="utf-8")
    Path(str(out) + ".stderr.txt").write_text(p.stderr, encoding="utf-8")

    secs = [l for l in p.stdout.splitlines() if l.startswith("###")]
    print("leg=%s  n=%d  %.1fs  rc=%d  %s  last_section=%s"
          % (leg, n, dt, p.returncode, "OK" if p.returncode == 0 else "THROW",
             secs[-1] if secs else "-"))
    print("stdout -> %s (%d bytes)" % (out, len(p.stdout)))
    for line in p.stderr.splitlines():
        if "not a function" in line or "ERROR" in line:
            print("  " + line.strip()[:120]); break
    return p.returncode


if __name__ == "__main__":
    sys.exit(main())
