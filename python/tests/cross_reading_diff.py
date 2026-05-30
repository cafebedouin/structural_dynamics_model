#!/usr/bin/env python3
"""OQ-10 (smallest useful version): cross-reading comparison for one kernel.

Given a kernel_id, discover its readings, then diff the engine's per-reading
H1 / orbit_signature / per-context computed type and classify every finding as:

  reading-ROBUST    identical across all runnable readings  -> a kernel-level fact
  reading-SPECIFIC  varies between readings                 -> a reading-bounded fact

It answers the question the per-reading .md reports could not: are the cyclopean
verdicts reading-robust, or artifacts of one reading? (altar essay's Omega_E.)

KEYS ONLY ON the per-constraint records in product_site_orbits.json. The corpus-wide
"Types: N mountain, M rope, ..." aggregate that appears identically in every .md
report is a whole-corpus summary and is deliberately never read here -- so the
Gap-B figure/ground confusion (reading report carrying corpus stats) cannot leak in.

Discovery also surfaces declared-but-unrunnable siblings (named in cs_reading_relation
with no loaded story) -- the "unrunnable readings" Gap B wanted reported.

Run:
    # ensure the export exists first (whole-corpus, one pass):
    cd ../../prolog && swipl -g "[stack],[product_site_export],run_product_export,halt" -t "halt(1)"
    cd ../python/audits
    python3 cross_reading_diff.py state_execution_authority
"""
import argparse, json, subprocess, sys
from pathlib import Path

PROLOG_DIR = (Path(__file__).resolve().parents[2] / "prolog")
EXPORT_JSON = PROLOG_DIR.parent / "outputs" / "product_site_orbits.json"


def run_swipl(goal: str) -> list[str]:
    """Load the corpus ([stack]) and run a one-shot goal; return stdout lines."""
    cmd = ["swipl", "-q", "-g", goal, "-t", "halt(1)"]
    r = subprocess.run(cmd, cwd=PROLOG_DIR, capture_output=True, text=True)
    if r.returncode not in (0, 1):
        sys.exit(f"swipl failed:\n{r.stderr}")
    return [ln.strip() for ln in r.stdout.splitlines() if ln.strip()]


def discover(kernel: str):
    """Runnable readings (have a loaded story) + declared-but-unrunnable siblings."""
    goal = (
        "['stack'], corpus_loader:load_all_testsets, "
        f"( findall(R, narrative_ontology:cs_kernel_id(R, {kernel}), Rs0) -> true ; Rs0=[] ), "
        "sort(Rs0, Rs), forall(member(R, Rs), (write('RUN '), writeln(R))), "
        "findall(Sib, ( narrative_ontology:cs_kernel_id(K, " + kernel + "), "
        "  narrative_ontology:cs_story_uid(K, U), "
        "  narrative_ontology:cs_reading_relation(U, Sib, _) ), Sib0), "
        "sort(Sib0, Sibs), forall(member(S, Sibs), (write('SIB '), writeln(S))), halt"
    )
    out = run_swipl(goal)
    runnable = [l[4:] for l in out if l.startswith("RUN ")]
    siblings = [l[4:] for l in out if l.startswith("SIB ")]
    unrunnable = [s for s in siblings if s not in runnable]
    return runnable, unrunnable


def diff(records: dict):
    """records: {reading_name: export_record}. Returns the comparison structure."""
    readings = list(records)
    # per-reading scalars
    per = {r: {"h1": records[r].get("h1"),
               "orbit": tuple(records[r].get("orbit_signature", []))}
           for r in readings}
    # per-context type, keyed by context then reading
    ctx_keys = set()
    for r in readings:
        ctx_keys |= set(records[r].get("contexts", {}))
    robust, specific = [], []
    for ck in sorted(ctx_keys):
        types = {r: records[r].get("contexts", {}).get(ck) for r in readings}
        (robust if len(set(types.values())) == 1 else specific).append((ck, types))
    return per, robust, specific


def verdict(per, robust, specific, readings):
    if len(readings) < 2:
        return ("INCOMPARABLE",
                f"only {len(readings)} runnable reading; need >=2 to compare.")
    orbits = {p["orbit"] for p in per.values()}
    if not specific and len(orbits) == 1:
        return ("COLLAPSE",
                "every context computes the same type for every reading, and all "
                "orbit_signatures are identical. The engine cannot distinguish these "
                "readings on TYPE. Either genuine convergence, or one reading named "
                "many times. Cross-check the authored epsilon / beneficiary in the .pl: "
                "if those differ but type collapses, the intended differentiation is "
                "not surviving into the computed type (the index-mismatch wall).")
    if robust and specific:
        return ("ROBUST CORE + SPECIFIC EDGES",
                f"{len(robust)} contexts agree across all readings (kernel-level / "
                f"reading-robust) and {len(specific)} diverge (reading-specific). "
                "The robust set is what you may state about the KERNEL; the specific "
                "set must be attributed to its reading.")
    return ("STRONG DIFFERENTIATION",
            f"{len(specific)} contexts diverge, {len(robust)} agree. The kernel "
            "genuinely fractures across readings; almost nothing is reading-robust.")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("kernel", help="kernel_id, e.g. state_execution_authority")
    ap.add_argument("--json", default=str(EXPORT_JSON), help="export JSON path")
    ap.add_argument("--examples", type=int, default=4, help="divergent contexts to print")
    a = ap.parse_args()

    runnable, unrunnable = discover(a.kernel)
    data = json.loads(Path(a.json).read_text())
    records = {r: data[r] for r in runnable if r in data}
    missing = [r for r in runnable if r not in data]

    print(f"\nKERNEL: {a.kernel}")
    print(f"  runnable readings in export : {list(records) or '(none)'}")
    if missing:
        print(f"  tagged but absent from JSON : {missing}  (re-run the export)")
    if unrunnable:
        print(f"  declared, NO runnable story : {unrunnable}  (Gap-B unrunnable readings)")

    if len(records) < 2:
        print("\n  Need >=2 runnable readings to diff. Author/load the siblings first.\n")
        return

    per, robust, specific = diff(records)
    print("\n  per-reading orbit (computed type space):")
    for r, p in per.items():
        print(f"    {r:32s} H1={p['h1']}  orbit={list(p['orbit'])}")

    tag, msg = verdict(per, robust, specific, list(records))
    print(f"\n  VERDICT: {tag}\n  {msg}")

    if specific:
        print(f"\n  reading-SPECIFIC contexts (showing {min(a.examples, len(specific))}):")
        for ck, types in specific[: a.examples]:
            print(f"    {ck}: " + ", ".join(f"{r}={t}" for r, t in types.items()))
        print("\n  (in-engine cross-check: these should match "
              f"cs_kernel_divergence({a.kernel}, Ctx, _, _).)")
    print()


if __name__ == "__main__":
    main()
