#!/usr/bin/env python3
"""Pipeline orchestrator — pure-Python replacement for `make -j4 quick`.

Runs the full DR analysis pipeline (Prolog analyses, Python reports,
enrichment) with structured error reporting and parallel execution.

Usage:
    python3 python/run_pipeline.py          # standalone
    from run_pipeline import run_pipeline   # as library
"""

import contextlib
import io
import subprocess
import sys
import threading
import time
from concurrent.futures import ThreadPoolExecutor, as_completed
from dataclasses import dataclass, field
from pathlib import Path
from typing import Callable, Optional

# ---------------------------------------------------------------------------
# Path constants
# ---------------------------------------------------------------------------

REPO_ROOT = Path(__file__).resolve().parent.parent
PROLOG_DIR = REPO_ROOT / "prolog"
OUTPUTS_DIR = REPO_ROOT / "outputs"
TESTSETS_DIR = PROLOG_DIR / "testsets"

# Ensure sibling modules are importable
if str(Path(__file__).resolve().parent) not in sys.path:
    sys.path.insert(0, str(Path(__file__).resolve().parent))

# ---------------------------------------------------------------------------
# Data classes
# ---------------------------------------------------------------------------

@dataclass
class StepResult:
    name: str
    status: str = "ok"       # "ok" | "error" | "skipped"
    duration_s: float = 0.0
    error: str = ""


@dataclass
class PipelineResult:
    steps: list[StepResult] = field(default_factory=list)
    total_duration_s: float = 0.0
    errors: list[str] = field(default_factory=list)


# ---------------------------------------------------------------------------
# Exceptions
# ---------------------------------------------------------------------------

class PrologError(RuntimeError):
    """Raised when a Prolog subprocess exits non-zero."""


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def run_prolog(modules: list[str], goal: str, timeout: int = 300) -> subprocess.CompletedProcess:
    """Run a SWI-Prolog command and return the CompletedProcess.

    Args:
        modules: List of .pl files to load via -l flags.
        goal: Prolog goal string (without trailing halt).
        timeout: Subprocess timeout in seconds.

    Returns:
        subprocess.CompletedProcess with captured stdout/stderr.

    Raises:
        PrologError: On non-zero exit code.
    """
    cmd = ["swipl"]
    for mod in modules:
        cmd.extend(["-l", mod])
    cmd.extend(["-g", f"{goal}, halt."])

    result = subprocess.run(
        cmd,
        cwd=str(PROLOG_DIR),
        capture_output=True,
        text=True,
        timeout=timeout,
    )
    if result.returncode != 0:
        raise PrologError(
            f"Prolog goal '{goal}' failed (rc={result.returncode}): "
            f"{result.stderr[:300]}"
        )
    return result


# Preamble markers for reports that produce *_raw output
_PREAMBLE_MARKERS = {
    "orbit":      "<!-- ORBIT_REPORT_START -->",
    "fpn":        "<!-- FPN_REPORT_START -->",
    "maxent":     "<!-- MAXENT_REPORT_START -->",
    "abductive":  "<!-- ABDUCTIVE_REPORT_START -->",
    "trajectory": "<!-- TRAJECTORY_REPORT_START -->",
}


def strip_preamble(text: str, marker: str) -> str:
    """Extract content after *marker* line (mirrors sed -n + tail -n +2)."""
    idx = text.find(marker)
    if idx == -1:
        return text
    # Skip marker line itself
    newline = text.find("\n", idx)
    if newline == -1:
        return ""
    return text[newline + 1:]


def _run_step(name: str, fn, progress) -> StepResult:
    """Run *fn* inside a try/except and return a StepResult."""
    t0 = time.time()
    try:
        fn()
        return StepResult(name=name, status="ok", duration_s=time.time() - t0)
    except Exception as e:
        msg = f"{name}: {e}"
        if progress:
            progress("pipeline", f"[WARN] {msg} (non-critical, continuing)")
        return StepResult(name=name, status="error", duration_s=time.time() - t0, error=str(e))


def _run_parallel(tasks: list[tuple[str, callable]], progress, parallel: int) -> list[StepResult]:
    """Run [(name, fn), ...] in a ThreadPoolExecutor, return StepResults."""
    results = []
    with ThreadPoolExecutor(max_workers=parallel) as pool:
        futures = {
            pool.submit(_run_step, name, fn, progress): name
            for name, fn in tasks
        }
        for future in as_completed(futures):
            results.append(future.result())
    return results


# ---------------------------------------------------------------------------
# Phase 1: PREP
# ---------------------------------------------------------------------------

def _phase_prep(progress):
    """Generate domain registry and test suite."""
    import domain_priors
    import python_test_suite

    if progress:
        progress("pipeline", "[PREP] Preparing test suite...")

    domain_priors.generate_domain_registry(str(TESTSETS_DIR), str(PROLOG_DIR / "domain_registry.pl"))
    python_test_suite.build_suite()

    if progress:
        progress("pipeline", "[PREP] Done.")


# ---------------------------------------------------------------------------
# Phase 2: PROLOG ANALYSES (parallel)
# ---------------------------------------------------------------------------

def _prolog_validation():
    """Run validation_suite → output.txt."""
    OUTPUTS_DIR.mkdir(parents=True, exist_ok=True)
    header = f"Initializing Validation Suite - {time.strftime('%c')}\n"
    header += "------------------------------------------\n"

    try:
        result = subprocess.run(
            ["swipl", "-g", "[validation_suite], run_dynamic_suite, halt."],
            cwd=str(PROLOG_DIR),
            capture_output=True,
            text=True,
            timeout=300,
        )
        body = result.stdout
    except Exception as e:
        body = f"[WARN] validation_suite failed: {e}\n"

    footer = "------------------------------------------\n"
    footer += f"Test suite completed at: {time.strftime('%c')}\n"

    (OUTPUTS_DIR / "output.txt").write_text(header + body + footer, encoding="utf-8")


def _prolog_fingerprint():
    """Run fingerprint_report → fingerprint_report.md."""
    result = subprocess.run(
        ["swipl", "-g", "[fingerprint_report], halt."],
        cwd=str(PROLOG_DIR),
        capture_output=True,
        text=True,
        timeout=300,
    )
    (OUTPUTS_DIR / "fingerprint_report.md").write_text(result.stdout, encoding="utf-8")


def _prolog_orbit():
    """Run orbit_report → orbit_report.md + orbit_data.json (sidecar)."""
    result = run_prolog(
        ["stack.pl", "covering_analysis.pl", "dirac_classification.pl", "orbit_report.pl"],
        "run_orbit_report",
    )
    raw = result.stdout
    if raw.strip():
        cleaned = strip_preamble(raw, _PREAMBLE_MARKERS["orbit"])
        (OUTPUTS_DIR / "orbit_report.md").write_text(cleaned, encoding="utf-8")
    else:
        (OUTPUTS_DIR / "orbit_report.md").write_text("", encoding="utf-8")


def _prolog_fpn():
    """Run fpn_report → fpn_report.md."""
    result = run_prolog(
        ["stack.pl", "covering_analysis.pl", "fpn_report.pl"],
        "run_fpn_report",
    )
    raw = result.stdout
    if raw.strip():
        cleaned = strip_preamble(raw, _PREAMBLE_MARKERS["fpn"])
        (OUTPUTS_DIR / "fpn_report.md").write_text(cleaned, encoding="utf-8")
    else:
        (OUTPUTS_DIR / "fpn_report.md").write_text("", encoding="utf-8")


def _prolog_maxent():
    """Run maxent_report → maxent_report.md."""
    result = run_prolog(
        ["stack.pl", "covering_analysis.pl", "dirac_classification.pl",
         "maxent_classifier.pl", "maxent_report.pl"],
        "run_maxent_report",
    )
    raw = result.stdout
    if raw.strip():
        cleaned = strip_preamble(raw, _PREAMBLE_MARKERS["maxent"])
        (OUTPUTS_DIR / "maxent_report.md").write_text(cleaned, encoding="utf-8")
    else:
        (OUTPUTS_DIR / "maxent_report.md").write_text("", encoding="utf-8")


def _prolog_abductive():
    """Run abductive_report → abductive_report.md + abductive_data.json (sidecar)."""
    result = run_prolog(
        ["stack.pl", "covering_analysis.pl", "dirac_classification.pl",
         "maxent_classifier.pl", "abductive_engine.pl", "abductive_report.pl"],
        "run_abductive_report",
    )
    raw = result.stdout
    if raw.strip():
        cleaned = strip_preamble(raw, _PREAMBLE_MARKERS["abductive"])
        (OUTPUTS_DIR / "abductive_report.md").write_text(cleaned, encoding="utf-8")
    else:
        (OUTPUTS_DIR / "abductive_report.md").write_text("", encoding="utf-8")


def _prolog_trajectory():
    """Run trajectory_report (conditional) → trajectory_report.md."""
    # Check if trajectory is enabled
    try:
        check = subprocess.run(
            ["swipl", "-g",
             "use_module(config), (config:param(trajectory_enabled, 1) -> write(1) ; write(0)), halt."],
            cwd=str(PROLOG_DIR),
            capture_output=True,
            text=True,
            timeout=30,
        )
        enabled = check.stdout.strip() == "1"
    except Exception:
        enabled = False

    if not enabled:
        (OUTPUTS_DIR / "trajectory_report.md").write_text("", encoding="utf-8")
        return

    result = run_prolog(
        ["stack.pl", "covering_analysis.pl", "dirac_classification.pl",
         "maxent_classifier.pl", "trajectory_mining.pl", "trajectory_report.pl"],
        "run_trajectory_report",
    )
    raw = result.stdout
    if raw.strip():
        cleaned = strip_preamble(raw, _PREAMBLE_MARKERS["trajectory"])
        (OUTPUTS_DIR / "trajectory_report.md").write_text(cleaned, encoding="utf-8")
    else:
        (OUTPUTS_DIR / "trajectory_report.md").write_text("", encoding="utf-8")


def _prolog_covering():
    """Run covering_analysis → covering_analysis.md."""
    result = run_prolog(
        ["stack.pl", "covering_analysis.pl"],
        "run_covering_analysis",
    )
    (OUTPUTS_DIR / "covering_analysis.md").write_text(result.stdout, encoding="utf-8")


def _prolog_giant_comp():
    """Run giant_component_analysis → giant_component_analysis.md."""
    result = run_prolog(
        ["stack.pl", "giant_component_analysis.pl"],
        "run_giant_component_analysis",
    )
    (OUTPUTS_DIR / "giant_component_analysis.md").write_text(result.stdout, encoding="utf-8")


def _prolog_coupling():
    """Run coupling_protocol → coupling_protocol.md."""
    result = run_prolog(
        ["stack.pl", "covering_analysis.pl", "inferred_coupling_protocol.pl"],
        "run_coupling_protocol",
    )
    (OUTPUTS_DIR / "coupling_protocol.md").write_text(result.stdout, encoding="utf-8")


def _prolog_maxent_diag():
    """Run maxent_diagnostic → maxent_diagnostic_report.md."""
    result = run_prolog(
        ["stack.pl", "covering_analysis.pl", "maxent_classifier.pl",
         "dirac_classification.pl", "maxent_diagnostic.pl"],
        "run_maxent_diagnostic",
    )
    (OUTPUTS_DIR / "maxent_diagnostic_report.md").write_text(result.stdout, encoding="utf-8")


def _phase_prolog(progress, parallel):
    """Phase 2: run all Prolog analyses in parallel."""
    if progress:
        progress("pipeline", "[PROLOG] Running analyses...")

    tasks = [
        ("validation",  _prolog_validation),
        ("fingerprint", _prolog_fingerprint),
        ("orbit",       _prolog_orbit),
        ("fpn",         _prolog_fpn),
        ("maxent",      _prolog_maxent),
        ("abductive",   _prolog_abductive),
        ("trajectory",  _prolog_trajectory),
        ("covering",    _prolog_covering),
        ("giant_comp",  _prolog_giant_comp),
        ("coupling",    _prolog_coupling),
        ("maxent_diag", _prolog_maxent_diag),
    ]
    results = _run_parallel(tasks, progress, parallel)

    if progress:
        ok = sum(1 for r in results if r.status == "ok")
        progress("pipeline", f"[PROLOG] Done ({ok}/{len(results)} succeeded).")

    return results


# ---------------------------------------------------------------------------
# Phase 3: POST-PROLOG (sequential)
# ---------------------------------------------------------------------------

def _phase_post_prolog(progress):
    """Orbit normalization + JSON report generation."""
    results = []

    # Orbit normalization
    def _orbit_norm():
        if (OUTPUTS_DIR / "orbit_data.json").exists():
            import normalize_orbit_ids
            normalize_orbit_ids.normalize()

    if progress:
        progress("pipeline", "[NORM] Normalizing orbit data IDs...")
    results.append(_run_step("orbit_norm", _orbit_norm, progress))

    # JSON report (depends on abductive_data.json existing)
    def _json_report():
        run_prolog(
            ["stack.pl", "covering_analysis.pl", "maxent_classifier.pl",
             "dirac_classification.pl", "diagnostic_summary.pl",
             "post_synthesis.pl", "json_report.pl"],
            "run_json_report",
        )

    if progress:
        progress("pipeline", "[JSON] Generating structured JSON report...")
    results.append(_run_step("json_report", _json_report, progress))

    return results


# ---------------------------------------------------------------------------
# Phase 4: PYTHON TIER 1 (parallel)
# ---------------------------------------------------------------------------

def _phase_python_tier1(progress, parallel):
    """Type reports, omega, corpus extraction, meta report."""
    if progress:
        progress("pipeline", "[TIER1] Running Python tier 1...")

    import type_reporter
    from orbit_utils import load_orbit_data

    pipeline_data = type_reporter.load_pipeline_data()
    orbit_data = load_orbit_data()

    TYPE_KEYS = ["snare", "rope", "scaffold", "piton", "mountain",
                 "tangled_rope", "false_mountain"]

    def _make_type_fn(key, pdata, odata):
        def fn():
            type_reporter.run_type_report(key, pdata, odata)
        return fn

    tasks = []
    for key in TYPE_KEYS:
        tasks.append((f"type_{key}", _make_type_fn(key, pipeline_data, orbit_data)))

    # Omega reporter
    def _omega():
        import omega_reporter
        omega_reporter.main()
    tasks.append(("omega", _omega))

    # Corpus extraction
    def _corpus():
        from extract_corpus_data import CorpusExtractor
        extractor = CorpusExtractor(str(OUTPUTS_DIR / "output.txt"))
        extractor.extract_all()
        extractor.save_json(str(OUTPUTS_DIR / "corpus_data.json"))
    tasks.append(("corpus_extract", _corpus))

    # Meta reporter (prints to stdout — capture)
    def _meta():
        import meta_reporter
        buf = io.StringIO()
        with contextlib.redirect_stdout(buf):
            reporter = meta_reporter.MetaReporter()
            reporter.parse()
            reporter.generate_report()
        (OUTPUTS_DIR / "meta_report.txt").write_text(buf.getvalue(), encoding="utf-8")
    tasks.append(("meta_report", _meta))

    results = _run_parallel(tasks, progress, parallel)

    if progress:
        ok = sum(1 for r in results if r.status == "ok")
        progress("pipeline", f"[TIER1] Done ({ok}/{len(results)} succeeded).")

    return results


# ---------------------------------------------------------------------------
# Phase 5: PYTHON TIER 2 (parallel)
# ---------------------------------------------------------------------------

def _phase_python_tier2(progress, parallel):
    """Variance, pattern mining, sufficiency — depend on corpus_data.json."""
    if progress:
        progress("pipeline", "[TIER2] Running Python tier 2...")

    corpus_path = str(OUTPUTS_DIR / "corpus_data.json")
    pipeline_path = str(OUTPUTS_DIR / "pipeline_output.json")

    def _variance():
        from variance_analyzer import VarianceAnalyzer
        analyzer = VarianceAnalyzer(corpus_path)
        analyzer.generate_report(str(OUTPUTS_DIR / "variance_analysis.md"))

    def _pattern():
        from pattern_miner import PatternMiner
        miner = PatternMiner(corpus_path)
        miner.generate_report(str(OUTPUTS_DIR / "pattern_mining.md"))

    def _sufficiency():
        from sufficiency_tester import SufficiencyTester
        tester = SufficiencyTester(corpus_path, pipeline_path)
        tester.generate_report(
            str(OUTPUTS_DIR / "index_sufficiency.md"),
            str(OUTPUTS_DIR / "index_sufficiency.json"),
        )

    tasks = [
        ("variance",    _variance),
        ("pattern",     _pattern),
        ("sufficiency", _sufficiency),
    ]
    results = _run_parallel(tasks, progress, parallel)

    if progress:
        ok = sum(1 for r in results if r.status == "ok")
        progress("pipeline", f"[TIER2] Done ({ok}/{len(results)} succeeded).")

    return results


# ---------------------------------------------------------------------------
# Phase 6: ENRICHMENT (sequential)
# ---------------------------------------------------------------------------

def _phase_enrichment(progress):
    """Produce enriched_pipeline.json."""
    if progress:
        progress("pipeline", "[ENRICH] Producing enriched_pipeline.json...")

    def _enrich():
        import enrich_pipeline_json
        enrich_pipeline_json.main()

    result = _run_step("enrich_pipeline", _enrich, progress)
    return [result]


# ---------------------------------------------------------------------------
# Phase 7: PYTHON TIER 3 (parallel)
# ---------------------------------------------------------------------------

_ARGV_LOCK = threading.Lock()


def _phase_python_tier3(progress, parallel):
    """Tangled decomposition, classification confidence, boundary normality,
    boolean independence, institutional dissent."""
    if progress:
        progress("pipeline", "[TIER3] Running Python tier 3...")

    def _tangled():
        import tangled_decomposition
        with _ARGV_LOCK:
            saved = sys.argv
            sys.argv = ["tangled_decomposition.py"]
            try:
                tangled_decomposition.main()
            finally:
                sys.argv = saved

    def _confidence():
        import classification_confidence
        classification_confidence.main()

    def _boundary():
        import boundary_normality
        boundary_normality.main()

    def _boolean():
        import boolean_independence
        boolean_independence.main()

    def _dissent():
        import institutional_dissent_analysis
        institutional_dissent_analysis.main()

    tasks = [
        ("tangled_decomposition",      _tangled),
        ("classification_confidence",  _confidence),
        ("boundary_normality",         _boundary),
        ("boolean_independence",       _boolean),
        ("institutional_dissent",      _dissent),
    ]
    results = _run_parallel(tasks, progress, parallel)

    if progress:
        ok = sum(1 for r in results if r.status == "ok")
        progress("pipeline", f"[TIER3] Done ({ok}/{len(results)} succeeded).")

    return results


# ---------------------------------------------------------------------------
# Phase 8: OMEGA ENRICHMENT (sequential)
# ---------------------------------------------------------------------------

def _phase_omega_enrichment(progress):
    """Enrich omega report — conditional on corpus_data.json + omega_data.json."""
    if progress:
        progress("pipeline", "[ENRICH] Enriching omega report...")

    corpus_exists = (OUTPUTS_DIR / "corpus_data.json").exists()
    omega_exists = (OUTPUTS_DIR / "omega_data.json").exists()

    if not (corpus_exists and omega_exists):
        if progress:
            progress("pipeline", "[ENRICH] Skipping omega enrichment (missing dependencies).")
        return [StepResult(name="omega_enrich", status="skipped")]

    def _enrich():
        import omega_enricher
        omega_enricher.main()

    result = _run_step("omega_enrich", _enrich, progress)
    return [result]


# ---------------------------------------------------------------------------
# Main pipeline
# ---------------------------------------------------------------------------

def run_pipeline(
    progress: Optional[Callable[[str, str], None]] = None,
    parallel: int = 4,
) -> PipelineResult:
    """Execute the full DR analysis pipeline.

    Args:
        progress: Optional callback(step, message) for status updates.
        parallel: Max concurrent threads for parallel phases.

    Returns:
        PipelineResult with per-step results and any errors.
    """
    pipeline_result = PipelineResult()
    t0 = time.time()

    OUTPUTS_DIR.mkdir(parents=True, exist_ok=True)

    def collect(step_results):
        if isinstance(step_results, list):
            for sr in step_results:
                pipeline_result.steps.append(sr)
                if sr.status == "error":
                    pipeline_result.errors.append(sr.error)
        else:
            pipeline_result.steps.append(step_results)
            if step_results.status == "error":
                pipeline_result.errors.append(step_results.error)

    # Phase 1: PREP
    collect(_run_step("prep", lambda: _phase_prep(progress), progress))

    # Phase 2: PROLOG ANALYSES (parallel)
    collect(_phase_prolog(progress, parallel))

    # Phase 3: POST-PROLOG (sequential)
    collect(_phase_post_prolog(progress))

    # Abort if pipeline_output.json was not produced
    if not (OUTPUTS_DIR / "pipeline_output.json").exists():
        msg = "pipeline_output.json not produced — aborting downstream phases"
        if progress:
            progress("pipeline", f"[FATAL] {msg}")
        pipeline_result.errors.append(msg)
        pipeline_result.total_duration_s = time.time() - t0
        return pipeline_result

    # Phase 4: PYTHON TIER 1 (parallel)
    collect(_phase_python_tier1(progress, parallel))

    # Phase 5: PYTHON TIER 2 (parallel) — depends on corpus_data.json
    if (OUTPUTS_DIR / "corpus_data.json").exists():
        collect(_phase_python_tier2(progress, parallel))
    else:
        if progress:
            progress("pipeline", "[TIER2] Skipping (corpus_data.json missing).")

    # Phase 6: ENRICHMENT (sequential)
    collect(_phase_enrichment(progress))

    # Phase 7: PYTHON TIER 3 (parallel) — depends on enriched_pipeline.json + corpus_data.json
    if (OUTPUTS_DIR / "enriched_pipeline.json").exists():
        collect(_phase_python_tier3(progress, parallel))
    else:
        if progress:
            progress("pipeline", "[TIER3] Skipping (enriched_pipeline.json missing).")

    # Phase 8: OMEGA ENRICHMENT (sequential)
    collect(_phase_omega_enrichment(progress))

    pipeline_result.total_duration_s = time.time() - t0

    if progress:
        ok = sum(1 for s in pipeline_result.steps if s.status == "ok")
        total = len(pipeline_result.steps)
        progress("pipeline",
                 f"Pipeline complete: {ok}/{total} steps OK in "
                 f"{pipeline_result.total_duration_s:.1f}s")

    return pipeline_result


# ---------------------------------------------------------------------------
# CLI entry point
# ---------------------------------------------------------------------------

if __name__ == "__main__":
    def _cli_progress(step, msg):
        print(f"[{step}] {msg}")

    result = run_pipeline(progress=_cli_progress)

    print("\n" + "=" * 60)
    print("PIPELINE SUMMARY")
    print("=" * 60)
    for s in result.steps:
        dur = f" [{s.duration_s:.1f}s]" if s.duration_s else ""
        err = f"  error: {s.error[:200]}" if s.error else ""
        print(f"  {s.name:30s} {s.status:8s}{dur}")
        if err:
            print(err)
    print(f"\n  Total time: {result.total_duration_s:.1f}s")
    if result.errors:
        print(f"  Errors: {len(result.errors)}")
        for e in result.errors:
            print(f"    - {e[:200]}")

    # Exit non-zero only if the critical json_report step failed
    critical_failed = any(
        s.status == "error" and s.name == "json_report"
        for s in result.steps
    )
    sys.exit(1 if critical_failed else 0)
