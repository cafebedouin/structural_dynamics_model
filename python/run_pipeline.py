#!/usr/bin/env python3
"""Pipeline orchestrator — runs the full DR analysis pipeline.

Runs the full DR analysis pipeline (Prolog analyses, Python reports,
enrichment) with structured error reporting and parallel execution.

Usage:
    python3 python/run_pipeline.py          # standalone
    from run_pipeline import run_pipeline   # as library
"""

import contextlib
import hashlib
import io
import json
import subprocess
import sys
import threading
import time
from concurrent.futures import ThreadPoolExecutor, as_completed
from dataclasses import dataclass, field
from datetime import datetime, timezone
from pathlib import Path
from typing import Callable, Optional

# ---------------------------------------------------------------------------
# Path constants
# ---------------------------------------------------------------------------

REPO_ROOT = Path(__file__).resolve().parent.parent
PROLOG_DIR = REPO_ROOT / "prolog"
OUTPUTS_DIR = REPO_ROOT / "outputs"
TESTSETS_DIR = PROLOG_DIR / "testsets"
TESTSETS_SOTU_DIR = PROLOG_DIR / "testsets_sotu"

# Ensure sibling modules are importable
if str(Path(__file__).resolve().parent) not in sys.path:
    sys.path.insert(0, str(Path(__file__).resolve().parent))

# ---------------------------------------------------------------------------
# Manifest helpers
# ---------------------------------------------------------------------------

def _git_head_sha() -> str:
    try:
        result = subprocess.run(
            ["git", "rev-parse", "HEAD"],
            cwd=str(REPO_ROOT),
            capture_output=True,
            text=True,
            timeout=10,
        )
        return result.stdout.strip() if result.returncode == 0 else "unknown"
    except Exception:
        return "unknown"


def _git_dirty() -> bool:
    try:
        result = subprocess.run(
            ["git", "status", "--porcelain"],
            cwd=str(REPO_ROOT),
            capture_output=True,
            text=True,
            timeout=10,
        )
        return bool(result.stdout.strip()) if result.returncode == 0 else False
    except Exception:
        return False


def _compute_corpus_hash(testsets_dir: Path) -> str:
    """sha256 of sorted (filename, file_content) pairs — corpus identity fingerprint.

    Detects membership changes (add/remove testset) AND in-place content edits.
    Does not detect changes in subdirectories (testsets/<run_tag>/ archive).
    See OQ-29 for known limits.
    """
    pairs = []
    for p in sorted(testsets_dir.glob("*.pl")):
        pairs.append(p.name + "\n" + p.read_text(encoding="utf-8", errors="replace"))
    return hashlib.sha256("\n---\n".join(pairs).encode()).hexdigest()[:12]



def check_orbits_corpus_hash(orbits_path: Path) -> None:
    """Raise RuntimeError if orbits file exists but lacks corpus_hash.

    The corpus_hash must be stamped atomically with orbit generation
    (via regenerate_orbits.py). A missing hash means the file was produced
    by the old two-step path and is unverifiable for staleness.
    """
    if orbits_path.exists():
        data = json.loads(orbits_path.read_text(encoding="utf-8"))
        if "corpus_hash" not in data:
            raise RuntimeError(
                "product_site_orbits.json has no corpus_hash — "
                "run 'python3 python/sweeps/regenerate_orbits.py' before the pipeline."
            )


def build_manifest(run_at: str, testsets_dir: Path = TESTSETS_DIR) -> dict:
    """Build the manifest dict for a pipeline run.

    Args:
        run_at: ISO 8601 UTC timestamp string captured at pipeline start.
        testsets_dir: corpus directory to count (default TESTSETS_DIR). When a
            NON-default corpus is classified (classify_corpus, B1), n_constraints
            is counted there and a `corpus_path` key is stamped so an output can
            never be read as the default corpus. The no-arg pipeline passes the
            default and the manifest stays byte-identical (B1 inertness control).
    """
    n_constraints = len(list(testsets_dir.glob("*.pl"))) if testsets_dir.exists() else 0
    n_sotu = len(list(TESTSETS_SOTU_DIR.glob("*.pl"))) if TESTSETS_SOTU_DIR.exists() else 0
    commit = _git_head_sha()
    manifest = {
        "pipeline_run_at": run_at,
        "n_constraints": n_constraints,
        "n_sotu_constraints": n_sotu,
        "code_commit": commit,
        "code_commit_short": commit[:7] if commit != "unknown" else "unknown",
        "code_dirty": _git_dirty(),
        # 2 (OQ-98): per_constraint entries carry verdict_join (joined headline
        # verdict + raw inputs) as a sibling of diagnostic_verdict.
        "schema_version": 2,
    }
    # Stamp corpus_path ONLY for a non-default corpus — keeps the no-arg manifest
    # byte-for-byte unchanged (only difference from a default run is the absence of
    # this key, so the inertness diff is empty modulo pipeline_run_at).
    if testsets_dir.resolve() != TESTSETS_DIR.resolve():
        manifest["corpus_path"] = testsets_dir.name
    return manifest


def inject_manifest(src_path: Path, dst_path: Path, manifest: dict) -> None:
    """Read *src_path* (the Prolog export's raw artifact), prepend manifest as
    first key, write *dst_path* (the canonical manifest-bearing artifact).

    Single-writer convention: the swipl export writes pipeline_output.raw.json
    only; THIS function is the sole writer of pipeline_output.json. A direct
    swipl re-export therefore cannot clobber the canonical artifact's
    provenance (swipl_load_path_and_probe_gotchas.md §5).
    """
    with open(src_path, "r", encoding="utf-8") as f:
        data = json.load(f)
    # manifest goes first; existing keys follow unchanged
    out = {"manifest": manifest}
    out.update(data)
    with open(dst_path, "w", encoding="utf-8") as f:
        json.dump(out, f, ensure_ascii=False, indent=2)


def classify_corpus(corpus_path: str, output_name: str,
                    expected_model: Optional[str],
                    run_at: Optional[str] = None) -> dict:
    """Classify a NON-default corpus into its own manifest-bearing output (B1).

    A minimal, gate-free, fresh-process driver for the twin-comparison harness. Runs
    the same _json_report swipl goal the no-arg pipeline runs, but overlays
    config:param(corpus_path) to *corpus_path* via retract-default-then-assert — a
    single deterministic clause, so corpus_loader.pl's non-deterministic param read
    cannot reach a shadowed default on backtrack. Does NOT run the full pipeline
    (that overwrites shared outputs/ + tracked validation_suite.pl) and never touches
    the canonical pipeline_output.json — it writes OUTPUTS_DIR/output_name.

    *corpus_path* is relative to prolog/ (resolved there by corpus_loader).
    *expected_model*: a model-id PREFIX every loaded story_provenance must match
    (e.g. 'claude-haiku-4-5' or 'gemini-2.5-flash'); None for a mixed-model corpus
    (the essay/control regime), which skips the single-model fingerprint.

    Refuses (raises) rather than emit a swap or a partial:
      - zero-glob: a relative-path miss is loud, not a silent empty run.
      - load completeness: corpus_constraint count == glob_count (no file failed to load).
      - provenance fingerprint (single-model corpora): every loaded story_provenance
        model starts with *expected_model* — a count cannot catch a name-identical
        haiku<->flash swap, the model can. Non-vacuous: #story_provenance == glob_count
        is also asserted, so the model-match cannot pass over an empty fact set.
      - raw freshness: the raw artifact is deleted pre-run and must reappear newer.
      - seen == classified: len(per_constraint) == glob_count == manifest.n_constraints.

    Returns the manifest dict written into output_name.
    """
    run_at = run_at or datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")
    corpus_dir = (PROLOG_DIR / corpus_path).resolve()
    glob_count = len(list(corpus_dir.glob("*.pl"))) if corpus_dir.exists() else 0
    if glob_count == 0:
        raise RuntimeError(
            f"classify_corpus: zero .pl files at {corpus_dir} (relative path "
            f"{corpus_path!r} did not resolve to a populated corpus) — refusing")

    raw_path = OUTPUTS_DIR / "pipeline_output.raw.json"
    raw_path.unlink(missing_ok=True)
    delete_marker = time.time()

    # Single deterministic overlay clause (retract default first), then the standard
    # export, then an in-process gate that THROWS on mismatch (non-zero exit ->
    # run_prolog raises -> no inject_manifest; the swap/partial is refused, not echoed).
    overlay = (
        "retractall(config:param(corpus_path,_)), "
        f"assertz(config:param(corpus_path,'{corpus_path}')), "
    )
    gate = (
        f"absolute_file_name('{corpus_path}', AbsDir), "
        "format(user_error, '[classify] resolved corpus dir: ~w~n', [AbsDir]), "
        "findall(Mdl, narrative_ontology:story_provenance(_,_,_,_,_,_,Mdl,_), Mdls), "
        "length(Mdls, NProv), "
        "findall(Cc, corpus_loader:corpus_constraint(Cc), Ccs), length(Ccs, NCorp), "
        "format(user_error, '[classify] story_provenance=~w corpus_constraint=~w "
        f"glob={glob_count}~n', [NProv, NCorp]), "
        f"( NCorp =:= {glob_count} -> true ; throw(classify_load_incomplete(NCorp, {glob_count})) )"
    )
    if expected_model is not None:
        # Non-vacuous single-model fingerprint: full provenance coverage + prefix match.
        gate += (
            f", ( NProv =:= {glob_count} -> true ; throw(classify_provenance_coverage(NProv, {glob_count})) )"
            ", ( NProv > 0 -> true ; throw(classify_provenance_empty) )"
            f", ( forall(member(Mm, Mdls), atom_concat('{expected_model}', _, Mm)) "
            f"     -> true ; sort(Mdls, US), throw(classify_model_mismatch('{expected_model}', US)) )"
        )
    goal = overlay + "run_json_report, " + gate
    run_prolog(
        ["stack.pl", "covering_analysis.pl", "maxent_classifier.pl",
         "dirac_classification.pl", "diagnostic_summary.pl",
         "post_synthesis.pl", "json_report.pl"],
        goal,
    )

    # Raw freshness: must exist and be newer than the pre-run delete.
    if not raw_path.exists():
        raise RuntimeError("classify_corpus: pipeline_output.raw.json not produced")
    if raw_path.stat().st_mtime < delete_marker:
        raise RuntimeError("classify_corpus: raw artifact is stale (older than pre-run delete)")

    manifest = build_manifest(run_at, corpus_dir)
    out_path = OUTPUTS_DIR / output_name
    inject_manifest(raw_path, out_path, manifest)

    # Seen == classified: glob == per_constraint == manifest.n_constraints. A seen file
    # that failed to classify makes glob-n and per_constraint diverge silently.
    written = json.loads(out_path.read_text(encoding="utf-8"))
    n_pc = len(written.get("per_constraint", []))
    if not (n_pc == glob_count == manifest["n_constraints"]):
        raise RuntimeError(
            f"classify_corpus: seen!=classified — per_constraint={n_pc}, glob={glob_count}, "
            f"manifest.n={manifest['n_constraints']} (a seen file failed to classify; refusing)")
    return manifest


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
    "commentary_census": "<!-- COMMENTARY_CENSUS_START -->",
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
        import traceback
        tb = traceback.format_exc()
        msg = f"{name}: {type(e).__name__}: {e}"
        if progress:
            progress("pipeline", f"[WARN] {msg} (non-critical, continuing)")
            progress("pipeline", tb[:500])
        return StepResult(name=name, status="error", duration_s=time.time() - t0, error=f"{msg}\n{tb[:500]}")


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
    import python_test_suite

    if progress:
        progress("pipeline", "[PREP] Preparing test suite...")

    # domain_registry.pl regeneration RETIRED (OQ-96 close, 2026-06-11): the
    # consuming module was deleted 2026-02-18 and the last dangling reference
    # was removed 2026-06-10 — regenerating it every run was a
    # producer-without-consumer (Pattern 1).
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
        timeout=900,
    )
    (OUTPUTS_DIR / "covering_analysis.md").write_text(result.stdout, encoding="utf-8")


def _prolog_giant_comp():
    """Run giant_component_analysis → giant_component_analysis.md."""
    result = run_prolog(
        ["stack.pl", "giant_component_analysis.pl"],
        "run_giant_component_analysis",
        timeout=900,
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


def _prolog_commentary_census():
    """Run commentary_census → commentary_census.json + commentary_census.md (OQ-134).

    Corpus-wide commentary-grade census (q6_crosscheck + extraction_reading).
    Prolog computes the per-source bucket histograms (single source of truth);
    this transports the CENSUS* machine lines into a manifest-bearing JSON + the
    human table. COMMENTARY-GRADE: reads engine predicates only, never feeds
    classification — nothing touched here is on the dr_type path.

    Self-checking invariants (fail-loud, never under-report):
      - Σ buckets == n_corpus per source (a dropped/double-counted constraint
        breaks it — structurally enforces commentary_cell's one-bucket contract).
      - n_corpus > 0 (a 0==0 sum would pass vacuously if the corpus didn't load).
    Coverage is computed ONLY for sources whose absence buckets are RULED
    (CENSUS_COVERAGE decidable); undecided sources (extraction_reading) ship
    coverage null, never a default 1.0 (Build Discipline Pattern 6).
    """
    result = run_prolog(
        ["stack.pl", "commentary_census.pl"],
        "run_commentary_census",
    )
    raw = result.stdout

    # Parse the machine block (by line prefix — robust to interleaved load noise).
    sources: dict = {}

    def _src(name):
        return sources.setdefault(
            name, {"n_corpus": None, "buckets": {}, "absence_buckets": [],
                   "coverage_decidable": False})

    for line in raw.splitlines():
        parts = line.split()
        if not parts:
            continue
        if parts[0] == "CENSUS_META" and len(parts) == 4 and parts[2] == "n_corpus":
            _src(parts[1])["n_corpus"] = int(parts[3])
        elif parts[0] == "CENSUS" and len(parts) == 4:
            _src(parts[1])["buckets"][parts[2]] = int(parts[3])
        elif parts[0] == "CENSUS_ABSENCE" and len(parts) == 3:
            _src(parts[1])["absence_buckets"].append(parts[2])
        elif parts[0] == "CENSUS_COVERAGE" and len(parts) == 3 and parts[2] == "decidable":
            _src(parts[1])["coverage_decidable"] = True

    if not sources:
        raise PrologError(
            "commentary_census: no CENSUS* lines parsed from Prolog output — "
            "the census did not run (corpus not loaded?).")

    for src, d in sources.items():
        n = d["n_corpus"]
        total = sum(d["buckets"].values())
        if n is None:
            raise PrologError(f"commentary_census[{src}]: no CENSUS_META n_corpus line.")
        if n <= 0:
            raise PrologError(
                f"commentary_census[{src}]: n_corpus={n} (corpus did not load) — refusing "
                "(a 0==0 sum would pass the invariant vacuously).")
        if total != n:
            raise PrologError(
                f"commentary_census[{src}]: Σ buckets ({total}) != n_corpus ({n}) — a "
                "constraint was dropped or double-counted (commentary_cell is not "
                "one-solution-per-constraint).")
        # Coverage = (n − Σ absence)/n, ONLY where the absence set is RULED complete.
        # An undecided source ships null — never a defaulted 1.0 (Pattern 6).
        if d["coverage_decidable"]:
            absent = sum(d["buckets"].get(b, 0) for b in d["absence_buckets"])
            d["coverage"] = (n - absent) / n
        else:
            d["coverage"] = None  # "N/A" — absence semantics unruled

    # Manifest carries corpus identity so live-vs-twin is self-labeling (never a
    # hardcoded 0 for an empty cell — the cell is empty FOR THIS corpus).
    run_at = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")
    manifest = build_manifest(run_at)
    manifest["corpus_hash"] = _compute_corpus_hash(TESTSETS_DIR)
    out = {"manifest": manifest, "sources": sources}
    (OUTPUTS_DIR / "commentary_census.json").write_text(
        json.dumps(out, ensure_ascii=False, indent=2), encoding="utf-8")

    cleaned = strip_preamble(raw, _PREAMBLE_MARKERS["commentary_census"])
    (OUTPUTS_DIR / "commentary_census.md").write_text(
        cleaned if cleaned.strip() else "# Commentary Census (empty)\n", encoding="utf-8")


def _phase_prolog(progress, parallel):
    """Phase 2: run all Prolog analyses in parallel."""
    # Diagnostic — remove after debugging
    if progress:
        import shutil
        progress("pipeline", f"PROLOG_DIR = {PROLOG_DIR}")
        progress("pipeline", f"PROLOG_DIR exists = {PROLOG_DIR.exists()}")
        progress("pipeline", f"swipl path = {shutil.which('swipl')}")
        progress("pipeline", f"testsets count = {len(list(TESTSETS_DIR.glob('*.pl')))}")

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
        ("commentary_census", _prolog_commentary_census),
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

    # Fingerprint JSON sidecar (fingerprint_report.md produced in Phase 2)
    def _fingerprint_json():
        import parse_fingerprint_json
        parse_fingerprint_json.main()

    if progress:
        progress("pipeline", "[NORM] Generating fingerprint_data.json...")
    results.append(_run_step("fingerprint_json", _fingerprint_json, progress))

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
# Phase 9: CROSS-CONSTRAINT ANALYSIS (parallel 9a, then sequential 9b)
# ---------------------------------------------------------------------------

def _phase_cross_constraint(progress, parallel):
    """Phase 9a: scenario convergence + omega cross-constraint (parallel)."""
    if progress:
        progress("pipeline", "[XCON] Running cross-constraint analyses...")

    def _scenario():
        import scenario_convergence
        scenario_convergence.run()

    def _omega_xcon():
        import omega_cross_constraint
        omega_cross_constraint.run()

    tasks = [
        ("scenario_convergence",   _scenario),
        ("omega_cross_constraint",  _omega_xcon),
    ]
    results = _run_parallel(tasks, progress, parallel)

    if progress:
        ok = sum(1 for r in results if r.status == "ok")
        progress("pipeline", f"[XCON] Done ({ok}/{len(results)} succeeded).")

    return results


def _phase_evaluative_convergence(progress):
    """Phase 9b: evaluative convergence synthesis (sequential, always runs)."""
    if progress:
        progress("pipeline", "[EVAL] Running evaluative convergence synthesis...")

    def _eval():
        import evaluative_convergence
        evaluative_convergence.main()

    result = _run_step("evaluative_convergence", _eval, progress)
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
    run_at = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")

    OUTPUTS_DIR.mkdir(parents=True, exist_ok=True)

    # ── ISSUES.md status-grammar gate (2026-06-04) ──────────────────────────
    # DO NOT REMOVE OR BYPASS THIS GATE. If it fails: fix ISSUES.md so that
    #   python3 python/issues_status.py --check
    # passes (the malformed entries are named in the output), THEN re-run.
    # Rationale: the status ledger is the repo's open-question census; a
    # checker that isn't run isn't checking, and an unparseable ledger
    # silently corrupts the next census. Grammar: `**Status:** <token>` with
    # token in {open, investigating, mitigated, partial, resolved, disposed, future},
    # optionally ` — detail` — see the ISSUES.md footer.
    from issues_status import scan as _issues_scan
    _, _issues_problems = _issues_scan()
    if _issues_problems:
        for _p in _issues_problems:
            _msg = f"[ISSUES-GATE] MALFORMED: {_p}"
            if progress:
                progress("pipeline", _msg)
            print(_msg, file=sys.stderr)
        raise SystemExit(
            f"ISSUES.md status-grammar gate failed ({len(_issues_problems)} "
            "malformed). Fix ISSUES.md — do NOT remove this gate — verify "
            "with `python3 python/issues_status.py --check`, then re-run "
            "the pipeline."
        )

    # --- LOAD-WARNING GATE (OQ-96; sibling of the ISSUES gate above) -------
    # A swipl load warning outside the allowlist (prolog/
    # load_warning_allowlist.txt) aborts the run. Rationale: the
    # domain_registry dangling-module warning was emitted at EVERY load for
    # four months while ad-hoc `grep -v Warning` filters hid it, until the
    # dead reference crashed the validation suite at runtime (OQ-96). Do NOT
    # remove or bypass; to accept a new known-benign warning, add its
    # normalized record to the allowlist (see
    # `python3 python/load_warning_gate.py --baseline`).
    from load_warning_gate import collect_warnings as _lw_collect
    _lw_allow_path = PROLOG_DIR / "load_warning_allowlist.txt"
    _lw_allowed = set()
    if _lw_allow_path.exists():
        _lw_allowed = {ln.strip() for ln in _lw_allow_path.read_text().splitlines()
                       if ln.strip() and not ln.startswith("#")}
    _lw_unexpected = [r for r in _lw_collect() if r not in _lw_allowed]
    if _lw_unexpected:
        for _r in _lw_unexpected:
            _msg = f"[WARNING-GATE] UNEXPECTED: {_r}"
            if progress:
                progress("pipeline", _msg)
            print(_msg, file=sys.stderr)
        raise SystemExit(
            f"swipl load-warning gate failed ({len(_lw_unexpected)} unexpected). "
            "Fix the warning or allowlist it deliberately — verify with "
            "`python3 python/load_warning_gate.py`, then re-run the pipeline."
        )

    # --- GRID FIRST-CONTACT GATE (OQ-93 flip ruling, 2026-06-11) ----------
    # Every grid-authoring story gets the three-indicator plausibility audit
    # ONCE before any consumer read (ledger: python/grid_audit_ledger.json).
    # C-echo in a new story HALTS the run and demands the opt-in flip be
    # reverted. Do NOT remove or bypass — per-story exclusion is the
    # fail-closed half of the operator's split κ ruling.
    from grid_first_contact_gate import run_gate as _grid_gate
    if _grid_gate() != 0:
        raise SystemExit(
            "Grid first-contact gate failed — a grid-authoring story fired a "
            "plausibility indicator (see [GRID-GATE] lines). Remove/fix the "
            "story or record an operator waiver in "
            "python/grid_audit_ledger.json, then re-run."
        )

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

    # Abort if the raw export was not produced (json_report writes the raw
    # file; the canonical pipeline_output.json is written by _manifest_step)
    if not (OUTPUTS_DIR / "pipeline_output.raw.json").exists():
        msg = "pipeline_output.raw.json not produced — aborting downstream phases"
        if progress:
            progress("pipeline", f"[FATAL] {msg}")
        pipeline_result.errors.append(msg)
        pipeline_result.total_duration_s = time.time() - t0
        return pipeline_result

    # Write canonical pipeline_output.json = raw export + manifest; verify orbits
    def _manifest_step():
        manifest = build_manifest(run_at)
        inject_manifest(OUTPUTS_DIR / "pipeline_output.raw.json",
                        OUTPUTS_DIR / "pipeline_output.json", manifest)
        # Sidecar provenance for orbit_data.json. orbit_data.json is a pure
        # id->orbit dict consumed by iterating readers (game_theory_*, sheaf_audit,
        # extract_corpus_data, normalize_orbit_ids, meta_reporter,
        # container_typology_analysis), so a sibling "manifest" key cannot be
        # injected in-file. The sidecar carries the SAME manifest dict, making
        # orbit_data.json provably the same run as pipeline_output.json.
        orbit_manifest_path = OUTPUTS_DIR / "orbit_data.manifest.json"
        with open(orbit_manifest_path, "w", encoding="utf-8") as f:
            json.dump({"manifest": manifest}, f, indent=2)
        orbits_path = OUTPUTS_DIR / "product_site_orbits.json"
        check_orbits_corpus_hash(orbits_path)
        if progress:
            progress("pipeline",
                     f"[MANIFEST] Stamped pipeline_output.json + orbit_data.manifest.json: "
                     f"run_at={manifest['pipeline_run_at']}, "
                     f"commit={manifest['code_commit_short']}, "
                     f"n={manifest['n_constraints']}, dirty={manifest['code_dirty']}")

    collect(_run_step("manifest_inject", _manifest_step, progress))

    # Phase 4: PYTHON TIER 1 (parallel)
    collect(_phase_python_tier1(progress, parallel))

    # False mountain JSON sidecar (false_mountain_report.md produced in Tier 1)
    def _false_mountain_json():
        import parse_false_mountain_json
        parse_false_mountain_json.main()

    collect(_run_step("false_mountain_json", _false_mountain_json, progress))

    # W1 × sheaf_status join — refresh the ranked obstruction artifact each run so it
    # never goes stale against pipeline_output.json. Depends on the manifest step
    # (its same-run guard reads orbit_data.manifest.json + pipeline_output.json's
    # manifest). Non-critical: a guard failure raises JoinAborted, caught by _run_step.
    def _w1_sheaf_join():
        import w1_sheaf_join
        w1_sheaf_join.main()

    collect(_run_step("w1_sheaf_join", _w1_sheaf_join, progress))

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

    # Phase 9: CROSS-CONSTRAINT ANALYSIS (depends on enriched_pipeline.json)
    if (OUTPUTS_DIR / "enriched_pipeline.json").exists():
        collect(_phase_cross_constraint(progress, parallel))
        collect(_phase_evaluative_convergence(progress))
    else:
        if progress:
            progress("pipeline", "[XCON] Skipping (enriched_pipeline.json missing).")

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
