"""LLM Presheaf Diagnostic — Perspective Experiment Orchestrator.

Generates multiple constraint stories from different observer perspectives,
runs the DR pipeline, generates enhanced reports, and logs results for
post-experiment analysis.

Usage:
    # Explicit constraint/perspective selection
    python3 agent/perspective_experiment.py \\
        --constraints academic_peer_review_gatekeeping,subscription_economy_model \\
        --perspectives u1,u2,u3,u4 --framing experiential --runs 2

    # MVP mode: 5 hardcoded constraints x 4 perspectives x 2 runs = 40 calls
    python3 agent/perspective_experiment.py --mvp
"""

import argparse
import json
import os
import re
import subprocess
import sys
import time
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path
from typing import Any

# ---------------------------------------------------------------------------
# Path setup
# ---------------------------------------------------------------------------

REPO_ROOT = Path(__file__).resolve().parent.parent
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))
if str(REPO_ROOT / "python") not in sys.path:
    sys.path.insert(0, str(REPO_ROOT / "python"))

from agent.story_generator_base import (
    _get_client,
    _SYSTEM_INSTRUCTION,
    build_prompt,
    process_response,
    retry_with_backoff,
    DEFAULT_MODEL,
    TESTSETS_DIR,
)
from python.generate_constraint_pl import generate_pl, validate_json
from python.linter import lint_file

# ---------------------------------------------------------------------------
# Path constants
# ---------------------------------------------------------------------------

PREAMBLE_DIR = REPO_ROOT / "prompts" / "perspective_preambles"
JSON_EXPERIMENT_DIR = REPO_ROOT / "json" / "perspective_experiment"
GAPTESTS_DIR = REPO_ROOT / "prolog" / "gaptests"
RESULTS_DIR = REPO_ROOT / "results" / "perspective_experiment"
JSON_DIR = REPO_ROOT / "json"
PROLOG_DIR = REPO_ROOT / "prolog"

# ---------------------------------------------------------------------------
# MVP constraints (selected for H1 diversity per plan Section 1.2)
# ---------------------------------------------------------------------------

MVP_CONSTRAINTS = [
    "academic_peer_review_gatekeeping",
    "subscription_economy_model",
    "26usc469_real_estate_exemption",
    "antifragility",
    "epistemic_process_of_verification",
]

ALL_PERSPECTIVES = ["u1", "u2", "u3", "u4"]

# Framing abbreviations for mangled IDs
FRAMING_ABBREV = {
    "experiential": "exp",
    "structural": "str",
}


# ---------------------------------------------------------------------------
# Data classes
# ---------------------------------------------------------------------------

@dataclass
class GenerationResult:
    constraint_id: str
    perspective: str
    framing: str
    run: int
    mangled_id: str
    success: bool
    epsilon: float | None = None
    claimed_type: str = ""
    lint_errors: list[str] = field(default_factory=list)
    lint_passed: bool = True
    error: str = ""
    tokens_in: int = 0
    tokens_out: int = 0
    duration_s: float = 0.0


@dataclass
class ExperimentResult:
    config: dict = field(default_factory=dict)
    generations: list[GenerationResult] = field(default_factory=list)
    pipeline_status: str = ""
    report_ids: list[str] = field(default_factory=list)
    total_tokens_in: int = 0
    total_tokens_out: int = 0
    total_duration_s: float = 0.0


# ---------------------------------------------------------------------------
# PL file header parser
# ---------------------------------------------------------------------------

_RE_HUMAN_READABLE = re.compile(
    r'human_readable\(\s*[\'"]?(\w+)[\'"]?\s*,\s*"([^"]+)"\s*\)'
)
_RE_TOPIC_DOMAIN = re.compile(
    r'topic_domain\(\s*[\'"]?(\w+)[\'"]?\s*,\s*"([^"]+)"\s*\)'
)
_RE_CONSTRAINT_CLAIM = re.compile(
    r'constraint_claim\(\s*[\'"]?(\w+)[\'"]?\s*,\s*(\w+)\s*\)'
)
_RE_SUMMARY = re.compile(
    r'\*\s*SUMMARY:\s*\n((?:\s*\*\s+.*\n)+)', re.MULTILINE
)


def _parse_pl_source(constraint_id: str) -> str:
    """Extract a source description from an existing .pl testset file.

    Returns a human-readable description suitable for use as source_description
    in the generation prompt.  Does NOT include metric values (the model must
    derive them independently for the epsilon invariance test).
    """
    pl_path = TESTSETS_DIR / f"{constraint_id}.pl"
    if not pl_path.exists():
        # Try unquoted variant
        pl_path = TESTSETS_DIR / f"{constraint_id}.pl"
        if not pl_path.exists():
            return f"CONSTRAINT: {constraint_id}"

    content = pl_path.read_text(encoding="utf-8")

    parts = [f"CONSTRAINT: {constraint_id}"]

    # Extract human_readable
    m = _RE_HUMAN_READABLE.search(content)
    if m:
        parts.append(f"Human-readable name: {m.group(2)}")

    # Extract topic_domain
    m = _RE_TOPIC_DOMAIN.search(content)
    if m:
        parts.append(f"Domain: {m.group(2)}")

    # Extract claimed_type
    m = _RE_CONSTRAINT_CLAIM.search(content)
    if m:
        parts.append(f"Previously classified as: {m.group(2)}")

    # Extract summary from narrative context comment block
    m = _RE_SUMMARY.search(content)
    if m:
        summary_lines = []
        for line in m.group(1).splitlines():
            cleaned = line.strip().lstrip("* ").strip()
            if cleaned:
                summary_lines.append(cleaned)
        if summary_lines:
            summary = " ".join(summary_lines)
            # Truncate to keep prompt reasonable
            if len(summary) > 800:
                summary = summary[:800] + "..."
            parts.append(f"Context: {summary}")

    return "\n".join(parts)


def _load_json_source(constraint_id: str) -> str | None:
    """Try to load source description from a JSON authoring file."""
    json_path = JSON_DIR / f"{constraint_id}.json"
    if not json_path.exists():
        return None

    try:
        data = json.loads(json_path.read_text(encoding="utf-8"))
        bp = data.get("base_properties", {})
        commentary = data.get("commentary", {})

        parts = [f"CONSTRAINT: {constraint_id}"]
        if bp.get("human_readable"):
            parts.append(f"Human-readable name: {bp['human_readable']}")
        if bp.get("topic_domain"):
            parts.append(f"Domain: {bp['topic_domain']}")
        if bp.get("claimed_type"):
            parts.append(f"Previously classified as: {bp['claimed_type']}")
        if commentary.get("narrative_context"):
            ctx = commentary["narrative_context"]
            if len(ctx) > 800:
                ctx = ctx[:800] + "..."
            parts.append(f"Context: {ctx}")

        return "\n".join(parts)
    except Exception:
        return None


# ---------------------------------------------------------------------------
# Experiment orchestrator
# ---------------------------------------------------------------------------

class PerspectiveExperiment:
    """Orchestrates perspective-prompted constraint story generation."""

    def __init__(
        self,
        constraints: list[str],
        perspectives: list[str] | None = None,
        framing: str = "experiential",
        runs: int = 2,
        model: str | None = None,
        temperature: float = 0.2,
        start_run: int = 1,
        append_log: bool = False,
    ):
        self.constraints = constraints
        self.perspectives = perspectives or ALL_PERSPECTIVES
        self.framing = framing
        self.runs = runs
        self.model = model or DEFAULT_MODEL
        self.temperature = temperature
        self.start_run = start_run
        self.append_log = append_log

    # ------------------------------------------------------------------
    # ID mangling
    # ------------------------------------------------------------------

    @staticmethod
    def _mangle_id(constraint_id: str, perspective: str, framing: str, run: int) -> str:
        """Build a mangled constraint ID for perspective-generated stories.

        Format: {original_id}_{perspective}_{framing_abbrev}_r{run}
        """
        abbrev = FRAMING_ABBREV.get(framing, framing[:3])
        return f"{constraint_id}_{perspective}_{abbrev}_r{run}"

    # ------------------------------------------------------------------
    # Preamble loading
    # ------------------------------------------------------------------

    @staticmethod
    def _load_preamble(perspective: str, framing: str) -> str:
        """Load a perspective preamble template."""
        filename = f"{perspective}_{framing}.md"
        path = PREAMBLE_DIR / filename
        if not path.exists():
            raise FileNotFoundError(f"Preamble not found: {path}")
        return path.read_text(encoding="utf-8").strip()

    # ------------------------------------------------------------------
    # Source loading
    # ------------------------------------------------------------------

    @staticmethod
    def _load_constraint_source(constraint_id: str) -> str:
        """Load constraint source description (JSON preferred, .pl fallback)."""
        source = _load_json_source(constraint_id)
        if source:
            return source
        return _parse_pl_source(constraint_id)

    # ------------------------------------------------------------------
    # Gemini call
    # ------------------------------------------------------------------

    def _call(self, prompt: str, system_instruction: str) -> tuple[str, int, int]:
        """Call Gemini and return (text, tokens_in, tokens_out)."""
        from google.genai import types

        client = _get_client()
        config = types.GenerateContentConfig(
            temperature=self.temperature,
            system_instruction=system_instruction,
        )

        response = retry_with_backoff(
            client.models.generate_content,
            model=self.model,
            contents=prompt,
            config=config,
        )

        text = response.text if response else ""
        meta = getattr(response, "usage_metadata", None)
        tokens_in = getattr(meta, "prompt_token_count", 0) or 0
        tokens_out = getattr(meta, "candidates_token_count", 0) or 0
        return text, tokens_in, tokens_out

    # ------------------------------------------------------------------
    # Single generation
    # ------------------------------------------------------------------

    def _generate_one(
        self,
        constraint_id: str,
        perspective: str,
        framing: str,
        run: int,
    ) -> GenerationResult:
        """Generate one perspective-prompted constraint story."""
        mangled_id = self._mangle_id(constraint_id, perspective, framing, run)
        t0 = time.time()

        result = GenerationResult(
            constraint_id=constraint_id,
            perspective=perspective,
            framing=framing,
            run=run,
            mangled_id=mangled_id,
            success=False,
        )

        try:
            # Load components
            preamble = self._load_preamble(perspective, framing)
            source_desc = self._load_constraint_source(constraint_id)

            # Compose system instruction with perspective preamble
            system_instruction = _SYSTEM_INSTRUCTION + "\n\n" + preamble

            # Build prompt
            prompt = build_prompt(source_desc)

            # Call Gemini
            print(f"  [{mangled_id}] Calling {self.model}...")
            raw_text, tokens_in, tokens_out = self._call(prompt, system_instruction)
            result.tokens_in = tokens_in
            result.tokens_out = tokens_out

            if not raw_text:
                result.error = "Empty response from model"
                result.duration_s = time.time() - t0
                return result

            # Process response (parse JSON + validate schema)
            story_dict, errors = process_response(raw_text)

            if story_dict is None:
                result.error = f"JSON parse failed: {errors[0] if errors else 'unknown'}"
                result.duration_s = time.time() - t0
                return result

            if errors:
                # Retry once with error feedback
                print(f"  [{mangled_id}] Validation errors, retrying...")
                feedback = "\nYour previous attempt had these validation errors:\n"
                for err in errors:
                    feedback += f"  - {err}\n"
                feedback += "Fix these specific errors while keeping the rest correct.\n"

                retry_prompt = build_prompt(source_desc, feedback)
                raw_text, tin2, tout2 = self._call(retry_prompt, system_instruction)
                result.tokens_in += tin2
                result.tokens_out += tout2

                story_dict, errors = process_response(raw_text)
                if story_dict is None or errors:
                    result.error = f"Validation failed after retry: {errors}"
                    result.duration_s = time.time() - t0
                    return result

            # Override constraint_id with mangled version
            story_dict["header"]["constraint_id"] = mangled_id
            story_dict["header"]["module_name_override"] = f"constraint_{mangled_id}"

            # Record epsilon and claimed_type
            result.epsilon = story_dict["base_properties"].get("extractiveness")
            result.claimed_type = story_dict["base_properties"].get("claimed_type", "")

            # Compile JSON -> Prolog
            pl_content = generate_pl(story_dict)

            # Lint via temp file in testsets/ (linter resolves config.pl
            # via dirname(dirname(filepath)), so file must be inside prolog/testsets/)
            tmp_path = TESTSETS_DIR / f".tmp_{mangled_id}.pl"
            try:
                tmp_path.write_text(pl_content, encoding="utf-8")
                lint_errors = lint_file(str(tmp_path))
                if lint_errors:
                    result.lint_errors = lint_errors[:10]
                    result.lint_passed = False
                    print(f"  [{mangled_id}] Lint warnings ({len(lint_errors)}):")
                    for err in lint_errors[:3]:
                        print(f"    - {err}")
            except Exception as e:
                print(f"  [{mangled_id}] Linter crashed: {e}")
                result.lint_errors = [f"LINTER_CRASH: {e}"]
                result.lint_passed = False
            finally:
                tmp_path.unlink(missing_ok=True)

            # Save to 3 locations
            # 1. JSON -> json/perspective_experiment/
            JSON_EXPERIMENT_DIR.mkdir(parents=True, exist_ok=True)
            json_path = JSON_EXPERIMENT_DIR / f"{mangled_id}.json"
            json_path.write_text(
                json.dumps(story_dict, indent=2) + "\n", encoding="utf-8"
            )

            # 2. .pl -> prolog/gaptests/ (permanent)
            GAPTESTS_DIR.mkdir(parents=True, exist_ok=True)
            gap_path = GAPTESTS_DIR / f"{mangled_id}.pl"
            gap_path.write_text(pl_content, encoding="utf-8")

            # 3. .pl -> prolog/testsets/ (temporary, for enhanced_report.py)
            testset_path = TESTSETS_DIR / f"{mangled_id}.pl"
            testset_path.write_text(pl_content, encoding="utf-8")

            result.success = True
            print(f"  [{mangled_id}] Saved (eps={result.epsilon}, type={result.claimed_type})")

        except Exception as e:
            result.error = str(e)
            print(f"  [{mangled_id}] Error: {e}")

        result.duration_s = time.time() - t0
        return result

    # ------------------------------------------------------------------
    # Pipeline execution
    # ------------------------------------------------------------------

    def _run_pipeline(self):
        """Run the DR analysis pipeline."""
        print("\n=== Phase 2: Running DR Pipeline ===")
        try:
            from python.run_pipeline import run_pipeline
            pipeline_result = run_pipeline(
                progress=lambda step, msg: print(f"  [{step}] {msg}"),
                parallel=4,
            )
            if pipeline_result.errors:
                for e in pipeline_result.errors:
                    print(f"  [pipeline] warning: {e}")
            return "success"
        except Exception as e:
            print(f"  [pipeline] FAILED: {e}")
            return f"error: {e}"

    # ------------------------------------------------------------------
    # Enhanced reports
    # ------------------------------------------------------------------

    def _run_reports(self, mangled_ids: list[str]) -> list[str]:
        """Run enhanced_report.py for mangled constraint IDs."""
        print(f"\n=== Phase 3: Enhanced Reports ({len(mangled_ids)} constraints) ===")
        if not mangled_ids:
            return []

        try:
            proc = subprocess.run(
                ["python3", "python/enhanced_report.py"] + mangled_ids,
                cwd=str(REPO_ROOT),
                capture_output=True,
                text=True,
                timeout=600,
            )
            if proc.returncode != 0:
                print(f"  enhanced_report.py returned {proc.returncode}")
                if proc.stderr:
                    print(f"  stderr: {proc.stderr[:500]}")
        except subprocess.TimeoutExpired:
            print("  Report generation timed out (600s)")
        except Exception as e:
            print(f"  Report generation failed: {e}")

        # Check which reports were produced
        reports_dir = REPO_ROOT / "outputs" / "constraint_reports"
        produced = []
        for mid in mangled_ids:
            if (reports_dir / f"{mid}_report.md").exists():
                produced.append(mid)

        print(f"  Produced {len(produced)}/{len(mangled_ids)} reports")
        return produced

    # ------------------------------------------------------------------
    # Cleanup
    # ------------------------------------------------------------------

    def _cleanup_testsets(self, mangled_ids: list[str]):
        """Remove mangled .pl files from testsets/ and restore validation_suite.pl."""
        print("\n=== Phase 4: Cleanup ===")
        removed = 0
        for mid in mangled_ids:
            testset_path = TESTSETS_DIR / f"{mid}.pl"
            if testset_path.exists():
                testset_path.unlink()
                removed += 1
        print(f"  Removed {removed} mangled .pl files from testsets/")

        # Restore validation_suite.pl from standard testsets only
        try:
            import python.python_test_suite as pts
            pts.build_suite()
            print("  Restored validation_suite.pl")
        except Exception as e:
            print(f"  Warning: Could not restore validation_suite.pl: {e}")

    # ------------------------------------------------------------------
    # Experiment log
    # ------------------------------------------------------------------

    def _save_experiment_log(self, result: ExperimentResult):
        """Save experiment results to JSON log.

        If self.append_log is True and an existing log exists, merge new
        generations into the existing log (deduplicating by mangled_id).
        """
        RESULTS_DIR.mkdir(parents=True, exist_ok=True)
        log_path = RESULTS_DIR / "experiment_log.json"

        new_generations = [
            {
                "constraint_id": g.constraint_id,
                "perspective": g.perspective,
                "framing": g.framing,
                "run": g.run,
                "mangled_id": g.mangled_id,
                "success": g.success,
                "epsilon": g.epsilon,
                "claimed_type": g.claimed_type,
                "lint_passed": g.lint_passed,
                "lint_errors": g.lint_errors,
                "error": g.error,
                "tokens_in": g.tokens_in,
                "tokens_out": g.tokens_out,
                "duration_s": round(g.duration_s, 1),
            }
            for g in result.generations
        ]

        # Merge with existing log if appending
        if self.append_log and log_path.exists():
            try:
                existing = json.loads(log_path.read_text(encoding="utf-8"))
                existing_gens = existing.get("generations", [])
                existing_ids = {g["mangled_id"] for g in existing_gens}
                merged_gens = existing_gens + [
                    g for g in new_generations if g["mangled_id"] not in existing_ids
                ]
                print(f"  Appending {len(new_generations)} new generations to "
                      f"{len(existing_gens)} existing ({len(merged_gens)} total)")
            except Exception as e:
                print(f"  Warning: Could not read existing log for append: {e}")
                merged_gens = new_generations
        else:
            merged_gens = new_generations

        # Recompute summary from merged data
        all_gens_result = ExperimentResult(generations=[])
        for g_dict in merged_gens:
            all_gens_result.generations.append(GenerationResult(
                constraint_id=g_dict["constraint_id"],
                perspective=g_dict["perspective"],
                framing=g_dict["framing"],
                run=g_dict["run"],
                mangled_id=g_dict["mangled_id"],
                success=g_dict["success"],
                epsilon=g_dict.get("epsilon"),
                claimed_type=g_dict.get("claimed_type", ""),
                lint_passed=g_dict.get("lint_passed", True),
                lint_errors=g_dict.get("lint_errors", []),
            ))

        log_data = {
            "timestamp": datetime.now().isoformat(),
            "config": result.config,
            "summary": {
                "total_generations": len(merged_gens),
                "successful": sum(1 for g in merged_gens if g.get("success")),
                "failed": sum(1 for g in merged_gens if not g.get("success")),
                "lint_failures": sum(1 for g in merged_gens if g.get("success") and not g.get("lint_passed")),
                "pipeline_status": result.pipeline_status,
                "reports_generated": len(result.report_ids),
                "total_tokens_in": sum(g.get("tokens_in", 0) for g in merged_gens),
                "total_tokens_out": sum(g.get("tokens_out", 0) for g in merged_gens),
                "total_duration_s": round(result.total_duration_s, 1),
            },
            "lint_stats": self._compute_lint_stats(all_gens_result),
            "generations": merged_gens,
        }

        log_path.write_text(json.dumps(log_data, indent=2) + "\n", encoding="utf-8")
        print(f"\n  Experiment log saved to {log_path}")

    def _compute_lint_stats(self, result: ExperimentResult) -> dict:
        """Compute lint failure rates per perspective and framing."""
        stats: dict[str, dict[str, Any]] = {}

        for g in result.generations:
            if not g.success:
                continue
            key = f"{g.perspective}_{FRAMING_ABBREV.get(g.framing, g.framing[:3])}"
            if key not in stats:
                stats[key] = {"total": 0, "passed": 0, "failed": 0}
            stats[key]["total"] += 1
            if g.lint_passed:
                stats[key]["passed"] += 1
            else:
                stats[key]["failed"] += 1

        return stats

    # ------------------------------------------------------------------
    # Main run method
    # ------------------------------------------------------------------

    def run(self) -> ExperimentResult:
        """Execute the full perspective experiment."""
        t0 = time.time()

        result = ExperimentResult(
            config={
                "constraints": self.constraints,
                "perspectives": self.perspectives,
                "framing": self.framing,
                "runs": self.runs,
                "model": self.model,
                "temperature": self.temperature,
            }
        )

        total_tasks = len(self.constraints) * len(self.perspectives) * self.runs
        print(f"\n{'='*60}")
        print(f"PERSPECTIVE EXPERIMENT")
        print(f"{'='*60}")
        print(f"  Constraints:  {len(self.constraints)}")
        print(f"  Perspectives: {self.perspectives}")
        print(f"  Framing:      {self.framing}")
        print(f"  Runs:         {self.runs}")
        print(f"  Total calls:  {total_tasks}")
        print(f"  Model:        {self.model}")
        print(f"{'='*60}\n")

        # Phase 1: Generate stories
        end_run = self.start_run + self.runs - 1
        print("=== Phase 1: Generating Perspective Stories ===")
        if self.start_run > 1:
            print(f"  (runs {self.start_run}-{end_run}, appending to existing data)\n")
        else:
            print()
        task_num = 0
        for constraint_id in self.constraints:
            print(f"\n--- Constraint: {constraint_id} ---")
            for perspective in self.perspectives:
                for run in range(self.start_run, end_run + 1):
                    task_num += 1
                    print(f"\n[{task_num}/{total_tasks}] {constraint_id} / {perspective} / r{run}")
                    gen_result = self._generate_one(
                        constraint_id, perspective, self.framing, run
                    )
                    result.generations.append(gen_result)
                    result.total_tokens_in += gen_result.tokens_in
                    result.total_tokens_out += gen_result.tokens_out

        # Collect successful mangled IDs
        successful_ids = [g.mangled_id for g in result.generations if g.success]
        failed_count = sum(1 for g in result.generations if not g.success)
        print(f"\n  Phase 1 complete: {len(successful_ids)} succeeded, {failed_count} failed")

        if not successful_ids:
            print("\n  No successful generations — skipping pipeline and reports.")
            result.pipeline_status = "skipped"
            result.total_duration_s = time.time() - t0
            self._save_experiment_log(result)
            return result

        # Phase 2: Run DR pipeline
        result.pipeline_status = self._run_pipeline()

        # Phase 3: Enhanced reports
        result.report_ids = self._run_reports(successful_ids)

        # Phase 4: Cleanup
        self._cleanup_testsets(successful_ids)

        # Phase 5: Save experiment log
        result.total_duration_s = time.time() - t0
        self._save_experiment_log(result)

        # Print summary
        print(f"\n{'='*60}")
        print(f"EXPERIMENT COMPLETE")
        print(f"{'='*60}")
        print(f"  Successful generations: {len(successful_ids)}/{total_tasks}")
        print(f"  Lint failures:          {sum(1 for g in result.generations if g.success and not g.lint_passed)}")
        print(f"  Pipeline:               {result.pipeline_status}")
        print(f"  Reports generated:      {len(result.report_ids)}")
        print(f"  Total tokens:           {result.total_tokens_in} -> {result.total_tokens_out}")
        print(f"  Total time:             {result.total_duration_s:.1f}s")
        print(f"{'='*60}")

        return result


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(
        description="LLM Presheaf Diagnostic — Perspective Experiment"
    )
    parser.add_argument(
        "--constraints", "-c",
        help="Comma-separated constraint IDs (e.g., academic_peer_review_gatekeeping,antifragility)"
    )
    parser.add_argument(
        "--perspectives", "-p",
        default="u1,u2,u3,u4",
        help="Comma-separated perspectives (default: u1,u2,u3,u4)"
    )
    parser.add_argument(
        "--framing", "-f",
        choices=["experiential", "structural"],
        default="experiential",
        help="Framing variant (default: experiential)"
    )
    parser.add_argument(
        "--runs", "-r",
        type=int, default=2,
        help="Number of runs per constraint x perspective (default: 2)"
    )
    parser.add_argument(
        "--model", "-m",
        default=None,
        help=f"Gemini model to use (default: {DEFAULT_MODEL})"
    )
    parser.add_argument(
        "--temperature", "-t",
        type=float, default=0.2,
        help="Generation temperature (default: 0.2)"
    )
    parser.add_argument(
        "--mvp", action="store_true",
        help="Run MVP experiment: 5 constraints x 4 perspectives x 2 runs = 40 calls"
    )
    parser.add_argument(
        "--start-run", type=int, default=1,
        help="Starting run number (default: 1). Use 3 to add runs 3-N to existing r1-r2 data."
    )
    parser.add_argument(
        "--append-log", action="store_true",
        help="Merge new results into existing experiment_log.json instead of overwriting"
    )
    args = parser.parse_args()

    if args.mvp:
        constraints = MVP_CONSTRAINTS
    elif args.constraints:
        constraints = [c.strip() for c in args.constraints.split(",")]
    else:
        parser.error("Provide --constraints or use --mvp")

    perspectives = [p.strip() for p in args.perspectives.split(",")]
    framing = args.framing
    runs = args.runs

    experiment = PerspectiveExperiment(
        constraints=constraints,
        perspectives=perspectives,
        framing=framing,
        runs=runs,
        model=args.model,
        temperature=args.temperature,
        start_run=args.start_run,
        append_log=args.append_log,
    )
    experiment.run()


if __name__ == "__main__":
    main()
