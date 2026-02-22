"""DR Audit Pipeline — CLI-first orchestrator.

Chains: research → SCOPE → generate → corpus update → enhanced reports → essay.
No Streamlit imports.  Pure Python with optional progress callback.

Usage:
    python3 agent/orchestrator.py "Alberta separatism"
    python3 agent/orchestrator.py --input-file topic.txt --axes 3
    python3 agent/orchestrator.py --dry-run "Alberta separatism"
"""

import argparse
import json
import subprocess
import sys
import time
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Callable

from agent.story_generator_base import (
    process_response,
    save_story,
    strip_json_fences,
    retry_with_backoff,
    REPO_ROOT,
    JSON_DIR,
    TESTSETS_DIR,
    _get_client,
    _load_context_file,
    build_prompt,
    _SYSTEM_INSTRUCTION,
)

# ---------------------------------------------------------------------------
# Data classes
# ---------------------------------------------------------------------------

@dataclass
class StepResult:
    step: str           # research | decompose | generate | corpus_update | reports | essay
    status: str         # success | error | skipped
    data: Any = None    # step-specific payload
    error: str = ""
    tokens_in: int = 0
    tokens_out: int = 0
    duration_s: float = 0.0


@dataclass
class PipelineResult:
    family_id: str = ""
    domain: str = ""
    scope_manifest: dict | None = None
    stories: list[dict] = field(default_factory=list)
    report_paths: list[Path] = field(default_factory=list)
    essay: str = ""
    steps: list[StepResult] = field(default_factory=list)
    total_tokens_in: int = 0
    total_tokens_out: int = 0
    total_duration_s: float = 0.0


# ---------------------------------------------------------------------------
# Orchestrator
# ---------------------------------------------------------------------------

class DRAuditOrchestrator:
    """Pure-Python DR audit pipeline.  No Streamlit dependency."""

    MODELS = {
        "researcher": "gemini-2.0-flash",
        "architect":  "gemini-2.5-pro",
        "essayist":   "gemini-2.5-pro",
    }

    def __init__(
        self,
        axes: int = 3,
        skip_corpus_update: bool = False,
        skip_search: bool = False,
        skip_essay: bool = False,
        dry_run: bool = False,
        progress_callback: Callable[[str, str], None] | None = None,
    ):
        self.axes = axes
        self.skip_corpus_update = skip_corpus_update
        self.skip_search = skip_search
        self.skip_essay = skip_essay
        self.dry_run = dry_run
        self._progress = progress_callback or (lambda step, msg: print(f"[{step}] {msg}"))

        # Load protocol files (cached via lru_cache in story_generator_base)
        self.protocols = {
            "uke_scope":  _load_context_file(str(REPO_ROOT / "prompts" / "uke_scope_v2_json.md")),
            "gen_prompt": _load_context_file(str(REPO_ROOT / "prompts" / "constraint_story_generation_prompt_json.md")),
            "schema":     _load_context_file(str(REPO_ROOT / "python" / "constraint_story_schema.json")),
            "example":    _load_context_file(str(REPO_ROOT / "json" / "antifragility.json")),
            "uke_w":      _load_context_file(str(Path(__file__).parent / "uke_write_v2.1.md")),
        }

    # ------------------------------------------------------------------
    # Gemini call helper
    # ------------------------------------------------------------------

    def _call(self, prompt, model, system_instruction="", temperature=0.2, tools=None):
        """Call Gemini and return (text, tokens_in, tokens_out)."""
        from google.genai import types

        client = _get_client()
        config_kwargs = {"temperature": temperature}
        if system_instruction:
            config_kwargs["system_instruction"] = system_instruction
        if tools:
            config_kwargs["tools"] = tools

        config = types.GenerateContentConfig(**config_kwargs)

        response = retry_with_backoff(
            client.models.generate_content,
            model=model,
            contents=prompt,
            config=config,
        )

        text = response.text if response else ""
        meta = getattr(response, "usage_metadata", None)
        tokens_in = getattr(meta, "prompt_token_count", 0) or 0
        tokens_out = getattr(meta, "candidates_token_count", 0) or 0
        return text, tokens_in, tokens_out

    # ------------------------------------------------------------------
    # Pipeline
    # ------------------------------------------------------------------

    def run(self, topic: str) -> PipelineResult:
        """Execute the full DR audit pipeline for *topic*."""
        result = PipelineResult()
        t0 = time.time()

        # Step 1: Research
        step = self._step_research(topic)
        result.steps.append(step)
        research_context = step.data or topic

        # Step 2: Decompose (SCOPE)
        step = self._step_decompose(topic, research_context)
        result.steps.append(step)

        if step.status == "error":
            result.total_duration_s = time.time() - t0
            self._tally_tokens(result)
            return result

        manifest = step.data
        result.scope_manifest = manifest
        result.family_id = manifest.get("family_id", "")
        result.domain = manifest.get("domain", "")

        if self.dry_run:
            self._progress("dry_run", "Manifest assembled — dry-run stops here")
            result.total_duration_s = time.time() - t0
            self._tally_tokens(result)
            return result

        # Step 3: Generate constraint stories
        step = self._step_generate(manifest)
        result.steps.append(step)
        result.stories = step.data or []

        generated_ids = [s["header"]["constraint_id"] for s in result.stories]

        # Step 4: Corpus update
        step = self._step_corpus_update()
        result.steps.append(step)

        # Step 5: Enhanced reports
        step = self._step_reports(generated_ids)
        result.steps.append(step)
        result.report_paths = step.data or []

        # Step 6: Essay
        step = self._step_essay(manifest, result.report_paths, research_context)
        result.steps.append(step)
        result.essay = step.data or ""

        result.total_duration_s = time.time() - t0
        self._tally_tokens(result)
        return result

    # ------------------------------------------------------------------
    # Step 1: Research (search grounding)
    # ------------------------------------------------------------------

    def _step_research(self, topic: str) -> StepResult:
        if self.skip_search:
            return StepResult(step="research", status="skipped")

        self._progress("research", "Running search grounding...")
        t0 = time.time()

        try:
            from google.genai import types
            search_tool = types.Tool(google_search=types.GoogleSearch())
            prompt = (
                f"Research the following topic thoroughly. Provide factual background, "
                f"key actors, recent developments, structural tensions, and data points.\n\n"
                f"TOPIC: {topic}"
            )
            text, tin, tout = self._call(
                prompt,
                model=self.MODELS["researcher"],
                temperature=0.1,
                tools=[search_tool],
            )
            self._progress("research", "Search grounding complete")
            return StepResult(
                step="research", status="success", data=text,
                tokens_in=tin, tokens_out=tout,
                duration_s=time.time() - t0,
            )
        except Exception as e:
            self._progress("research", f"Search failed, proceeding without: {e}")
            return StepResult(
                step="research", status="error", error=str(e),
                duration_s=time.time() - t0,
            )

    # ------------------------------------------------------------------
    # Step 2: Decompose (UKE_SCOPE)
    # ------------------------------------------------------------------

    def _step_decompose(self, topic: str, research_context: str) -> StepResult:
        self._progress("decompose", "Running UKE_SCOPE decomposition...")
        t0 = time.time()

        prompt = (
            f"Analyze the following topic using the UKE_SCOPE protocol.\n\n"
            f"TOPIC: {topic}\n\n"
            f"RESEARCH CONTEXT:\n{research_context}\n\n"
            f"Select exactly {self.axes} axes for generation.\n\n"
            f"Remember: OUTPUT ONLY valid JSON — no markdown fences, no commentary outside the JSON."
        )

        try:
            text, tin, tout = self._call(
                prompt,
                model=self.MODELS["architect"],
                system_instruction=self.protocols["uke_scope"],
                temperature=0.2,
            )
        except Exception as e:
            self._progress("decompose", f"SCOPE call failed: {e}")
            return StepResult(
                step="decompose", status="error", error=str(e),
                duration_s=time.time() - t0,
            )

        # Parse JSON
        try:
            manifest = json.loads(strip_json_fences(text))
        except json.JSONDecodeError as e:
            self._progress("decompose", f"JSON parse failed: {e}")
            return StepResult(
                step="decompose", status="error",
                error=f"JSON parse failed: {e}\nRaw output:\n{text[:500]}",
                duration_s=time.time() - t0,
            )

        # Validate required fields
        required = ["protocol", "domain", "family_id", "axes", "generation_sequence"]
        missing = [f for f in required if f not in manifest]
        if missing:
            self._progress("decompose", f"Manifest missing fields: {missing}")
            return StepResult(
                step="decompose", status="error",
                error=f"Missing required fields: {missing}",
                data=manifest,
                duration_s=time.time() - t0,
            )

        # Check for fracture scan warnings
        fracture = manifest.get("fracture_scan", {})
        if fracture.get("f03_hasty_generalization") or fracture.get("f34_epistemic_trespass"):
            notes = fracture.get("notes", "")
            self._progress("decompose", f"Fracture warning: {notes}")

        self._progress("decompose", f"SCOPE complete — {len(manifest['generation_sequence'])} axes selected")
        return StepResult(
            step="decompose", status="success", data=manifest,
            tokens_in=tin, tokens_out=tout,
            duration_s=time.time() - t0,
        )

    # ------------------------------------------------------------------
    # Step 3: Generate constraint stories
    # ------------------------------------------------------------------

    def _step_generate(self, manifest: dict) -> StepResult:
        self._progress("generate", "Generating constraint stories...")
        t0 = time.time()

        sequence = manifest["generation_sequence"]
        axes_by_id = {a["claim_id"]: a for a in manifest["axes"]}
        generated_stories = []
        total_tin, total_tout = 0, 0

        for i, claim_id in enumerate(sequence):
            axis = axes_by_id.get(claim_id)
            if not axis:
                self._progress("generate", f"Axis {claim_id} not found in manifest, skipping")
                continue

            self._progress("generate", f"[{i+1}/{len(sequence)}] Generating {claim_id}...")

            # Build source description from axis fields
            source_desc = (
                f"TOPIC: {manifest.get('domain', 'Unknown')}\n"
                f"CONSTRAINT: {claim_id}\n"
                f"Structural delta: {axis['structural_delta']}\n"
                f"Primary observable: {axis['primary_observable']}\n"
                f"Hypothesis type: {axis['hypothesis']}\n"
                f"Epsilon bin: {axis['epsilon_bin']}"
            )
            if axis.get("beneficiary"):
                source_desc += f"\nBeneficiary: {axis['beneficiary']}"
            if axis.get("victim"):
                source_desc += f"\nVictim: {axis['victim']}"

            # Build upstream context for downstream axes (§5.1)
            upstream_context = ""
            for upstream_id in axis.get("downstream_of", []):
                upstream_story = next(
                    (s for s in generated_stories
                     if s["header"]["constraint_id"] == upstream_id),
                    None,
                )
                if upstream_story:
                    upstream_context += (
                        f"\nUPSTREAM CONSTRAINT: {upstream_id}\n"
                        f"  claimed_type: {upstream_story['base_properties'].get('claimed_type', 'unknown')}\n"
                        f"  affects_constraint: {upstream_id} → {claim_id}\n"
                    )

            context_text = upstream_context if upstream_context else ""

            # Build prompt and call
            prompt = build_prompt(source_desc, context_text)

            try:
                text, tin, tout = self._call(
                    prompt,
                    model=self.MODELS["architect"],
                    system_instruction=_SYSTEM_INSTRUCTION,
                    temperature=0.2,
                )
                total_tin += tin
                total_tout += tout
            except Exception as e:
                self._progress("generate", f"API error for {claim_id}: {e}")
                continue

            if not text:
                self._progress("generate", f"Empty response for {claim_id}")
                continue

            # Process and validate
            story_dict, errors = process_response(text)

            if story_dict is None or errors:
                # Retry once with error feedback
                self._progress("generate", f"Validation errors for {claim_id}, retrying...")
                feedback = ""
                if errors:
                    feedback = "\nYour previous attempt had these validation errors:\n"
                    for err in errors:
                        feedback += f"  - {err}\n"
                    feedback += "Fix these specific errors while keeping the rest correct.\n"

                retry_prompt = build_prompt(source_desc, context_text + feedback)
                try:
                    text, tin2, tout2 = self._call(
                        retry_prompt,
                        model=self.MODELS["architect"],
                        system_instruction=_SYSTEM_INSTRUCTION,
                        temperature=0.2,
                    )
                    total_tin += tin2
                    total_tout += tout2
                    story_dict, errors = process_response(text)
                except Exception as e:
                    self._progress("generate", f"Retry failed for {claim_id}: {e}")
                    continue

            if story_dict is None or errors:
                self._progress("generate", f"Failed to generate valid story for {claim_id}")
                continue

            # Save
            json_path, pl_path = save_story(story_dict, overwrite=True)
            if json_path:
                generated_stories.append(story_dict)
                self._progress("generate", f"Saved {claim_id}")

        self._progress("generate", f"Generated {len(generated_stories)}/{len(sequence)} stories")
        return StepResult(
            step="generate", status="success", data=generated_stories,
            tokens_in=total_tin, tokens_out=total_tout,
            duration_s=time.time() - t0,
        )

    # ------------------------------------------------------------------
    # Step 4: Corpus update
    # ------------------------------------------------------------------

    def _step_corpus_update(self) -> StepResult:
        if self.skip_corpus_update:
            return StepResult(step="corpus_update", status="skipped")

        self._progress("corpus_update", "Running make quick...")
        t0 = time.time()

        try:
            proc = subprocess.run(
                ["make", "-j4", "quick"],
                cwd=str(REPO_ROOT),
                capture_output=True,
                text=True,
                timeout=600,
            )
            if proc.returncode != 0:
                self._progress("corpus_update", f"make quick returned {proc.returncode}")
                return StepResult(
                    step="corpus_update", status="error",
                    error=proc.stderr[:500],
                    duration_s=time.time() - t0,
                )
            self._progress("corpus_update", "Corpus update complete")
            return StepResult(
                step="corpus_update", status="success",
                duration_s=time.time() - t0,
            )
        except subprocess.TimeoutExpired:
            self._progress("corpus_update", "make quick timed out (600s)")
            return StepResult(
                step="corpus_update", status="error",
                error="Timeout after 600s",
                duration_s=time.time() - t0,
            )
        except Exception as e:
            self._progress("corpus_update", f"Corpus update failed: {e}")
            return StepResult(
                step="corpus_update", status="error", error=str(e),
                duration_s=time.time() - t0,
            )

    # ------------------------------------------------------------------
    # Step 5: Enhanced reports
    # ------------------------------------------------------------------

    def _step_reports(self, constraint_ids: list[str]) -> StepResult:
        if not constraint_ids:
            return StepResult(step="reports", status="skipped")

        self._progress("reports", f"Generating reports for {len(constraint_ids)} constraints...")
        t0 = time.time()

        try:
            proc = subprocess.run(
                ["python3", "python/enhanced_report.py"] + constraint_ids,
                cwd=str(REPO_ROOT),
                capture_output=True,
                text=True,
                timeout=300,
            )
            if proc.returncode != 0:
                self._progress("reports", f"enhanced_report.py returned {proc.returncode}")
                # Continue — partial reports may exist
        except subprocess.TimeoutExpired:
            self._progress("reports", "Report generation timed out (300s)")
        except Exception as e:
            self._progress("reports", f"Report generation failed: {e}")
            return StepResult(
                step="reports", status="error", error=str(e),
                duration_s=time.time() - t0,
            )

        # Collect report paths
        reports_dir = REPO_ROOT / "outputs" / "constraint_reports"
        report_paths = []
        for cid in constraint_ids:
            rpath = reports_dir / f"{cid}_report.md"
            if rpath.exists():
                report_paths.append(rpath)

        self._progress("reports", f"Found {len(report_paths)}/{len(constraint_ids)} reports")
        return StepResult(
            step="reports", status="success", data=report_paths,
            duration_s=time.time() - t0,
        )

    # ------------------------------------------------------------------
    # Step 6: Essay synthesis
    # ------------------------------------------------------------------

    def _step_essay(self, manifest: dict, report_paths: list[Path],
                    research_context: str) -> StepResult:
        if self.skip_essay:
            return StepResult(step="essay", status="skipped")

        self._progress("essay", "Synthesizing essay...")
        t0 = time.time()

        # Gather report texts
        report_texts = []
        for rp in report_paths:
            try:
                report_texts.append(rp.read_text(encoding="utf-8"))
            except Exception:
                pass

        # Build essay prompt
        prompt = (
            f"Write a comprehensive analytical essay based on the following materials.\n\n"
            f"=== SCOPE MANIFEST ===\n{json.dumps(manifest, indent=2)}\n\n"
            f"=== RESEARCH CONTEXT ===\n{research_context}\n\n"
        )
        if report_texts:
            prompt += "=== CONSTRAINT REPORTS ===\n"
            for rt in report_texts:
                prompt += f"\n---\n{rt}\n"

        try:
            text, tin, tout = self._call(
                prompt,
                model=self.MODELS["essayist"],
                system_instruction=self.protocols["uke_w"],
                temperature=0.7,
            )
        except Exception as e:
            self._progress("essay", f"Essay generation failed: {e}")
            return StepResult(
                step="essay", status="error", error=str(e),
                duration_s=time.time() - t0,
            )

        # Save essay
        slug = manifest.get("family_id", "essay")
        essays_dir = REPO_ROOT / "outputs" / "essays"
        essays_dir.mkdir(parents=True, exist_ok=True)
        essay_path = essays_dir / f"{slug}.md"
        essay_path.write_text(text, encoding="utf-8")

        self._progress("essay", f"Essay saved to {essay_path.relative_to(REPO_ROOT)}")
        return StepResult(
            step="essay", status="success", data=text,
            tokens_in=tin, tokens_out=tout,
            duration_s=time.time() - t0,
        )

    # ------------------------------------------------------------------
    # Helpers
    # ------------------------------------------------------------------

    @staticmethod
    def _tally_tokens(result: PipelineResult):
        result.total_tokens_in = sum(s.tokens_in for s in result.steps)
        result.total_tokens_out = sum(s.tokens_out for s in result.steps)


# ---------------------------------------------------------------------------
# CLI entry point
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(description="DR Audit Pipeline")
    parser.add_argument("topic", nargs="?", help="Topic text (or use --input-file / stdin)")
    parser.add_argument("--input-file", "-f", help="Read topic from file")
    parser.add_argument("--axes", type=int, default=3, help="Number of axes to select (default: 3)")
    parser.add_argument("--skip-corpus-update", action="store_true", help="Skip make quick")
    parser.add_argument("--skip-search", action="store_true", help="Skip search grounding")
    parser.add_argument("--skip-essay", action="store_true", help="Skip essay synthesis")
    parser.add_argument("--dry-run", action="store_true", help="Run SCOPE only, print manifest")
    args = parser.parse_args()

    # Resolve topic
    if args.topic:
        topic = args.topic
    elif args.input_file:
        topic = Path(args.input_file).read_text(encoding="utf-8").strip()
    elif not sys.stdin.isatty():
        topic = sys.stdin.read().strip()
    else:
        parser.error("Provide a topic as argument, via --input-file, or on stdin")

    orch = DRAuditOrchestrator(
        axes=args.axes,
        skip_corpus_update=args.skip_corpus_update,
        skip_search=args.skip_search,
        skip_essay=args.skip_essay,
        dry_run=args.dry_run,
    )
    result = orch.run(topic)

    # Print summary
    print("\n" + "=" * 60)
    print("PIPELINE SUMMARY")
    print("=" * 60)
    for s in result.steps:
        tok = f" ({s.tokens_in}→{s.tokens_out} tokens)" if s.tokens_in else ""
        dur = f" [{s.duration_s:.1f}s]" if s.duration_s else ""
        print(f"  {s.step:20s} {s.status:8s}{tok}{dur}")
        if s.error:
            print(f"    error: {s.error[:200]}")
    print(f"\n  Total tokens: {result.total_tokens_in}→{result.total_tokens_out}")
    print(f"  Total time:   {result.total_duration_s:.1f}s")

    if result.scope_manifest and args.dry_run:
        print("\n" + "=" * 60)
        print("SCOPE MANIFEST")
        print("=" * 60)
        print(json.dumps(result.scope_manifest, indent=2))

    if result.essay:
        print("\n" + "=" * 60)
        print("ESSAY")
        print("=" * 60)
        print(result.essay)


if __name__ == "__main__":
    main()
