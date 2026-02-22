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
import re
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
    step: str           # research | decompose | generate | corpus_update | reports | iterate | essay
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

        # Step 5.5: Iterate on non-GREEN verdicts
        if not self.skip_essay and result.report_paths:
            step = self._step_iterate(result.stories, result.report_paths)
            result.steps.append(step)
            if step.data:
                result.stories = step.data.get("stories", result.stories)
                result.report_paths = step.data.get("report_paths", result.report_paths)

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
    # Step 5.5: Verdict-driven iteration
    # ------------------------------------------------------------------

    EXPECTED_TENSION_CODES = {
        "cohomological_fracture_divergence",
        "perspectival_orbit_variance",
        "constructed_non_compliance",
        "fcr_deferred_signature_mismatch",
    }

    def _parse_report(self, report_text: str) -> dict:
        """Extract structured data from a deterministic enhanced report."""
        parsed: dict[str, Any] = {}

        # Verdict banner
        m = re.search(r"VERDICT:\s*(GREEN|YELLOW|RED)", report_text)
        parsed["verdict"] = m.group(1) if m else "UNKNOWN"

        m = re.search(r"(\d+)/(\d+)\s*subsystems", report_text)
        parsed["subsystems_checked"] = (int(m.group(1)), int(m.group(2))) if m else (0, 0)

        m = re.search(r"(\d+)\s*tension\(s\)", report_text)
        parsed["tension_count"] = int(m.group(1)) if m else 0

        # Tensions list — each line: "    subsystem: code(detail...)"
        tensions = []
        m = re.search(r"Tensions\s*\(\d+\):\s*\n((?:\s+\w+:.*\n)+)", report_text)
        if m:
            for line in m.group(1).strip().splitlines():
                line = line.strip()
                if not line:
                    continue
                colon_idx = line.find(":")
                if colon_idx > 0:
                    subsystem = line[:colon_idx].strip()
                    rest = line[colon_idx + 1:].strip()
                    # Extract functor name (word before first '(')
                    paren_idx = rest.find("(")
                    code = rest[:paren_idx].strip() if paren_idx > 0 else rest
                    tensions.append({
                        "subsystem": subsystem,
                        "code": code,
                        "detail": rest,
                    })
        parsed["tensions"] = tensions

        # Expected Conflicts
        expected_conflicts = []
        m = re.search(
            r"Expected Conflicts\s*\(\d+\):\s*\n((?:\s+\w+:.*\n(?:\s{6}.*\n)*)+)",
            report_text,
        )
        if m:
            for line in m.group(1).strip().splitlines():
                line = line.strip()
                if not line:
                    continue
                colon_idx = line.find(":")
                if colon_idx > 0:
                    subsystem = line[:colon_idx].strip()
                    code = line[colon_idx + 1:].strip()
                    expected_conflicts.append({"subsystem": subsystem, "code": code})
        parsed["expected_conflicts"] = expected_conflicts

        # Convergent Rejections
        m = re.search(r"Convergent Rejections:\s*(.+)", report_text)
        parsed["convergent_rejections"] = m.group(1).strip() if m else "none"

        # Classification data
        classification: dict[str, Any] = {}

        m = re.search(r"Confidence:\s+([\d.]+)\s+\((\w+)\)", report_text)
        if m:
            classification["confidence"] = float(m.group(1))
            classification["confidence_band"] = m.group(2)

        m = re.search(r"Rival Type:\s+(\w+)\s+\(P=([\d.]+)\)", report_text)
        if m:
            classification["rival_type"] = m.group(1)
            classification["rival_p"] = float(m.group(2))

        m = re.search(r"Boundary:\s+(\S+)", report_text)
        if m:
            classification["boundary"] = m.group(1)

        m = re.search(r"Tangled psi:\s+([\d.]+)\s+\((\w+)\)", report_text)
        if m:
            classification["psi"] = float(m.group(1))
            classification["psi_label"] = m.group(2)

        m = re.search(r"Claimed Type:\s+(\w+)", report_text)
        if m:
            classification["claimed_type"] = m.group(1)

        parsed["classification"] = classification

        # Hard disagreement
        m = re.search(r"HARD DISAGREEMENT: Pipeline says (\w+), MaxEnt says (\w+)", report_text)
        if m:
            parsed["hard_disagreement"] = {
                "pipeline": m.group(1),
                "maxent": m.group(2),
            }

        # Drift events
        drift_events = []
        for dm in re.finditer(r"\[(\w+)\]\s+(\w+)\s*\n\s+Evidence:", report_text):
            drift_events.append({"severity": dm.group(1), "type": dm.group(2)})
        parsed["drift_events"] = drift_events

        # Mandatrophy gap
        m = re.search(r"MANDATROPHY GAP: delta_chi = ([\d.]+)\s+\((\w+)\)", report_text)
        if m:
            parsed["mandatrophy_gap"] = {
                "delta_chi": float(m.group(1)),
                "severity": m.group(2),
            }

        # Structural signature
        m = re.search(r"Signature:\s+(\w+)", report_text)
        if m:
            parsed["structural_signature"] = m.group(1)

        # Purity
        m = re.search(r"Purity:\s+([\d.]+)\s+\((\w+)\)", report_text)
        if m:
            parsed["purity"] = {
                "value": float(m.group(1)),
                "band": m.group(2),
            }

        return parsed

    def _should_iterate(self, parsed_report: dict) -> bool:
        """Decide whether to iterate on this axis based on report verdict."""
        verdict = parsed_report.get("verdict", "UNKNOWN")

        if verdict == "GREEN":
            return False

        if verdict == "RED":
            return True

        # YELLOW cases
        tensions = parsed_report.get("tensions", [])
        convergent = parsed_report.get("convergent_rejections", "none")

        # YELLOW with convergent rejections → iterate
        if convergent.lower() != "none":
            return True

        # YELLOW with low confidence → iterate
        confidence = parsed_report.get("classification", {}).get("confidence", 1.0)
        if confidence < 0.3:
            return True

        # YELLOW with only expected-conflict codes and no tensions → accept
        if not tensions:
            return False

        # YELLOW with tensions → iterate
        return True

    def _format_tensions(self, tensions: list[dict]) -> str:
        """Convert parsed tensions into human-readable descriptions."""
        if not tensions:
            return "No tensions detected."
        lines = []
        for t in tensions:
            lines.append(f"- [{t['subsystem']}] {t['code']}: {t['detail']}")
        return "\n".join(lines)

    def _build_iteration_prompt(
        self, story_dict: dict, parsed_report: dict, iteration_num: int
    ) -> str:
        """Build a prompt asking the LLM to adjust a constraint story to resolve tensions."""
        verdict = parsed_report.get("verdict", "UNKNOWN")
        tensions = parsed_report.get("tensions", [])
        cls = parsed_report.get("classification", {})

        tension_text = self._format_tensions(tensions)

        # Tension-to-adjustment guidance
        guidance_lines = []
        for t in tensions:
            code = t["code"]
            if "abductive" in code:
                guidance_lines.append(
                    "- abductive_tension: Review extractiveness/suppression/theater_ratio "
                    "values. The diagnostic detected shadow classification divergence. "
                    "Adjust base_properties metrics to better match the claimed_type, or "
                    "change claimed_type if the metrics are correct."
                )
            elif "drift" in code or "critical_drift" in code:
                guidance_lines.append(
                    "- critical_drift: Review measurements timeline. Ensure metric "
                    "trajectories don't show runaway extraction or theater accumulation "
                    "that contradicts a stable claimed_type. Adjust measurement values "
                    "at later time indices to show stabilization or resolution."
                )
            elif "purity" in code:
                guidance_lines.append(
                    "- purity_tension: Review coupling and extraction metrics. High "
                    "coupling with rising extraction degrades purity. Consider reducing "
                    "extractiveness at some perspectives or adjusting measurement values."
                )
            elif "maxent" in code:
                guidance_lines.append(
                    "- maxent_divergence: The MaxEnt classifier disagrees with the "
                    "claimed type. Adjust base_properties or perspective classifications "
                    "to bring classifier agreement."
                )
            else:
                guidance_lines.append(
                    f"- {code}: Adjust story parameters to resolve this diagnostic tension."
                )
        guidance = "\n".join(guidance_lines) if guidance_lines else "No specific guidance."

        prompt = (
            f"=== ITERATION {iteration_num} — VERDICT: {verdict} ===\n\n"
            f"The diagnostic report for this constraint story returned a {verdict} verdict "
            f"with the following tensions:\n\n{tension_text}\n\n"
            f"Classification context:\n"
            f"  Claimed type: {cls.get('claimed_type', 'unknown')}\n"
            f"  Rival type: {cls.get('rival_type', 'unknown')} "
            f"(P={cls.get('rival_p', 'N/A')})\n"
            f"  Confidence: {cls.get('confidence', 'N/A')} "
            f"({cls.get('confidence_band', 'N/A')})\n"
            f"  Boundary: {cls.get('boundary', 'N/A')}\n\n"
            f"=== ADJUSTMENT GUIDANCE ===\n{guidance}\n\n"
            f"=== RULES ===\n"
            f"- You MUST NOT change: header.constraint_id, header.version\n"
            f"- You MAY change: base_properties (extractiveness, suppression, "
            f"theater_ratio, claimed_type), perspectives (classification_type, "
            f"agent_power, exit_options), measurements (values at time indices)\n"
            f"- Changes should be minimal and targeted to resolve the tensions\n"
            f"- The story must remain internally coherent\n\n"
            f"=== CURRENT STORY JSON ===\n"
            f"{json.dumps(story_dict, indent=2)}\n\n"
            f"Output ONLY the adjusted JSON — no markdown fences, no commentary."
        )
        return prompt

    def _rerun_single_report(self, constraint_id: str) -> Path | None:
        """Re-run enhanced_report.py for a single constraint and return the report path."""
        try:
            proc = subprocess.run(
                ["python3", "python/enhanced_report.py", constraint_id],
                cwd=str(REPO_ROOT),
                capture_output=True,
                text=True,
                timeout=120,
            )
            if proc.returncode != 0:
                self._progress("iterate", f"Report re-run failed for {constraint_id}: {proc.stderr[:200]}")
        except (subprocess.TimeoutExpired, Exception) as e:
            self._progress("iterate", f"Report re-run error for {constraint_id}: {e}")

        report_path = REPO_ROOT / "outputs" / "constraint_reports" / f"{constraint_id}_report.md"
        return report_path if report_path.exists() else None

    def _step_iterate(self, stories: list[dict], report_paths: list[Path]) -> StepResult:
        """Iterate on non-GREEN verdicts: adjust stories and re-run diagnostics."""
        self._progress("iterate", "Checking verdicts for iteration...")
        t0 = time.time()

        MAX_ITERATIONS = 3

        # Build lookup dicts
        stories_by_cid = {s["header"]["constraint_id"]: s for s in stories}
        reports_by_cid = {rp.stem.replace("_report", ""): rp for rp in report_paths}

        total_tin, total_tout = 0, 0
        iteration_stats: dict[str, dict] = {}  # cid → {iterations, final_verdict, tokens_in, tokens_out}

        for cid, report_path in reports_by_cid.items():
            story = stories_by_cid.get(cid)
            if not story:
                continue

            try:
                report_text = report_path.read_text(encoding="utf-8")
            except Exception:
                continue

            parsed = self._parse_report(report_text)

            if not self._should_iterate(parsed):
                self._progress("iterate", f"{cid}: {parsed['verdict']} — no iteration needed")
                iteration_stats[cid] = {
                    "iterations": 0,
                    "final_verdict": parsed["verdict"],
                    "tokens_in": 0,
                    "tokens_out": 0,
                }
                continue

            self._progress("iterate", f"{cid}: {parsed['verdict']} — iterating...")
            axis_tin, axis_tout = 0, 0
            iterations_run = 0

            for iteration in range(1, MAX_ITERATIONS + 1):
                iterations_run = iteration
                self._progress("iterate", f"{cid}: iteration {iteration}/{MAX_ITERATIONS}")

                prompt = self._build_iteration_prompt(story, parsed, iteration)

                try:
                    raw_text, tin, tout = self._call(
                        prompt,
                        model=self.MODELS["architect"],
                        system_instruction=self.protocols["gen_prompt"],
                        temperature=0.2,
                    )
                    axis_tin += tin
                    axis_tout += tout
                except Exception as e:
                    self._progress("iterate", f"{cid}: LLM call failed on iteration {iteration}: {e}")
                    continue

                if not raw_text:
                    self._progress("iterate", f"{cid}: empty response on iteration {iteration}")
                    continue

                # Validate
                new_story, errors = process_response(raw_text)

                if new_story is None or errors:
                    err_summary = "; ".join(errors[:3]) if errors else "parse failure"
                    self._progress("iterate", f"{cid}: validation failed ({err_summary}), continuing")
                    continue

                # Success — overwrite story files
                save_story(new_story, overwrite=True)
                story = new_story
                stories_by_cid[cid] = new_story

                # Re-run report
                new_report_path = self._rerun_single_report(cid)
                if new_report_path:
                    reports_by_cid[cid] = new_report_path
                    try:
                        report_text = new_report_path.read_text(encoding="utf-8")
                        parsed = self._parse_report(report_text)
                    except Exception:
                        break
                else:
                    break

                # Check verdict
                if parsed["verdict"] == "GREEN":
                    self._progress("iterate", f"{cid}: reached GREEN on iteration {iteration}")
                    break

                if parsed["verdict"] == "YELLOW" and iteration >= 2:
                    self._progress("iterate", f"{cid}: accepting YELLOW after iteration {iteration}")
                    break

            total_tin += axis_tin
            total_tout += axis_tout
            iteration_stats[cid] = {
                "iterations": iterations_run,
                "final_verdict": parsed["verdict"],
                "tokens_in": axis_tin,
                "tokens_out": axis_tout,
            }

        # Rebuild lists from dicts to preserve order
        updated_stories = []
        for s in stories:
            cid = s["header"]["constraint_id"]
            updated_stories.append(stories_by_cid.get(cid, s))

        updated_report_paths = []
        for rp in report_paths:
            cid = rp.stem.replace("_report", "")
            updated_report_paths.append(reports_by_cid.get(cid, rp))

        iterated_count = sum(1 for v in iteration_stats.values() if v["iterations"] > 0)
        self._progress(
            "iterate",
            f"Iteration complete — {iterated_count} axis(es) iterated, "
            f"{total_tin}→{total_tout} tokens"
        )

        return StepResult(
            step="iterate",
            status="success",
            data={
                "stories": updated_stories,
                "report_paths": updated_report_paths,
                "iteration_stats": iteration_stats,
            },
            tokens_in=total_tin,
            tokens_out=total_tout,
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
