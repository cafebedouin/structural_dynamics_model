"""DR Audit Pipeline — CLI-first orchestrator (Claude/Anthropic version).

Drop-in replacement for the Gemini orchestrator.  Swaps google.genai
for the Anthropic Python SDK while keeping all pipeline logic identical.

Chains: research → SCOPE → generate → corpus update → reports → essay.
No Streamlit imports.  Pure Python with optional progress callback.

Usage:
    python3 agent/orchestrator.py "Alberta separatism"
    python3 agent/orchestrator.py narrative_transform/originals/the_bridge.md
    python3 agent/orchestrator.py --input-file topic.txt --axes 3
    python3 agent/orchestrator.py --dry-run "Alberta separatism"
"""

import argparse
import json
import os
import subprocess
import sys
import time
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Callable

root_path = Path(__file__).resolve().parent.parent
if str(root_path) not in sys.path:
    sys.path.insert(0, str(root_path))

from agent.story_generator_base import (
    process_response,
    save_story,
    save_story_tagged,
    strip_extra_properties,
    load_schema,
    validate_json,
    strip_json_fences,
    REPO_ROOT,
    JSON_DIR,
    TESTSETS_DIR,
    _load_context_file,
    build_prompt,
    build_prompt_parts,
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
# Anthropic client helper
# ---------------------------------------------------------------------------

_anthropic_client = None

def _get_client():
    """Return a cached Anthropic client instance."""
    global _anthropic_client
    if _anthropic_client is None:
        import anthropic
        _anthropic_client = anthropic.Anthropic()   # reads ANTHROPIC_API_KEY from env
    return _anthropic_client


# ---------------------------------------------------------------------------
# Retry feedback sanitizer — strips numeric bound values so schema cutpoints
# are not leaked back to the model via jsonschema error messages on retry.
# ---------------------------------------------------------------------------
import re as _re

def _sanitize_feedback_error(msg: str) -> str:
    """Remove numeric threshold values from jsonschema validation error text."""
    msg = _re.sub(
        r'\d+(?:\.\d+)? is less than the minimum of \d+(?:\.\d+)?',
        'value is below the required minimum for this field', msg)
    msg = _re.sub(
        r'\d+(?:\.\d+)? is greater than the maximum of \d+(?:\.\d+)?',
        'value exceeds the required maximum for this field', msg)
    msg = _re.sub(
        r'\d+(?:\.\d+)? is less than or equal to the exclusiveMinimum(?:,? \d+(?:\.\d+)?)?',
        'value does not meet the required exclusive minimum', msg)
    msg = _re.sub(
        r'\d+(?:\.\d+)? is greater than or equal to the exclusiveMaximum(?:,? \d+(?:\.\d+)?)?',
        'value exceeds the required exclusive maximum', msg)
    return msg


# ---------------------------------------------------------------------------
# Orchestrator
# ---------------------------------------------------------------------------

class DRAuditOrchestrator:
    """Pure-Python DR audit pipeline.  No Streamlit dependency."""

    # ── Model mapping ────────────────────────────────────────────────
    # researcher : fast + cheap, equivalent to gemini-2.0-flash
    # architect  : structured output workhorse, equivalent to gemini-2.5-pro
    # essayist   : long-form synthesis, equivalent to gemini-2.5-pro
    #
    # Change these to claude-opus-4-5-20251101 (or claude-haiku-4-5-20251001)
    # if you want to test at different price points.
    MODELS = {
        "researcher": "claude-haiku-4-5-20251001",
        "architect":  "claude-sonnet-4-5-20250929",
        "essayist":   "claude-sonnet-4-5-20250929",
    }

    # Default max_tokens per role (Claude requires an explicit cap)
    MAX_TOKENS = {
        "researcher": 4096,
        "architect":  8192,
        "essayist":   12288,
    }

    def __init__(
        self,
        axes: int | None = None,
        serial_generate: bool = False,
        run_tag: str | None = None,
        skip_corpus_update: bool = False,
        skip_search: bool = False,
        skip_essay: bool = False,
        dry_run: bool = False,
        manifest_file: str | None = None,
        progress_callback: Callable[[str, str], None] | None = None,
    ):
        self.axes = axes
        self.serial_generate = serial_generate
        self._run_tag = run_tag
        self.skip_corpus_update = skip_corpus_update
        self.skip_search = skip_search
        self.skip_essay = skip_essay
        self.dry_run = dry_run
        self.manifest_file = manifest_file
        self._progress = progress_callback or (lambda step, msg: print(f"[{step}] {msg}"))

        # Output dirs — run-tagged if run_tag given, else flat main corpus dirs
        if run_tag:
            self._json_dir = REPO_ROOT / "json" / run_tag
            self._testsets_dir = REPO_ROOT / "prolog" / "testsets" / run_tag
            self._manifests_dir = REPO_ROOT / "outputs" / "kernel_manifests" / run_tag
            for d in (self._json_dir, self._testsets_dir, self._manifests_dir):
                d.mkdir(parents=True, exist_ok=True)
        else:
            self._json_dir = JSON_DIR
            self._testsets_dir = TESTSETS_DIR
            self._manifests_dir = None

        # Schema dict for path-aware strip (loaded once)
        self._schema = load_schema()

        # Load protocol files (cached via lru_cache in story_generator_base)
        self.protocols = {
            "uke_scope":  _load_context_file(str(REPO_ROOT / "prompts" / "uke_scope_v2_json.md")),
            "gen_prompt": _load_context_file(os.environ.get(
                "DR_GEN_PROMPT",
                str(REPO_ROOT / "prompts" / "constraint_story_generation_prompt_json.md")
            )),
            "example":    _load_context_file(str(REPO_ROOT / "json" / "antifragility.json")),
            "uke_w":      _load_context_file(str(Path(__file__).parent / "uke_summary.md")),
        }

    # ------------------------------------------------------------------
    # Claude call helper
    # ------------------------------------------------------------------

    @staticmethod
    def _extract_text(response) -> str:
        """Pull all text blocks out of a Claude response."""
        parts = []
        for block in response.content:
            if hasattr(block, "text"):
                parts.append(block.text)
        return "\n".join(parts)

    def _call(
        self,
        prompt: str,
        model: str,
        system_instruction: str = "",
        temperature: float = 0.2,
        max_tokens: int | None = None,
        tools: list | None = None,
    ) -> tuple[str, int, int]:
        """Call Claude and return (text, tokens_in, tokens_out).

        Handles the pause_turn continuation loop required by server-side
        tools like web_search.
        """
        client = _get_client()

        if max_tokens is None:
            # Pick a sensible default based on which model-role this is
            max_tokens = 8192

        kwargs: dict[str, Any] = {
            "model": model,
            "max_tokens": max_tokens,
            "temperature": temperature,
            "messages": [{"role": "user", "content": prompt}],
        }
        if system_instruction:
            kwargs["system"] = system_instruction
        if tools:
            kwargs["tools"] = tools

        total_in, total_out = 0, 0

        response = self._call_with_retry(client, **kwargs)

        total_in += response.usage.input_tokens
        total_out += response.usage.output_tokens

        # Handle pause_turn continuation (web search may need multiple rounds)
        max_continuations = 5
        messages = kwargs["messages"]

        while response.stop_reason == "pause_turn" and max_continuations > 0:
            max_continuations -= 1
            messages = [
                {"role": "user", "content": prompt},
                {"role": "assistant", "content": response.content},
            ]
            kwargs["messages"] = messages
            response = self._call_with_retry(client, **kwargs)
            total_in += response.usage.input_tokens
            total_out += response.usage.output_tokens

        text = self._extract_text(response)
        return text, total_in, total_out

    @staticmethod
    def _call_with_retry(client, max_retries: int = 3, **kwargs):
        """Retry with exponential backoff on transient errors."""
        import anthropic

        for attempt in range(max_retries):
            try:
                return client.messages.create(**kwargs)
            except (
                anthropic.RateLimitError,
                anthropic.InternalServerError,
                anthropic.APIConnectionError,
            ) as e:
                if attempt == max_retries - 1:
                    raise
                wait = 2 ** attempt * 2      # 2s, 4s, 8s
                time.sleep(wait)
            except anthropic.APIError:
                raise                        # don't retry auth / bad request

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

        # Step 2: Decompose (SCOPE) — or load frozen manifest if --manifest-file given
        if self.manifest_file:
            try:
                frozen = json.loads(Path(self.manifest_file).read_text(encoding="utf-8"))
                required = ["protocol", "domain", "family_id", "axes", "generation_sequence"]
                missing = [f for f in required if f not in frozen]
                if missing:
                    raise ValueError(f"Frozen manifest missing fields: {missing}")
                self._progress("decompose", f"Loaded frozen manifest from {self.manifest_file} "
                               f"({len(frozen['generation_sequence'])} axes)")
                step = StepResult(step="decompose", status="success", data=frozen)
            except Exception as e:
                self._progress("decompose", f"Failed to load manifest: {e}")
                step = StepResult(step="decompose", status="error", error=str(e))
        else:
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
    # Step 1: Research (web search grounding)
    # ------------------------------------------------------------------

    def _step_research(self, topic: str) -> StepResult:
        if self.skip_search:
            return StepResult(step="research", status="skipped")

        self._progress("research", "Running web-search grounding...")
        t0 = time.time()

        try:
            web_search_tool = {
                "type": "web_search_20250305",
                "name": "web_search",
                "max_uses": 5,
            }
            prompt = (
                f"Research the following topic thoroughly. Provide factual background, "
                f"key actors, recent developments, structural tensions, and data points.\n\n"
                f"TOPIC: {topic}"
            )
            text, tin, tout = self._call(
                prompt,
                model=self.MODELS["researcher"],
                max_tokens=self.MAX_TOKENS["researcher"],
                temperature=0.1,
                tools=[web_search_tool],
            )
            self._progress("research", "Web-search grounding complete")
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

        if self.axes is None:
            axes_instruction = (
                "Select every axis that survives the §3 pairwise independence test. "
                "Do not pad to a fixed count and do not truncate distinct axes to fit one."
            )
        else:
            axes_instruction = (
                f"Select every axis that survives the §3 pairwise independence test, "
                f"up to a budget ceiling of {self.axes} axes. Do not pad to reach the ceiling."
            )
        prompt = (
            f"Analyze the following topic using the UKE_SCOPE protocol.\n\n"
            f"TOPIC: {topic}\n\n"
            f"RESEARCH CONTEXT:\n{research_context}\n\n"
            f"{axes_instruction}\n\n"
            f"Remember: OUTPUT ONLY valid JSON — no markdown fences, no commentary outside the JSON."
        )

        try:
            text, tin, tout = self._call(
                prompt,
                model=self.MODELS["architect"],
                max_tokens=self.MAX_TOKENS["architect"],
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

        self._persist_manifest(manifest)
        self._progress("decompose", f"SCOPE complete — {len(manifest['generation_sequence'])} axes selected")
        return StepResult(
            step="decompose", status="success", data=manifest,
            tokens_in=tin, tokens_out=tout,
            duration_s=time.time() - t0,
        )

    def _persist_manifest(self, manifest: dict) -> None:
        """Persist the SCOPE manifest to disk (added 2026-06-05).

        Previously flat (un-run-tagged) runs DISCARDED the manifest — the axis
        traces (structural deltas, observables, deferred axes, fracture scans,
        per-axis hypotheses) existed only in memory, which made the
        SCOPE-hypothesis → authored-claim → computed-type three-layer trace
        unrecoverable after the run (witnessed: the 2026-06-05 first three rebuild
        runs). Run-tagged runs write to outputs/kernel_manifests/<run_tag>/;
        flat runs to outputs/kernel_manifests/flat/.
        """
        try:
            mdir = self._manifests_dir or (REPO_ROOT / "outputs" / "kernel_manifests" / "flat")
            mdir.mkdir(parents=True, exist_ok=True)
            fam = manifest.get("family_id") or "manifest"
            ts = time.strftime("%Y%m%d_%H%M%S")
            path = mdir / f"{fam}_{ts}.manifest.json"
            path.write_text(json.dumps(manifest, indent=2, ensure_ascii=False),
                            encoding="utf-8")
            self._progress("decompose", f"Manifest persisted: {path}")
        except Exception as e:  # persistence must never kill the run
            self._progress("decompose", f"Manifest persist FAILED (non-fatal): {e}")

    # ------------------------------------------------------------------
    # Step 3: Generate constraint stories
    # ------------------------------------------------------------------

    def _step_generate(self, manifest: dict) -> StepResult:
        """Dispatch: batch mode by default (2026-06-05; the uncapped axis budget makes
        6-8 sequential Sonnet calls the long pole). --serial-generate or
        DR_SERIAL_GENERATE=1 keeps the legacy per-item loop, which retains the
        LLM retry-with-feedback path."""
        if self.serial_generate or os.environ.get("DR_SERIAL_GENERATE") == "1":
            return self._step_generate_serial(manifest)
        return self._step_generate_batch(manifest)

    @staticmethod
    def _axis_source_desc(manifest: dict, claim_id: str, axis: dict) -> str:
        """Per-axis source description — shared by serial and batch paths."""
        source_desc = (
            f"TOPIC: {manifest.get('domain', 'Unknown')}\n"
            f"CONSTRAINT: {claim_id}\n"
            f"Structural delta: {axis['structural_delta']}\n"
            f"Primary observable: {axis['primary_observable']}\n"
            f"Hypothesis type: {axis['hypothesis']}"
        )
        if axis.get("beneficiary"):
            source_desc += f"\nBeneficiary: {axis['beneficiary']}"
        if axis.get("victim"):
            source_desc += f"\nVictim: {axis['victim']}"
        cs_recog = manifest.get("commitment_system_recognition")
        if cs_recog:
            source_desc += f"\nCommitment System Recognition: {json.dumps(cs_recog)}"
        return source_desc

    @staticmethod
    def _upstream_context(axis: dict, generated_by_id: dict, claim_id: str) -> str:
        """§5.1 upstream context from already-generated stories (shared by both paths)."""
        upstream_context = ""
        for upstream_id in axis.get("downstream_of", []):
            upstream_story = generated_by_id.get(upstream_id)
            if upstream_story:
                upstream_context += (
                    f"\nUPSTREAM CONSTRAINT: {upstream_id}\n"
                    f"  claimed_type: {upstream_story['base_properties'].get('claimed_type', 'unknown')}\n"
                    f"  affects_constraint: {upstream_id} → {claim_id}\n"
                )
        return upstream_context

    def _step_generate_batch(self, manifest: dict) -> StepResult:
        """Batched generation (2026-06-05): each dependency WAVE is one Anthropic
        batch — parallel server-side, 50% cheaper, static prefix cache-controlled
        (pattern shared with generate_kernel_corpus; poll_batch reused from there).

        Waves preserve §5.1 upstream context: an axis whose downstream_of names
        another axis in this run generates in a later wave, with the upstream's
        claimed_type injected. A pure dependency chain degenerates to sequential
        waves, which is the correct behavior.

        The per-item LLM retry-with-feedback loop of the serial path is replaced by
        deterministic repair (strip_extra_properties + repair_story — schema-shape
        only, never reconciling authored claims to metrics). Failures fall out for a
        later regenerate pass. NO LINTING here: authored-vs-computed divergence is
        the research signal and is read downstream (enhanced_report), never "fixed"
        at generation time.
        """
        self._progress("generate", "Generating constraint stories (batch mode)...")
        t0 = time.time()
        from agent.generate_kernel_corpus import poll_batch
        from story_repair import repair_story

        sequence = manifest["generation_sequence"]
        axes_by_id = {a["claim_id"]: a for a in manifest["axes"]}

        # Resolve sequence entries → (claim_id, axis, entry), preserving order
        items = []
        seen = set()
        for entry in sequence:
            if isinstance(entry, dict):
                claim_id = entry.get("claim_id") or entry.get("constraint_id")
            else:
                claim_id = entry
            if not claim_id or claim_id in seen:
                continue
            seen.add(claim_id)
            axis = axes_by_id.get(claim_id)
            if not axis:
                self._progress("generate", f"Axis {claim_id} not found in manifest, skipping")
                continue
            items.append((claim_id, axis, entry))

        run_ids = {cid for cid, _, _ in items}
        generated_stories: list[dict] = []
        generated_by_id: dict[str, dict] = {}
        failed_ids: set[str] = set()
        total_tin = total_tout = 0
        client = _get_client()

        remaining = list(items)
        wave_no = 0
        while remaining:
            wave_no += 1
            # An item is wave-ready when every in-run upstream has been generated
            # or has terminally failed (failed upstream ⇒ generate without its context
            # rather than deadlocking).
            wave = [it for it in remaining
                    if not any(u in run_ids and u not in generated_by_id and u not in failed_ids
                               for u in (it[1].get("downstream_of") or []))]
            if not wave:
                self._progress("generate",
                               f"Wave {wave_no}: dependency cycle among "
                               f"{[c for c, _, _ in remaining]} — generating without upstream context")
                wave = remaining
            remaining = [it for it in remaining if it not in wave]

            requests, idmap, entry_map = [], {}, {}
            for idx, (cid, axis, entry) in enumerate(wave):
                source_desc = self._axis_source_desc(manifest, cid, axis)
                context_text = self._upstream_context(axis, generated_by_id, cid)
                static_prefix, dynamic_tail = build_prompt_parts(source_desc, context_text)
                custom_id = f"w{wave_no}i{idx}"   # batch custom_id is capped at 64 chars
                idmap[custom_id] = cid
                entry_map[cid] = entry
                requests.append({
                    "custom_id": custom_id,
                    "params": {
                        "model": self.MODELS["architect"],
                        "max_tokens": self.MAX_TOKENS["architect"],
                        "temperature": float(os.environ.get("DR_TEMPERATURE", "0.2")),
                        "system": _SYSTEM_INSTRUCTION,
                        "messages": [{
                            "role": "user",
                            "content": [
                                {"type": "text", "text": static_prefix,
                                 "cache_control": {"type": "ephemeral"}},
                                {"type": "text", "text": dynamic_tail},
                            ],
                        }],
                    },
                })

            self._progress("generate",
                           f"Wave {wave_no}: submitting batch of {len(requests)} "
                           f"({', '.join(idmap.values())})")
            try:
                batch = client.messages.batches.create(requests=requests)
                poll_batch(client, batch.id, 15)
            except Exception as e:
                self._progress("generate", f"Wave {wave_no} batch failed: {e}")
                failed_ids.update(idmap.values())
                continue

            for result in client.messages.batches.results(batch.id):
                cid = idmap.get(result.custom_id, result.custom_id)
                entry = entry_map.get(cid, cid)
                if result.result.type != "succeeded":
                    self._progress("generate", f"FAIL {cid}: {result.result.type}")
                    failed_ids.add(cid)
                    continue
                msg = result.result.message
                usage = getattr(msg, "usage", None)
                if usage:
                    total_tin += getattr(usage, "input_tokens", 0) or 0
                    total_tout += getattr(usage, "output_tokens", 0) or 0
                raw = "".join(b.text for b in msg.content if b.type == "text")
                story_dict, errors = process_response(raw)

                if story_dict is not None and errors:
                    # Deterministic repair only — schema shape, never claim/metric
                    # reconciliation (the divergence is signal, not defect).
                    repaired, props_removed = strip_extra_properties(story_dict, self._schema)
                    repaired = repair_story(repaired, self._schema)
                    retry_errors = validate_json(repaired)
                    if not retry_errors:
                        story_dict, errors = repaired, []
                        if props_removed:
                            self._progress("generate", f"Repaired {cid}: stripped {props_removed}")

                if story_dict is None or errors:
                    self._progress("generate",
                                   f"FAIL {cid}: invalid after repair "
                                   f"({(errors or ['parse error'])[0][:90]})")
                    failed_ids.add(cid)
                    continue

                kernel_id = entry.get("kernel_id") if isinstance(entry, dict) else None
                if kernel_id:
                    story_dict["_kernel_id"] = kernel_id

                json_path, _pl_path = save_story_tagged(
                    story_dict, self._json_dir, self._testsets_dir, overwrite=True
                )
                if json_path:
                    generated_stories.append(story_dict)
                    generated_by_id[story_dict["header"]["constraint_id"]] = story_dict
                    self._progress("generate", f"Saved {cid}")
                else:
                    failed_ids.add(cid)

        if failed_ids:
            self._progress("generate",
                           f"Failed ({len(failed_ids)}): {sorted(failed_ids)} — "
                           f"re-run or use regenerate pass; not silently dropped")
        self._progress("generate",
                       f"Generated {len(generated_stories)}/{len(items)} stories in {wave_no} wave(s)")
        return StepResult(
            step="generate", status="success", data=generated_stories,
            tokens_in=total_tin, tokens_out=total_tout,
            duration_s=time.time() - t0,
        )

    def _step_generate_serial(self, manifest: dict) -> StepResult:
        self._progress("generate", "Generating constraint stories (serial mode)...")
        t0 = time.time()

        sequence = manifest["generation_sequence"]
        axes_by_id = {a["claim_id"]: a for a in manifest["axes"]}
        generated_stories = []
        total_tin, total_tout = 0, 0

        for i, entry in enumerate(sequence):
            # Handle both plain string claim_ids and kernel-style dict entries
            if isinstance(entry, dict):
                claim_id = entry.get("claim_id") or entry.get("constraint_id")
            else:
                claim_id = entry

            if not claim_id:
                continue

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
                f"Hypothesis type: {axis['hypothesis']}"
            )
            if axis.get("beneficiary"):
                source_desc += f"\nBeneficiary: {axis['beneficiary']}"
            if axis.get("victim"):
                source_desc += f"\nVictim: {axis['victim']}"
            cs_recog = manifest.get("commitment_system_recognition")
            if cs_recog:
                source_desc += f"\nCommitment System Recognition: {json.dumps(cs_recog)}"

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
                    max_tokens=self.MAX_TOKENS["architect"],
                    system_instruction=_SYSTEM_INSTRUCTION,
                    temperature=float(os.environ.get("DR_TEMPERATURE", "0.2")),
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

            # Path-aware strip: remove extra properties before retrying
            if story_dict is not None and errors:
                prop_errors = [e for e in errors if "Additional properties are not allowed" in e]
                other_errors = [e for e in errors if "Additional properties are not allowed" not in e]
                if prop_errors and not other_errors:
                    stripped, props_removed = strip_extra_properties(story_dict, self._schema)
                    if not validate_json(stripped):
                        story_dict = stripped
                        errors = []
                        self._progress("generate", f"Stripped extra props for {claim_id}: {props_removed}")

            if story_dict is None or errors:
                # Retry once with error feedback
                self._progress("generate", f"Validation errors for {claim_id}, retrying...")
                feedback = ""
                if errors:
                    feedback = "\nYour previous attempt had these validation errors:\n"
                    for err in errors:
                        feedback += f"  - {_sanitize_feedback_error(err)}\n"
                    feedback += "Fix these specific errors while keeping the rest correct.\n"

                retry_prompt = build_prompt(source_desc, context_text + feedback)
                try:
                    text, tin2, tout2 = self._call(
                        retry_prompt,
                        model=self.MODELS["architect"],
                        max_tokens=self.MAX_TOKENS["architect"],
                        system_instruction=_SYSTEM_INSTRUCTION,
                        temperature=float(os.environ.get("DR_TEMPERATURE", "0.2")),
                    )
                    total_tin += tin2
                    total_tout += tout2
                    story_dict, errors = process_response(text)
                    # Apply strip on retry result too
                    if story_dict is not None and errors:
                        prop_errors = [e for e in errors if "Additional properties are not allowed" in e]
                        other_errors = [e for e in errors if "Additional properties are not allowed" not in e]
                        if prop_errors and not other_errors:
                            stripped, props_removed = strip_extra_properties(story_dict, self._schema)
                            if not validate_json(stripped):
                                story_dict = stripped
                                errors = []
                                self._progress("generate", f"Stripped extra props (retry) for {claim_id}: {props_removed}")
                except Exception as e:
                    self._progress("generate", f"Retry failed for {claim_id}: {e}")
                    continue

            if story_dict is None or errors:
                self._progress("generate", f"Failed to generate valid story for {claim_id}")
                continue

            # Inject kernel_id for kernel-mode entries so generate_pl emits cs_kernel_id/2
            kernel_id = entry.get("kernel_id") if isinstance(entry, dict) else None
            if kernel_id:
                story_dict["_kernel_id"] = kernel_id

            # Save to run-tagged or flat dirs
            json_path, pl_path = save_story_tagged(
                story_dict, self._json_dir, self._testsets_dir, overwrite=True
            )
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
        if self._run_tag:
            self._progress(
                "corpus_update",
                f"Skipped — outputs in json/{self._run_tag}/ (run-tagged). "
                f"Promote to json/ before running corpus update."
            )
            return StepResult(step="corpus_update", status="skipped")

        self._progress("corpus_update", "Running pipeline...")
        t0 = time.time()

        try:
            sys.path.insert(0, str(REPO_ROOT / "python"))
            from run_pipeline import run_pipeline

            result = run_pipeline(progress=self._progress, parallel=4)

            if result.errors:
                for e in result.errors:
                    self._progress("corpus_update", f"warning: {e}")

            self._progress("corpus_update", "Corpus update complete")
            return StepResult(
                step="corpus_update", status="success",
                data=result,
                duration_s=time.time() - t0,
            )
        except Exception as e:
            self._progress("corpus_update", f"Pipeline failed: {e}")
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
        if self._run_tag:
            self._progress(
                "reports",
                f"Skipped — run-tagged output. Promote json/{self._run_tag}/ to json/ first."
            )
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
                max_tokens=self.MAX_TOKENS["essayist"],
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

        # Also save to agent/analysis/essays/
        analysis_essays_dir = Path(__file__).resolve().parent / "analysis" / "essays"
        analysis_essays_dir.mkdir(parents=True, exist_ok=True)
        analysis_essay_path = analysis_essays_dir / f"{slug}.md"
        analysis_essay_path.write_text(text, encoding="utf-8")

        self._progress("essay", f"Essay saved to {essay_path.relative_to(REPO_ROOT)}")
        self._progress("essay", f"Essay also saved to {analysis_essay_path.relative_to(REPO_ROOT)}")
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
    parser.add_argument("--serial-generate", action="store_true",
                        help="Use the legacy one-at-a-time generation loop (keeps the "
                             "per-item LLM retry-with-feedback path) instead of the batch API")
    parser.add_argument("--axes", type=int, default=None,
                        help="Optional budget ceiling on axes (default: none — the SCOPE §3 "
                             "pairwise independence test decides how many axes proceed)")
    parser.add_argument("--run-tag", default=None,
                        help="Output namespace (e.g. run_01). If set, saves to json/<run-tag>/ "
                             "and prolog/testsets/<run-tag>/. Corpus update and reports are skipped.")
    parser.add_argument("--skip-corpus-update", action="store_true", help="Skip run_pipeline")
    parser.add_argument("--skip-search", action="store_true", help="Skip search grounding")
    parser.add_argument("--skip-essay", action="store_true", help="Skip essay synthesis")
    parser.add_argument("--dry-run", action="store_true", help="Run SCOPE only, print manifest")
    parser.add_argument("--manifest-file", default=None,
                        help="Load frozen SCOPE manifest from file, skip decompose step")
    args = parser.parse_args()

    # Resolve topic — positional arg can be a file path or a literal string
    topic = None
    if args.topic:
        candidate = Path(args.topic)
        if candidate.is_file():
            topic = candidate.read_text(encoding="utf-8").strip()
        else:
            # Also check relative to repo root (agent/ prefix, etc.)
            repo_candidate = Path(__file__).resolve().parent.parent / args.topic
            if repo_candidate.is_file():
                topic = repo_candidate.read_text(encoding="utf-8").strip()
            else:
                topic = args.topic
    elif args.input_file:
        topic = Path(args.input_file).read_text(encoding="utf-8").strip()
    elif not sys.stdin.isatty():
        topic = sys.stdin.read().strip()
    else:
        parser.error("Provide a topic as argument, via --input-file, or on stdin")

    orch = DRAuditOrchestrator(
        axes=args.axes,
        serial_generate=args.serial_generate,
        run_tag=args.run_tag,
        skip_corpus_update=args.skip_corpus_update,
        skip_search=args.skip_search,
        skip_essay=args.skip_essay,
        dry_run=args.dry_run,
        manifest_file=args.manifest_file,
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
