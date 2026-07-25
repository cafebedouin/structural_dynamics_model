"""DR Audit Pipeline — CLI-first orchestrator (Claude/Anthropic version).

Drop-in replacement for the Gemini orchestrator.  Swaps google.genai
for the Anthropic Python SDK while keeping all pipeline logic identical.

Chains: research → SCOPE → generate → corpus update → reports → tensions ledger.
No Streamlit imports.  Pure Python with optional progress callback.

Usage:
    python3 agent/c-orchestrator.py "Alberta separatism"
    python3 agent/c-orchestrator.py narrative_transform/originals/the_bridge.md
    python3 agent/c-orchestrator.py --input-file topic.txt --axes 3
    python3 agent/c-orchestrator.py --dry-run "Alberta separatism"
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
    _SYSTEM_INSTRUCTION,
)
from agent import llm_call, make_brief
# Re-exported for back-compat; the canonical definition lives in llm_call so the
# refusal/empty detection cannot fork (Build Discipline pattern 2).
from agent.llm_call import ModelCallError

# ---------------------------------------------------------------------------
# Data classes
# ---------------------------------------------------------------------------

@dataclass
class StepResult:
    step: str           # research | decompose | generate | corpus_update | reports | ledger
    status: str         # success | error | skipped
    data: Any = None    # step-specific payload
    error: str = ""
    tokens_in: int = 0
    tokens_out: int = 0
    duration_s: float = 0.0
    refused: bool = False  # True iff the step failed on a safety refusal (stop_reason=refusal)


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
    # (essayist retired 2026-06-11 with the OQ-101 ledger replacement)
    #
    # Change these to claude-opus-4-8 (or claude-haiku-4-5-20251001)
    # if you want to test at different price points. Sonnet 5+/Opus 4.7+
    # reject temperature — llm_call.sampling_overrides gates it per model.
    MODELS = {
        "researcher": "claude-haiku-4-5-20251001",
        "architect":  "claude-sonnet-5",
    }

    # Default max_tokens per role (Claude requires an explicit cap)
    MAX_TOKENS = {
        "researcher": 4096,
        # Stories now carry 12-23 measurements and run 18-24KB JSON (~6-7k tokens);
        # 8192 was the orchestrator-era cap and truncation = parse-fail in batch mode.
        "architect":  16384,
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
        source_name: str = "",
        brief_mode: str = "auto",          # auto | force | never  (size-driven briefing)
        auto_bypass_refusal: bool = False,  # opt-in, logged bypass of a content refusal
        no_commit: bool = False,            # opt-out of the gated auto-commit of new stories
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
        self.source_name = source_name
        self._source_path = Path(source_name) if (source_name and Path(source_name).is_file()) else None
        self.brief_mode = brief_mode
        self.auto_bypass_refusal = auto_bypass_refusal
        self.no_commit = no_commit
        # Optional explicit token cap on the raw topic (env override). When unset,
        # the ingest ceiling is MEASURED per-step (see _ingest_decision).
        import os as _os
        self.brief_threshold = (
            int(_os.environ["DR_BRIEF_THRESHOLD"]) if "DR_BRIEF_THRESHOLD" in _os.environ else None
        )
        self.ingest_margin = int(_os.environ.get("DR_INGEST_MARGIN", "8000"))

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
            # Ensure the flat corpus dirs exist. Post-reset (2026-06-05) prolog/testsets/
            # is empty-by-intent and therefore absent on disk (git drops empty dirs), so a
            # flat run would crash on write-out (.tmp lint write + out_pl.write_text).
            for d in (self._json_dir, self._testsets_dir):
                d.mkdir(parents=True, exist_ok=True)

        # Schema dict for path-aware strip (loaded once)
        self._schema = load_schema()

        # Load protocol files (cached via lru_cache in story_generator_base).
        # NOTE (2026-06-05, OQ-47 closure): the former "gen_prompt" and "example"
        # entries were loaded here but never consumed — generation goes through
        # story_generator_base.build_prompt, whose example is
        # agent/verification_bottleneck.json (leak-clean). The dangling "example"
        # entry pointed at json/antifragility.json, which hardcodes the OLD NL-gate
        # exemplar values (accessibility_collapse 0.9 / resistance 0.08) — OQ-47's
        # confirmed leak. Removed so the leak path cannot be silently re-wired;
        # do not re-add an example here without the assembled-payload band grep.
        self.protocols = {
            "uke_scope":  _load_context_file(str(REPO_ROOT / "prompts" / "uke_scope_v2_json.md")),
            "uke_w":      _load_context_file(str(Path(__file__).parent / "uke_summary.md")),
        }

    # ------------------------------------------------------------------
    # Claude call helper
    # ------------------------------------------------------------------

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

        Thin wrapper over the canonical `llm_call.call` (pause_turn loop +
        refusal/empty detection live there). Maps the orchestrator's
        `system_instruction` kwarg to llm_call's `system`.
        """
        return llm_call.call(
            prompt, model,
            system=system_instruction,
            temperature=temperature,
            max_tokens=8192 if max_tokens is None else max_tokens,
            tools=tools,
        )

    # ------------------------------------------------------------------
    # Pipeline
    # ------------------------------------------------------------------

    def run(self, topic: str) -> PipelineResult:
        """Execute the full DR audit pipeline for *topic*."""
        result = PipelineResult()
        t0 = time.time()

        # Size-driven briefing (lossy fallback for oversized inputs). Skipped with
        # a frozen manifest (decompose is bypassed; the raw topic is unused there).
        if not self.manifest_file:
            try:
                topic = self._maybe_brief_for_size(topic)
            except make_brief.BriefRefusal as e:
                self._progress("brief", make_brief.manual_route_message(self.source_name, e.witness))
                result.steps.append(StepResult(step="brief", status="error",
                                               error=str(e), refused=True))
                result.total_duration_s = time.time() - t0
                return result

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
                self._last_manifest_path = str(self.manifest_file)
                self._progress("decompose", f"Loaded frozen manifest from {self.manifest_file} "
                               f"({len(frozen['generation_sequence'])} axes)")
                step = StepResult(step="decompose", status="success", data=frozen)
            except Exception as e:
                self._progress("decompose", f"Failed to load manifest: {e}")
                step = StepResult(step="decompose", status="error", error=str(e))
        else:
            step = self._step_decompose(topic, research_context)

        # Refusal handling — STOP by default; opt-in bypass briefs + retries ONCE.
        # (A size-fitting topic that still refuses is a content refusal; the right
        # default is the guided manual route, not a silent classifier bypass.)
        if (not self.manifest_file) and step.refused:
            if not self.auto_bypass_refusal:
                self._progress("decompose", make_brief.manual_route_message(self.source_name))
                result.steps.append(step)
                result.total_duration_s = time.time() - t0
                self._tally_tokens(result)
                return result
            self._progress("decompose", "AUTO-BYPASS (opt-in): briefing topic and retrying once.")
            try:
                brief = make_brief.make_brief(
                    topic, source_name=self.source_name,
                    on_progress=lambda m: self._progress("brief", m), auto_bypass=True)
            except make_brief.BriefRefusal as e:
                self._progress("decompose", make_brief.manual_route_message(self.source_name, e.witness))
                result.steps.append(step)
                result.total_duration_s = time.time() - t0
                self._tally_tokens(result)
                return result
            self._save_brief(brief)
            topic = brief
            step_r = self._step_research(topic)
            result.steps.append(step_r)
            research_context = step_r.data or topic
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
        corpus_update_ok = (step.status == "success")

        # Step 5: Enhanced reports
        step = self._step_reports(generated_ids)
        result.steps.append(step)
        result.report_paths = step.data or []

        # Step 6: Tensions ledger (OQ-101: the auto-essay is REMOVED — the
        # essay FORM collapses plurality regardless of synthesizer/prompt,
        # operator ruling 2026-06-10; the ledger is deterministic extraction
        # and cannot over-state by construction. Live synthesis stays with
        # the operator + the checklist in
        # audits/2026-06-10_external_review_xprize/README.md.)
        step = self._step_ledger(generated_ids)
        result.steps.append(step)
        result.essay = ""

        # Step 7: Commit the new stories (gated + scoped). Locks the determinism
        # frontier and prevents the untracked-story drift that excluded 7 stories
        # from a worktree-run probe (OQ-131, 2026-06-15).
        step = self._step_commit(generated_ids, manifest, corpus_update_ok)
        result.steps.append(step)

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
            text, tin, tout = self._call(
                self._research_prompt(topic),
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
        except ModelCallError as e:
            self._progress("research", f"Search failed, proceeding without: {e}")
            return StepResult(
                step="research", status="error", error=str(e),
                refused=(e.stop_reason == "refusal"),
                duration_s=time.time() - t0,
            )
        except Exception as e:
            self._progress("research", f"Search failed, proceeding without: {e}")
            return StepResult(
                step="research", status="error", error=str(e),
                duration_s=time.time() - t0,
            )

    @staticmethod
    def _research_prompt(topic: str) -> str:
        return (
            f"Research the following topic thoroughly. Provide factual background, "
            f"key actors, recent developments, structural tensions, and data points.\n\n"
            f"TOPIC: {topic}"
        )

    # ------------------------------------------------------------------
    # Step 2: Decompose (UKE_SCOPE)
    # ------------------------------------------------------------------

    def _step_decompose(self, topic: str, research_context: str) -> StepResult:
        self._progress("decompose", "Running UKE_SCOPE decomposition (kernel-first / primed)...")
        t0 = time.time()

        # Kernel-first routing (2026-06-06): use the PRIMED SCOPE user-prompt — the one that asks
        # the kernel question ("is this a contested kernel? if so emit READINGS; else decompose flat
        # with a collapse omega") — instead of the old unprimed §3-independence prompt that never
        # asked and silently flattened genuine kernels (magnifica; OQ-76 / OQ-79 mech-2). Phase 0
        # (outputs/kernel_first_phase0/PHASE0_READOUT.md) showed the primed verdict is a
        # KERNEL-LIBERAL gate: it routes to kernel whenever a foundational reading is constructible
        # (= the topic is contentful, per docs/seat-theorem-v1.md) and flat only when the situation
        # settles it by itself. A kernel-positive means "admits a foundational construction"
        # (dominance UNJUDGED) — never "certified dominant kernel"; kernels accrue uncurated.
        # Reuse gkc's _scope_user_prompt so the primed prompt has ONE source and cannot drift
        # between the two front-ends (both run the same uke_scope_v2_json.md §1.3-K system prompt).
        # Downstream _step_generate already handles kernel manifests: readings + the auto forced-flat
        # control (flatten_manifests) = the construction pair.
        from agent.generate_kernel_corpus import _scope_user_prompt
        prompt = _scope_user_prompt({"human_readable": topic, "summary": ""},
                                    research_context, self.axes)

        try:
            text, tin, tout = self._call(
                prompt,
                model=self.MODELS["architect"],
                max_tokens=self.MAX_TOKENS["architect"],
                system_instruction=self.protocols["uke_scope"],
                temperature=0.2,
            )
        except ModelCallError as e:
            self._progress("decompose", f"SCOPE call failed: {e}")
            return StepResult(
                step="decompose", status="error", error=str(e),
                refused=(e.stop_reason == "refusal"),
                duration_s=time.time() - t0,
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

        self._persist_manifest(manifest, topic=topic)
        self._progress("decompose", f"SCOPE complete — {len(manifest['generation_sequence'])} axes selected")
        return StepResult(
            step="decompose", status="success", data=manifest,
            tokens_in=tin, tokens_out=tout,
            duration_s=time.time() - t0,
        )

    def _persist_manifest(self, manifest: dict, topic: str = "unspecified") -> None:
        """Persist the SCOPE manifest to disk (added 2026-06-05).

        Previously flat (un-run-tagged) runs DISCARDED the manifest — the axis
        traces (structural deltas, observables, deferred axes, fracture scans,
        per-axis hypotheses) existed only in memory, which made the
        SCOPE-hypothesis → authored-claim → computed-type three-layer trace
        unrecoverable after the run (witnessed: the 2026-06-05 first three rebuild
        runs). Run-tagged runs write to outputs/kernel_manifests/<run_tag>/;
        flat runs to outputs/kernel_manifests/flat/.
        """
        # OQ-254 join key: mint BEFORE the write attempt so generated stories carry the
        # id even if persistence fails — the q_provenance readout then reports those
        # stories run_id_authored_manifest_unreachable (loud) instead of 'none' (silent).
        fam = manifest.get("family_id") or "manifest"
        ts = time.strftime("%Y%m%d_%H%M%S")
        manifest["_generation_run_id"] = f"{fam}_{ts}"  # == manifest filename stem
        try:
            # Self-provenance (OQ-254): the manifest records the Q-choice; this block
            # records the conditions the Q-choice was generated under.
            from agent.generate_kernel_corpus import _scope_manifest_provenance
            manifest["_provenance"] = _scope_manifest_provenance(
                self.MODELS["architect"], self.axes, topic=topic)
            # Tracked location (OQ-254 Step 3, generator-forward): outputs/ is
            # gitignored, which made every Q-choice record invisible to tracked read
            # sites and gone on a fresh clone (Pattern 6). processed.txt stays in
            # outputs/kernel_manifests/ — only the Q-record moves.
            mdir = REPO_ROOT / "agent" / "decompose_manifests" / (self._run_tag or "flat")
            mdir.mkdir(parents=True, exist_ok=True)
            path = mdir / f"{fam}_{ts}.manifest.json"
            path.write_text(json.dumps(manifest, indent=2, ensure_ascii=False),
                            encoding="utf-8")
            self._last_manifest_path = str(path)
            self._progress("decompose", f"Manifest persisted: {path}")
        except Exception as e:  # persistence must never kill the run
            self._progress("decompose", f"Manifest persist FAILED (non-fatal): {e}")

    # ------------------------------------------------------------------
    # Step 3: Generate constraint stories
    # ------------------------------------------------------------------

    def _step_generate(self, manifest: dict) -> StepResult:
        """Generate constraint stories from the SCOPE manifest.

        Default: the UNIFIED backend (generate_kernel_corpus.generate_from_manifests) — the
        single manifest->corpus path. It handles flat axes (c-orch framing + dependency
        waves), kernel readings, and forced-flat controls, plus the stamp / integrity /
        contradiction post-steps. The old _step_generate_batch (flat-only, which silently
        DROPPED recognized kernel readings — OQ-79) is deleted: the fork is healed by
        deletion, not left orphaned. --serial-generate / DR_SERIAL_GENERATE=1 keeps the
        legacy per-item loop (its own inline source_desc + LLM retry-with-feedback; a known
        legacy duplication, deletable later — NOT the wave logic).
        """
        if self.serial_generate or os.environ.get("DR_SERIAL_GENERATE") == "1":
            return self._step_generate_serial(manifest)

        self._progress("generate", "Generating constraint stories (unified backend)...")
        t0 = time.time()
        from agent.generate_kernel_corpus import generate_from_manifests

        # c-orchestrator runs are per-topic, not seed-laddered; give process_batch_results a
        # writable ladder next to where this run's manifest was persisted.
        processed_log = (self._manifests_dir or (REPO_ROOT / "outputs" / "kernel_manifests" / "flat")) / "processed.txt"
        processed_log.parent.mkdir(parents=True, exist_ok=True)

        # OQ-80 (resolved 2026-06-09): usage is summed by the backend into this out-param.
        # Before this, the StepResult carried a hard 0 that read as "zero tokens" when it
        # meant "not measured" — absence presenting as a measured value.
        token_acc = {"input_tokens": 0, "output_tokens": 0}
        succeeded, failed, _reasons = generate_from_manifests(
            [manifest], self._json_dir, self._testsets_dir, processed_log,
            model=self.MODELS["architect"], max_tokens=self.MAX_TOKENS["architect"],
            system=_SYSTEM_INSTRUCTION,
            temperature=float(os.environ.get("DR_TEMPERATURE", "0.2")),
            manifest_file=self.manifest_file, progress=self._progress,
            token_acc=token_acc,
        )

        # Rebuild the story-dict list downstream expects (reports reads header.constraint_id).
        stories = []
        for cid in succeeded:
            p = self._json_dir / f"{cid}.json"
            if p.exists():
                try:
                    stories.append(json.loads(p.read_text(encoding="utf-8")))
                except (json.JSONDecodeError, OSError):
                    pass
        if failed:
            self._progress("generate", f"Failed ({len(failed)}): {sorted(failed)} — "
                                       f"recoverable via --manifest-file (gap retry)")
        self._progress("generate",
                       f"Generated {len(stories)} stories via the unified backend "
                       f"({token_acc['input_tokens']}→{token_acc['output_tokens']} tokens).")
        return StepResult(step="generate", status="success", data=stories,
                          tokens_in=token_acc["input_tokens"],
                          tokens_out=token_acc["output_tokens"],
                          duration_s=time.time() - t0)

    def _step_generate_serial(self, manifest: dict) -> StepResult:
        self._progress("generate", "Generating constraint stories (serial mode)...")
        t0 = time.time()

        sequence = manifest["generation_sequence"]
        axes_by_id = {a["claim_id"]: a for a in manifest["axes"]}
        generated_stories = []
        total_tin, total_tout = 0, 0

        # OQ-81 (operator-ruled 2026-06-10): reading-typed upstream deps are suppressed —
        # same predicate as generate_kernel_corpus._flat_seeds_from_manifest (see its
        # docstring for rationale + witness). Kept in sync so the serial escape hatch
        # does not silently re-inject reading verdicts the unified backend suppresses.
        _csr = manifest.get("commitment_system_recognition") or {}
        reading_cids = {r.get("reading_id") for r in _csr.get("readings", [])} - {None}
        for _e in sequence:
            if isinstance(_e, dict) and _e.get("kernel_id"):
                _c = _e.get("claim_id") or _e.get("constraint_id")
                if _c:
                    reading_cids.add(_c)

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

            # Build upstream context for downstream axes (§5.1).
            # Reading-typed upstreams are skipped (OQ-81 suppression, see above).
            upstream_context = ""
            for upstream_id in axis.get("downstream_of", []):
                if upstream_id in reading_cids:
                    continue
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

            # OQ-254 join key (serial path): stamp the manifest identity into provenance
            # so epsilon_provenance/5 arg 4 joins story -> SCOPE manifest. Post-validation
            # injection of a schema-optional key; absent manifest id -> 'none' (loud-null).
            if isinstance(story_dict.get("provenance"), dict):
                story_dict["provenance"]["generation_run_id"] = (
                    manifest.get("_generation_run_id") or "none")

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
    # Step 6: Tensions ledger (OQ-101 — replaced essay synthesis)
    # ------------------------------------------------------------------
    # _step_essay REMOVED 2026-06-11 (OQ-101, operator ruling 2026-06-10):
    # single-voice generative synthesis collapses plurality by FORM (the
    # auto-essay announced "converges on a single structural conclusion"
    # over reports that preserved plurality; uke_think over-stated the same
    # way — invariant under synthesizer swap, so prompt guidance cannot fix
    # it). The ledger is non-generative extraction; final synthesis is the
    # operator's, live, with the checklist at
    # audits/2026-06-10_external_review_xprize/README.md.

    def _step_ledger(self, generated_ids: list[str]) -> StepResult:
        if self.skip_essay:
            return StepResult(step="ledger", status="skipped")

        self._progress("ledger", "Extracting tensions ledger (deterministic, no LLM)...")
        t0 = time.time()
        try:
            sys.path.insert(0, str(REPO_ROOT / "python"))
            import tensions_ledger
            out_path, n = tensions_ledger.build_ledger(generated_ids or None)
        except Exception as e:
            self._progress("ledger", f"Ledger extraction failed: {e}")
            return StepResult(
                step="ledger", status="error", error=str(e),
                duration_s=time.time() - t0,
            )
        self._progress("ledger", f"Ledger ({n} blocks) saved to {out_path}")
        return StepResult(
            step="ledger", status="success", data=str(out_path),
            duration_s=time.time() - t0,
        )

    # ------------------------------------------------------------------
    # Step 7: Commit the new constraint stories (gated + scoped)
    # ------------------------------------------------------------------

    def _step_commit(self, constraint_ids: list[str], manifest: dict,
                     corpus_update_ok: bool) -> StepResult:
        """Commit the json + testset files this run created. Gated + SCOPED —
        never `git add -A`:
          * skip on --no-commit, run-tag (output not promoted), or a
            corpus_update that did not succeed (a story that broke classification
            must not be committed — run_pipeline's gates are the quality bar);
          * stage ONLY json/<cid>.json + testsets/<cid>.pl for this run's cids;
          * multi-writer safety: refuse if the index already holds unrelated
            staged changes (its witness would be an unattributable count);
          * commit locally with provenance; NEVER push (that stays manual).
        """
        t0 = time.time()
        if self.no_commit:
            return StepResult(step="commit", status="skipped", data="--no-commit")
        if self._run_tag:
            return StepResult(step="commit", status="skipped",
                              data="run-tagged output not promoted to json/ — nothing to commit")
        if not corpus_update_ok:
            return StepResult(step="commit", status="skipped",
                              data="corpus_update did not succeed — refusing to commit unvalidated stories")
        if not constraint_ids:
            return StepResult(step="commit", status="skipped", data="no stories generated")

        rels: list[str] = []
        for cid in constraint_ids:
            for p in (self._json_dir / f"{cid}.json", self._testsets_dir / f"{cid}.pl"):
                if p.exists():
                    rels.append(str(p.relative_to(REPO_ROOT)))
        if not rels:
            return StepResult(step="commit", status="skipped", data="no story files on disk")

        def _git(*a, **kw):
            return subprocess.run(["git", *a], cwd=REPO_ROOT, capture_output=True,
                                  text=True, **kw)

        # Multi-writer safety: do not fold someone else's already-staged work in.
        pre = _git("diff", "--cached", "--name-only")
        if pre.stdout.strip():
            return StepResult(step="commit", status="skipped",
                              data=f"index not clean ({len(pre.stdout.split())} staged) — not committing")

        _git("add", "--", *rels, check=True)
        staged = _git("diff", "--cached", "--name-only").stdout.split()
        if not staged:
            return StepResult(step="commit", status="skipped",
                              data="no changes to commit (re-run produced identical bytes)")

        msg = self._commit_message(constraint_ids, manifest, len(staged))
        try:
            _git("commit", "-q", "-m", msg, check=True)
        except subprocess.CalledProcessError as e:
            # Leave the index as-is for the operator to inspect; report loudly.
            return StepResult(step="commit", status="error",
                              error=f"git commit failed: {e.stderr or e}",
                              duration_s=time.time() - t0)
        sha = _git("rev-parse", "--short", "HEAD").stdout.strip()
        self._progress("commit",
                       f"committed {len(staged)} files for {len(constraint_ids)} stories ({sha}); "
                       f"push stays manual")
        return StepResult(step="commit", status="success",
                          data={"sha": sha, "files": len(staged)},
                          duration_s=time.time() - t0)

    def _commit_message(self, constraint_ids: list[str], manifest: dict,
                        n_files: int) -> str:
        fam = manifest.get("family_id", "")
        domain = manifest.get("domain", "")
        shown = ", ".join(constraint_ids[:8]) + ("..." if len(constraint_ids) > 8 else "")
        head = f"corpus: add {len(constraint_ids)} constraint stories" + (f" ({fam})" if fam else "")
        return (
            f"{head}\n\n"
            f"Generated by agent/c-orchestrator.py" + (f" — domain {domain}" if domain else "") + ".\n"
            f"Committed after a successful corpus_update (run_pipeline + its gates); "
            f"scoped to the {n_files} files this run created. The committed JSON is the "
            f"determinism frontier (re-runs are new draws, never re-measurements).\n\n"
            f"Stories: {shown}\n\n"
            f"Generated-by: c-orchestrator.py\n"
        )

    # ------------------------------------------------------------------
    # Size-driven briefing (the LOSSY fallback for inputs that won't fit)
    # ------------------------------------------------------------------

    def _ingest_decision(self, topic: str) -> tuple[bool, dict]:
        """Decide whether *topic* must be briefed for size, by MEASURING per-step
        headroom (not asserting a KB number). The raw topic is packed only by the
        research and decompose steps (generate works from the manifest), so the
        ceiling is the min headroom across those two. Returns (should_brief, info).
        """
        from agent.generate_kernel_corpus import _scope_user_prompt
        info: dict = {"steps": {}}

        # Explicit operator cap on the raw topic (env DR_BRIEF_THRESHOLD), if set.
        if self.brief_threshold is not None:
            t = llm_call.count_tokens(self.MODELS["architect"], topic)
            info["topic_tokens"] = t
            info["explicit_cap"] = self.brief_threshold
            return (t > self.brief_threshold), info

        over = False
        # decompose — always packs the topic; the large uke_scope system usually binds.
        d_model = self.MODELS["architect"]
        d_user = _scope_user_prompt({"human_readable": topic, "summary": ""}, "", self.axes)
        d_tokens = llm_call.count_tokens(d_model, d_user, system=self.protocols["uke_scope"])
        d_cap = llm_call.context_window(d_model) - self.MAX_TOKENS["architect"] - self.ingest_margin
        info["steps"]["decompose"] = {"model": d_model, "tokens": d_tokens,
                                      "cap": d_cap, "headroom": d_cap - d_tokens}
        over = over or d_tokens > d_cap

        # research — only when not skipped. web_search injects results at runtime,
        # so the static prompt count + the reserved margin is a floor, not exact.
        if not self.skip_search:
            r_model = self.MODELS["researcher"]
            r_tokens = llm_call.count_tokens(r_model, self._research_prompt(topic))
            r_cap = llm_call.context_window(r_model) - self.MAX_TOKENS["researcher"] - self.ingest_margin
            info["steps"]["research"] = {"model": r_model, "tokens": r_tokens,
                                         "cap": r_cap, "headroom": r_cap - r_tokens}
            over = over or r_tokens > r_cap

        return over, info

    def _maybe_brief_for_size(self, topic: str) -> str:
        """Return *topic* unchanged if it fits; else a structural brief. STOPs the
        run (via BriefRefusal) if briefing is needed but the content is refused."""
        if self.brief_mode == "never":
            return topic

        if self.brief_mode == "force":
            should = True
        else:
            should, info = self._ingest_decision(topic)
            for name, s in info.get("steps", {}).items():
                self._progress("ingest", f"{name}: {s['tokens']:,} tok / cap {s['cap']:,} "
                               f"(headroom {s['headroom']:,}) [{s['model']}]")
            if "explicit_cap" in info:
                self._progress("ingest", f"topic {info['topic_tokens']:,} tok vs "
                               f"explicit DR_BRIEF_THRESHOLD {info['explicit_cap']:,}")
            if not should:
                return topic

        self._progress("brief", "LOSSY SUBSTITUTION: topic exceeds the measured ingest ceiling; "
                       "compressing to a structural brief. Results are BRIEF-DERIVED, not "
                       "whole-doc (Phase-0: whole reads richer).")
        brief = make_brief.make_brief(
            topic, source_name=self.source_name,
            on_progress=lambda m: self._progress("brief", m),
            auto_bypass=self.auto_bypass_refusal,
        )
        self._save_brief(brief)
        return brief

    def _save_brief(self, brief: str) -> None:
        stem = (Path(self.source_name).stem if self.source_name else "topic") or "topic"
        if self._source_path is not None:
            dest = self._source_path.with_name(self._source_path.stem + "_brief.md")
        else:
            dest = REPO_ROOT / "outputs" / "briefs" / f"{stem}_brief.md"
            dest.parent.mkdir(parents=True, exist_ok=True)
        dest.write_text(brief, encoding="utf-8")
        self._progress("brief", f"brief ({len(brief):,} chars) saved to {dest} — review it.")

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
    parser.add_argument("--skip-essay", action="store_true", help="Skip the tensions ledger step (flag kept for compatibility; OQ-101 replaced the essay)")
    parser.add_argument("--dry-run", action="store_true", help="Run SCOPE only, print manifest")
    parser.add_argument("--manifest-file", default=None,
                        help="Load frozen SCOPE manifest from file, skip decompose step")
    parser.add_argument("--no-commit", action="store_true",
                        help="Skip the gated auto-commit of new stories (default: commit "
                             "json/<cid>.json + testsets/<cid>.pl after a successful corpus update)")
    parser.add_argument("--auto-bypass-refusal", action="store_true",
                        help="On a content safety-refusal, attempt an opt-in, fully-logged "
                             "bypass (brief via permissive model + analytical-intent reframe) "
                             "instead of stopping with the manual-route guidance.")
    bg = parser.add_mutually_exclusive_group()
    bg.add_argument("--brief", dest="brief_mode", action="store_const", const="force",
                    help="Force size-briefing even if the topic would fit.")
    bg.add_argument("--no-brief", dest="brief_mode", action="store_const", const="never",
                    help="Never size-brief (feed the topic whole; may overflow context).")
    parser.set_defaults(brief_mode="auto")
    args = parser.parse_args()

    # Resolve topic — positional arg can be a file path or a literal string.
    # source_name carries the originating file path (for brief naming/save-beside).
    topic = None
    source_name = ""
    if args.topic:
        candidate = Path(args.topic)
        if candidate.is_file():
            topic = candidate.read_text(encoding="utf-8").strip()
            source_name = str(candidate)
        else:
            # Also check relative to repo root (agent/ prefix, etc.)
            repo_candidate = Path(__file__).resolve().parent.parent / args.topic
            if repo_candidate.is_file():
                topic = repo_candidate.read_text(encoding="utf-8").strip()
                source_name = str(repo_candidate)
            else:
                topic = args.topic
    elif args.input_file:
        topic = Path(args.input_file).read_text(encoding="utf-8").strip()
        source_name = args.input_file
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
        source_name=source_name,
        brief_mode=args.brief_mode,
        auto_bypass_refusal=args.auto_bypass_refusal,
        no_commit=args.no_commit,
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

    # OQ-101: the essay step is gone; surface the ledger path instead.
    for s_ in result.steps:
        if s_.step == "ledger" and s_.status == "success":
            print("\n" + "=" * 60)
            print("TENSIONS LEDGER (deterministic — operator synthesizes live)")
            print("=" * 60)
            print(f"  {s_.data}")


if __name__ == "__main__":
    main()
