"""UKE Pipeline — Gemini + Claude orchestrator with air-gap enforcement.

Two modes:
  - narrative: Stage 0 (Gemini) → Stages 1-5 (Claude)
    Story translation preserving constraint topology.
    NEW: Constraint story generation + Prolog engine between Stages 1 and 2.
  - artifact: Stage 0 (Gemini) → Stages 1-6 (Claude)
    Software artifact generation from constraint topology.

Shared infrastructure: providers, persistence, file loading, title extraction.
Mode-specific: stage count, instruction files, data flow, output directory.

Usage:
    # Narrative mode (default)
    python3 uke_narrative_orchestrator.py originals/eighty_yard_run.md
    python3 uke_narrative_orchestrator.py --resume outputs/run/ --from-stage stage_3

    # Artifact mode
    python3 uke_narrative_orchestrator.py --mode artifact narrative_transform/originals/eighty_yard_run.md
    python3 uke_narrative_orchestrator.py --mode artifact --dry-run story.txt

    # Skip constraint engine (fall back to original pipeline)
    python3 uke_narrative_orchestrator.py --skip-engine originals/story.md
"""

import argparse
import json
import logging
import os
import re
import subprocess
import sys
import time
from dataclasses import dataclass, field
from functools import lru_cache
from pathlib import Path
from typing import Any, Callable, Protocol

logging.basicConfig(level=logging.INFO, format="%(asctime)s [%(name)s] %(message)s")
_log = logging.getLogger("uke_pipeline")


# ---------------------------------------------------------------------------
# Path constants
# ---------------------------------------------------------------------------

NARRATIVE_TRANSFORM_DIR = Path(__file__).resolve().parent / "narrative_transform"
ORIGINALS_DIR = NARRATIVE_TRANSFORM_DIR / "originals"
STORIES_DIR = NARRATIVE_TRANSFORM_DIR / "stories"
ARTIFACTS_DIR = NARRATIVE_TRANSFORM_DIR / "artifacts"
LOGIC_NARRATIVE_PATH = NARRATIVE_TRANSFORM_DIR / "logic_narrative_v4.1.md"


# ---------------------------------------------------------------------------
# DR engine integration (optional — degrades gracefully if unavailable)
# ---------------------------------------------------------------------------

_DR_ENGINE_AVAILABLE = False
_REPO_ROOT = None

def _init_dr_engine():
    """Attempt to import the DR constraint story infrastructure.

    Returns True if the Prolog engine pipeline is available.
    """
    global _DR_ENGINE_AVAILABLE, _REPO_ROOT

    # The agent/ directory sits one level below the DR repo root
    candidate_root = Path(__file__).resolve().parent.parent
    pipeline_script = candidate_root / "python" / "run_pipeline.py"
    report_script = candidate_root / "python" / "enhanced_report.py"

    if pipeline_script.exists() and report_script.exists():
        if str(candidate_root) not in sys.path:
            sys.path.insert(0, str(candidate_root))
        try:
            from agent.story_generator_base import (
                process_response,
                save_story,
                strip_json_fences,
                build_prompt,
                _SYSTEM_INSTRUCTION,
                REPO_ROOT,
            )
            _REPO_ROOT = REPO_ROOT
            _DR_ENGINE_AVAILABLE = True
            _log.info("DR engine available at %s", candidate_root)
            return True
        except ImportError as e:
            _log.warning("DR engine import failed: %s", e)
    else:
        _log.info("DR engine not found (no run_pipeline.py at %s)", candidate_root)

    return False

# Lazy init on first use
_dr_engine_initialized = False

def _ensure_dr_engine():
    global _dr_engine_initialized
    if not _dr_engine_initialized:
        _init_dr_engine()
        _dr_engine_initialized = True
    return _DR_ENGINE_AVAILABLE


# ---------------------------------------------------------------------------
# Pipeline mode configuration
# ---------------------------------------------------------------------------

PIPELINE_MODES = {
    "narrative": {
        "stages": ["stage_0", "stage_1", "stage_2", "stage_3", "stage_4", "stage_5"],
        "file_prefix": "stage",              # stage0.md .. stage5.md
        "output_dir": STORIES_DIR,
        "validation_gates": set(),
        "air_gap_stage": "stage_4",          # narrative air gap at stage 4
    },
    "artifact": {
        "stages": ["stage_0", "stage_1", "stage_2", "stage_3", "stage_4", "stage_5", "stage_6"],
        "file_prefix": "artifact_stage",     # artifact_stage0.md .. artifact_stage6.md
        "output_dir": ARTIFACTS_DIR,
        "validation_gates": {"stage_2", "stage_6"},
        "air_gap_stage": "stage_5",          # artifact air gap at stage 5
    },
}

# Stage data flow: which prior outputs feed each stage.
# "source" = original text, "dr_logic" = logic reference, "stage_N" = output of stage N.
# "constraint_reports" = Prolog engine reports (NEW — air-gap safe, no source material)
STAGE_INPUTS = {
    "narrative": {
        "stage_0": ["source", "dr_logic"],
        "stage_1": ["stage_0", "dr_logic"],
        "stage_2": ["stage_1", "dr_logic"],
        "stage_3": ["stage_1", "stage_2", "dr_logic"],
        "stage_4": ["stage_1", "stage_2", "stage_3", "constraint_reports"],  # AIR GAP: no source, no stage_0
        "stage_5": ["stage_4", "stage_1"],
    },
    "artifact": {
        "stage_0": ["source", "dr_logic"],
        "stage_1": ["stage_0", "dr_logic"],
        "stage_2": ["stage_0", "stage_1"],                   # validation gate
        "stage_3": ["stage_1", "dr_logic"],                  # path/modality selection
        "stage_4": ["stage_1", "stage_3"],                   # interaction design
        "stage_5": ["stage_1", "stage_3", "stage_4"],        # AIR GAP: no source, no stage_0
        "stage_6": ["stage_1", "stage_5"],                   # validation gate
    },
}

# All possible stages across both modes (for CLI --from-stage choices)
ALL_POSSIBLE_STAGES = ["stage_0", "stage_1", "stage_2", "stage_3",
                       "stage_4", "stage_5", "stage_6"]


# ---------------------------------------------------------------------------
# File loading
# ---------------------------------------------------------------------------

@lru_cache(maxsize=None)
def _load_context_file(path: str) -> str:
    """Read and cache a context file."""
    return Path(path).read_text(encoding="utf-8")


# ---------------------------------------------------------------------------
# Title extraction helpers
# ---------------------------------------------------------------------------

def _extract_title(text: str) -> str:
    """Extract a title from the first markdown heading or first non-empty line."""
    for line in text.strip().splitlines():
        line = line.strip()
        if not line:
            continue
        m = re.match(r'^#{1,2}\s+(.+)$', line)
        if m:
            return m.group(1).strip()
        return line.strip()
    return "untitled"


def _title_to_filename(title: str) -> str:
    """Convert a title string to a safe filename slug."""
    slug = re.sub(r'[^a-z0-9]+', '_', title.lower()).strip('_')
    return slug[:80] if slug else "untitled"


# ---------------------------------------------------------------------------
# Data classes
# ---------------------------------------------------------------------------

@dataclass
class StepResult:
    """Result of a single pipeline stage."""
    step: str           # stage_0 .. stage_6, scope, constraint_gen, prolog_engine
    status: str         # success | error | skipped | gate_halt
    data: Any = None
    error: str = ""
    tokens_in: int = 0
    tokens_out: int = 0
    duration_s: float = 0.0
    model_used: str = ""
    provider: str = ""


@dataclass
class PipelineResult:
    """Aggregated result across all stages."""
    run_id: str = ""
    mode: str = "narrative"
    source_story: str = ""
    stage_outputs: dict[str, str] = field(default_factory=dict)
    steps: list[StepResult] = field(default_factory=list)
    total_tokens_in: int = 0
    total_tokens_out: int = 0
    total_duration_s: float = 0.0
    output_dir: Path | None = None
    story_path: Path | None = None
    original_title: str = ""
    # NEW: constraint engine artifacts
    scope_manifest: dict | None = None
    constraint_stories: list[dict] = field(default_factory=list)
    constraint_report_paths: list[Path] = field(default_factory=list)


# ---------------------------------------------------------------------------
# Provider abstraction
# ---------------------------------------------------------------------------

class LLMProvider(Protocol):
    """Interface for LLM providers."""
    def call(
        self,
        prompt: str,
        model: str,
        system_instruction: str = "",
        temperature: float = 0.2,
        max_tokens: int = 8192,
    ) -> tuple[str, int, int]:
        """Return (text, tokens_in, tokens_out)."""
        ...

    @property
    def name(self) -> str: ...


class AnthropicProvider:
    """Claude via Anthropic API."""

    name = "anthropic"

    def __init__(self):
        self._client = None

    def _get_client(self):
        if self._client is None:
            import anthropic
            self._client = anthropic.Anthropic()
        return self._client

    def call(
        self,
        prompt: str,
        model: str,
        system_instruction: str = "",
        temperature: float = 0.2,
        max_tokens: int = 8192,
    ) -> tuple[str, int, int]:
        import anthropic

        client = self._get_client()
        kwargs: dict[str, Any] = {
            "model": model,
            "max_tokens": max_tokens,
            "temperature": temperature,
            "messages": [{"role": "user", "content": prompt}],
        }
        if system_instruction:
            kwargs["system"] = system_instruction

        response = self._call_with_retry(client, **kwargs)
        total_in = response.usage.input_tokens
        total_out = response.usage.output_tokens

        # Handle pause_turn continuation (web search etc.)
        max_cont = 5
        while getattr(response, "stop_reason", None) == "pause_turn" and max_cont > 0:
            max_cont -= 1
            kwargs["messages"] = [
                {"role": "user", "content": prompt},
                {"role": "assistant", "content": response.content},
            ]
            response = self._call_with_retry(client, **kwargs)
            total_in += response.usage.input_tokens
            total_out += response.usage.output_tokens

        text = self._extract_text(response)
        return text, total_in, total_out

    @staticmethod
    def _extract_text(response) -> str:
        parts = []
        for block in response.content:
            if hasattr(block, "text"):
                parts.append(block.text)
        return "\n".join(parts)

    @staticmethod
    def _call_with_retry(client, max_retries: int = 3, **kwargs):
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
                wait = 2 ** attempt * 2
                _log.warning("Retry %d after %ss: %s", attempt + 1, wait, e)
                time.sleep(wait)
            except anthropic.APIError:
                raise


class GoogleProvider:
    """Gemini via Google GenAI API (google-genai SDK)."""

    name = "google"

    def __init__(self):
        self._client = None

    def _get_client(self):
        if self._client is None:
            from google import genai
            api_key = os.environ.get("GOOGLE_API_KEY") or os.environ.get("GEMINI_API_KEY")
            if api_key:
                self._client = genai.Client(api_key=api_key)
            else:
                self._client = genai.Client()
        return self._client

    def call(
        self,
        prompt: str,
        model: str,
        system_instruction: str = "",
        temperature: float = 0.2,
        max_tokens: int = 8192,
    ) -> tuple[str, int, int]:
        from google.genai import types, errors

        client = self._get_client()
        config = types.GenerateContentConfig(
            system_instruction=system_instruction or None,
            temperature=temperature,
            max_output_tokens=max_tokens,
        )

        max_retries = 3
        for attempt in range(max_retries):
            try:
                response = client.models.generate_content(
                    model=model,
                    contents=prompt,
                    config=config,
                )
                text = response.text or ""
                tin = getattr(response.usage_metadata, "prompt_token_count", 0)
                tout = getattr(response.usage_metadata, "candidates_token_count", 0)
                return text, tin, tout
            except errors.ServerError as e:
                if attempt == max_retries - 1:
                    raise
                wait = 2 ** attempt * 2
                _log.warning("Gemini retry %d after %ss (server error): %s", attempt + 1, wait, e)
                time.sleep(wait)
            except errors.ClientError as e:
                if getattr(e, "code", 0) == 429:
                    if attempt == max_retries - 1:
                        raise
                    wait = 2 ** attempt * 5
                    _log.warning("Gemini retry %d after %ss (rate limit): %s", attempt + 1, wait, e)
                    time.sleep(wait)
                else:
                    raise


# ---------------------------------------------------------------------------
# Provider registry
# ---------------------------------------------------------------------------

def _build_providers() -> dict[str, Any]:
    """Build available providers based on environment."""
    providers: dict[str, Any] = {}

    if os.environ.get("ANTHROPIC_API_KEY"):
        providers["anthropic"] = AnthropicProvider()
    else:
        _log.warning("ANTHROPIC_API_KEY not set — Claude stages will fail")

    if os.environ.get("GOOGLE_API_KEY") or os.environ.get("GEMINI_API_KEY"):
        providers["google"] = GoogleProvider()
    else:
        _log.warning("GOOGLE_API_KEY/GEMINI_API_KEY not set — stage 0 will fail")

    return providers


# ---------------------------------------------------------------------------
# Orchestrator
# ---------------------------------------------------------------------------

class UKEOrchestrator:
    """Two-provider pipeline for UKE constraint translation.

    Supports two modes:
      - narrative: 6-stage story translation (stage0.md .. stage5.md)
        NEW: Optional constraint engine integration between stages 1 and 2.
      - artifact: 7-stage software generation (artifact_stage0.md .. artifact_stage6.md)

    Stage 0: Google Gemini (constraint extraction)
    Remaining stages: Anthropic Claude

    Constraint Engine (narrative mode, optional):
      After Stage 1, the pipeline can:
      1. Run UKE_SCOPE on the Stage 1 formalization (not the source story)
      2. Generate constraint story JSONs from the SCOPE manifest
      3. Run the Prolog engine to produce diagnostic reports
      4. Feed those reports to Stage 4 as additional structural context

      This is air-gap safe: the constraint stories and reports derive from
      the abstract structural topology, not from source-identifying material.
    """

    DEFAULT_MODELS = {
        "stage_0": ("google",    "gemini-2.5-pro"),
        "stage_1": ("anthropic", "claude-sonnet-4-5-20250929"),
        "stage_2": ("anthropic", "claude-sonnet-4-5-20250929"),
        "stage_3": ("anthropic", "claude-sonnet-4-5-20250929"),
        "stage_4": ("anthropic", "claude-sonnet-4-5-20250929"),
        "stage_5": ("anthropic", "claude-sonnet-4-5-20250929"),
        "stage_6": ("anthropic", "claude-sonnet-4-5-20250929"),
    }

    TEMPERATURES = {
        "stage_0": 0.1,
        "stage_1": 0.1,
        "stage_2": 0.3,
        "stage_3": 0.3,
        "stage_4": 0.8,
        "stage_5": 0.3,
        "stage_6": 0.2,
    }

    TEMPERATURE_OVERRIDES = {
        "narrative": {
            "stage_2": 0.7,
            "stage_4": 0.8,
            "stage_5": 0.3,
        },
        "artifact": {
            "stage_2": 0.1,
            "stage_3": 0.5,
            "stage_4": 0.5,
            "stage_5": 0.7,
            "stage_6": 0.2,
        },
    }

    MAX_TOKENS = {
        "stage_0": 8192,
        "stage_1": 8192,
        "stage_2": 8192,
        "stage_3": 4096,
        "stage_4": 16384,
        "stage_5": 16384,
        "stage_6": 8192,
    }

    def __init__(
        self,
        mode: str = "narrative",
        models: dict[str, tuple[str, str]] | None = None,
        dr_logic_path: str | Path | None = None,
        output_dir: str | Path | None = None,
        skip_final_audit: bool = False,
        skip_engine: bool = False,          # NEW: skip constraint engine
        dry_run: bool = False,
        force_gate: bool = False,
        progress_callback: Callable[[str, str], None] | None = None,
    ):
        self.mode = mode
        mode_config = PIPELINE_MODES[mode]
        self.all_stages = mode_config["stages"]
        self.validation_gates = mode_config["validation_gates"]
        self.air_gap_stage = mode_config["air_gap_stage"]
        self.final_output_dir = mode_config["output_dir"]

        self.models = {**self.DEFAULT_MODELS, **(models or {})}
        self.skip_final_audit = skip_final_audit
        self.skip_engine = skip_engine
        self.dry_run = dry_run
        self.force_gate = force_gate
        self._progress = progress_callback or (lambda step, msg: print(f"[{step}] {msg}"))

        # Build mode-specific temperatures
        self.temperatures = {**self.TEMPERATURES}
        self.temperatures.update(self.TEMPERATURE_OVERRIDES.get(mode, {}))

        # Load stage instructions from .md files
        file_prefix = mode_config["file_prefix"]
        self.stage_prompts: dict[str, str] = {}
        for i, stage in enumerate(self.all_stages):
            stage_file = NARRATIVE_TRANSFORM_DIR / f"{file_prefix}{i}.md"
            if stage_file.exists():
                self.stage_prompts[stage] = _load_context_file(str(stage_file))
            else:
                _log.warning("Stage instruction file not found: %s", stage_file)
                self.stage_prompts[stage] = ""

        # Load DR logic reference (default: logic_narrative_v4.1.md)
        if dr_logic_path is None:
            dr_logic_path = LOGIC_NARRATIVE_PATH
        self.dr_logic = ""
        if dr_logic_path and Path(dr_logic_path).exists():
            self.dr_logic = _load_context_file(str(dr_logic_path))

        # Output directory for intermediate results
        self.output_dir = Path(output_dir) if output_dir else None

        # Build provider registry
        self.providers = _build_providers()

        # ── NEW: Load constraint engine protocols ────────────────────
        self.engine_protocols: dict[str, str] = {}
        if not self.skip_engine and self.mode == "narrative":
            self._load_engine_protocols()

    def _load_engine_protocols(self):
        """Load UKE_SCOPE, generation prompt, and schema for constraint engine."""
        if not _ensure_dr_engine() or _REPO_ROOT is None:
            self._progress("engine", "DR engine not available — constraint reports disabled")
            self.skip_engine = True
            return

        # gen_prompt, schema, and example are loaded for availability checks
        # (if any is missing, engine is disabled). The actual prompt assembly
        # delegates to story_generator_base.build_prompt() which loads its own copies.
        # These loaded protocols are also the injection point if narrative-derived
        # constraint stories ever need different prompts than analytical ones.
        protocol_files = {
            "uke_scope":  _REPO_ROOT / "prompts" / "uke_scope_v2_json.md",
            "gen_prompt": _REPO_ROOT / "prompts" / "constraint_story_generation_prompt_json.md",
            "schema":     _REPO_ROOT / "python" / "constraint_story_schema.json",
            "example":    _REPO_ROOT / "json" / "antifragility.json",
        }

        for key, path in protocol_files.items():
            if path.exists():
                self.engine_protocols[key] = _load_context_file(str(path))
            else:
                self._progress("engine", f"Missing protocol file: {path.name} — constraint reports disabled")
                self.skip_engine = True
                return

        self._progress("engine", "Constraint engine protocols loaded")

    # ------------------------------------------------------------------
    # Core call dispatcher
    # ------------------------------------------------------------------

    def _call(
        self,
        stage: str,
        prompt: str,
        system_override: str | None = None,
    ) -> tuple[str, int, int, str, str]:
        """Dispatch a call to the provider/model configured for this stage.

        Returns (text, tokens_in, tokens_out, model_used, provider_name).
        """
        provider_name, model = self.models[stage]
        provider = self.providers.get(provider_name)
        if provider is None:
            raise RuntimeError(f"No provider registered for '{provider_name}'")

        system = system_override or self.stage_prompts.get(stage, "")
        temp = self.temperatures.get(stage, 0.3)
        max_tok = self.MAX_TOKENS.get(stage, 8192)

        text, tin, tout = provider.call(
            prompt=prompt,
            model=model,
            system_instruction=system,
            temperature=temp,
            max_tokens=max_tok,
        )
        return text, tin, tout, model, provider_name

    def _call_engine(
        self,
        prompt: str,
        system_instruction: str = "",
        temperature: float = 0.2,
        max_tokens: int = 8192,
    ) -> tuple[str, int, int]:
        """Call Claude for constraint engine steps (SCOPE, generation).

        Uses the stage_1 model config (architect role).
        """
        provider_name, model = self.models["stage_1"]
        provider = self.providers.get(provider_name)
        if provider is None:
            raise RuntimeError(f"No provider registered for '{provider_name}'")

        text, tin, tout = provider.call(
            prompt=prompt,
            model=model,
            system_instruction=system_instruction,
            temperature=temperature,
            max_tokens=max_tokens,
        )
        return text, tin, tout

    # ------------------------------------------------------------------
    # Persistence helpers
    # ------------------------------------------------------------------

    def _save_stage_output(self, stage: str, text: str, result: PipelineResult):
        """Write intermediate output to disk for resume capability."""
        if self.output_dir is None:
            return
        self.output_dir.mkdir(parents=True, exist_ok=True)
        out_path = self.output_dir / f"{stage}_output.md"
        out_path.write_text(text, encoding="utf-8")
        result.stage_outputs[stage] = text

    def _load_stage_output(self, stage: str) -> str | None:
        """Load a previously saved stage output for resume."""
        if self.output_dir is None:
            return None
        path = self.output_dir / f"{stage}_output.md"
        if path.exists():
            return path.read_text(encoding="utf-8")
        return None

    # ------------------------------------------------------------------
    # Output save logic
    # ------------------------------------------------------------------

    @staticmethod
    def _extract_original_title(source_path: Path | None) -> str:
        """Get the original story's title from its content or filename."""
        if source_path is None:
            return "Unknown"
        try:
            text = source_path.read_text(encoding="utf-8")
            title = _extract_title(text)
            if title and title != "untitled":
                return title
        except Exception:
            pass
        return source_path.stem.replace('_', ' ').title()

    @staticmethod
    def _extract_code_block(text: str) -> str | None:
        """Extract the dominant code block from text."""
        stripped = text.strip()
        code_starts = (
            "import ", "export ", "'use client'", '"use client"',
            "const ", "function ", "class ", "type ", "interface ",
            "// ", "/* ", "<!DOCTYPE", "<html",
        )
        if any(stripped.startswith(prefix) for prefix in code_starts):
            return stripped

        fences = list(re.finditer(r'```\w*\n(.*?)```', stripped, re.DOTALL))
        if len(fences) == 1:
            return fences[0].group(1).strip()
        if len(fences) > 1:
            blocks = [(f.group(1).strip(), len(f.group(1))) for f in fences]
            blocks.sort(key=lambda x: x[1], reverse=True)
            if blocks[0][1] > blocks[1][1] * 3:
                return blocks[0][0]
        return None

    @staticmethod
    def _is_code_output(text: str) -> bool:
        """Check if the output is code (for artifact mode)."""
        stripped = text.strip()
        code_indicators = (
            "import ", "export ", "'use client'", '"use client"',
            "const ", "function ", "class ", "type ",
        )
        return any(stripped.startswith(ind) for ind in code_indicators)

    def _save_final_output(
        self,
        content: str,
        original_title: str,
        output_dir: Path,
        is_code: bool = False,
    ) -> Path:
        """Save the final output (story or artifact) to the output directory."""
        if is_code:
            code = self._extract_code_block(content)
            if code:
                content = code
            ext = ".tsx"
            base = _title_to_filename(original_title) if original_title != "Unknown" else "artifact"
            trailer = f"\n// Original: {original_title}\n"
        else:
            ext = ".md"
            title = _extract_title(content)
            base = _title_to_filename(title)
            trailer = f"\n\n---\n*Original: {original_title}*\n"

        output_dir.mkdir(parents=True, exist_ok=True)
        out_path = output_dir / f"{base}{ext}"

        if out_path.exists():
            counter = 2
            while out_path.exists():
                out_path = output_dir / f"{base}_{counter}{ext}"
                counter += 1

        out_path.write_text(content + trailer, encoding="utf-8")
        return out_path

    # ------------------------------------------------------------------
    # Validation gate logic
    # ------------------------------------------------------------------

    @staticmethod
    def _check_validation_gate(step: StepResult) -> str:
        """Check if a validation stage output indicates PASS or FAIL."""
        if not step.data:
            return "halt"
        text = step.data
        fail_patterns = [r'\bFAIL\b', r'\bHALT\b', r'VALIDATION:\s*FAIL', r'GATE:\s*FAIL']
        pass_patterns = [r'\bPASS\b', r'VALIDATION:\s*PASS', r'GATE:\s*PASS']
        has_fail = any(re.search(p, text, re.IGNORECASE) for p in fail_patterns)
        has_pass = any(re.search(p, text, re.IGNORECASE) for p in pass_patterns)
        if has_fail:
            return "halt"
        if has_pass:
            return "pass"
        _log.warning("Validation gate output ambiguous for %s, halting", step.step)
        return "halt"

    # ------------------------------------------------------------------
    # Artifact-specific prompt suffixes (unchanged from original)
    # ------------------------------------------------------------------

    ARTIFACT_PROMPT_SUFFIXES = {
        "stage_3": (
            "Follow the path selection and system naturalization protocol in your "
            "system instructions. You must output ALL of the following:\n"
            "1. Selected path (A/B/C/D/E) with justification from the decision matrix\n"
            "2. Selected modality with compatibility rating\n"
            "3. Air gap level confirmed\n"
            "4. Complete system architecture naturalization for the selected path\n"
            "5. Constraint-driven aesthetic specification (type-to-interaction and "
            "index-to-feel mappings for EVERY constraint)\n"
            "6. System personality specification (voice, diagnostic vocabulary, "
            "behavioral constants, self-description) — every trait must trace to "
            "a constraint or coupling\n\n"
            "The aesthetic and personality specs must be concrete enough to implement "
            "directly. No placeholders."
        ),
        "stage_4": (
            "Follow the interaction design and indexical revelation protocol in your "
            "system instructions. You must output ALL of the following:\n"
            "1. Canonical state object (complete JavaScript schema with all constraints, "
            "transformation rules, couplings, and system state)\n"
            "2. Index view derivation functions (read functions, not separate data stores)\n"
            "3. Interaction mapping for EVERY constraint from EVERY index position "
            "(metrics displayed, interface feel, available actions)\n"
            "4. At least one hysteresis point that changes what the user CAN DO or "
            "BELIEVES, not just what they see\n"
            "5. Misrecognition tolerance spec with anti-help constraints\n"
            "6. Shock events mapped from transformation rules\n"
            "7. State transition planning for each transformation rule\n"
            "8. Terminal state design\n\n"
            "The canonical state object must be complete and implementable. "
            "Stage 5 will build directly from it."
        ),
        "stage_5": (
            "Follow the artifact generation protocol in your system instructions. "
            "Output ONLY the complete, runnable .tsx file. No markdown wrapping, "
            "no commentary, no explanation, no validation sections. "
            "The entire response should be valid TypeScript/JSX that can be "
            "saved directly as a .tsx file and executed.\n\n"
            "CRITICAL ARCHITECTURE: Use immutable state with pure derivation functions. "
            "Do NOT use a mutable class with notifyListeners(). Instead:\n"
            "- State is a plain object, updated via a pure reducer function\n"
            "- Index views are pure functions of state (deriveIndexView(state, index))\n"
            "- React state management via useState/useReducer, not external subscriptions\n"
            "- No tick() or dispatch() methods on a class — use React's own dispatch\n\n"
            "Every constraint metric must be causally integrated: changing a value "
            "must propagate through couplings and affect system behavior. "
            "If calculateChi exists, verify it receives numeric parameters and "
            "returns a number used in comparisons."
        ),
        "stage_6": (
            "Follow the validation protocol in your system instructions.\n\n"
            "CRITICAL: Do not simply declare PASS. For each test, show your work:\n"
            "1. CONSTRAINT PRESERVATION: For each constraint, quote the code that "
            "implements it. Trace one value change through its coupling chain.\n"
            "2. INDEX DERIVATION: Find every calculateChi or index-derivation call. "
            "List the parameter types. Flag any comparison between an object and "
            "a string/number literal.\n"
            "3. CAUSAL INTEGRATION: Pick 3 constraints. For each, describe what "
            "happens in the code when its value changes. If nothing propagates, "
            "report FAIL.\n"
            "4. HYSTERESIS: Trace the hysteresis implementation. Does it change "
            "available actions or metric readings, or only visual overlays? "
            "Visual-only = FAIL.\n"
            "5. UCZ VARIANCE: Are random/stochastic calls present? Could they "
            "produce different outcomes on repeated runs?\n"
            "6. TYPE SAFETY: Check for patterns like (object === string), "
            "(array.includes(object)), or numeric operations on undefined.\n\n"
            "Output a structured report. End with PASS or FAIL and specific issues."
        ),
    }

    def _get_prompt_suffix(self, stage: str) -> str:
        if self.mode == "artifact" and stage in self.ARTIFACT_PROMPT_SUFFIXES:
            return self.ARTIFACT_PROMPT_SUFFIXES[stage]
        return "Follow the protocol in your system instructions for this stage."

    # ------------------------------------------------------------------
    # Generic stage runner (used by artifact mode)
    # ------------------------------------------------------------------

    def _run_stage_generic(
        self,
        stage: str,
        stage_outputs: dict[str, str],
        source_story: str,
    ) -> StepResult:
        """Data-driven stage runner. Assembles prompt from STAGE_INPUTS config."""
        stage_num = stage.split("_")[1]
        is_air_gap = (stage == self.air_gap_stage)

        if is_air_gap:
            self._progress(stage, f"Running stage {stage_num} (Claude, air gap active)...")
        else:
            self._progress(stage, f"Running stage {stage_num} (Claude)...")

        t0 = time.time()
        input_keys = STAGE_INPUTS[self.mode][stage]

        if is_air_gap:
            assert "source" not in input_keys, f"Air gap violation: source in {stage}"
            assert "stage_0" not in input_keys, f"Air gap violation: stage_0 in {stage}"

        prompt_parts = []
        for key in input_keys:
            if key == "source":
                prompt_parts.append(f"=== SOURCE MATERIAL ===\n{source_story}\n\n")
            elif key == "dr_logic":
                if self.dr_logic:
                    prompt_parts.append(
                        f"=== INDEXED CONSTRAINT LOGIC REFERENCE ===\n{self.dr_logic}\n\n"
                    )
            elif key == "constraint_reports":
                content = stage_outputs.get(key, "")
                if content:
                    prompt_parts.append(
                        f"=== CONSTRAINT ENGINE REPORTS ===\n{content}\n\n"
                    )
            else:
                content = stage_outputs.get(key, "")
                snum = key.split("_")[1]
                prompt_parts.append(f"=== STAGE {snum} OUTPUT ===\n{content}\n\n")

        prompt_parts.append(self._get_prompt_suffix(stage))
        prompt = "".join(prompt_parts)

        try:
            text, tin, tout, model, provider = self._call(stage, prompt)
            self._progress(stage, f"Stage {stage_num} complete ({tin}→{tout} tokens)")
            return StepResult(
                step=stage, status="success", data=text,
                tokens_in=tin, tokens_out=tout,
                duration_s=time.time() - t0,
                model_used=model, provider=provider,
            )
        except Exception as e:
            self._progress(stage, f"Failed: {e}")
            return StepResult(
                step=stage, status="error", error=str(e),
                duration_s=time.time() - t0,
            )

    # ==================================================================
    # NEW: Constraint Engine Steps
    # ==================================================================

    def _step_scope(self, stage_1_output: str) -> StepResult:
        """Run UKE_SCOPE on Stage 1 formalization to decompose constraint axes.

        ╔═════════════════════════════════════════════════════════╗
        ║  AIR GAP: Operates on Stage 1's abstract structural    ║
        ║  topology, NOT on the source story. The SCOPE output   ║
        ║  contains no source-identifying information.           ║
        ╚═════════════════════════════════════════════════════════╝
        """
        self._progress("scope", "Running UKE_SCOPE on Stage 1 formalization...")
        t0 = time.time()

        prompt = (
            "Analyze the following constraint formalization using the UKE_SCOPE protocol.\n\n"
            "This is an abstract structural specification extracted from a source narrative. "
            "Your job is to identify the general constraint dynamics (e.g., 'agency depletion "
            "through contradictory authority,' 'unrequited love as asymmetric extraction') "
            "and decompose them into independent axes suitable for constraint story generation.\n\n"
            "CRITICAL: Use abstract structural language for claim_ids, human_readable names, "
            "and structural_delta fields. Do NOT reference any specific characters, settings, "
            "or narrative details from the formalization — extract the GENERAL DYNAMIC only.\n\n"
            "=== CONSTRAINT FORMALIZATION ===\n"
            f"{stage_1_output}\n\n"
            "Select exactly 3 axes for generation (or fewer if the topology is genuinely simple).\n\n"
            "Remember: OUTPUT ONLY valid JSON — no markdown fences, no commentary outside the JSON."
        )

        try:
            text, tin, tout = self._call_engine(
                prompt,
                system_instruction=self.engine_protocols["uke_scope"],
                temperature=0.2,
                max_tokens=8192,
            )
        except Exception as e:
            self._progress("scope", f"SCOPE call failed: {e}")
            return StepResult(
                step="scope", status="error", error=str(e),
                duration_s=time.time() - t0,
            )

        # Parse JSON
        try:
            from agent.story_generator_base import strip_json_fences
            manifest = json.loads(strip_json_fences(text))
        except (json.JSONDecodeError, ImportError) as e:
            # Fallback strip
            cleaned = re.sub(r'^```\w*\n?', '', text.strip())
            cleaned = re.sub(r'\n?```$', '', cleaned.strip())
            try:
                manifest = json.loads(cleaned)
            except json.JSONDecodeError:
                self._progress("scope", f"JSON parse failed: {e}")
                return StepResult(
                    step="scope", status="error",
                    error=f"JSON parse failed: {e}\nRaw:\n{text[:500]}",
                    duration_s=time.time() - t0,
                )

        # Validate minimum fields
        required = ["axes", "generation_sequence"]
        missing = [f for f in required if f not in manifest]
        if missing:
            self._progress("scope", f"Manifest missing fields: {missing}")
            return StepResult(
                step="scope", status="error",
                error=f"Missing fields: {missing}",
                data=manifest,
                duration_s=time.time() - t0,
            )

        # Log fracture warnings
        fracture = manifest.get("fracture_scan", {})
        if fracture.get("f03_hasty_generalization") or fracture.get("f34_epistemic_trespass"):
            self._progress("scope", f"Fracture warning: {fracture.get('notes', '')}")

        seq = manifest.get("generation_sequence", [])
        self._progress("scope", f"SCOPE complete — {len(seq)} axes: {seq}")

        # Save manifest
        if self.output_dir:
            self.output_dir.mkdir(parents=True, exist_ok=True)
            manifest_path = self.output_dir / "scope_manifest.json"
            manifest_path.write_text(json.dumps(manifest, indent=2), encoding="utf-8")

        return StepResult(
            step="scope", status="success", data=manifest,
            tokens_in=tin, tokens_out=tout,
            duration_s=time.time() - t0,
        )

    def _step_generate_constraint_stories(self, manifest: dict) -> StepResult:
        """Generate constraint story JSONs from SCOPE manifest axes.

        Uses the same generation logic as c-orchestrator._step_generate,
        but imports the infrastructure from story_generator_base.
        """
        self._progress("constraint_gen", "Generating constraint story JSONs...")
        t0 = time.time()

        from agent.story_generator_base import (
            process_response, save_story, build_prompt, _SYSTEM_INSTRUCTION,
        )

        sequence = manifest.get("generation_sequence", [])
        axes_by_id = {a["claim_id"]: a for a in manifest.get("axes", [])}
        generated_stories = []
        total_tin, total_tout = 0, 0

        for i, claim_id in enumerate(sequence):
            axis = axes_by_id.get(claim_id)
            if not axis:
                self._progress("constraint_gen", f"Axis {claim_id} not found, skipping")
                continue

            self._progress("constraint_gen", f"[{i+1}/{len(sequence)}] Generating {claim_id}...")

            # Build source description from axis fields
            source_desc = (
                f"TOPIC: {manifest.get('domain', 'Structural Analysis')}\n"
                f"CONSTRAINT: {claim_id}\n"
                f"Structural delta: {axis.get('structural_delta', 'Unknown')}\n"
                f"Primary observable: {axis.get('primary_observable', 'Unknown')}\n"
                f"Hypothesis type: {axis.get('hypothesis', 'Unknown')}\n"
                f"Epsilon bin: {axis.get('epsilon_bin', 'Unknown')}"
            )
            if axis.get("beneficiary"):
                source_desc += f"\nBeneficiary: {axis['beneficiary']}"
            if axis.get("victim"):
                source_desc += f"\nVictim: {axis['victim']}"

            # Build upstream context for downstream axes
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

            prompt = build_prompt(source_desc, upstream_context)

            try:
                text, tin, tout = self._call_engine(
                    prompt,
                    system_instruction=_SYSTEM_INSTRUCTION,
                    temperature=0.2,
                    max_tokens=8192,
                )
                total_tin += tin
                total_tout += tout
            except Exception as e:
                self._progress("constraint_gen", f"API error for {claim_id}: {e}")
                continue

            if not text:
                self._progress("constraint_gen", f"Empty response for {claim_id}")
                continue

            # Process and validate
            story_dict, errors = process_response(text)

            if story_dict is None or errors:
                # Retry once with error feedback
                self._progress("constraint_gen", f"Validation errors for {claim_id}, retrying...")
                feedback = ""
                if errors:
                    feedback = "\nYour previous attempt had these validation errors:\n"
                    for err in errors:
                        feedback += f"  - {err}\n"
                    feedback += "Fix these specific errors while keeping the rest correct.\n"

                retry_prompt = build_prompt(source_desc, upstream_context + feedback)
                try:
                    text, tin2, tout2 = self._call_engine(
                        retry_prompt,
                        system_instruction=_SYSTEM_INSTRUCTION,
                        temperature=0.2,
                        max_tokens=8192,
                    )
                    total_tin += tin2
                    total_tout += tout2
                    story_dict, errors = process_response(text)
                except Exception as e:
                    self._progress("constraint_gen", f"Retry failed for {claim_id}: {e}")
                    continue

            if story_dict is None or errors:
                self._progress("constraint_gen", f"Failed to generate valid story for {claim_id}")
                continue

            # Save to DR corpus (enables Prolog engine to find them)
            json_path, pl_path = save_story(story_dict, overwrite=True)
            if json_path:
                generated_stories.append(story_dict)
                self._progress("constraint_gen", f"Saved {claim_id}")

                # Also save to run output dir
                if self.output_dir:
                    self.output_dir.mkdir(parents=True, exist_ok=True)
                    run_json = self.output_dir / f"{claim_id}.json"
                    run_json.write_text(json.dumps(story_dict, indent=2), encoding="utf-8")

        self._progress(
            "constraint_gen",
            f"Generated {len(generated_stories)}/{len(sequence)} stories"
        )
        return StepResult(
            step="constraint_gen", status="success", data=generated_stories,
            tokens_in=total_tin, tokens_out=total_tout,
            duration_s=time.time() - t0,
        )

    def _step_prolog_engine(self, constraint_ids: list[str]) -> StepResult:
        """Run the Prolog engine pipeline and generate enhanced reports.

        Steps:
        1. run_pipeline (compile .pl, load into engine)
        2. enhanced_report.py (per-constraint diagnostic reports)

        Returns report texts concatenated as the step data.
        """
        if not constraint_ids:
            return StepResult(step="prolog_engine", status="skipped")

        self._progress("prolog_engine", f"Running Prolog engine for {len(constraint_ids)} constraints...")
        t0 = time.time()

        # Step 1: Corpus update (compile JSON → .pl, load into engine)
        try:
            sys.path.insert(0, str(_REPO_ROOT / "python"))
            from run_pipeline import run_pipeline
            pipeline_result = run_pipeline(
                progress=lambda step, msg: self._progress("prolog_engine", f"[pipeline] {msg}"),
                parallel=4,
            )
            if pipeline_result.errors:
                for e in pipeline_result.errors:
                    self._progress("prolog_engine", f"pipeline warning: {e}")
        except Exception as e:
            self._progress("prolog_engine", f"Pipeline failed: {e}")
            return StepResult(
                step="prolog_engine", status="error", error=str(e),
                duration_s=time.time() - t0,
            )

        # Step 2: Enhanced reports
        try:
            proc = subprocess.run(
                ["python3", "python/enhanced_report.py"] + constraint_ids,
                cwd=str(_REPO_ROOT),
                capture_output=True,
                text=True,
                timeout=300,
            )
            if proc.returncode != 0:
                self._progress("prolog_engine", f"enhanced_report.py returned {proc.returncode}")
        except subprocess.TimeoutExpired:
            self._progress("prolog_engine", "Report generation timed out (300s)")
        except Exception as e:
            self._progress("prolog_engine", f"Report generation failed: {e}")
            return StepResult(
                step="prolog_engine", status="error", error=str(e),
                duration_s=time.time() - t0,
            )

        # Collect report texts
        reports_dir = _REPO_ROOT / "outputs" / "constraint_reports"
        report_paths = []
        report_texts = []
        for cid in constraint_ids:
            rpath = reports_dir / f"{cid}_report.md"
            if rpath.exists():
                report_paths.append(rpath)
                try:
                    report_texts.append(rpath.read_text(encoding="utf-8"))
                except Exception:
                    pass

                # Also copy to run output dir
                if self.output_dir:
                    self.output_dir.mkdir(parents=True, exist_ok=True)
                    dest = self.output_dir / f"{cid}_report.md"
                    dest.write_text(rpath.read_text(encoding="utf-8"), encoding="utf-8")

        self._progress(
            "prolog_engine",
            f"Reports generated: {len(report_paths)}/{len(constraint_ids)}"
        )

        # Concatenate reports into a single text block for Stage 4
        combined_reports = ""
        for i, (cid, text) in enumerate(zip(constraint_ids, report_texts)):
            combined_reports += f"\n{'='*60}\n"
            combined_reports += f"CONSTRAINT REPORT: {cid}\n"
            combined_reports += f"{'='*60}\n"
            combined_reports += text
            combined_reports += "\n"

        return StepResult(
            step="prolog_engine", status="success",
            data={"report_paths": report_paths, "combined_text": combined_reports},
            duration_s=time.time() - t0,
        )

    # ------------------------------------------------------------------
    # Pipeline dispatch
    # ------------------------------------------------------------------

    def run(
        self,
        source_story: str,
        from_stage: str = "stage_0",
        source_path: Path | None = None,
    ) -> PipelineResult:
        """Execute the pipeline in the configured mode."""
        if self.mode == "narrative":
            return self._run_narrative(source_story, from_stage, source_path)
        return self._run_artifact(source_story, from_stage, source_path)

    # ------------------------------------------------------------------
    # Narrative pipeline (with constraint engine integration)
    # ------------------------------------------------------------------

    def _run_narrative(
        self,
        source_story: str,
        from_stage: str = "stage_0",
        source_path: Path | None = None,
    ) -> PipelineResult:
        """Execute the UKE_Narrative pipeline (6 stages + optional constraint engine)."""
        result = PipelineResult(
            run_id=f"uke_{int(time.time())}",
            mode="narrative",
            source_story=source_story,
        )
        if self.output_dir:
            result.output_dir = self.output_dir

        result.original_title = self._extract_original_title(source_path)

        t0 = time.time()
        start_idx = self.all_stages.index(from_stage)

        # Load cached outputs for stages before from_stage
        for stage in self.all_stages[:start_idx]:
            cached = self._load_stage_output(stage)
            if cached:
                result.stage_outputs[stage] = cached
                self._progress(stage, f"Loaded from cache ({len(cached)} chars)")
            else:
                self._progress(stage, "WARNING: No cached output found, pipeline may fail")

        # Also check for cached constraint reports
        if self.output_dir:
            cached_reports = self._load_stage_output("constraint_reports")
            if cached_reports:
                result.stage_outputs["constraint_reports"] = cached_reports

            # Also restore scope_manifest for summary output
            manifest_path = self.output_dir / "scope_manifest.json"
            if manifest_path.exists():
                try:
                    result.scope_manifest = json.loads(manifest_path.read_text())
                except Exception:
                    pass

        # ── Stage 0: Constraint Logic Extraction (Gemini) ─────────────
        if start_idx <= 0:
            step = self._run_stage_0(source_story)
            result.steps.append(step)
            if step.status == "error":
                result.total_duration_s = time.time() - t0
                self._tally(result)
                return result
            result.stage_outputs["stage_0"] = step.data
            self._save_stage_output("stage_0", step.data, result)

            if self.dry_run:
                self._progress("dry_run", "Stage 0 complete — dry-run stops here")
                result.total_duration_s = time.time() - t0
                self._tally(result)
                return result

        # ── Stage 1: Formalization (Claude) ────────────────────────────
        if start_idx <= 1:
            stage_0_out = result.stage_outputs.get("stage_0", "")
            step = self._run_stage_1(stage_0_out)
            result.steps.append(step)
            if step.status == "error":
                result.total_duration_s = time.time() - t0
                self._tally(result)
                return result
            result.stage_outputs["stage_1"] = step.data
            self._save_stage_output("stage_1", step.data, result)

        # ══════════════════════════════════════════════════════════════
        # NEW: Constraint Engine (between Stage 1 and Stage 2)
        #
        # Runs UKE_SCOPE on Stage 1 output → generates constraint story
        # JSONs → runs Prolog engine → produces diagnostic reports.
        #
        # AIR GAP SAFE: SCOPE operates on the Stage 1 formalization
        # (abstract structural topology), not on the source story.
        # Constraint stories and reports contain no source-identifying
        # material.
        # ══════════════════════════════════════════════════════════════
        if start_idx <= 1 and not self.skip_engine:
            stage_1_out = result.stage_outputs.get("stage_1", "")

            # Step A: SCOPE decomposition
            step = self._step_scope(stage_1_out)
            result.steps.append(step)

            if step.status == "success" and step.data:
                manifest = step.data
                result.scope_manifest = manifest
                self._save_stage_output(
                    "scope_manifest",
                    json.dumps(manifest, indent=2),
                    result,
                )

                # Step B: Generate constraint story JSONs
                step = self._step_generate_constraint_stories(manifest)
                result.steps.append(step)

                if step.status == "success" and step.data:
                    stories = step.data
                    result.constraint_stories = stories
                    constraint_ids = [
                        s["header"]["constraint_id"] for s in stories
                    ]

                    # Step C: Prolog engine + reports
                    step = self._step_prolog_engine(constraint_ids)
                    result.steps.append(step)

                    if step.status == "success" and step.data:
                        result.constraint_report_paths = step.data.get("report_paths", [])
                        combined_text = step.data.get("combined_text", "")
                        result.stage_outputs["constraint_reports"] = combined_text
                        self._save_stage_output(
                            "constraint_reports", combined_text, result
                        )
                        self._progress(
                            "engine",
                            f"Constraint engine complete — {len(result.constraint_report_paths)} reports ready"
                        )
                    else:
                        self._progress("engine", "Prolog engine failed — continuing without reports")
                else:
                    self._progress("engine", "Constraint story generation failed — continuing without reports")
            else:
                self._progress("engine", "SCOPE failed — continuing without constraint reports")

        elif self.skip_engine:
            self._progress("engine", "Constraint engine skipped (--skip-engine)")

        # ── Stage 2: Naturalization (Claude) ──────────────────────────
        if start_idx <= 2:
            stage_1_out = result.stage_outputs.get("stage_1", "")
            step = self._run_stage_2(stage_1_out)
            result.steps.append(step)
            if step.status == "error":
                result.total_duration_s = time.time() - t0
                self._tally(result)
                return result
            result.stage_outputs["stage_2"] = step.data
            self._save_stage_output("stage_2", step.data, result)

        # ── Stage 3: Editorial Decisions (Claude) ─────────────────────
        if start_idx <= 3:
            stage_1_out = result.stage_outputs.get("stage_1", "")
            stage_2_out = result.stage_outputs.get("stage_2", "")
            step = self._run_stage_3(stage_1_out, stage_2_out)
            result.steps.append(step)
            if step.status == "error":
                result.total_duration_s = time.time() - t0
                self._tally(result)
                return result
            result.stage_outputs["stage_3"] = step.data
            self._save_stage_output("stage_3", step.data, result)

        # ── Stage 4: Narrative Generation (Claude, AIR GAP ENFORCED) ──
        if start_idx <= 4:
            stage_1_out = result.stage_outputs.get("stage_1", "")
            stage_2_out = result.stage_outputs.get("stage_2", "")
            stage_3_out = result.stage_outputs.get("stage_3", "")
            constraint_reports = result.stage_outputs.get("constraint_reports", "")
            step = self._run_stage_4_narrative(
                stage_1_out, stage_2_out, stage_3_out, constraint_reports
            )
            result.steps.append(step)
            if step.status == "error":
                result.total_duration_s = time.time() - t0
                self._tally(result)
                return result
            result.stage_outputs["stage_4"] = step.data
            self._save_stage_output("stage_4", step.data, result)

        # ── Stage 5: Subtractive Audit (Claude, optional) ────────────
        if start_idx <= 5 and not self.skip_final_audit:
            stage_4_out = result.stage_outputs.get("stage_4", "")
            stage_1_out = result.stage_outputs.get("stage_1", "")
            step = self._run_stage_5_narrative(stage_4_out, stage_1_out)
            result.steps.append(step)
            result.stage_outputs["stage_5"] = step.data or stage_4_out
            self._save_stage_output("stage_5", result.stage_outputs["stage_5"], result)
        elif self.skip_final_audit:
            result.steps.append(StepResult(step="stage_5", status="skipped"))

        # Save final story
        final_key = "stage_5" if (not self.skip_final_audit and "stage_5" in result.stage_outputs) else "stage_4"
        final_text = result.stage_outputs.get(final_key, "")
        if final_text:
            story_path = self._save_final_output(final_text, result.original_title, STORIES_DIR)
            result.story_path = story_path
            self._progress("save", f"Final story saved to {story_path}")

        result.total_duration_s = time.time() - t0
        self._tally(result)
        return result

    # ------------------------------------------------------------------
    # Artifact pipeline (generic loop with validation gates — unchanged)
    # ------------------------------------------------------------------

    def _run_artifact(
        self,
        source_story: str,
        from_stage: str = "stage_0",
        source_path: Path | None = None,
    ) -> PipelineResult:
        """Execute the UKE_Artifact pipeline (7 stages with validation gates)."""
        result = PipelineResult(
            run_id=f"uke_artifact_{int(time.time())}",
            mode="artifact",
            source_story=source_story,
        )
        if self.output_dir:
            result.output_dir = self.output_dir

        result.original_title = self._extract_original_title(source_path)

        t0 = time.time()
        start_idx = self.all_stages.index(from_stage)

        for stage in self.all_stages[:start_idx]:
            cached = self._load_stage_output(stage)
            if cached:
                result.stage_outputs[stage] = cached
                self._progress(stage, f"Loaded from cache ({len(cached)} chars)")
            else:
                self._progress(stage, "WARNING: No cached output found, pipeline may fail")

        for i, stage in enumerate(self.all_stages):
            if i < start_idx:
                continue

            if stage == "stage_0":
                step = self._run_stage_0(source_story)
            else:
                step = self._run_stage_generic(stage, result.stage_outputs, source_story)

            result.steps.append(step)

            if step.status == "error":
                result.total_duration_s = time.time() - t0
                self._tally(result)
                return result

            result.stage_outputs[stage] = step.data
            self._save_stage_output(stage, step.data, result)

            if self.dry_run and stage == "stage_0":
                self._progress("dry_run", "Stage 0 complete — dry-run stops here")
                break

            if stage in self.validation_gates and not self.force_gate:
                gate_result = self._check_validation_gate(step)
                if gate_result == "halt":
                    step.status = "gate_halt"
                    self._progress(stage,
                        f"VALIDATION GATE: HALT. Review {stage}_output.md and "
                        f"--resume --from-stage {self.all_stages[i + 1] if i + 1 < len(self.all_stages) else stage}")
                    break

        gen_stage = "stage_5"
        if gen_stage in result.stage_outputs:
            final_text = result.stage_outputs[gen_stage]
            artifact_path = self._save_final_output(
                final_text, result.original_title, ARTIFACTS_DIR,
                is_code=self._is_code_output(final_text),
            )
            result.story_path = artifact_path
            self._progress("save", f"Final artifact saved to {artifact_path}")

        result.total_duration_s = time.time() - t0
        self._tally(result)
        return result

    # ------------------------------------------------------------------
    # Narrative-specific stage implementations
    # ------------------------------------------------------------------

    def _run_stage_0(self, source_story: str) -> StepResult:
        """Stage 0: Constraint Scoping & Extraction (Gemini). Shared by both modes."""
        self._progress("stage_0", "Extracting constraint logic (Gemini)...")
        t0 = time.time()

        prompt_parts = [
            "Analyze the following story to extract its constraint logic.\n\n",
            "=== SOURCE STORY ===\n",
            source_story,
            "\n\n",
        ]
        if self.dr_logic:
            prompt_parts.extend([
                "=== INDEXED CONSTRAINT LOGIC REFERENCE ===\n",
                self.dr_logic,
                "\n\n",
            ])
        prompt_parts.append(
            "Follow the constraint scoping and extraction protocol in your system "
            "instructions. Extract constraint types FROM EACH CHARACTER'S INDEX. "
            "Document indexical variance, power-scaling calculations, "
            "error dynamics, and terminal attractor. "
            "Do NOT reference the story by title or use framework terminology "
            "in constraint descriptions."
        )
        prompt = "".join(prompt_parts)

        try:
            text, tin, tout, model, provider = self._call("stage_0", prompt)
            self._progress("stage_0", f"Extraction complete ({tin}→{tout} tokens)")
            return StepResult(
                step="stage_0", status="success", data=text,
                tokens_in=tin, tokens_out=tout,
                duration_s=time.time() - t0,
                model_used=model, provider=provider,
            )
        except Exception as e:
            self._progress("stage_0", f"Failed: {e}")
            return StepResult(
                step="stage_0", status="error", error=str(e),
                duration_s=time.time() - t0,
            )

    def _run_stage_1(self, stage_0_output: str) -> StepResult:
        """Stage 1: Formalization (Claude). Narrative mode."""
        self._progress("stage_1", "Formalizing constraint specification (Claude)...")
        t0 = time.time()

        prompt_parts = [
            "Formalize the following constraint analysis into an operational "
            "specification with index-sensitive mechanics.\n\n",
            "=== STAGE 0 CONSTRAINT ANALYSIS ===\n",
            stage_0_output,
            "\n\n",
        ]
        if self.dr_logic:
            prompt_parts.extend([
                "=== INDEXED CONSTRAINT LOGIC REFERENCE ===\n",
                self.dr_logic,
                "\n\n",
            ])
        prompt_parts.append(
            "Follow the formalization protocol in your system instructions. "
            "Include: indexed classifications with χ calculations, "
            "transformation rules (IF-THEN, index-sensitive), "
            "error manifestations, institutional rationality model (PIR/BIR), "
            "terminal attractor selection, and validation checklist."
        )
        prompt = "".join(prompt_parts)

        try:
            text, tin, tout, model, provider = self._call("stage_1", prompt)
            self._progress("stage_1", f"Formalization complete ({tin}→{tout} tokens)")
            return StepResult(
                step="stage_1", status="success", data=text,
                tokens_in=tin, tokens_out=tout,
                duration_s=time.time() - t0,
                model_used=model, provider=provider,
            )
        except Exception as e:
            self._progress("stage_1", f"Failed: {e}")
            return StepResult(
                step="stage_1", status="error", error=str(e),
                duration_s=time.time() - t0,
            )

    def _run_stage_2(self, stage_1_output: str) -> StepResult:
        """Stage 2: Naturalization (Claude). Narrative mode."""
        self._progress("stage_2", "Designing naturalized context (Claude)...")
        t0 = time.time()

        prompt_parts = [
            "Design a narrative context for the following constraint specification.\n\n",
            "=== CONSTRAINT SPECIFICATION (Stage 1) ===\n",
            stage_1_output,
            "\n\n",
        ]
        if self.dr_logic:
            prompt_parts.extend([
                "=== INDEXED CONSTRAINT LOGIC REFERENCE ===\n",
                self.dr_logic,
                "\n\n",
            ])
        prompt_parts.append(
            "Follow the naturalization protocol in your system instructions. "
            "Create a setting where these exact constraints naturally occur. "
            "Output TWO sections:\n"
            "Section 1: CONTEXT DESCRIPTION (clean, no Omega markers, no framework terms)\n"
            "Section 2: OMEGA LOG (tracking & resolution record)\n\n"
            "The setting must be temporally/culturally displaced from any likely source. "
            "Framework must be INVISIBLE. Power differentials must be "
            "naturalized through setting structure."
        )
        prompt = "".join(prompt_parts)

        try:
            text, tin, tout, model, provider = self._call("stage_2", prompt)
            self._progress("stage_2", f"Naturalization complete ({tin}→{tout} tokens)")
            return StepResult(
                step="stage_2", status="success", data=text,
                tokens_in=tin, tokens_out=tout,
                duration_s=time.time() - t0,
                model_used=model, provider=provider,
            )
        except Exception as e:
            self._progress("stage_2", f"Failed: {e}")
            return StepResult(
                step="stage_2", status="error", error=str(e),
                duration_s=time.time() - t0,
            )

    def _run_stage_3(self, stage_1_output: str, stage_2_output: str) -> StepResult:
        """Stage 3: Editorial Decisions (Claude). Narrative mode."""
        self._progress("stage_3", "Making editorial decisions (Claude)...")
        t0 = time.time()

        prompt_parts = [
            "Make editorial decisions for the following narrative.\n\n",
            "=== CONSTRAINT SPECIFICATION (Stage 1) ===\n",
            stage_1_output,
            "\n\n",
            "=== CONTEXT DESIGN (Stage 2) ===\n",
            stage_2_output,
            "\n\n",
        ]
        if self.dr_logic:
            prompt_parts.extend([
                "=== INDEXED CONSTRAINT LOGIC REFERENCE ===\n",
                self.dr_logic,
                "\n\n",
            ])
        prompt_parts.append(
            "Follow the operational specification protocol in your system instructions. "
            "Provide decisions on:\n"
            "1. Terminal attractor verification\n"
            "2. Voice archetype selection (with justification)\n"
            "3. Indexical revelation strategy\n"
            "4. Editorial decisions (length, POV, tense, character count, naming)\n"
            "5. Primary physical marker\n"
            "6. Story blueprint in XML format\n\n"
            "CRITICAL: Verify attractor is compatible with constraint logic "
            "and rationality model."
        )
        prompt = "".join(prompt_parts)

        try:
            text, tin, tout, model, provider = self._call("stage_3", prompt)
            self._progress("stage_3", f"Editorial decisions complete ({tin}→{tout} tokens)")
            return StepResult(
                step="stage_3", status="success", data=text,
                tokens_in=tin, tokens_out=tout,
                duration_s=time.time() - t0,
                model_used=model, provider=provider,
            )
        except Exception as e:
            self._progress("stage_3", f"Failed: {e}")
            return StepResult(
                step="stage_3", status="error", error=str(e),
                duration_s=time.time() - t0,
            )

    def _run_stage_4_narrative(
        self,
        stage_1_output: str,
        stage_2_output: str,
        stage_3_output: str,
        constraint_reports: str = "",       # NEW parameter
    ) -> StepResult:
        """Stage 4: Narrative Generation (Claude).

        ╔══════════════════════════════════════════════════════╗
        ║  AIR GAP ENFORCED: This stage receives ONLY         ║
        ║  Stages 1-3 output + constraint engine reports.     ║
        ║  The original story and Stage 0 output are NEVER    ║
        ║  included in this call.                             ║
        ║                                                     ║
        ║  Constraint reports are AIR GAP SAFE: they derive   ║
        ║  from abstract structural topology via UKE_SCOPE,   ║
        ║  not from the source narrative.                     ║
        ╚══════════════════════════════════════════════════════╝
        """
        self._progress("stage_4", "Generating narrative (Claude, air gap active)...")
        t0 = time.time()

        prompt_parts = [
            "Write a complete story based on the following specifications.\n\n",
            "=== CONSTRAINT MECHANICS (Stage 1) ===\n",
            stage_1_output,
            "\n\n",
            "=== CONTEXT & WORLD (Stage 2) ===\n",
            stage_2_output,
            "\n\n",
            "=== EDITORIAL DECISIONS (Stage 3) ===\n",
            stage_3_output,
            "\n\n",
        ]

        # NEW: Include constraint engine reports if available
        if constraint_reports:
            prompt_parts.extend([
                "=== STRUCTURAL ANALYSIS (Constraint Engine) ===\n",
                "The following diagnostic reports were produced by the Prolog constraint "
                "engine analyzing the structural topology of this story's constraints. "
                "Use them to inform structural depth:\n"
                "- DRIFT ANALYSIS shows how constraints tighten or loosen over time — "
                "use for pacing and character arc intensity\n"
                "- COUPLING SCORES show how constraints interact — strongly coupled "
                "constraints should feel entangled in the narrative\n"
                "- PERSPECTIVAL GAPS (H^1 band, mandatrophy gap) show the distance "
                "between how different characters experience the same constraint — "
                "this IS the story's central dramatic tension\n"
                "- SIGNATURES (false_natural_law, coordination_washing) reveal what "
                "the constraint PRETENDS to be vs what it IS — characters inside may "
                "believe the cover story\n"
                "- THEOREM INSTANTIATIONS describe the structural physics — T4 (oracle gap) "
                "means confident observers are wrong; T2 (discrete blocs) means "
                "perspectives can't be reconciled by talking\n\n"
                "Do NOT reference any of this terminology in the story. These are "
                "structural instructions for you, the author. The story must be "
                "COMPLETELY INVISIBLE to framework analysis.\n\n",
                constraint_reports,
                "\n\n",
            ])

        prompt_parts.append(
            "Follow the generation protocol in your system instructions. "
            "Write the story now. Framework must be COMPLETELY INVISIBLE. "
            "Find new events in this world that embody the constraint logic — "
            "this is reimagining, not adaptation. "
            "Stay in the world completely. Trust the structure."
        )
        prompt = "".join(prompt_parts)

        try:
            text, tin, tout, model, provider = self._call("stage_4", prompt)
            self._progress("stage_4", f"Story generated ({tin}→{tout} tokens, {len(text)} chars)")
            return StepResult(
                step="stage_4", status="success", data=text,
                tokens_in=tin, tokens_out=tout,
                duration_s=time.time() - t0,
                model_used=model, provider=provider,
            )
        except Exception as e:
            self._progress("stage_4", f"Failed: {e}")
            return StepResult(
                step="stage_4", status="error", error=str(e),
                duration_s=time.time() - t0,
            )

    def _run_stage_5_narrative(self, stage_4_output: str, stage_1_output: str) -> StepResult:
        """Stage 5: Subtractive Audit (Claude). Narrative mode."""
        self._progress("stage_5", "Running subtractive audit (Claude)...")
        t0 = time.time()

        prompt = (
            "Audit the following story. Apply the subtractive audit protocol "
            "from your system instructions.\n\n"
            "=== CONSTRAINT SPECIFICATION (for validation) ===\n"
            f"{stage_1_output}\n\n"
            "=== STORY (Stage 4) ===\n"
            f"{stage_4_output}\n\n"
            "Perform: EARNED/FORCED scan, INHABITED/DEPLOYED scan, "
            "anti-pattern removal, compression audit. "
            "Output the revised story (should be tighter than input) "
            "followed by the validation report."
        )

        try:
            text, tin, tout, model, provider = self._call("stage_5", prompt)
            self._progress("stage_5", f"Subtractive audit complete ({tin}→{tout} tokens)")
            return StepResult(
                step="stage_5", status="success", data=text,
                tokens_in=tin, tokens_out=tout,
                duration_s=time.time() - t0,
                model_used=model, provider=provider,
            )
        except Exception as e:
            self._progress("stage_5", f"Failed: {e}")
            return StepResult(
                step="stage_5", status="error", error=str(e),
                duration_s=time.time() - t0,
            )

    # ------------------------------------------------------------------
    # Helpers
    # ------------------------------------------------------------------

    @staticmethod
    def _tally(result: PipelineResult):
        result.total_tokens_in = sum(s.tokens_in for s in result.steps)
        result.total_tokens_out = sum(s.tokens_out for s in result.steps)


# ---------------------------------------------------------------------------
# CLI entry point
# ---------------------------------------------------------------------------

VALID_PROVIDERS = {"google", "anthropic"}


def _parse_model_overrides(args, parser) -> dict[str, tuple[str, str]] | None:
    """Parse --stage-N-model flags into model override dict."""
    overrides = {}
    for stage in ALL_POSSIBLE_STAGES:
        attr = f"{stage}_model"
        value = getattr(args, attr, None)
        if value:
            if ":" not in value:
                parser.error(f"--{stage.replace('_', '-')}-model must be provider:model (got '{value}')")
            provider, model = value.split(":", 1)
            if provider not in VALID_PROVIDERS:
                parser.error(f"Unknown provider '{provider}'. Valid: {VALID_PROVIDERS}")
            overrides[stage] = (provider, model)
    return overrides if overrides else None


def _run_single(args, parser):
    """Run the pipeline on a single story."""
    # Resolve story source
    story_path = None
    source_story = None

    if args.resume:
        output_dir = Path(args.resume)
        source_file = output_dir / "source_story.txt"
        if source_file.exists():
            source_story = source_file.read_text(encoding="utf-8")
        else:
            parser.error(f"No source_story.txt in {output_dir}")
    else:
        path = args.story or getattr(args, "story_file", None)
        if path:
            story_path = Path(path)
            if not story_path.exists():
                parser.error(f"File not found: {story_path}")
            source_story = story_path.read_text(encoding="utf-8")
        else:
            parser.error("Provide a story file or --resume directory")

    model_overrides = _parse_model_overrides(args, parser)

    # Output directory
    if args.resume:
        output_dir = Path(args.resume)
    elif args.output_dir:
        output_dir = Path(args.output_dir)
    else:
        prefix = "uke_artifact" if args.mode == "artifact" else "uke_output"
        slug = _title_to_filename(story_path.stem) if story_path else "input"
        output_dir = Path(f"{prefix}_{slug}_{int(time.time())}")

    output_dir.mkdir(parents=True, exist_ok=True)

    # Save source for resume
    if not args.resume and source_story:
        (output_dir / "source_story.txt").write_text(source_story, encoding="utf-8")

    orch = UKEOrchestrator(
        mode=args.mode,
        models=model_overrides if model_overrides else None,
        dr_logic_path=args.dr_logic,
        output_dir=output_dir,
        skip_final_audit=args.skip_final_audit,
        skip_engine=args.skip_engine,           # NEW flag
        dry_run=args.dry_run,
        force_gate=args.force_gate,
    )

    result = orch.run(
        source_story,
        from_stage=args.from_stage,
        source_path=story_path,
    )

    # Print summary
    print(f"\n{'=' * 70}")
    print(f"PIPELINE SUMMARY — {args.mode.upper()} MODE")
    print(f"{'=' * 70}")

    for s in result.steps:
        tok = f" ({s.tokens_in:,}→{s.tokens_out:,} tokens)" if s.tokens_in else ""
        dur = f" [{s.duration_s:.1f}s]" if s.duration_s else ""
        model = f" ({s.model_used})" if s.model_used else ""
        print(f"  {s.step:20s} {s.status:10s}{tok}{dur}{model}")
        if s.error:
            print(f"    error: {s.error[:200]}")

    print(f"\n  Total tokens: {result.total_tokens_in:,} → {result.total_tokens_out:,}")
    print(f"  Total time:   {result.total_duration_s:.1f}s")

    # NEW: constraint engine summary
    if result.scope_manifest:
        seq = result.scope_manifest.get("generation_sequence", [])
        print(f"\n  Constraint engine: {len(seq)} axes decomposed, "
              f"{len(result.constraint_stories)} stories generated, "
              f"{len(result.constraint_report_paths)} reports produced")

    if result.story_path:
        print(f"\n  Output: {result.story_path}")

    # Cost estimate
    cost_in = result.total_tokens_in / 1_000_000 * 3.0
    cost_out = result.total_tokens_out / 1_000_000 * 15.0
    print(f"  Est cost: ~${cost_in + cost_out:.2f}")


def _run_batch(args, parser):
    """Run the pipeline on all stories in originals/."""
    originals = sorted(ORIGINALS_DIR.glob("*.md"))
    if not originals:
        parser.error(f"No stories found in {ORIGINALS_DIR}")
        return

    model_overrides = _parse_model_overrides(args, parser)
    mode_config = PIPELINE_MODES[args.mode]
    output_base = mode_config["output_dir"]

    def _is_completed(story_path: Path) -> bool:
        text = story_path.read_text(encoding="utf-8")
        title = _extract_title(text)
        marker = f"Original: {title}"
        for out_file in output_base.glob("*"):
            if not out_file.is_file():
                continue
            try:
                content = out_file.read_text(encoding="utf-8")
                if marker in content:
                    return True
            except Exception:
                continue
        return False

    to_process = []
    skipped = []
    for p in originals:
        if _is_completed(p):
            skipped.append(p)
        else:
            to_process.append(p)

    total = len(originals)
    skip_count = len(skipped)

    print(f"\n{'=' * 70}")
    print(f"UKE BATCH RUN — {args.mode.upper()} MODE")
    print(f"{'=' * 70}")
    print(f"  Source dir:     {ORIGINALS_DIR}")
    print(f"  Output dir:     {output_base}")
    print(f"  Stories found:  {total}")
    print(f"  Engine:         {'enabled' if not args.skip_engine else 'disabled'}")
    if skip_count:
        print(f"  Already done:   {skip_count} (skipping)")
        for p in skipped:
            print(f"    - {p.name}")
    print(f"  To process:     {len(to_process)}")
    print()

    if not to_process:
        print("Nothing to do.")
        return

    batch_results: list[tuple[Path, PipelineResult | None, str]] = []
    batch_t0 = time.time()
    batch_tokens_in = 0
    batch_tokens_out = 0

    for i, story_path in enumerate(to_process, 1):
        source_story = story_path.read_text(encoding="utf-8")
        title = _extract_title(source_story)

        print(f"\n{'─' * 70}")
        print(f"  [{i}/{len(to_process)}] {story_path.name}")
        print(f"  Title: {title}")
        print(f"{'─' * 70}")

        prefix = "uke_artifact" if args.mode == "artifact" else "uke_output"
        slug = _title_to_filename(story_path.stem)
        output_dir = Path(f"{prefix}_{slug}_{int(time.time())}")
        output_dir.mkdir(parents=True, exist_ok=True)

        story_save = output_dir / "source_story.txt"
        story_save.write_text(source_story, encoding="utf-8")

        orch = UKEOrchestrator(
            mode=args.mode,
            models=model_overrides if model_overrides else None,
            dr_logic_path=args.dr_logic,
            output_dir=output_dir,
            skip_final_audit=args.skip_final_audit,
            skip_engine=args.skip_engine,
            dry_run=args.dry_run,
            force_gate=args.force_gate,
        )

        try:
            result = orch.run(source_story, from_stage="stage_0", source_path=story_path)
            batch_tokens_in += result.total_tokens_in
            batch_tokens_out += result.total_tokens_out

            final_step = result.steps[-1] if result.steps else None
            if final_step and final_step.status == "gate_halt":
                status = "GATE_HALT"
            elif final_step and final_step.status == "error":
                status = "ERROR"
            elif result.story_path:
                status = "OK"
            else:
                status = "INCOMPLETE"

            batch_results.append((story_path, result, status))
            print(f"\n  >> {status} | {result.total_tokens_in:,}→{result.total_tokens_out:,} tokens | {result.total_duration_s:.0f}s")
            if result.story_path:
                print(f"  >> Saved: {result.story_path}")

        except Exception as e:
            _log.error("Pipeline crashed for %s: %s", story_path.name, e)
            batch_results.append((story_path, None, "CRASH"))
            print(f"\n  >> CRASH: {e}")

    batch_duration = time.time() - batch_t0

    print(f"\n\n{'=' * 70}")
    print(f"BATCH SUMMARY — {args.mode.upper()} MODE")
    print(f"{'=' * 70}")
    print()

    print(f"  {'Story':<35s} {'Status':<12s} {'Tokens In':>10s} {'Tokens Out':>10s} {'Time':>7s}")
    print(f"  {'─' * 35} {'─' * 12} {'─' * 10} {'─' * 10} {'─' * 7}")

    for story_path, result, status in batch_results:
        name = story_path.stem[:33]
        if result:
            tin = f"{result.total_tokens_in:,}"
            tout = f"{result.total_tokens_out:,}"
            dur = f"{result.total_duration_s:.0f}s"
        else:
            tin = tout = dur = "—"
        print(f"  {name:<35s} {status:<12s} {tin:>10s} {tout:>10s} {dur:>7s}")

    if skipped:
        for p in skipped:
            name = p.stem[:33]
            print(f"  {name:<35s} {'SKIPPED':<12s} {'—':>10s} {'—':>10s} {'—':>7s}")

    ok_count = sum(1 for _, _, s in batch_results if s == "OK")
    fail_count = sum(1 for _, _, s in batch_results if s != "OK")

    print(f"\n  Total:    {ok_count} OK, {fail_count} failed/halted, {skip_count} skipped")
    print(f"  Tokens:   {batch_tokens_in:,} → {batch_tokens_out:,}")
    print(f"  Duration: {batch_duration:.0f}s ({batch_duration / 60:.1f}m)")

    cost_in = batch_tokens_in / 1_000_000 * 3.0
    cost_out = batch_tokens_out / 1_000_000 * 15.0
    print(f"  Est cost: ~${cost_in + cost_out:.2f} (Sonnet input ${cost_in:.2f} + output ${cost_out:.2f})")


def main():
    parser = argparse.ArgumentParser(
        description="UKE Pipeline — Gemini + Claude constraint translation (narrative or artifact)"
    )
    parser.add_argument(
        "story", nargs="?",
        help="Path to source story file (default: searches narrative_transform/originals/)"
    )
    parser.add_argument("--story", "-s", dest="story_file", help="Path to source story file")
    parser.add_argument(
        "--mode", "-m",
        choices=["narrative", "artifact"],
        default="narrative",
        help="Pipeline mode: narrative (story) or artifact (software)"
    )
    parser.add_argument(
        "--dr-logic",
        default=str(LOGIC_NARRATIVE_PATH),
        help=f"Path to constraint logic reference (default: {LOGIC_NARRATIVE_PATH.name})"
    )
    parser.add_argument("--output-dir", "-o", help="Directory for intermediate outputs")
    parser.add_argument("--resume", help="Resume from output directory")
    parser.add_argument(
        "--from-stage", default="stage_0",
        choices=ALL_POSSIBLE_STAGES,
        help="Resume from this stage (default: stage_0)"
    )
    parser.add_argument("--skip-final-audit", action="store_true",
                        help="Skip final audit stage (stage 5 narrative / stage 6 artifact)")
    parser.add_argument("--skip-engine", action="store_true",
                        help="Skip constraint engine (SCOPE → stories → Prolog reports)")
    parser.add_argument("--force-gate", action="store_true",
                        help="Do not halt on validation gate failures (artifact mode)")
    parser.add_argument("--dry-run", action="store_true", help="Run Stage 0 only")
    parser.add_argument("--batch", action="store_true",
                        help="Process all stories in originals/. Skips already-completed ones.")

    for stage in ALL_POSSIBLE_STAGES:
        parser.add_argument(
            f"--{stage.replace('_', '-')}-model",
            help=f"Override model for {stage} (format: provider:model)",
        )

    args = parser.parse_args()

    mode_config = PIPELINE_MODES[args.mode]
    if args.from_stage not in mode_config["stages"]:
        parser.error(
            f"Stage '{args.from_stage}' not valid for --mode {args.mode}. "
            f"Valid stages: {mode_config['stages']}"
        )

    if args.batch:
        _run_batch(args, parser)
    else:
        _run_single(args, parser)


if __name__ == "__main__":
    main()
