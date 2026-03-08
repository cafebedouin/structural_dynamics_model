# ARCHIVED: Superseded by uke_narrative_orchestrator.py (constraint engine integration)
"""UKE Pipeline — Gemini + Claude orchestrator with air-gap enforcement.

Two modes:
  - narrative: Stage 0 (Gemini) → Stages 1-5 (Claude)
    Story translation preserving constraint topology.
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
"""

import argparse
import logging
import os
import re
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
STAGE_INPUTS = {
    "narrative": {
        "stage_0": ["source", "dr_logic"],
        "stage_1": ["stage_0", "dr_logic"],
        "stage_2": ["stage_1", "dr_logic"],
        "stage_3": ["stage_1", "stage_2", "dr_logic"],
        "stage_4": ["stage_1", "stage_2", "stage_3"],       # AIR GAP: no source, no stage_0
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
    step: str           # stage_0 .. stage_6
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
                # Retry on 429 (rate limit), fail fast on other 4xx
                if getattr(e, "code", 0) == 429:
                    if attempt == max_retries - 1:
                        raise
                    wait = 2 ** attempt * 5  # longer backoff for rate limits
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
      - artifact: 7-stage software generation (artifact_stage0.md .. artifact_stage6.md)

    Stage 0: Google Gemini (constraint extraction)
    Remaining stages: Anthropic Claude
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
        "stage_2": 0.3,   # validation/naturalization
        "stage_3": 0.3,   # editorial/path selection
        "stage_4": 0.8,   # narrative generation / interaction design
        "stage_5": 0.3,   # audit / artifact generation
        "stage_6": 0.2,   # artifact validation
    }

    # Mode-specific temperature overrides
    TEMPERATURE_OVERRIDES = {
        "narrative": {
            "stage_2": 0.7,   # naturalization needs creativity
            "stage_4": 0.8,   # narrative generation
            "stage_5": 0.3,   # subtractive audit
        },
        "artifact": {
            "stage_2": 0.1,   # validation (precision)
            "stage_3": 0.5,   # path/modality selection
            "stage_4": 0.5,   # interaction design
            "stage_5": 0.7,   # artifact generation (creative)
            "stage_6": 0.2,   # validation (precision)
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
        """Extract the dominant code block from text, handling preamble/postamble.

        Handles:
          - Raw code (starts with import/export/etc.)
          - Code wrapped in markdown fences (```tsx ... ```)
          - Code preceded by commentary ("Here's the artifact:\n\n```tsx\n...")
          - Multiple small code blocks (returns None — not a single artifact)
        """
        stripped = text.strip()

        # Case 1: raw code — no fences, starts with code patterns
        code_starts = (
            "import ", "export ", "'use client'", '"use client"',
            "const ", "function ", "// ", "/* ",
        )
        if stripped.startswith(code_starts):
            return stripped

        # Case 2: find code fences — extract the largest one
        fence_pattern = re.compile(
            r'^```[^\n]*\n(.*?)^```', re.MULTILINE | re.DOTALL
        )
        blocks = fence_pattern.findall(stripped)
        if not blocks:
            return None

        # Use the largest code block (the artifact, not inline snippets)
        largest = max(blocks, key=len).strip()

        # Must be substantial (>200 chars) and look like code, not a snippet
        if len(largest) > 200 and largest.startswith(code_starts):
            return largest

        return None

    @staticmethod
    def _is_code_output(text: str) -> bool:
        """Detect whether stage output is primarily code (not markdown prose)."""
        return UKEOrchestrator._extract_code_block(text) is not None

    @staticmethod
    def _save_final_output(
        text: str, original_title: str, output_dir: Path, is_code: bool = False,
    ) -> Path:
        """Save final output to the mode-specific directory.

        Appends original title at the end (Python, not AI).
        For code artifacts: extracts code from fences/preamble, uses .tsx.
        Returns the path of the saved file.
        """
        if is_code:
            content = UKEOrchestrator._extract_code_block(text) or text.strip()
            ext = ".tsx"
            # Derive filename from first component/function name or fallback
            name_match = re.search(
                r'(?:export\s+(?:default\s+)?function|const)\s+(\w+)', content
            )
            base = _title_to_filename(name_match.group(1)) if name_match else "artifact"
            trailer = f"\n// Original: {original_title}\n"
        else:
            content = text.strip()
            ext = ".md"
            title = _extract_title(content)
            base = _title_to_filename(title)
            trailer = f"\n\n---\n*Original: {original_title}*\n"

        output_dir.mkdir(parents=True, exist_ok=True)
        out_path = output_dir / f"{base}{ext}"

        # Handle collision
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
        """Check if a validation stage output indicates PASS or FAIL.

        Returns 'pass' or 'halt'.
        """
        if not step.data:
            return "halt"

        text = step.data

        # Look for explicit FAIL markers
        fail_patterns = [r'\bFAIL\b', r'\bHALT\b', r'VALIDATION:\s*FAIL', r'GATE:\s*FAIL']
        pass_patterns = [r'\bPASS\b', r'VALIDATION:\s*PASS', r'GATE:\s*PASS']

        has_fail = any(re.search(p, text, re.IGNORECASE) for p in fail_patterns)
        has_pass = any(re.search(p, text, re.IGNORECASE) for p in pass_patterns)

        # Any FAIL halts, regardless of whether PASS also appears
        # (e.g., "Test 1: PASS ... Test 3: FAIL ... Overall: FAIL")
        if has_fail:
            return "halt"
        if has_pass:
            return "pass"

        # Ambiguous — halt by default (safe for batch runs)
        _log.warning("Validation gate output ambiguous for %s, halting", step.step)
        return "halt"

    # ------------------------------------------------------------------
    # Artifact-specific prompt suffixes
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
        """Return stage-specific prompt suffix, or generic fallback."""
        if self.mode == "artifact" and stage in self.ARTIFACT_PROMPT_SUFFIXES:
            return self.ARTIFACT_PROMPT_SUFFIXES[stage]
        return "Follow the protocol in your system instructions for this stage."

    # ------------------------------------------------------------------
    # Generic stage runner (used by artifact mode, usable by narrative)
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

        # Air gap enforcement
        if is_air_gap:
            assert "source" not in input_keys, f"Air gap violation: source in {stage}"
            assert "stage_0" not in input_keys, f"Air gap violation: stage_0 in {stage}"

        # Assemble prompt from input keys
        prompt_parts = []
        for key in input_keys:
            if key == "source":
                prompt_parts.append(f"=== SOURCE MATERIAL ===\n{source_story}\n\n")
            elif key == "dr_logic":
                if self.dr_logic:
                    prompt_parts.append(
                        f"=== INDEXED CONSTRAINT LOGIC REFERENCE ===\n{self.dr_logic}\n\n"
                    )
            else:
                # key is "stage_N"
                content = stage_outputs.get(key, "")
                snum = key.split("_")[1]
                prompt_parts.append(f"=== STAGE {snum} OUTPUT ===\n{content}\n\n")

        # Stage-specific prompt suffixes
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
    # Narrative pipeline (preserved from original implementation)
    # ------------------------------------------------------------------

    def _run_narrative(
        self,
        source_story: str,
        from_stage: str = "stage_0",
        source_path: Path | None = None,
    ) -> PipelineResult:
        """Execute the UKE_Narrative pipeline (6 stages)."""
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
            step = self._run_stage_4_narrative(stage_1_out, stage_2_out, stage_3_out)
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
    # Artifact pipeline (generic loop with validation gates)
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

        # Load cached outputs for stages before from_stage
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

            # Stage 0 reuses the narrative stage 0 (same extraction logic)
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

            # Dry run: stop after stage 0
            if self.dry_run and stage == "stage_0":
                self._progress("dry_run", "Stage 0 complete — dry-run stops here")
                break

            # Validation gate check
            if stage in self.validation_gates and not self.force_gate:
                gate_result = self._check_validation_gate(step)
                if gate_result == "halt":
                    step.status = "gate_halt"
                    self._progress(stage,
                        f"VALIDATION GATE: HALT. Review {stage}_output.md and "
                        f"--resume --from-stage {self.all_stages[i + 1] if i + 1 < len(self.all_stages) else stage}")
                    break

        # Save final artifact if pipeline completed through generation stage
        gen_stage = "stage_5"  # artifact generation
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
    ) -> StepResult:
        """Stage 4: Narrative Generation (Claude).

        ╔══════════════════════════════════════════════════════╗
        ║  AIR GAP ENFORCED: This stage receives ONLY         ║
        ║  Stages 1-3 output. The original story and          ║
        ║  Stage 0 output are NEVER included in this call.    ║
        ╚══════════════════════════════════════════════════════╝
        """
        self._progress("stage_4", "Generating narrative (Claude, air gap active)...")
        t0 = time.time()

        prompt = (
            "Write a complete story based on the following specifications.\n\n"
            "=== CONSTRAINT MECHANICS (Stage 1) ===\n"
            f"{stage_1_output}\n\n"
            "=== CONTEXT & WORLD (Stage 2) ===\n"
            f"{stage_2_output}\n\n"
            "=== EDITORIAL DECISIONS (Stage 3) ===\n"
            f"{stage_3_output}\n\n"
            "Follow the generation protocol in your system instructions. "
            "Write the story now. Framework must be COMPLETELY INVISIBLE. "
            "Find new events in this world that embody the constraint logic — "
            "this is reimagining, not adaptation. "
            "Stay in the world completely. Trust the structure."
        )

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


def _parse_model_overrides(args, parser) -> dict:
    """Parse --stage-N-model overrides from CLI args."""
    overrides = {}
    for stage in ALL_POSSIBLE_STAGES:
        attr = f"{stage}_model"
        val = getattr(args, attr, None)
        if val and ":" in val:
            provider, model = val.split(":", 1)
            if provider not in VALID_PROVIDERS:
                parser.error(f"Unknown provider '{provider}'. Use: {VALID_PROVIDERS}")
            overrides[stage] = (provider, model)
    return overrides


def _print_run_summary(result: PipelineResult, mode: str, output_dir: Path):
    """Print summary for a single pipeline run."""
    mode_label = "UKE_NARRATIVE" if mode == "narrative" else "UKE_ARTIFACT"
    print(f"\n{'=' * 70}")
    print(f"{mode_label} PIPELINE SUMMARY")
    print("=" * 70)
    print(f"  Run ID:         {result.run_id}")
    print(f"  Mode:           {mode}")
    print(f"  Original title: {result.original_title}")
    print(f"  Output dir:     {output_dir}")
    print()

    for s in result.steps:
        tok = f" ({s.tokens_in:,}→{s.tokens_out:,} tokens)" if s.tokens_in else ""
        dur = f" [{s.duration_s:.1f}s]" if s.duration_s else ""
        model_info = f" [{s.provider}:{s.model_used}]" if s.model_used else ""
        print(f"  {s.step:12s} {s.status:10s}{tok}{dur}{model_info}")
        if s.error:
            print(f"    error: {s.error[:200]}")

    print(f"\n  Total tokens: {result.total_tokens_in:,} → {result.total_tokens_out:,}")
    print(f"  Total time:   {result.total_duration_s:.1f}s")

    if result.story_path:
        label = "Final story" if mode == "narrative" else "Final artifact"
        print(f"\n  {label}:  {result.story_path}")
        try:
            print(f"  Output length: {len(result.story_path.read_text()):,} chars")
        except Exception:
            pass

    air_gap = PIPELINE_MODES[mode]["air_gap_stage"]
    air_num = air_gap.split("_")[1]
    print(f"\n  ╔══════════════════════════════════════════════════════╗")
    print(f"  ║  AIR GAP STATUS: ENFORCED                           ║")
    print(f"  ║  Stage {air_num} never received source story or Stage 0.    ║")
    print(f"  ╚══════════════════════════════════════════════════════╝")


def _run_single(args, parser) -> PipelineResult:
    """Run the pipeline on a single story. Returns the PipelineResult."""
    mode_config = PIPELINE_MODES[args.mode]

    # Resolve story path
    story_path_str = args.story or args.story_file
    story_path: Path | None = None

    if args.resume:
        output_dir = Path(args.resume)
        story_cache = output_dir / "source_story.txt"
        if story_cache.exists():
            source_story = story_cache.read_text(encoding="utf-8")
            story_path = story_cache
        elif story_path_str:
            story_path = Path(story_path_str)
            source_story = story_path.read_text(encoding="utf-8")
        else:
            source_story = ""
    elif story_path_str:
        story_path = Path(story_path_str)
        source_story = story_path.read_text(encoding="utf-8")
    else:
        # Auto-discover from originals directory
        originals = sorted(ORIGINALS_DIR.glob("*.md"))
        if len(originals) == 1:
            story_path = originals[0]
            source_story = story_path.read_text(encoding="utf-8")
            print(f"Auto-selected: {story_path.name}")
        elif originals:
            parser.error(
                f"Multiple stories in {ORIGINALS_DIR}. Specify one or use --batch:\n"
                + "\n".join(f"  {p.name}" for p in originals)
            )
            return  # unreachable but satisfies type checker
        else:
            parser.error(f"No stories found in {ORIGINALS_DIR}")
            return

    model_overrides = _parse_model_overrides(args, parser)

    # Determine output directory
    prefix = "uke_artifact" if args.mode == "artifact" else "uke_output"
    output_dir = Path(args.resume or args.output_dir or f"{prefix}_{int(time.time())}")

    # Save source story for resume capability
    output_dir.mkdir(parents=True, exist_ok=True)
    story_save = output_dir / "source_story.txt"
    if source_story and not story_save.exists():
        story_save.write_text(source_story, encoding="utf-8")

    orch = UKEOrchestrator(
        mode=args.mode,
        models=model_overrides if model_overrides else None,
        dr_logic_path=args.dr_logic,
        output_dir=output_dir,
        skip_final_audit=args.skip_final_audit,
        dry_run=args.dry_run,
        force_gate=args.force_gate,
    )

    result = orch.run(source_story, from_stage=args.from_stage, source_path=story_path)
    _print_run_summary(result, args.mode, output_dir)
    return result


def _run_batch(args, parser):
    """Run the pipeline on all stories in originals/. Skips already-completed ones."""
    originals = sorted(ORIGINALS_DIR.glob("*.md"))
    if not originals:
        parser.error(f"No stories found in {ORIGINALS_DIR}")
        return

    model_overrides = _parse_model_overrides(args, parser)
    mode_config = PIPELINE_MODES[args.mode]
    output_base = mode_config["output_dir"]

    # Determine which stories already have output
    def _is_completed(story_path: Path) -> bool:
        """Check if a final output already exists for this story.

        Scans output dir files for the traceability marker matching
        this story's title (handles AI-generated output filenames).
        """
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
    if skip_count:
        print(f"  Already done:   {skip_count} (skipping)")
        for p in skipped:
            print(f"    - {p.name}")
    print(f"  To process:     {len(to_process)}")
    print()

    if not to_process:
        print("Nothing to do.")
        return

    # Track results for summary table
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
            dry_run=args.dry_run,
            force_gate=args.force_gate,
        )

        try:
            result = orch.run(source_story, from_stage="stage_0", source_path=story_path)
            batch_tokens_in += result.total_tokens_in
            batch_tokens_out += result.total_tokens_out

            # Determine status
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

    # ── Batch summary table ──────────────────────────────────────────
    batch_duration = time.time() - batch_t0

    print(f"\n\n{'=' * 70}")
    print(f"BATCH SUMMARY — {args.mode.upper()} MODE")
    print(f"{'=' * 70}")
    print()

    # Table header
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

    # Totals
    ok_count = sum(1 for _, _, s in batch_results if s == "OK")
    fail_count = sum(1 for _, _, s in batch_results if s != "OK")

    print(f"\n  Total:    {ok_count} OK, {fail_count} failed/halted, {skip_count} skipped")
    print(f"  Tokens:   {batch_tokens_in:,} → {batch_tokens_out:,}")
    print(f"  Duration: {batch_duration:.0f}s ({batch_duration / 60:.1f}m)")

    # Cost estimate (Sonnet 4.5 pricing as of Feb 2026)
    # Gemini stage 0 is cheap enough to ignore for estimation
    cost_in = batch_tokens_in / 1_000_000 * 3.0    # $3/MTok input
    cost_out = batch_tokens_out / 1_000_000 * 15.0  # $15/MTok output
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
    parser.add_argument("--force-gate", action="store_true",
                        help="Do not halt on validation gate failures (artifact mode)")
    parser.add_argument("--dry-run", action="store_true", help="Run Stage 0 only")
    parser.add_argument("--batch", action="store_true",
                        help="Process all stories in originals/. Skips already-completed ones.")

    # Model overrides (provider:model format) for all possible stages
    for stage in ALL_POSSIBLE_STAGES:
        parser.add_argument(
            f"--{stage.replace('_', '-')}-model",
            help=f"Override model for {stage} (format: provider:model)",
        )

    args = parser.parse_args()

    # Validate --from-stage against selected mode
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
