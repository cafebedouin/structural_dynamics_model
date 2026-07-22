"""Editorial Analysis Pipeline — post-essay review and revision.

Takes an essay (typically from c-orchestrator stage 6) and runs it through
four independent editorial protocols, then revises using uke_write_v2.2.md.

Usage:
    python3 agent/analysis.py outputs/essays/some_essay.md
    python3 agent/analysis.py outputs/essays/some_essay.md --subject my_topic
    cat essay.md | python3 agent/analysis.py --subject my_topic
"""

import argparse
import re
import sys
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Callable

root_path = Path(__file__).resolve().parent.parent
if str(root_path) not in sys.path:
    sys.path.insert(0, str(root_path))

from agent.story_generator_base import _load_context_file, REPO_ROOT

# ---------------------------------------------------------------------------
# Data class
# ---------------------------------------------------------------------------

@dataclass
class StepResult:
    step: str
    status: str         # success | error
    data: Any = None
    error: str = ""
    tokens_in: int = 0
    tokens_out: int = 0
    duration_s: float = 0.0

# ---------------------------------------------------------------------------
# Anthropic client helper
# ---------------------------------------------------------------------------

_anthropic_client = None

def _get_client():
    global _anthropic_client
    if _anthropic_client is None:
        import anthropic
        _anthropic_client = anthropic.Anthropic()
    return _anthropic_client

# ---------------------------------------------------------------------------
# Editorial passes
# ---------------------------------------------------------------------------

EDITORIAL_PASSES = [
    ("editing",   "uke_editing"),
    ("grounding", "uke_grounding"),
    ("audit",     "uke_audit"),
    ("reality",   "uke_reality"),
]

# ---------------------------------------------------------------------------
# Pipeline
# ---------------------------------------------------------------------------

class EditorialAnalysisPipeline:
    MODEL = "claude-sonnet-4-5-20250929"
    MAX_TOKENS = 8192

    def __init__(self, progress_callback: Callable[[str, str], None] | None = None):
        self._progress = progress_callback or (lambda step, msg: print(f"[{step}] {msg}"))
        agent_dir = Path(__file__).resolve().parent
        self.protocols = {
            "uke_editing":   _load_context_file(str(agent_dir / "analysis" / "uke_editing.md")),
            "uke_grounding": _load_context_file(str(agent_dir / "analysis" / "uke_grounding.md")),
            "uke_audit":     _load_context_file(str(agent_dir / "analysis" / "uke_audit.md")),
            "uke_reality":   _load_context_file(str(agent_dir / "analysis" / "uke_reality.md")),
            "uke_write":     _load_context_file(str(agent_dir / "uke_write_v2.2.md")),
        }

    # -- API helpers (same pattern as c-orchestrator) ----------------------

    @staticmethod
    def _extract_text(response) -> str:
        parts = []
        for block in response.content:
            if hasattr(block, "text"):
                parts.append(block.text)
        return "\n".join(parts)

    def _call(
        self,
        prompt: str,
        system_instruction: str = "",
        temperature: float = 0.3,
        max_tokens: int | None = None,
    ) -> tuple[str, int, int]:
        client = _get_client()
        if max_tokens is None:
            max_tokens = self.MAX_TOKENS

        kwargs: dict[str, Any] = {
            "model": self.MODEL,
            "max_tokens": max_tokens,
            "temperature": temperature,
            "messages": [{"role": "user", "content": prompt}],
        }
        if system_instruction:
            kwargs["system"] = system_instruction

        response = self._call_with_retry(client, **kwargs)
        return (
            self._extract_text(response),
            response.usage.input_tokens,
            response.usage.output_tokens,
        )

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
                time.sleep(wait)
            except anthropic.APIError:
                raise

    # -- Editorial pass ----------------------------------------------------

    def _run_editorial_pass(self, essay: str, name: str, protocol_key: str) -> StepResult:
        self._progress(name, "Running...")
        t0 = time.time()
        try:
            text, tin, tout = self._call(
                prompt=essay,
                system_instruction=self.protocols[protocol_key],
                temperature=0.3,
            )
        except Exception as e:
            self._progress(name, f"Failed: {e}")
            return StepResult(step=name, status="error", error=str(e),
                              duration_s=time.time() - t0)

        self._progress(name, f"Done ({tout} tokens out)")
        return StepResult(step=name, status="success", data=text,
                          tokens_in=tin, tokens_out=tout,
                          duration_s=time.time() - t0)

    # -- Revision pass -----------------------------------------------------

    def _run_revision(self, essay: str, editorial_results: dict[str, str]) -> StepResult:
        self._progress("revision", "Revising essay with editorial feedback...")
        t0 = time.time()

        prompt = (
            "Revise this essay incorporating the editorial feedback below.\n\n"
            f"=== ORIGINAL ESSAY ===\n{essay}\n\n"
            f"=== EDITING PASS ===\n{editorial_results['editing']}\n\n"
            f"=== GROUNDING PASS ===\n{editorial_results['grounding']}\n\n"
            f"=== AUDIT PASS ===\n{editorial_results['audit']}\n\n"
            f"=== REALITY CHECK ===\n{editorial_results['reality']}\n\n"
        )

        try:
            text, tin, tout = self._call(
                prompt=prompt,
                system_instruction=self.protocols["uke_write"],
                temperature=0.7,
            )
        except Exception as e:
            self._progress("revision", f"Failed: {e}")
            return StepResult(step="revision", status="error", error=str(e),
                              duration_s=time.time() - t0)

        self._progress("revision", f"Done ({tout} tokens out)")
        return StepResult(step="revision", status="success", data=text,
                          tokens_in=tin, tokens_out=tout,
                          duration_s=time.time() - t0)

    # -- Full run ----------------------------------------------------------

    def run(self, essay_text: str, subject: str) -> Path:
        timestamp = int(time.time())
        output_dir = REPO_ROOT / "agent" / "narrative_transform" / "uke" / f"{subject}_{timestamp}"
        output_dir.mkdir(parents=True, exist_ok=True)

        # Save source
        (output_dir / "source_essay.md").write_text(essay_text, encoding="utf-8")
        self._progress("setup", f"Output dir: {output_dir.relative_to(REPO_ROOT)}")

        steps: list[StepResult] = []
        editorial_results: dict[str, str] = {}

        # Run 4 independent editorial passes
        for name, protocol_key in EDITORIAL_PASSES:
            result = self._run_editorial_pass(essay_text, name, protocol_key)
            steps.append(result)
            if result.status == "success":
                (output_dir / f"{name}_output.md").write_text(result.data, encoding="utf-8")
                editorial_results[name] = result.data
            else:
                editorial_results[name] = f"[PASS FAILED: {result.error}]"

        # Revision pass
        result = self._run_revision(essay_text, editorial_results)
        steps.append(result)
        if result.status == "success":
            (output_dir / "revised_essay.md").write_text(result.data, encoding="utf-8")

        # Summary
        total_in = sum(s.tokens_in for s in steps)
        total_out = sum(s.tokens_out for s in steps)
        total_dur = sum(s.duration_s for s in steps)

        print(f"\n{'Step':<12} {'Status':<8} {'Tok In':>8} {'Tok Out':>8} {'Time':>7}")
        print("-" * 48)
        for s in steps:
            print(f"{s.step:<12} {s.status:<8} {s.tokens_in:>8} {s.tokens_out:>8} {s.duration_s:>6.1f}s")
        print("-" * 48)
        print(f"{'TOTAL':<12} {'':8} {total_in:>8} {total_out:>8} {total_dur:>6.1f}s")
        print(f"\nOutput: {output_dir}")

        return output_dir


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def _slugify(text: str) -> str:
    slug = re.sub(r'[^a-z0-9]+', '_', text.lower().strip())
    return slug.strip('_')[:60]


def main():
    parser = argparse.ArgumentParser(description="Editorial Analysis Pipeline")
    parser.add_argument("essay_path", nargs="?", help="Path to essay file (or use stdin)")
    parser.add_argument("--subject", "-s", help="Slug for output directory name (default: derived from filename)")
    args = parser.parse_args()

    # Read essay
    if args.essay_path:
        p = Path(args.essay_path)
        if not p.is_file():
            repo_candidate = REPO_ROOT / args.essay_path
            if repo_candidate.is_file():
                p = repo_candidate
            else:
                parser.error(f"File not found: {args.essay_path}")
        essay_text = p.read_text(encoding="utf-8")
        subject = args.subject or _slugify(p.stem)
    elif not sys.stdin.isatty():
        essay_text = sys.stdin.read()
        subject = args.subject or "stdin_essay"
    else:
        parser.error("Provide an essay file path or pipe essay text on stdin")

    pipeline = EditorialAnalysisPipeline()
    pipeline.run(essay_text, subject)


if __name__ == "__main__":
    main()
