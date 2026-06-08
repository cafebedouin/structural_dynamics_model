"""Canonical Anthropic call path — shared by the orchestrator and make_brief.

Single source of truth for: the cached client, the retry/stream wrapper, the
pause_turn continuation loop, text extraction, and the refusal/empty-completion
detection (`ModelCallError`) introduced in commit 7e85b261.

Why a separate module: the orchestrator's filename (`c-orchestrator.py`) carries
a hyphen and cannot be imported normally, but more importantly there should be
ONE call path so the refusal detection and accounting cannot fork (Build
Discipline pattern 2). Both `c-orchestrator.py` and `make_brief.py` import here.
"""

from __future__ import annotations

import time
from typing import Any

# Known input-context windows (tokens). Used by the orchestrator to MEASURE its
# ingest ceiling rather than assert a KB number. Conservative; update on model
# changes.
MODEL_CONTEXT_WINDOW = {
    "claude-sonnet-4-5-20250929": 200_000,
    "claude-haiku-4-5-20251001": 200_000,
    "claude-opus-4-5-20251101": 200_000,
}


def context_window(model: str, default: int = 200_000) -> int:
    """Input-context window (tokens) for *model*, with a safe default."""
    return MODEL_CONTEXT_WINDOW.get(model, default)


# ---------------------------------------------------------------------------
# Exceptions
# ---------------------------------------------------------------------------

class ModelCallError(RuntimeError):
    """A completion came back unusable — a safety refusal or an empty body.

    Carries the witness (`stop_reason`, `model`, any `refusal_text`) so callers
    can LOG what was refused, not merely the fact that something was. Raised by
    `call` so an empty refusal body is reported explicitly instead of surfacing
    downstream as a misleading "JSON parse failed: ... char 0".
    """

    def __init__(self, message: str, *, stop_reason: str | None = None,
                 model: str | None = None, refusal_text: str = ""):
        super().__init__(message)
        self.stop_reason = stop_reason
        self.model = model
        self.refusal_text = refusal_text


# ---------------------------------------------------------------------------
# Client
# ---------------------------------------------------------------------------

_anthropic_client = None


def get_client():
    """Return a cached Anthropic client instance (reads ANTHROPIC_API_KEY)."""
    global _anthropic_client
    if _anthropic_client is None:
        import anthropic
        _anthropic_client = anthropic.Anthropic()
    return _anthropic_client


# ---------------------------------------------------------------------------
# Response handling
# ---------------------------------------------------------------------------

def extract_text(response) -> str:
    """Pull all text blocks out of a Claude response."""
    parts = []
    for block in response.content:
        if hasattr(block, "text"):
            parts.append(block.text)
    return "\n".join(parts)


def call_with_retry(client, max_retries: int = 3, **kwargs):
    """Retry with exponential backoff on transient errors.

    Large-cap calls stream: the SDK refuses non-streaming requests whose
    max_tokens implies >10 minutes. get_final_message() returns the same Message
    object create() would, so usage accounting and the pause_turn loop are
    unaffected.
    """
    import anthropic

    for attempt in range(max_retries):
        try:
            if kwargs.get("max_tokens", 0) >= 16384:
                with client.messages.stream(**kwargs) as s:
                    return s.get_final_message()
            return client.messages.create(**kwargs)
        except (
            anthropic.RateLimitError,
            anthropic.InternalServerError,
            anthropic.APIConnectionError,
        ):
            if attempt == max_retries - 1:
                raise
            time.sleep(2 ** attempt * 2)      # 2s, 4s, 8s
        except anthropic.APIError:
            raise                              # don't retry auth / bad request


def call(prompt: str, model: str, *, system: str = "", temperature: float = 0.2,
         max_tokens: int = 8192, tools: list | None = None) -> tuple[str, int, int]:
    """Call Claude and return (text, tokens_in, tokens_out).

    Handles the pause_turn continuation loop required by server-side tools like
    web_search, and raises `ModelCallError` on a safety refusal or empty body.
    """
    client = get_client()

    kwargs: dict[str, Any] = {
        "model": model,
        "max_tokens": max_tokens,
        "temperature": temperature,
        "messages": [{"role": "user", "content": prompt}],
    }
    if system:
        kwargs["system"] = system
    if tools:
        kwargs["tools"] = tools

    total_in, total_out = 0, 0
    response = call_with_retry(client, **kwargs)
    total_in += response.usage.input_tokens
    total_out += response.usage.output_tokens

    # pause_turn continuation (web search may need multiple rounds)
    max_continuations = 5
    while response.stop_reason == "pause_turn" and max_continuations > 0:
        max_continuations -= 1
        kwargs["messages"] = [
            {"role": "user", "content": prompt},
            {"role": "assistant", "content": response.content},
        ]
        response = call_with_retry(client, **kwargs)
        total_in += response.usage.input_tokens
        total_out += response.usage.output_tokens

    text = extract_text(response)

    # An API safety refusal returns stop_reason=="refusal" with no text blocks;
    # a truncated/empty completion returns no text under any other stop_reason.
    # Either way json.loads(text) downstream throws the misleading "Expecting
    # value: line 1 column 1 (char 0)". Report the real cause here.
    # (max_tokens with partial text is left alone: text is non-empty, so the
    # caller's own parse/validation handles it.)
    if response.stop_reason == "refusal":
        raise ModelCallError(
            f"API safety refusal (stop_reason=refusal) from {model}; "
            f"no content returned.",
            stop_reason="refusal", model=model, refusal_text=text,
        )
    if not text.strip():
        raise ModelCallError(
            f"empty completion (stop_reason={response.stop_reason}) from "
            f"{model}; nothing to parse.",
            stop_reason=response.stop_reason, model=model,
        )

    return text, total_in, total_out


def count_tokens(model: str, prompt: str, *, system: str = "") -> int:
    """Token count for a prompt (+optional system) under *model*.

    Used to MEASURE ingest headroom rather than assert a byte threshold. Falls
    back to a conservative chars/4 estimate if the count_tokens endpoint is
    unavailable.
    """
    try:
        client = get_client()
        kwargs: dict[str, Any] = {
            "model": model,
            "messages": [{"role": "user", "content": prompt}],
        }
        if system:
            kwargs["system"] = system
        resp = client.messages.count_tokens(**kwargs)
        return resp.input_tokens
    except Exception:
        return (len(prompt) + len(system)) // 4
