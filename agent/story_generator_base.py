"""Shared logic for JSON constraint story generation via Gemini.

Provides prompt assembly, response processing, validation, compilation to
.pl, linting, and file saving.  The three generator scripts (RSS, URL, text)
import from here and supply only their source-specific logic.
"""

import json
import os
import random
import sys
import time
from functools import lru_cache
from pathlib import Path

# ---------------------------------------------------------------------------
# Path constants
# ---------------------------------------------------------------------------
REPO_ROOT = Path(__file__).resolve().parent.parent
PROMPT_PATH = REPO_ROOT / "prompts" / "constraint_story_generation_prompt_json.md"
SCHEMA_PATH = REPO_ROOT / "python" / "constraint_story_schema.json"
EXAMPLE_PATH = REPO_ROOT / "json" / "antifragility.json"
JSON_DIR = REPO_ROOT / "json"
TESTSETS_DIR = REPO_ROOT / "prolog" / "testsets"
PROLOG_DIR = REPO_ROOT / "prolog"

# ---------------------------------------------------------------------------
# Imports from python/ (sibling package)
# ---------------------------------------------------------------------------
sys.path.insert(0, str(REPO_ROOT / "python"))
from generate_constraint_pl import validate_json, generate_pl  # noqa: E402
from linter import lint_file  # noqa: E402

# ---------------------------------------------------------------------------
# Exclusion patterns (shared by RSS and URL generators)
# ---------------------------------------------------------------------------
EXCLUSION_PATTERNS = [
    r'https?://www\.newsminimalist\.com',
    r'https?://(www\.)?beehiv\.com',
    r'https?://(www\.)?beehiiv\.com',
    r'/image/',
    r'numlock\.com',
    r'substack\.com',
    r'twitter\.com',
    r'facebook\.com',
    r'linkedin\.com',
    r'adobe\.com',
    r'theverge\.com',
    r'techcrunch\.com',
    r'gizmodo\.com',
    r'9to5mac\.com',
    r'arstechnica\.com',
]

# ---------------------------------------------------------------------------
# Default model
# ---------------------------------------------------------------------------
DEFAULT_MODEL = os.environ.get("GEMINI_MODEL", "gemini-2.5-pro")

# ---------------------------------------------------------------------------
# System instruction (from regenerate_stories.py)
# ---------------------------------------------------------------------------
_SYSTEM_INSTRUCTION = (
    "You are a constraint story generator for the Deferential Realism "
    "indexical classification system. You will produce a JSON representation "
    "of a constraint story conforming to the provided schema. Output ONLY "
    "valid JSON — no markdown fences, no commentary outside the JSON."
)


# ---------------------------------------------------------------------------
# Context file loader
# ---------------------------------------------------------------------------
@lru_cache(maxsize=None)
def _load_context_file(path):
    """Read and cache a context file (prompt, schema, example)."""
    return Path(path).read_text(encoding="utf-8")


# ---------------------------------------------------------------------------
# Gemini client (lazy singleton)
# ---------------------------------------------------------------------------
_client = None


def _get_client():
    """Lazily initialize and return the Gemini client."""
    global _client
    if _client is None:
        from google import genai
        api_key = os.environ.get("GOOGLE_API_KEY") or os.environ.get("GEMINI_API_KEY")
        if api_key:
            _client = genai.Client(api_key=api_key)
        else:
            _client = genai.Client()
    return _client


# ---------------------------------------------------------------------------
# Retry with backoff
# ---------------------------------------------------------------------------
def retry_with_backoff(func, *args, max_retries=5, **kwargs):
    """Exponential backoff for API 429/503 errors."""
    from google.api_core import exceptions
    for i in range(max_retries):
        try:
            return func(*args, **kwargs)
        except (exceptions.ResourceExhausted, exceptions.ServiceUnavailable) as e:
            if i == max_retries - 1:
                raise e
            wait_time = (2 ** i) + random.random()
            print(f"Rate limited. Retrying in {wait_time:.2f}s...")
            time.sleep(wait_time)
    return None


# ---------------------------------------------------------------------------
# Text processing
# ---------------------------------------------------------------------------
def strip_json_fences(text):
    """Remove ```json ... ``` wrapping if present."""
    text = text.strip()
    if text.startswith("```json"):
        text = text[len("```json"):].strip()
    elif text.startswith("```"):
        text = text[3:].strip()
    if text.endswith("```"):
        text = text[:-3].strip()
    return text


# ---------------------------------------------------------------------------
# Prompt builder
# ---------------------------------------------------------------------------
def build_prompt(source_description, context_text=""):
    """Assemble the full generation prompt with schema and example context.

    *source_description* is e.g. ``"URL: https://..."`` or ``"TOPIC: AI safety"``.
    *context_text* is optional extra context (original content, error feedback).
    """
    prompt_text = _load_context_file(PROMPT_PATH)
    schema_text = _load_context_file(SCHEMA_PATH)
    example_text = _load_context_file(EXAMPLE_PATH)

    parts = [
        "=== GENERATION PROMPT ===\n",
        prompt_text,
        "\n\n=== JSON SCHEMA ===\n",
        schema_text,
        "\n\n=== EXAMPLE JSON ===\n",
        example_text,
        "\n\n=== YOUR TASK ===\n",
        f"Generate a complete constraint story JSON for: {source_description}\n",
        "Follow the schema exactly. Output ONLY valid JSON — no markdown fences, no commentary.\n",
    ]
    if context_text:
        parts.append("\n")
        parts.append(context_text)
    return "".join(parts)


# ---------------------------------------------------------------------------
# Response processing
# ---------------------------------------------------------------------------
def process_response(raw_text):
    """Strip fences, parse JSON, validate against schema.

    Returns ``(story_dict, errors)`` where *errors* is a list of strings
    (empty means valid).  On JSON parse failure, returns ``(None, [error])``.
    """
    json_text = strip_json_fences(raw_text)
    try:
        story_dict = json.loads(json_text)
    except json.JSONDecodeError as e:
        return None, [f"JSON_PARSE_ERROR: {e}"]

    errors = validate_json(story_dict)
    return story_dict, errors


# ---------------------------------------------------------------------------
# Save story (JSON + compiled .pl)
# ---------------------------------------------------------------------------
def save_story(story_dict, overwrite=False):
    """Compile *story_dict* to .pl, lint, and write both files.

    Returns ``(json_path, pl_path)`` on success, ``(None, None)`` on skip/error.
    """
    constraint_id = story_dict["header"]["constraint_id"]
    json_path = JSON_DIR / f"{constraint_id}.json"
    pl_path = TESTSETS_DIR / f"{constraint_id}.pl"

    if not overwrite and (json_path.exists() or pl_path.exists()):
        print(f"  Skipping {constraint_id} — file already exists (use overwrite=True to replace)")
        return None, None

    # Compile JSON → Prolog
    pl_content = generate_pl(story_dict)

    # Safety-net lint via temp file in testsets/ (linter resolves config.pl
    # via dirname(dirname(filepath)), so file must be inside prolog/testsets/)
    tmp_path = TESTSETS_DIR / f".tmp_{constraint_id}.pl"
    try:
        tmp_path.write_text(pl_content, encoding="utf-8")
        lint_errors = lint_file(str(tmp_path))
        if lint_errors:
            print(f"  Lint warnings for {constraint_id} (non-blocking):")
            for err in lint_errors[:5]:
                print(f"    - {err}")
    except Exception as e:
        print(f"  Linter crashed for {constraint_id}: {e}")
    finally:
        tmp_path.unlink(missing_ok=True)

    # Ensure output directories exist
    JSON_DIR.mkdir(parents=True, exist_ok=True)
    TESTSETS_DIR.mkdir(parents=True, exist_ok=True)

    # Write .json
    json_path.write_text(
        json.dumps(story_dict, indent=2) + "\n", encoding="utf-8"
    )

    # Write .pl
    pl_path.write_text(pl_content, encoding="utf-8")

    print(f"  Saved: {json_path.name} + {pl_path.name}")
    return json_path, pl_path


# ---------------------------------------------------------------------------
# Processed-log helpers
# ---------------------------------------------------------------------------
def load_processed_log(log_path):
    """Read a processed-items log file into a set of strings."""
    try:
        return set(Path(log_path).read_text(encoding="utf-8").splitlines())
    except FileNotFoundError:
        return set()


def append_to_log(log_path, entry):
    """Append a single line to a processed-items log file."""
    with open(log_path, "a", encoding="utf-8") as f:
        f.write(entry + "\n")


# ---------------------------------------------------------------------------
# Full single-story generation flow
# ---------------------------------------------------------------------------
def generate_story(source_description, processed_log_path, log_key,
                   context_text="", model=None, max_retries=2,
                   overwrite=False):
    """Generate one constraint story end-to-end.

    1. Build prompt
    2. Call Gemini
    3. Process response (parse + validate)
    4. On validation errors, retry with error feedback
    5. Save JSON + .pl
    6. Log success

    Returns ``True`` on success, ``False`` on failure.
    """
    model = model or DEFAULT_MODEL
    client = _get_client()
    retry_errors = None

    for attempt in range(1, max_retries + 1):
        label = f"(attempt {attempt}/{max_retries})"

        # Build prompt, including error feedback on retries
        extra = context_text
        if retry_errors:
            feedback = "\nYour previous attempt had these validation errors:\n"
            for err in retry_errors:
                feedback += f"  - {err}\n"
            feedback += "Fix these specific errors while keeping the rest correct.\n"
            extra = (context_text + "\n" + feedback) if context_text else feedback

        prompt = build_prompt(source_description, extra)

        # API call with rate-limit backoff
        print(f"  Calling Gemini {model} {label}...")
        try:
            response = retry_with_backoff(
                client.models.generate_content,
                model=model,
                contents=prompt,
                config={"system_instruction": _SYSTEM_INSTRUCTION},
            )
        except Exception as e:
            print(f"  API error {label}: {e}")
            retry_errors = [f"API_ERROR: {e}"]
            continue

        if not response or not response.text:
            print(f"  Empty response {label}")
            retry_errors = ["EMPTY_RESPONSE"]
            continue

        # Process response
        story_dict, errors = process_response(response.text)

        if story_dict is None:
            print(f"  JSON parse failed {label}: {errors[0]}")
            retry_errors = errors
            continue

        if errors:
            print(f"  Validation errors {label}: {len(errors)} error(s)")
            for err in errors[:3]:
                print(f"    - {err}")
            retry_errors = errors
            continue

        # Validation passed — save
        json_path, pl_path = save_story(story_dict, overwrite=overwrite)
        if json_path is None:
            return False

        # Log success
        append_to_log(processed_log_path, log_key)
        return True

    print(f"  Failed after {max_retries} attempts for: {source_description}")
    return False
