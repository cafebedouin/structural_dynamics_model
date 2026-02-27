#!/usr/bin/env python3
"""Batch regeneration of constraint stories via Gemini.

Reads failing source files, sends them to Gemini with the generation prompt,
JSON schema, and example as context, validates JSON output against the schema,
compiles to .pl via generate_constraint_pl.py, and routes results to
testsets/ (pass) or probsets/ (fail).

Supports two modes:
  --batch       (default) Submit all items via the Gemini batch API.
                Failures are re-submitted with error feedback for up to
                MAX_RETRIES rounds.  Context caching is attempted.
  --sequential  Original one-at-a-time mode with per-item retries.

Usage:
    python python/regenerate_stories.py --dry-run
    python python/regenerate_stories.py --files child_marriage
    python python/regenerate_stories.py --group A --limit 5
    python python/regenerate_stories.py --source lint --limit 10
    python python/regenerate_stories.py --source json-validate --dry-run
    python python/regenerate_stories.py --sequential --files child_marriage
"""

import argparse
import json
import logging
import os
import re
import sys
import time
import random
from datetime import datetime, timedelta
from pathlib import Path

# ---------------------------------------------------------------------------
# Paths
# ---------------------------------------------------------------------------
SCRIPT_DIR = Path(__file__).resolve().parent
ROOT_DIR = SCRIPT_DIR.parent
ORIGSETS_DIR = ROOT_DIR / "prolog" / "origsets"
TESTSETS_DIR = ROOT_DIR / "prolog" / "testsets"
PROBSETS_DIR = ROOT_DIR / "prolog" / "probsets"
JSON_TESTSETS_DIR = ROOT_DIR / "json"
PROMPT_PATH = ROOT_DIR / "prompts" / "constraint_story_generation_prompt.md"
SCHEMA_PATH = SCRIPT_DIR / "constraint_story_schema.json"
EXAMPLE_JSON_PATH = ROOT_DIR / "agent" / "verification_bottleneck.json"
REGEN_LIST_PATH = ROOT_DIR / "outputs" / "regeneration_list.md"
LINT_ERRORS_PATH = ROOT_DIR / "outputs" / "lint_errors.txt"
OUTPUTS_DIR = ROOT_DIR / "outputs"

# ---------------------------------------------------------------------------
# Imports from sibling modules
# ---------------------------------------------------------------------------
sys.path.insert(0, str(SCRIPT_DIR))
from linter import lint_file
from duplicate_checker import check_incoming_file
from generate_constraint_pl import validate_json, generate_pl

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------
MODEL = "gemini-2.0-flash"
TEMPERATURE = 0.3
MAX_RETRIES = 3
BACKOFF_BASE = 2.0
BACKOFF_MAX_RETRIES = 5
CACHE_TTL_SECONDS = 3600  # 1 hour
CACHE_REFRESH_MARGIN = 300  # refresh when < 5 min remaining
BATCH_POLL_INTERVAL = 30  # seconds

# ---------------------------------------------------------------------------
# Logging setup
# ---------------------------------------------------------------------------
def setup_logging():
    ts = datetime.now().strftime("%Y%m%d_%H%M%S")
    log_path = OUTPUTS_DIR / f"regen_{ts}.log"
    fmt = "%(asctime)s %(levelname)-8s %(message)s"
    handlers = [
        logging.StreamHandler(sys.stdout),
        logging.FileHandler(log_path, encoding="utf-8"),
    ]
    logging.basicConfig(level=logging.INFO, format=fmt, handlers=handlers)
    logging.info("Log file: %s", log_path)
    return log_path


# =========================================================================
# Linter pre-run
# =========================================================================
def run_linter():
    """Run structural linter on testsets/ and origsets/, write lint_errors.txt."""
    logging.info("Running structural linter on testsets/ and origsets/…")
    lint_pass = 0
    lint_fail = 0

    with open(LINT_ERRORS_PATH, "w", encoding="utf-8") as f:
        for directory in [TESTSETS_DIR, ORIGSETS_DIR]:
            if not directory.exists():
                continue
            dir_name = directory.name
            for pl_file in sorted(directory.glob("*.pl")):
                errors = lint_file(str(pl_file))
                if errors:
                    lint_fail += 1
                    f.write(f"{dir_name}/{pl_file.name}:\n")
                    for e in errors:
                        f.write(f"  {e}\n")
                    f.write("\n")
                else:
                    lint_pass += 1

    total = lint_pass + lint_fail
    logging.info("Linter: %d/%d passed, %d with errors → %s",
                 lint_pass, total, lint_fail, LINT_ERRORS_PATH)
    return lint_fail


# =========================================================================
# Work-list parsers
# =========================================================================
def parse_regen_list(group_filter=None):
    """Parse outputs/regeneration_list.md → list of (basename, [errors])."""
    content = REGEN_LIST_PATH.read_text(encoding="utf-8")
    results = []
    current_group = None
    in_code_block = False

    for line in content.splitlines():
        # Detect group headers
        gm = re.match(r"^## Group ([A-E]):", line)
        if gm:
            current_group = gm.group(1)
            continue
        if line.strip() == "```":
            in_code_block = not in_code_block
            continue
        if not in_code_block or not line.strip():
            continue
        if group_filter and current_group != group_filter:
            continue

        # Lines like: "basename  [ERR1, ERR2]" or just "basename"
        m = re.match(r"^(\S+)\s*(?:\[(.+)\])?$", line.strip())
        if m:
            basename = m.group(1)
            errors = [e.strip() for e in m.group(2).split(",")] if m.group(2) else []
            results.append((basename, None, errors))
    return results


def parse_lint_errors():
    """Parse outputs/lint_errors.txt → list of (basename, source_dir, [errors]).

    Handles both dir-prefixed entries (``testsets/foo.pl:``) produced by
    ``run_linter()`` and bare entries (``foo.pl:``) for backward compat.

    Skips files whose only issue is WARNING_MISSING_CLAIM (advisory only).
    """
    content = LINT_ERRORS_PATH.read_text(encoding="utf-8")
    results = []
    current_file = None
    current_dir = None
    current_errors = []

    for line in content.splitlines():
        # Match dir-prefixed "testsets/foo.pl:" or bare "foo.pl:"
        fm = re.match(r"^(?:(testsets|origsets)/)?(\S+\.pl):$", line)
        if fm:
            # Flush previous
            if current_file and current_errors:
                results.append((current_file, current_dir, current_errors))
            current_dir = fm.group(1)  # None for bare filenames
            current_file = fm.group(2).replace(".pl", "")
            current_errors = []
            continue
        stripped = line.strip()
        if stripped and current_file:
            current_errors.append(stripped)

    # Flush last
    if current_file and current_errors:
        results.append((current_file, current_dir, current_errors))
    return results


def parse_json_validate():
    """Scan testsets for .json files and validate against schema.

    Returns list of (basename, source_dir_name, [errors]) for invalid files.
    """
    results = []
    for directory in [TESTSETS_DIR, JSON_TESTSETS_DIR]:
        if not directory.exists():
            continue
        dir_name = directory.name
        for json_file in sorted(directory.glob("*.json")):
            try:
                data = json.loads(json_file.read_text(encoding="utf-8"))
            except json.JSONDecodeError as e:
                results.append((json_file.stem, dir_name, [f"JSON_PARSE_ERROR: {e}"]))
                continue
            errors = validate_json(data)
            if errors:
                results.append((json_file.stem, dir_name, errors))
    return results


# =========================================================================
# Gemini client wrapper
# =========================================================================
class GeminiRegenClient:
    def __init__(self):
        from google import genai
        from google.genai import types

        api_key = os.environ.get("GOOGLE_API_KEY") or os.environ.get("GEMINI_API_KEY")
        if not api_key:
            raise RuntimeError(
                "Set GOOGLE_API_KEY or GEMINI_API_KEY environment variable."
            )

        self.client = genai.Client(api_key=api_key)
        self.types = types
        self._cache = None
        self._cache_expires = None

        # Pre-load context files
        self.prompt_text = PROMPT_PATH.read_text(encoding="utf-8")
        self.schema_text = SCHEMA_PATH.read_text(encoding="utf-8")
        self.example_json_text = EXAMPLE_JSON_PATH.read_text(encoding="utf-8")

        self._system_instruction = (
            "You are a constraint story generator for the Deferential Realism "
            "indexical classification system. You will produce a JSON representation "
            "of a constraint story conforming to the provided schema. Output ONLY "
            "valid JSON — no markdown fences, no commentary outside the JSON."
        )

        self._static_content = (
            "=== GENERATION PROMPT ===\n"
            + self.prompt_text
            + "\n\n=== JSON SCHEMA ===\n"
            + self.schema_text
            + "\n\n=== EXAMPLE JSON ===\n"
            + self.example_json_text
        )

    # ----- context caching -----
    def _ensure_cache(self):
        """Create or refresh the cached context."""
        now = datetime.utcnow()
        if self._cache and self._cache_expires and now < self._cache_expires - timedelta(seconds=CACHE_REFRESH_MARGIN):
            return  # cache still valid

        logging.info("Creating/refreshing Gemini context cache (TTL=%ds)…", CACHE_TTL_SECONDS)
        try:
            cached_content = self.client.caches.create(
                model=MODEL,
                config=self.types.CreateCachedContentConfig(
                    display_name="regen-context",
                    system_instruction=self._system_instruction,
                    contents=[
                        self.types.Content(
                            role="user",
                            parts=[
                                self.types.Part(text=self._static_content),
                            ],
                        ),
                    ],
                    ttl=f"{CACHE_TTL_SECONDS}s",
                ),
            )
            self._cache = cached_content
            self._cache_expires = now + timedelta(seconds=CACHE_TTL_SECONDS)
            logging.info("Cache created: %s (expires ~%s)", cached_content.name, self._cache_expires.isoformat())
        except Exception as e:
            logging.warning("Cache creation failed (%s), falling back to non-cached mode.", e)
            self._cache = None
            self._cache_expires = None

    @property
    def cache_name(self):
        """Return the current cache name, or None."""
        self._ensure_cache()
        return self._cache.name if self._cache else None

    def cleanup_cache(self):
        """Best-effort cleanup of the context cache."""
        if self._cache:
            try:
                self.client.caches.delete(name=self._cache.name)
                logging.info("Cache deleted: %s", self._cache.name)
            except Exception as e:
                logging.warning("Cache cleanup failed (will auto-expire): %s", e)
            self._cache = None
            self._cache_expires = None

    def generate(self, user_prompt):
        """Send a generation request, using cache if available.

        Returns the response text.
        """
        self._ensure_cache()

        config_kwargs = dict(
            temperature=TEMPERATURE,
            max_output_tokens=65536,
        )

        if self._cache:
            config_kwargs["cached_content"] = self._cache.name
        else:
            config_kwargs["system_instruction"] = self._system_instruction

        if self._cache:
            # Use cached context — user_prompt only
            contents = user_prompt
        else:
            # No cache — inline the context
            contents = (
                self._static_content
                + "\n\n=== USER REQUEST ===\n"
                + user_prompt
            )

        config = self.types.GenerateContentConfig(**config_kwargs)

        for attempt in range(BACKOFF_MAX_RETRIES + 1):
            try:
                response = self.client.models.generate_content(
                    model=MODEL,
                    contents=contents,
                    config=config,
                )
                return self._extract_text(response)
            except Exception as e:
                if self._is_rate_limit(e) and attempt < BACKOFF_MAX_RETRIES:
                    self._backoff(attempt, e)
                else:
                    raise

    @staticmethod
    def _extract_text(response):
        text = response.text
        if text is None:
            # Safety filter, empty response, or blocked content
            reason = ""
            if hasattr(response, "candidates") and response.candidates:
                c = response.candidates[0]
                if hasattr(c, "finish_reason"):
                    reason = f" (finish_reason={c.finish_reason})"
            raise ValueError(f"Gemini returned empty response{reason}")
        return text

    @staticmethod
    def _is_rate_limit(exc):
        msg = str(exc).lower()
        return "429" in msg or "rate" in msg or "resource_exhausted" in msg

    @staticmethod
    def _backoff(attempt, exc):
        delay = BACKOFF_BASE * (2 ** attempt) + random.uniform(0, 1)
        logging.warning("Rate-limited (attempt %d): %s — backing off %.1fs", attempt + 1, exc, delay)
        time.sleep(delay)


# =========================================================================
# Core regeneration logic (shared)
# =========================================================================
def strip_markdown_fences(text):
    """Remove markdown code fences from Gemini output."""
    # Remove opening fence (```prolog, ```pl, ```)
    text = re.sub(r"^```(?:prolog|pl)?\s*\n", "", text, count=1)
    # Remove closing fence
    text = re.sub(r"\n```\s*$", "", text, count=1)
    return text.strip()


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


def lint_temp(basename, content):
    """Write content to a temp file in testsets/ and run linter.

    The linter resolves config.pl via os.path.dirname(os.path.dirname(filepath)),
    so the temp file must be inside a testsets-like directory structure.

    Returns (errors, temp_path).
    """
    tmp_path = TESTSETS_DIR / f".tmp_{basename}.pl"
    tmp_path.write_text(content, encoding="utf-8")
    try:
        errors = lint_file(str(tmp_path))
    except Exception as e:
        errors = [f"LINTER_CRASH: {e}"]
    return errors, tmp_path


def build_user_prompt(basename, original_content, known_errors, retry_errors=None):
    """Build the prompt sent to Gemini for (re)generation."""
    parts = [
        f"Produce the JSON representation of this constraint story: {basename}\n",
        "Follow the schema exactly. Output ONLY valid JSON — no markdown fences, no commentary.\n",
    ]
    if known_errors:
        parts.append("Known errors in the original:\n")
        for e in known_errors:
            parts.append(f"  - {e}\n")
    if retry_errors:
        parts.append("\nYour previous attempt had these validation errors:\n")
        for e in retry_errors:
            parts.append(f"  - {e}\n")
        parts.append("\nFix these specific errors while keeping the rest correct.\n")
    parts.append("\n=== ORIGINAL CONTENT ===\n")
    parts.append(original_content)
    return "".join(parts)


def find_source_file(basename, source_dir=None):
    """Locate the source file (.json preferred over .pl).

    If *source_dir* is given (``"testsets"`` or ``"origsets"``), look there
    first.  Otherwise fall back to testsets/ then origsets/.
    Prefers .json over .pl when both exist.
    """
    dirs_map = {
        "testsets": TESTSETS_DIR,
        "origsets": ORIGSETS_DIR,
    }
    # Build ordered list of directories to search (no duplicates)
    search_dirs = []
    if source_dir and source_dir in dirs_map:
        search_dirs.append(dirs_map[source_dir])
    for d in [TESTSETS_DIR, JSON_TESTSETS_DIR, ORIGSETS_DIR]:
        if d not in search_dirs:
            search_dirs.append(d)

    # Check .json first across all dirs, then .pl
    for ext in [".json", ".pl"]:
        for d in search_dirs:
            path = d / f"{basename}{ext}"
            if path.exists():
                return path
    return None


def validate_and_save(basename, raw_text, source_path, stats):
    """Validate a raw Gemini response text and save on success.

    Returns ``(True, None)`` on success, ``(False, errors)`` on validation
    failure (eligible for retry), or ``(False, None)`` on hard failure
    (duplicate — not retriable).  Also stores the last parsed ``story_dict``
    on the returned errors list as ``errors.story_dict`` attribute.
    """
    json_text = strip_json_fences(raw_text)

    # Parse JSON
    try:
        story_dict = json.loads(json_text)
    except json.JSONDecodeError as e:
        errors = [f"JSON_PARSE_ERROR: {e}"]
        errors.story_dict = None  # type: ignore[attr-defined]
        return False, errors

    # Validate against schema
    validation_errors = validate_json(story_dict)
    if validation_errors:
        validation_errors.story_dict = story_dict  # type: ignore[attr-defined]
        return False, validation_errors

    # Compile to .pl
    pl_content = generate_pl(story_dict)

    # Safety-net lint
    lint_errors, tmp_path = lint_temp(basename, pl_content)
    if lint_errors:
        logging.warning("  LINT_WARNING %s — %s", basename, "; ".join(lint_errors[:3]))

    # Duplicate check
    dupes = check_incoming_file(str(TESTSETS_DIR), str(tmp_path))
    tmp_path.unlink(missing_ok=True)
    if dupes:
        logging.warning("  DUPLICATE %s — %s", basename, "; ".join(dupes))
        stats["failed"] += 1
        return False, None  # hard fail, no retry

    # Save
    dest_dir = source_path.parent
    json_dest = dest_dir / f"{basename}.json"
    pl_dest = dest_dir / f"{basename}.pl"
    json_dest.write_text(json.dumps(story_dict, indent=2) + "\n", encoding="utf-8")
    pl_dest.write_text(pl_content, encoding="utf-8")
    return True, None


# =========================================================================
# Sequential mode (original behavior)
# =========================================================================
def process_file(client, basename, source_dir, known_errors, stats):
    """Regenerate a single file as JSON, validate, compile to .pl."""
    source_path = find_source_file(basename, source_dir)
    if source_path is None:
        logging.warning("[SKIP] %s — not found in testsets/ or origsets/", basename)
        stats["skipped"] += 1
        return False

    original_content = source_path.read_text(encoding="utf-8")
    retry_errors = None
    story_dict = None  # last successfully parsed JSON

    for attempt in range(1, MAX_RETRIES + 1):
        label = f"(attempt {attempt}/{MAX_RETRIES})"
        logging.info("  Generating %s %s…", basename, label)

        prompt = build_user_prompt(basename, original_content, known_errors, retry_errors)

        try:
            raw = client.generate(prompt)
        except Exception as e:
            logging.warning("  API/empty response for %s %s: %s", basename, label, e)
            retry_errors = [f"EMPTY_RESPONSE: {e}"]
            continue

        json_text = strip_json_fences(raw)

        # Step 1: Parse JSON
        try:
            story_dict = json.loads(json_text)
        except json.JSONDecodeError as e:
            logging.info("  JSON_PARSE_ERROR %s %s — %s", basename, label, e)
            retry_errors = [f"JSON_PARSE_ERROR: {e}"]
            story_dict = None
            continue

        # Step 2: Validate against schema
        validation_errors = validate_json(story_dict)
        if validation_errors:
            logging.info("  SCHEMA_ERROR %s %s — %d error(s): %s",
                         basename, label, len(validation_errors),
                         "; ".join(validation_errors[:3]))
            retry_errors = validation_errors
            continue

        # Step 3: Compile to .pl
        pl_content = generate_pl(story_dict)

        # Step 4: Lint on compiled .pl — blocking, triggers retry
        lint_errors, tmp_path = lint_temp(basename, pl_content)
        if lint_errors:
            logging.info("  LINT_ERROR %s %s — %d error(s): %s",
                         basename, label, len(lint_errors),
                         "; ".join(lint_errors[:3]))
            tmp_path.unlink(missing_ok=True)
            retry_errors = lint_errors
            continue

        # Step 5: Duplicate check
        dupes = check_incoming_file(str(TESTSETS_DIR), str(tmp_path))
        tmp_path.unlink(missing_ok=True)
        if dupes:
            logging.warning("  DUPLICATE %s — %s", basename, "; ".join(dupes))
            stats["failed"] += 1
            return False

        # Step 6: Pass — write both .json and .pl to source directory
        dest_dir = source_path.parent
        json_dest = dest_dir / f"{basename}.json"
        pl_dest = dest_dir / f"{basename}.pl"
        json_dest.write_text(json.dumps(story_dict, indent=2) + "\n", encoding="utf-8")
        pl_dest.write_text(pl_content, encoding="utf-8")
        logging.info("  PASS %s on attempt %d → %s", basename, attempt, dest_dir.name)
        stats["passed"] += 1
        return True

    # All retries exhausted
    if story_dict is not None:
        # JSON parsed but had validation errors — write to probsets
        PROBSETS_DIR.mkdir(parents=True, exist_ok=True)
        json_dest = PROBSETS_DIR / f"{basename}.json"
        json_dest.write_text(json.dumps(story_dict, indent=2) + "\n", encoding="utf-8")
        try:
            pl_content = generate_pl(story_dict)
            pl_dest = PROBSETS_DIR / f"{basename}.pl"
            pl_dest.write_text(pl_content, encoding="utf-8")
        except Exception:
            pass
        logging.info("  PROBSET %s — wrote to probsets/ after %d attempts", basename, MAX_RETRIES)
    else:
        logging.error("  FAILED %s — no parseable JSON after %d attempts", basename, MAX_RETRIES)
    stats["failed"] += 1
    return False


def run_sequential(work_list, stats):
    """Process work list one item at a time (original behavior)."""
    client = GeminiRegenClient()
    total = len(work_list)
    start_time = time.time()

    for i, (basename, source_dir, known_errors) in enumerate(work_list, 1):
        elapsed = time.time() - start_time
        rate = (i - 1) / (elapsed / 60) if elapsed > 0 and i > 1 else 0
        remaining = total - i + 1
        eta = remaining / rate if rate > 0 else 0
        err_count = len(known_errors) if known_errors else "?"

        logging.info(
            "[%d/%d] %s (%s errors) [%.1f/min, ETA %.0fmin]",
            i, total, basename, err_count, rate, eta,
        )
        process_file(client, basename, source_dir, known_errors, stats)


# =========================================================================
# Batch mode
# =========================================================================
def poll_batch(client, batch_name, poll_interval=BATCH_POLL_INTERVAL):
    """Poll a batch job until it reaches a terminal state."""
    terminal_states = {
        "JOB_STATE_SUCCEEDED", "JOB_STATE_FAILED",
        "JOB_STATE_CANCELLED", "JOB_STATE_EXPIRED",
        "JOB_STATE_PARTIALLY_SUCCEEDED",
    }
    while True:
        batch = client.batches.get(name=batch_name)
        state = batch.state.name if batch.state else "UNKNOWN"
        st = batch.completion_stats
        if st:
            logging.info(
                "  Batch: %s — succeeded=%s, failed=%s",
                state, st.successful_count, st.failed_count,
            )
        else:
            logging.info("  Batch: %s", state)
        if state in terminal_states:
            return batch
        time.sleep(poll_interval)


def run_batch(work_list, stats, poll_interval=BATCH_POLL_INTERVAL):
    """Process work list via the Gemini batch API with multi-round retries."""
    client = GeminiRegenClient()

    # --- Phase 0: Resolve source files and read content ---
    items = []
    for basename, source_dir, known_errors in work_list:
        source_path = find_source_file(basename, source_dir)
        if source_path is None:
            logging.warning("[SKIP] %s — not found in testsets/ or origsets/", basename)
            stats["skipped"] += 1
            continue
        original_content = source_path.read_text(encoding="utf-8")
        items.append({
            "basename": basename,
            "source_path": source_path,
            "known_errors": known_errors,
            "original_content": original_content,
            "retry_errors": None,
            "last_story_dict": None,
        })

    if not items:
        logging.info("No items to process after source resolution.")
        return

    # --- Retry rounds ---
    try:
        for attempt in range(1, MAX_RETRIES + 1):
            if not items:
                break

            logging.info(
                "Batch round %d/%d: %d item(s)", attempt, MAX_RETRIES, len(items),
            )

            # Build requests
            cache_name = client.cache_name
            requests = []
            for item in items:
                prompt = build_user_prompt(
                    item["basename"],
                    item["original_content"],
                    item["known_errors"],
                    item["retry_errors"],
                )

                if cache_name:
                    contents = [{"role": "user", "parts": [{"text": prompt}]}]
                    config = {
                        "cached_content": cache_name,
                        "temperature": TEMPERATURE,
                        "max_output_tokens": 65536,
                    }
                else:
                    full_prompt = (
                        client._static_content
                        + "\n\n=== USER REQUEST ===\n"
                        + prompt
                    )
                    contents = [{"role": "user", "parts": [{"text": full_prompt}]}]
                    config = {
                        "system_instruction": client._system_instruction,
                        "temperature": TEMPERATURE,
                        "max_output_tokens": 65536,
                    }

                requests.append({
                    "contents": contents,
                    "metadata": {"key": item["basename"]},
                    "config": config,
                })

            # Submit batch
            logging.info("Submitting batch of %d requests…", len(requests))
            batch = client.client.batches.create(model=MODEL, src=requests)
            logging.info("Batch created: %s", batch.name)

            # Poll
            batch = poll_batch(client.client, batch.name, poll_interval)

            # Process results
            if not batch.dest or not batch.dest.inlined_responses:
                logging.error("  No inlined responses in batch result")
                break

            # Index items by basename for lookup
            items_by_name = {it["basename"]: it for it in items}
            retry_items = []

            for resp in batch.dest.inlined_responses:
                basename = (resp.metadata or {}).get("key", "unknown")
                item = items_by_name.get(basename)
                if item is None:
                    logging.warning("  Response for unknown key: %s", basename)
                    continue

                # API-level error
                if resp.error:
                    logging.info("  FAIL %s: API error — %s", basename, resp.error)
                    item["retry_errors"] = [f"API_ERROR: {resp.error}"]
                    retry_items.append(item)
                    continue

                if not resp.response or not resp.response.text:
                    logging.info("  FAIL %s: empty response", basename)
                    item["retry_errors"] = ["EMPTY_RESPONSE"]
                    retry_items.append(item)
                    continue

                raw_text = resp.response.text
                json_text = strip_json_fences(raw_text)

                # Parse JSON
                try:
                    story_dict = json.loads(json_text)
                    item["last_story_dict"] = story_dict
                except json.JSONDecodeError as e:
                    logging.info("  FAIL %s: JSON parse error — %s", basename, e)
                    item["retry_errors"] = [f"JSON_PARSE_ERROR: {e}"]
                    retry_items.append(item)
                    continue

                # Validate
                validation_errors = validate_json(story_dict)
                if validation_errors:
                    logging.info(
                        "  FAIL %s: %d validation error(s): %s",
                        basename, len(validation_errors),
                        "; ".join(validation_errors[:3]),
                    )
                    item["retry_errors"] = validation_errors
                    retry_items.append(item)
                    continue

                # Compile, lint (blocking), duplicate check, save
                pl_content = generate_pl(story_dict)
                lint_errors, tmp_path = lint_temp(basename, pl_content)
                if lint_errors:
                    tmp_path.unlink(missing_ok=True)
                    logging.info(
                        "  LINT_ERROR %s: %d error(s): %s",
                        basename, len(lint_errors),
                        "; ".join(lint_errors[:3]),
                    )
                    item["retry_errors"] = lint_errors
                    retry_items.append(item)
                    continue
                dupes = check_incoming_file(str(TESTSETS_DIR), str(tmp_path))
                tmp_path.unlink(missing_ok=True)
                if dupes:
                    logging.warning("  DUPLICATE %s — %s", basename, "; ".join(dupes))
                    stats["failed"] += 1
                    continue  # hard fail, no retry

                # Save
                dest_dir = item["source_path"].parent
                json_dest = dest_dir / f"{basename}.json"
                pl_dest = dest_dir / f"{basename}.pl"
                json_dest.write_text(
                    json.dumps(story_dict, indent=2) + "\n", encoding="utf-8",
                )
                pl_dest.write_text(pl_content, encoding="utf-8")
                logging.info("  PASS %s on round %d → %s", basename, attempt, dest_dir.name)
                stats["passed"] += 1

            items = retry_items

        # --- Exhausted retries: route remaining to probsets or fail ---
        for item in items:
            basename = item["basename"]
            story_dict = item.get("last_story_dict")
            if story_dict is not None:
                PROBSETS_DIR.mkdir(parents=True, exist_ok=True)
                json_dest = PROBSETS_DIR / f"{basename}.json"
                json_dest.write_text(
                    json.dumps(story_dict, indent=2) + "\n", encoding="utf-8",
                )
                try:
                    pl_content = generate_pl(story_dict)
                    pl_dest = PROBSETS_DIR / f"{basename}.pl"
                    pl_dest.write_text(pl_content, encoding="utf-8")
                except Exception:
                    pass
                logging.info(
                    "  PROBSET %s — wrote to probsets/ after %d rounds", basename, MAX_RETRIES,
                )
            else:
                logging.error(
                    "  FAILED %s — no parseable JSON after %d rounds", basename, MAX_RETRIES,
                )
            stats["failed"] += 1

    finally:
        client.cleanup_cache()


# =========================================================================
# Main
# =========================================================================
def main():
    parser = argparse.ArgumentParser(
        description="Batch regeneration of constraint stories via Gemini."
    )
    parser.add_argument(
        "--source", choices=["regen", "lint", "json-validate"], default="regen",
        help="Work list source: 'regen' (regeneration_list.md), 'lint' (lint_errors.txt), or 'json-validate' (schema validation)."
    )
    parser.add_argument(
        "--files", nargs="+", metavar="BASENAME",
        help="Process only these specific basenames."
    )
    parser.add_argument(
        "--group", choices=list("ABCDE"),
        help="Process only one group from regeneration_list.md (requires --source regen)."
    )
    parser.add_argument(
        "--no-resume", action="store_true",
        help="Force reprocessing of files that already pass validation."
    )
    parser.add_argument(
        "--limit", type=int, default=0,
        help="Cap number of files processed (0 = unlimited)."
    )
    parser.add_argument(
        "--dry-run", action="store_true",
        help="Show work list without making API calls."
    )
    parser.add_argument(
        "--sequential", action="store_true",
        help="Use original one-at-a-time mode instead of batch."
    )
    parser.add_argument(
        "--poll-interval", type=int, default=BATCH_POLL_INTERVAL,
        help=f"Batch poll interval in seconds (default: {BATCH_POLL_INTERVAL})."
    )
    args = parser.parse_args()

    setup_logging()

    # --- Build work list ---
    if args.files:
        work_list = [(f, None, []) for f in args.files]
        logging.info("Source: explicit --files (%d)", len(work_list))
    elif args.source == "json-validate":
        work_list = parse_json_validate()
        logging.info("Source: json-validate (%d files with errors)", len(work_list))
    elif args.source == "lint":
        run_linter()
        work_list = parse_lint_errors()
        logging.info("Source: lint_errors.txt (%d files)", len(work_list))
    else:
        work_list = parse_regen_list(group_filter=args.group)
        label = f"group {args.group}" if args.group else "all groups"
        logging.info("Source: regeneration_list.md %s (%d files)", label, len(work_list))

    # --- Resume: filter out files that already pass validation ---
    # On by default — no point re-processing files that already pass.
    # Use --no-resume to force reprocessing.
    if not args.no_resume:
        before = len(work_list)
        filtered = []
        for basename, source_dir, errors in work_list:
            src = find_source_file(basename, source_dir)
            if src is not None:
                if src.suffix == ".json":
                    # JSON source — check schema validation AND compiled lint
                    try:
                        data = json.loads(src.read_text(encoding="utf-8"))
                        validation_errors = validate_json(data)
                        if validation_errors:
                            pass  # needs regeneration
                        else:
                            # Schema OK — also check compiled .pl passes lint
                            pl_content = generate_pl(data)
                            lint_errs, tmp_path = lint_temp(basename, pl_content)
                            tmp_path.unlink(missing_ok=True)
                            if not lint_errs:
                                logging.info("  [skip] %s — .json passes validation + lint in %s",
                                             basename, src.parent.name)
                                continue
                            else:
                                logging.info("  [keep] %s — .json valid but .pl has %d lint error(s)",
                                             basename, len(lint_errs))
                    except (json.JSONDecodeError, Exception):
                        pass  # needs regeneration
                else:
                    # .pl source — check lint
                    existing_errors = lint_file(str(src))
                    if not existing_errors:
                        logging.info("  [skip] %s — already passes lint in %s",
                                     basename, src.parent.name)
                        continue
            filtered.append((basename, source_dir, errors))
        work_list = filtered
        logging.info("Resume filter: %d → %d files", before, len(work_list))

    # --- Apply limit ---
    if args.limit > 0:
        work_list = work_list[: args.limit]
        logging.info("Limit applied: processing %d files", len(work_list))

    # --- Dry run ---
    if args.dry_run:
        mode = "sequential" if args.sequential else "batch"
        print(f"\n{'='*70}")
        print(f"DRY RUN — {len(work_list)} file(s) would be processed ({mode} mode)")
        print(f"{'='*70}\n")
        for i, (basename, source_dir, errors) in enumerate(work_list, 1):
            dir_label = f" ({source_dir}/)" if source_dir else ""
            err_str = f"  [{', '.join(errors)}]" if errors else ""
            print(f"  {i:4d}. {basename}{dir_label}{err_str}")
        print()
        return

    if not work_list:
        logging.info("Nothing to process.")
        return

    # --- Process ---
    mode = "sequential" if args.sequential else "batch"
    logging.info("Processing %d file(s) in %s mode", len(work_list), mode)
    stats = {"passed": 0, "failed": 0, "skipped": 0, "api_errors": 0}
    start_time = time.time()

    if args.sequential:
        run_sequential(work_list, stats)
    else:
        run_batch(work_list, stats, poll_interval=args.poll_interval)

    # --- Summary ---
    elapsed = time.time() - start_time
    logging.info("")
    logging.info("=" * 60)
    logging.info("REGENERATION COMPLETE")
    logging.info("=" * 60)
    logging.info("  Processed: %d", len(work_list))
    logging.info("  Passed:    %d", stats["passed"])
    logging.info("  Failed:    %d (→ probsets/)", stats["failed"])
    logging.info("  Skipped:   %d", stats["skipped"])
    logging.info("  API errors:%d", stats["api_errors"])
    logging.info("  Elapsed:   %.1f min", elapsed / 60)


if __name__ == "__main__":
    main()
