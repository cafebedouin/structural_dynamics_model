"""Canonical repository paths — the single source of truth for filesystem roots.

WHY THIS EXISTS: before this module, 72 scripts re-derived the repo root inline
under four different names (`REPO_ROOT`/`ROOT`/`REPO`/`BASE_DIR`) and four
*depth-dependent* expressions (`.parent.parent`, `.parents[2]`, ...). A script at
the wrong depth silently computes the wrong root, and three files hardcoded an
absolute `/home/scott/...` path that breaks on any other clone. This module ends
that fork: import the root from here, never re-derive it.

ROOT DETECTION is depth-AGNOSTIC: it walks up from this file to the directory
containing `pyproject.toml` (the repo's dedicated, committed, root-unique marker —
robust where `.git` is not, since `.git` is a *file* in git worktrees and absent
in tarball/CI checkouts). The same walk is what nested scripts use to bootstrap
(see USAGE), so there is no `parents[N]` left anywhere to copy wrong.

USAGE
  Top-level `python/foo.py` (run as `python3 python/foo.py`):
      from paths import REPO_ROOT, PROLOG_DIR, OUTPUTS

  Nested `python/audits|sweeps|tests/foo.py` (or any depth, or `python -m`):
  prepend this depth-agnostic bootstrap — it is byte-identical in every file, so
  copy-pasting the wrong neighbor's copy still yields the right path:

      import sys
      from pathlib import Path
      _here = Path(__file__).resolve()
      _root = next(c for c in (_here, *_here.parents) if (c / "pyproject.toml").is_file())
      sys.path.insert(0, str(_root / "python"))
      from paths import REPO_ROOT, PROLOG_DIR, OUTPUTS
"""
from pathlib import Path

_MARKER = "pyproject.toml"


def find_repo_root(start: Path | str | None = None) -> Path:
    """Return the repo root: nearest ancestor of *start* containing the marker.

    Depth-agnostic — correct from any directory depth. Defaults to this file's
    location (so `from paths import REPO_ROOT` is correct wherever paths.py sits).
    """
    here = Path(start if start is not None else __file__).resolve()
    for cand in (here, *here.parents):
        if (cand / _MARKER).is_file():
            return cand
    raise RuntimeError(
        f"repo root not found: no {_MARKER!r} in {here} or any parent "
        f"(this is the dedicated repo-root marker — do not delete it)"
    )


# --- canonical roots (import these; never re-derive) ---
REPO_ROOT = find_repo_root()
PYTHON_DIR = REPO_ROOT / "python"
AGENT_DIR = REPO_ROOT / "agent"
PROLOG_DIR = REPO_ROOT / "prolog"
TESTSETS_DIR = PROLOG_DIR / "testsets"
JSON_DIR = REPO_ROOT / "json"
OUTPUTS = REPO_ROOT / "outputs"
SCHEMAS = REPO_ROOT / "schemas"
PROMPTS = REPO_ROOT / "prompts"
DOCS = REPO_ROOT / "docs"
AUDITS = REPO_ROOT / "audits"

__all__ = [
    "find_repo_root", "REPO_ROOT", "PYTHON_DIR", "AGENT_DIR", "PROLOG_DIR",
    "TESTSETS_DIR", "JSON_DIR", "OUTPUTS", "SCHEMAS", "PROMPTS", "DOCS", "AUDITS",
]
