#!/usr/bin/env python3
"""Single source of truth for the corpus identity fingerprint (OQ-29).

Every producer of a result `*.json` stamps `corpus_hash`; every consumer checks
it (flag/raise stale, never read a dead-corpus file as authoritative). The stamp
and the staleness-check MUST compute the compared quantity from one place — two
copies that drift would make a stale file read as current. This module is that
one place.

Consolidated 2026-06-18 from four byte-identical copies (perturb.py:31,
run_pipeline.py:72, census_sweep.py:54, plus the regenerate_orbits/demotion_pass
imports of perturb's copy). See OQ-29.
"""

import hashlib
import json
from pathlib import Path


def compute_corpus_hash(testsets_dir: Path) -> str:
    """sha256 of sorted (filename, file_content) pairs — corpus identity fingerprint.

    Detects both membership changes (add/remove testset) AND in-place content edits.
    Filename-only would miss in-place edits; mtime is cheaper but not git-reproducible.
    Known limit: does not detect changes in testsets/<run_tag>/ subdirs (not loaded by
    corpus_loader). Documented in OQ-29.
    """
    pairs = []
    for p in sorted(Path(testsets_dir).glob("*.pl")):
        pairs.append(p.name + "\n" + p.read_text(encoding="utf-8", errors="replace"))
    return hashlib.sha256("\n---\n".join(pairs).encode()).hexdigest()[:12]


def assert_corpus_current(path: Path, testsets_dir: Path) -> None:
    """Fail-closed staleness guard for a result/orbits JSON (OQ-29, Thread C).

    Raises RuntimeError if the file exists but (a) lacks `corpus_hash`, or (b)
    carries a `corpus_hash` that does not match the current corpus. A missing or
    mismatched field means the file may describe a dead corpus — never read it as
    authoritative (Build Discipline Pattern 5/6: absence/mismatch must fail-closed,
    not pass-open). A non-existent file is not stale; the caller handles absence.
    """
    path = Path(path)
    if not path.exists():
        return
    data = json.loads(path.read_text(encoding="utf-8"))
    stored = data.get("corpus_hash")
    if stored is None:
        raise RuntimeError(
            f"{path.name} has no corpus_hash — cannot verify it against the current "
            "corpus; regenerate it before use (OQ-29)."
        )
    current = compute_corpus_hash(testsets_dir)
    if stored != current:
        raise RuntimeError(
            f"{path.name} is stale: corpus_hash {stored} != current {current}. "
            "It was computed against a different corpus; regenerate before use (OQ-29)."
        )
