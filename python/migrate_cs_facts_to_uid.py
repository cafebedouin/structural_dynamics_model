#!/usr/bin/env python3
"""Phase B: Corpus migration — rekey CS base facts to UUID surrogate identity.

CS base facts currently use constraint_id (C) as first argument. Two stories
sharing the same C merge fact sets. This script replaces C with a UUIDv4
surrogate in every CS base fact and adds cs_story_uid/2 + cs_created_at/2
identity facts to each migrated file.

Execution sequence (strictly ordered, abort+rollback on any failure):
  B0 → Backup verify: existence + readability + FILE COUNT
  B1 → One-file gate: sanctity_reading (Format A) + flat abolition_reading (Format B)
  B2 → Full corpus migration

Rollback rule: non-destructive — mv partial trees, cp backup into place.
Never rm -rf the live corpus before restore completes.

Facts re-keyed (first arg C → UID):
  cs_axiom/3, cs_axiom_grounding/3, cs_drift_state/3, cs_kernel_codification/2,
  cs_authority_grounding/2, cs_interpretation_layer_present/1, cs_reference_frame/2,
  cs_reading_relation/3 (arg 0 only; sibling name in arg 1 unchanged)

NOT changed:
  cs_axiom_status/2, cs_axiom_contradiction/2, cs_kernel_id/2 (axiom/corpus-level)

Run from repo root:
  python3 python/migrate_cs_facts_to_uid.py
"""

import json
import re
import subprocess
import sys
import uuid
from datetime import datetime
from pathlib import Path

REPO       = Path(__file__).resolve().parent.parent
TESTSETS   = REPO / "prolog" / "testsets"
JSON_DIR   = REPO / "json"
PROLOG_DIR = REPO / "prolog"

# CS functors whose first arg (constraint_id) must be replaced with UUID.
CS_FUNCTORS_TO_REKEY = {
    "cs_kernel_codification",
    "cs_authority_grounding",
    "cs_interpretation_layer_present",
    "cs_reading_relation",          # only arg 0; sibling name in arg 1 unchanged
    "cs_axiom",
    "cs_axiom_grounding",
    "cs_reference_frame",
    "cs_drift_state",
}

_FUNCTORS_ALT = "|".join(re.escape(f) for f in CS_FUNCTORS_TO_REKEY)

# Matches: narrative_ontology:FUNCTOR( WHITESPACE ATOM WHITESPACE [,)]
# where ATOM is an unquoted Prolog atom (constraint_id: lowercase + underscores/digits).
# This does NOT match already-migrated facts (first arg would be a quoted UUID).
_REKEY_PAT = re.compile(
    r"(narrative_ontology:(?:" + _FUNCTORS_ALT + r")\s*\(\s*)"
    r"([a-z][a-z0-9_]*)"
    r"(\s*[,)])",
    re.MULTILINE,
)

# Matches the start of a CS fact on its own line (not indented = not in multifile block).
_FIRST_CS_PAT = re.compile(
    r"(?m)^(narrative_ontology:(?:" + _FUNCTORS_ALT + r")\s*\()",
)


# ─────────────────────────────────────────────────────────────────────────────
# Content migration helpers
# ─────────────────────────────────────────────────────────────────────────────

def migrate_content(content: str, old_cid: str, uid_quoted: str) -> str:
    """Replace first argument of all CS facts: old_cid → uid_quoted.

    old_cid: constraint_id atom as it appears in the file (e.g. 'abolition_reading')
    uid_quoted: single-quoted UUID atom (e.g. "'3f2a4b5c-...'")

    Only rewrites facts for old_cid — a file with cs_reading_relation(abolition, retributive, ...)
    will only rekey arg 0 if arg 0 == old_cid, leaving other constraints' facts intact.
    """
    def _replace(m: re.Match) -> str:
        before, found_cid, after = m.group(1), m.group(2), m.group(3)
        if found_cid == old_cid:
            return before + uid_quoted + after
        return m.group(0)

    return _REKEY_PAT.sub(_replace, content)


def add_identity_before_first_cs(content: str, cid: str, uid_quoted: str) -> str:
    """Insert cs_story_uid/2 + cs_created_at/2 before the first CS fact on its own line.

    Insertion before the first CS fact groups the identity facts with the CS block.
    Falls back to appending at end of file if no CS fact found at line-start.

    cs_created_at sentinel '' sorts @< all real ISO timestamps, so migrated
    stories appear older than newly generated ones in the A12 tie-break.
    """
    identity = (
        f"narrative_ontology:cs_story_uid({cid}, {uid_quoted}).\n"
        f"narrative_ontology:cs_created_at({uid_quoted}, '').\n"
    )
    m = _FIRST_CS_PAT.search(content)
    if m:
        return content[: m.start()] + identity + content[m.start() :]
    return content.rstrip("\n") + "\n" + identity


def migrate_pl_file(pl_path: Path, uid: str) -> None:
    """Rewrite pl_path in-place: rekey CS facts, prepend identity facts."""
    cid = pl_path.stem
    uid_quoted = f"'{uid}'"
    content = pl_path.read_text(encoding="utf-8")
    content = migrate_content(content, cid, uid_quoted)
    content = add_identity_before_first_cs(content, cid, uid_quoted)
    pl_path.write_text(content, encoding="utf-8")


# ─────────────────────────────────────────────────────────────────────────────
# JSON write-back
# ─────────────────────────────────────────────────────────────────────────────

def write_uid_to_json(json_path: Path, uid: str) -> bool:
    """Write story_uid to JSON header.story_uid. Returns False if file absent."""
    if not json_path.exists():
        print(f"  Warning: {json_path.relative_to(REPO)} not found (orphaned .pl)")
        return False
    data = json.loads(json_path.read_text(encoding="utf-8"))
    data.setdefault("header", {})["story_uid"] = uid
    json_path.write_text(
        json.dumps(data, indent=2, ensure_ascii=False) + "\n",
        encoding="utf-8",
    )
    return True


def pl_to_json_path(pl_path: Path) -> Path:
    """Map prolog/testsets/[subdir/]name.pl → json/[subdir/]name.json.

    Path-relative (not name-based) to avoid collisions between flat and
    kernel_run_02 instances of the same constraint name.
    """
    rel = pl_path.relative_to(TESTSETS)
    return JSON_DIR / rel.with_suffix(".json")


def is_cs_bearing(pl_path: Path) -> bool:
    """True if the file contains any CS base fact (text presence check)."""
    content = pl_path.read_text(encoding="utf-8")
    return any(
        f"cs_{kw}" in content
        for kw in [
            "kernel_codification", "axiom", "drift_state",
            "authority_grounding", "interpretation_layer_present",
            "reference_frame", "reading_relation",
        ]
    )


# ─────────────────────────────────────────────────────────────────────────────
# B0: Backup and verify
# ─────────────────────────────────────────────────────────────────────────────

def backup_and_verify():
    stamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    ts_backup = TESTSETS.parent / f"testsets_backup_{stamp}"
    j_backup  = JSON_DIR.parent / f"json_backup_{stamp}"

    print(f"  Copying testsets/ → {ts_backup.name}/ ...")
    subprocess.run(["cp", "-r", str(TESTSETS), str(ts_backup)], check=True)
    print(f"  Copying json/ → {j_backup.name}/ ...")
    subprocess.run(["cp", "-r", str(JSON_DIR), str(j_backup)], check=True)

    orig_ts = len([f for f in TESTSETS.rglob("*")  if f.is_file()])
    bkup_ts = len([f for f in ts_backup.rglob("*") if f.is_file()])
    orig_j  = len([f for f in JSON_DIR.rglob("*")  if f.is_file()])
    bkup_j  = len([f for f in j_backup.rglob("*")  if f.is_file()])

    assert bkup_ts == orig_ts, (
        f"testsets backup count mismatch: {orig_ts} orig vs {bkup_ts} backup"
    )
    assert bkup_j == orig_j, (
        f"json backup count mismatch: {orig_j} orig vs {bkup_j} backup"
    )

    # Spot-readability: one file from each backup tree
    next(ts_backup.rglob("*.pl")).read_bytes()
    next(j_backup.rglob("*.json")).read_bytes()

    print(f"  B0 Backup OK: {bkup_ts} testset files, {bkup_j} json files")
    return ts_backup, j_backup


# ─────────────────────────────────────────────────────────────────────────────
# Non-destructive rollback
# ─────────────────────────────────────────────────────────────────────────────

def rollback(ts_backup: Path, j_backup: Path, reason: str = "") -> None:
    """Move partial/failed trees aside, restore from backup.

    Uses mv (not rm -rf) so the partial tree is preserved if restore fails.
    The live corpus is never in a deleted-but-not-restored state.
    Backup copies remain untouched throughout.
    """
    stamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    ts_failed = TESTSETS.parent / f"testsets_failed_{stamp}"
    j_failed  = JSON_DIR.parent / f"json_failed_{stamp}"

    print(f"\nROLLBACK triggered: {reason}")
    print(f"  Moving partial trees:")
    print(f"    testsets/ → {ts_failed.name}/")
    print(f"    json/     → {j_failed.name}/")
    TESTSETS.rename(ts_failed)   # atomic rename on same filesystem
    JSON_DIR.rename(j_failed)
    print("  Restoring from backup (backup copies remain untouched)...")
    subprocess.run(["cp", "-r", str(ts_backup), str(TESTSETS)], check=True)
    subprocess.run(["cp", "-r", str(j_backup),  str(JSON_DIR)],  check=True)
    print("ROLLBACK complete — corpus restored to pre-migration state")


# ─────────────────────────────────────────────────────────────────────────────
# B1: One-file gate (two distinct CS formats)
# ─────────────────────────────────────────────────────────────────────────────

def _swipl_check(goal: str) -> subprocess.CompletedProcess:
    """Run a SWI-Prolog goal from prolog/ directory with 60s timeout."""
    return subprocess.run(
        ["swipl", "-q", "-g", goal, "-t", "halt(1)"],
        capture_output=True, text=True,
        cwd=str(PROLOG_DIR), timeout=60,
    )


def _check_one_file(
    pl_path: Path, json_path: Path,
    ts_backup: Path, j_backup: Path,
) -> None:
    """Migrate pl_path in-place, verify three invariants. Rollback+abort on failure."""
    cid        = pl_path.stem
    uid        = str(uuid.uuid4())
    uid_quoted = f"'{uid}'"

    try:
        migrate_pl_file(pl_path, uid)
        json_written = write_uid_to_json(json_path, uid)

        # Preamble: load dependency modules from prolog/ so that use_module
        # directives inside the testset file are no-ops (already loaded).
        preamble = (
            "use_module(narrative_ontology), "
            "use_module(domain_priors), "
            "use_module(constraint_indexing), "
            f"load_files(['{pl_path}'], []), "
        )

        # Check 1: file is syntactically valid Prolog
        r = _swipl_check(preamble + "halt")
        if r.returncode != 0:
            raise RuntimeError(
                f"Parse check failed (rc={r.returncode}):\n{r.stderr[-500:]}"
            )

        # Check 2: cs_drift_state(UID,_,_) accessible — exercises the REWRITER
        # code path (not just the freshly-added cs_story_uid path).
        # If the rewriter dropped the 'narrative_ontology:' prefix, rewritten facts
        # are invisible and this check fails with a clear error message.
        r = _swipl_check(
            preamble
            + f"(narrative_ontology:cs_drift_state({uid_quoted}, _, _) "
            + f"-> write(drift_ok) ; (write(drift_missing), halt(1))), halt"
        )
        if "drift_ok" not in r.stdout or r.returncode != 0:
            raise RuntimeError(
                f"cs_drift_state({uid[:8]}..., _, _) not found.\n"
                "Likely cause: 'narrative_ontology:' prefix lost in rewrite.\n"
                f"stdout: {r.stdout!r}\nstderr: {r.stderr[-300:]!r}"
            )

        # Check 3: JSON story_uid matches uid (skip when JSON absent — orphaned .pl)
        if json_written:
            data   = json.loads(json_path.read_text(encoding="utf-8"))
            actual = data.get("header", {}).get("story_uid")
            if actual != uid:
                raise RuntimeError(
                    f"JSON story_uid mismatch for {cid}: "
                    f"got {actual!r}, expected {uid!r}"
                )

        print(f"  B1 PASSED ({cid}, uid={uid[:8]}...)")

    except Exception as exc:
        rollback(ts_backup, j_backup, reason=f"B1 failed on {cid}: {exc}")
        sys.exit(1)


def one_file_gate(ts_backup: Path, j_backup: Path) -> None:
    """B1: verify migration on one file per distinct CS fact format.

    Format A (sanctity_reading): has cs_axiom + cs_drift_state (newer format)
    Format B (flat abolition_reading): no cs_axiom, has cs_reading_relation + cs_drift_state

    A read_term/rewrite edge case in Format B (no cs_axiom branch ever exercised)
    would otherwise first surface mid-B2, triggering rollback mid-corpus.
    """
    _check_one_file(
        TESTSETS / "sanctity_reading.pl",
        JSON_DIR  / "sanctity_reading.json",
        ts_backup, j_backup,
    )
    _check_one_file(
        TESTSETS / "abolition_reading.pl",
        JSON_DIR  / "abolition_reading.json",
        ts_backup, j_backup,
    )


# ─────────────────────────────────────────────────────────────────────────────
# B2: Full corpus migration
# ─────────────────────────────────────────────────────────────────────────────

def full_migration(ts_backup: Path, j_backup: Path) -> None:
    pl_files = sorted(TESTSETS.rglob("*.pl"))
    cs_files = [f for f in pl_files if is_cs_bearing(f)]
    print(f"  B2: {len(cs_files)} CS-bearing files found")

    migrated = 0
    skipped  = 0

    for pl_path in cs_files:
        # Skip files already migrated in B1 (or a previous partial run)
        if "cs_story_uid" in pl_path.read_text(encoding="utf-8"):
            skipped += 1
            continue

        uid = str(uuid.uuid4())
        try:
            migrate_pl_file(pl_path, uid)
            write_uid_to_json(pl_to_json_path(pl_path), uid)
            migrated += 1
        except Exception as exc:
            rollback(
                ts_backup, j_backup,
                reason=f"B2 error at {pl_path.relative_to(REPO)}: {exc}",
            )
            sys.exit(1)

    print(f"  B2 complete: {migrated} migrated, {skipped} already done (from B1 gate)")


# ─────────────────────────────────────────────────────────────────────────────
# Main
# ─────────────────────────────────────────────────────────────────────────────

def main() -> None:
    print("=== CS Corpus Migration: Phase B ===\n")

    print("B0: Backup and verify...")
    ts_backup, j_backup = backup_and_verify()

    print("\nB1: One-file gate (Format A: sanctity_reading, Format B: flat abolition_reading)...")
    one_file_gate(ts_backup, j_backup)

    print("\nB2: Full corpus migration...")
    full_migration(ts_backup, j_backup)

    print("\n=== Migration complete ===")
    print(f"Backups retained at:")
    print(f"  {ts_backup}")
    print(f"  {j_backup}")
    print()
    print("Next: run verification checks 6 and 7.")
    print("  Check 6 (dual-load isolation) — from prolog/:")
    print("    swipl -g \"use_module(narrative_ontology), \\")
    print("      consult('testsets/abolition_reading.pl'), \\")
    print("      consult('testsets/kernel_run_02/abolition_reading.pl'), \\")
    print("      findall(U, narrative_ontology:cs_story_uid(abolition_reading,U), UIDs), \\")
    print("      length(UIDs, 2), format('Two UIDs: ~w~n', [UIDs]), halt\" -t \"halt(1)\"")
    print("  Check 7 (pipeline):")
    print("    python3 python/run_pipeline.py")


if __name__ == "__main__":
    main()
