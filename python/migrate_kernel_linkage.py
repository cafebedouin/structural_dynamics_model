"""ONE-TIME MIGRATION: bucket and (partially) link ~83 unlinked stories in flat prolog/testsets/.

These stories were written by the old commitment_corpus/generate_kernel_corpus.py, which
called save_story() (flat main corpus) without _kernel_id injection.  The canonical
agent/generate_kernel_corpus.py writes to run-tagged subdirs and injects _kernel_id; this
script handles the legacy flat files.

Three buckets:
  (A) *_contradictions.pl  — kernel contradiction-analysis, NOT readings.
      Action: stamp cs_contradiction_of(story_atom, kernel_atom) by stem-match.
              Add multifile declaration for narrative_ontology:cs_contradiction_of/2.
              Idempotent — skip if fact already present.

  (B) *_reading.pl (or any file with cs_story_uid) where the manifest says it belongs
      to a contested kernel but cs_kernel_id is absent.
      Action: PRINT WORKLIST ONLY — do not auto-edit.  Human confirms before linking.

  (C) Everything else — candidate standalones or files with insufficient manifest evidence.
      Action: LIST ONLY.

Run:
    python3 python/migrate_kernel_linkage.py
    python3 python/migrate_kernel_linkage.py --dry-run      # bucket only, no edits even for A
"""
import argparse
import json
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
TESTSETS_DIR = REPO_ROOT / "prolog" / "testsets"
MANIFESTS_ROOT = REPO_ROOT / "outputs" / "kernel_manifests"


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _extract_cs_fact(pl_text, functor, cid):
    """Return first positional arg of narrative_ontology:functor(cid, <ARG>)., or None."""
    m = re.search(
        rf"narrative_ontology:{re.escape(functor)}\({re.escape(cid)},\s*'?([^')]+)'?\)",
        pl_text,
    )
    return m.group(1).strip() if m else None


def _has_cs_fact(pl_text, functor, cid):
    return bool(re.search(
        rf"narrative_ontology:{re.escape(functor)}\({re.escape(cid)}[,)]",
        pl_text,
    ))


def _has_cs_contradiction_of(pl_text, cid):
    return bool(re.search(
        rf"narrative_ontology:cs_contradiction_of\({re.escape(cid)},",
        pl_text,
    ))


# ---------------------------------------------------------------------------
# Build constraint_id → kernel_id map from all manifests
# ---------------------------------------------------------------------------

def build_constraint_map():
    """Walk all *.manifest.json in outputs/kernel_manifests/ subdirs.

    Returns {constraint_id: (kernel_id, run_tag)} for every contested kernel reading.
    """
    cmap = {}
    for run_dir in sorted(MANIFESTS_ROOT.iterdir()):
        if not run_dir.is_dir():
            continue
        run_tag = run_dir.name
        for mf in run_dir.glob("*.manifest.json"):
            try:
                m = json.loads(mf.read_text(encoding="utf-8"))
            except Exception as e:
                print(f"  WARNING: cannot parse {mf}: {e}", file=sys.stderr)
                continue
            csr = m.get("commitment_system_recognition", {}) or {}
            if not csr.get("is_contested_kernel"):
                continue
            kernel_id = csr.get("kernel_id")
            if not kernel_id:
                continue
            for axis in m.get("generation_sequence", []):
                if isinstance(axis, dict):
                    cid = axis.get("claim_id") or axis.get("constraint_id")
                    if cid:
                        # Last writer wins — newer runs take precedence
                        cmap[cid] = (kernel_id, run_tag)
    return cmap


# ---------------------------------------------------------------------------
# Bucket A edit: stamp cs_contradiction_of
# ---------------------------------------------------------------------------

def stamp_contradiction_of(pl_path, story_atom, kernel_atom, dry_run=False):
    """Insert cs_contradiction_of fact and (if absent) multifile declaration.

    Insertion point: immediately after the cs_story_uid line, or at end of file
    if cs_story_uid is absent.  Returns True if file was (or would be) changed.
    """
    text = pl_path.read_text(encoding="utf-8")

    if _has_cs_contradiction_of(text, story_atom):
        return False  # already present

    fact_line = (
        f"narrative_ontology:cs_contradiction_of({story_atom}, {kernel_atom})."
    )
    multifile_line = ":- multifile narrative_ontology:cs_contradiction_of/2."

    # Insert multifile declaration if missing
    needs_multifile = multifile_line not in text

    # Find insertion point: after cs_story_uid line
    uid_match = re.search(
        rf"(narrative_ontology:cs_story_uid\({re.escape(story_atom)},[^\n]+\n)",
        text,
    )
    if uid_match:
        insert_after = uid_match.end()
        insert_block = fact_line + "\n"
        new_text = text[:insert_after] + insert_block + text[insert_after:]
    else:
        # No cs_story_uid — append at end
        new_text = text.rstrip("\n") + "\n" + fact_line + "\n"

    # Prepend multifile declaration near top (after last existing :- multifile block)
    if needs_multifile:
        last_mf = list(re.finditer(r":- multifile\b[^\n]+\n(?:    [^\n]+\n)*", new_text))
        if last_mf:
            ins = last_mf[-1].end()
            new_text = new_text[:ins] + multifile_line + "\n" + new_text[ins:]
        else:
            # Prepend after module declaration or at top
            mod_match = re.search(r":- module\([^\n]+\n", new_text)
            ins = mod_match.end() if mod_match else 0
            new_text = new_text[:ins] + multifile_line + "\n" + new_text[ins:]

    if not dry_run:
        pl_path.write_text(new_text, encoding="utf-8")
    return True


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--dry-run", action="store_true",
                    help="report all three buckets but write nothing (not even bucket A)")
    args = ap.parse_args()

    print("Building constraint map from all manifests...")
    cmap = build_constraint_map()
    print(f"  {len(cmap)} contested constraint_ids found across all runs")

    # Collect flat .pl files (not in subdirs — subdirs are run-tagged staging areas)
    flat_pls = sorted(p for p in TESTSETS_DIR.glob("*.pl") if p.is_file())
    print(f"  {len(flat_pls)} flat .pl files in prolog/testsets/\n")

    bucket_a = []   # (*_contradictions.pl)
    bucket_b = []   # orphaned readings
    bucket_c = []   # candidate standalones / unknowns

    for pl_path in flat_pls:
        stem = pl_path.stem
        text = pl_path.read_text(encoding="utf-8")
        has_uid = _has_cs_fact(text, "cs_story_uid", stem)
        has_kid = _has_cs_fact(text, "cs_kernel_id", stem)
        has_contra = _has_cs_contradiction_of(text, stem)

        if stem.endswith("_contradictions"):
            kernel_atom = stem[: -len("_contradictions")]
            bucket_a.append((pl_path, stem, kernel_atom, has_contra))
        elif has_uid and not has_kid:
            if stem in cmap:
                kernel_id, run_tag = cmap[stem]
                bucket_b.append((stem, kernel_id, run_tag))
            else:
                bucket_c.append((stem, "no manifest entry"))
        elif not has_uid and not has_kid:
            bucket_c.append((stem, "no cs facts at all"))
        # else: has_uid and has_kid → already linked; skip silently

    # -----------------------------------------------------------------------
    # Bucket A — auto-edit
    # -----------------------------------------------------------------------
    print("=" * 60)
    print("BUCKET A: Contradictions files (auto-edited)")
    print("=" * 60)
    a_stamped = a_skipped = 0
    for pl_path, story_atom, kernel_atom, already_done in bucket_a:
        if already_done:
            print(f"  SKIP  {story_atom}  (cs_contradiction_of already present)")
            a_skipped += 1
            continue
        changed = stamp_contradiction_of(pl_path, story_atom, kernel_atom, dry_run=args.dry_run)
        tag = "DRY-RUN" if args.dry_run else "STAMPED"
        print(f"  {tag}  {story_atom}  →  cs_contradiction_of(_, {kernel_atom})")
        if changed:
            a_stamped += 1
    print(f"\n  Total: {a_stamped} stamped, {a_skipped} already done\n")

    # -----------------------------------------------------------------------
    # Bucket B — worklist only
    # -----------------------------------------------------------------------
    print("=" * 60)
    print("BUCKET B: Orphaned readings (HAND-CONFIRM before editing)")
    print("=" * 60)
    for stem, kernel_id, run_tag in bucket_b:
        print(f"  {stem}")
        print(f"    likely kernel: {kernel_id}  [run: {run_tag}]")
    print(f"\n  Total: {len(bucket_b)} orphaned readings\n")

    # -----------------------------------------------------------------------
    # Bucket C — eyeball list
    # -----------------------------------------------------------------------
    print("=" * 60)
    print("BUCKET C: Candidate standalones / unknowns (eyeball, no edit)")
    print("=" * 60)
    for stem, reason in bucket_c:
        print(f"  {stem}  ({reason})")
    print(f"\n  Total: {len(bucket_c)} candidates\n")

    print("=" * 60)
    print(f"Summary: A={a_stamped} stamped, B={len(bucket_b)} worklist, C={len(bucket_c)} eyeball")
    if args.dry_run:
        print("(DRY RUN — no files written)")


if __name__ == "__main__":
    main()
