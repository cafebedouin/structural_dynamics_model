#!/usr/bin/env python3
"""OQ-352: byte-compare the DIRTY pair-arm artifacts against the clean-HEAD re-witness.

Why this is run at all. The dirty artifacts were produced with uncommitted driver
fixes in the tree (`code_dirty: True`), so no commit reconstructs them. Re-running
at clean HEAD produces a reproducible set — but keeping BOTH and diffing them turns
the caveat into a measurement:

  IDENTICAL beyond the known-varying keys  -> POSITIVE EVIDENCE the dirty artifacts
        were substantively sound; the caveat is bookkeeping, not substance.
  DIFFERENT anywhere else                  -> a FINDING about what the uncommitted
        fixes changed, which is information we would have destroyed by overwriting.

Either outcome is informative, which is why the dirty set was preserved rather than
replaced. Known-varying keys are declared here rather than discovered, so a
difference cannot be waved through as "probably just the timestamp":

  manifest.pipeline_run_at   — re-stamped every run by construction
  manifest.code_commit(_short) — the whole point of the re-witness
  manifest.code_dirty        — likewise
  artifact_sha256 / artifact_bytes / corpus_hash in a sidecar — derived, checked
        separately against the artifact they describe

A positive control rides along: the comparator must FLAG a planted byte change, or
its "identical" verdict is untested.
"""
import hashlib, json, sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
DIRTY = Path(sys.argv[1]) if len(sys.argv) > 1 else None
LEGS = ["testsets_sonnet2", "testsets_sonnet3"]
VARY = {"pipeline_run_at", "code_commit", "code_commit_short", "code_dirty"}


def sha(p: Path) -> str:
    h = hashlib.sha256()
    with open(p, "rb") as fh:
        for c in iter(lambda: fh.read(1 << 20), b""):
            h.update(c)
    return h.hexdigest()


def normalize(p: Path):
    """Strip declared-varying keys so a difference elsewhere is a real difference."""
    if p.suffix != ".json":
        return p.read_bytes()
    try:
        d = json.loads(p.read_text(encoding="utf-8"))
    except Exception:
        return p.read_bytes()

    def strip(o):
        if isinstance(o, dict):
            return {k: strip(v) for k, v in o.items() if k not in VARY}
        if isinstance(o, list):
            return [strip(x) for x in o]
        return o
    return json.dumps(strip(d), sort_keys=True).encode()


def compare(label, a: Path, b: Path):
    if not a.exists() or not b.exists():
        return ("MISSING", f"{label}: dirty={a.exists()} clean={b.exists()}")
    if sha(a) == sha(b):
        return ("IDENTICAL_RAW", label)
    if normalize(a) == normalize(b):
        return ("IDENTICAL_NORMALIZED", label)
    return ("DIFFERS", label)


def main():
    if DIRTY is None or not DIRTY.is_dir():
        print("usage: compare_dirty_vs_clean.py <preserved-dirty-dir>", file=sys.stderr)
        return 2
    rows = []
    for leg in LEGS:
        d, c = DIRTY / leg, REPO / "outputs" / "legs" / leg
        names = sorted({p.name for p in d.glob("*")} | {p.name for p in c.glob("*")})
        verdicts = {}
        for n in names:
            if n == "report_corpus.result.json":
                continue          # carries run_at/ledger timings by construction
            verdicts[n] = compare(f"{leg}/{n}", d / n, c / n)
        # TRANSITIVE-VARIANCE RECLASSIFICATION, and the comparator needed it because it
        # over-reported on its first run. A sidecar records `artifact_sha256` — the RAW
        # hash of the artifact it describes. When that artifact itself embeds a run stamp
        # (commentary_census.json carries its own manifest with pipeline_run_at), its raw
        # hash CANNOT be stable even though its content is identical once the declared
        # varying keys are stripped. Such a sidecar differing ONLY in artifact_sha256, over
        # an underlying artifact that is itself IDENTICAL_NORMALIZED, is a transitive
        # consequence of the SAME declared keys — not a substantive difference. Reported as
        # its own class rather than folded into either, so the distinction stays visible.
        for n, (v, lbl) in list(verdicts.items()):
            if v != "DIFFERS" or not n.endswith(".manifest.json"):
                continue
            base = n[:-len(".manifest.json")]
            bv = verdicts.get(base, (None, None))[0]
            if bv not in ("IDENTICAL_NORMALIZED", "IDENTICAL_RAW"):
                continue
            try:
                a = json.loads((d / n).read_text()); b = json.loads((c / n).read_text())
            except Exception:
                continue
            diff_keys = {k for k in set(a) | set(b) if k != "manifest" and a.get(k) != b.get(k)}
            if diff_keys == {"artifact_sha256"}:
                verdicts[n] = ("IDENTICAL_TRANSITIVE",
                               f"{lbl}  (artifact_sha256 only, over an artifact that embeds a run stamp)")
        rows.extend(verdicts.values())
    for leg in LEGS:
        n = f"pipeline_output.{leg[len('testsets_'):]}.json"
        rows.append(compare(n, DIRTY / n, REPO / "outputs" / n))

    for verdict in ("DIFFERS", "MISSING", "IDENTICAL_TRANSITIVE", "IDENTICAL_NORMALIZED", "IDENTICAL_RAW"):
        sel = [r for r in rows if r[0] == verdict]
        if sel:
            print(f"\n{verdict}: {len(sel)}")
            for _, lbl in sel:
                print(f"  {lbl}")

    # --- positive control: the comparator must catch a planted byte change ---
    import tempfile, shutil
    with tempfile.TemporaryDirectory() as td:
        src = next((DIRTY / LEGS[0]).glob("*.md"), None)
        if src:
            p = Path(td) / src.name
            shutil.copy2(src, p)
            p.write_bytes(p.read_bytes() + b"\n% planted\n")
            v, _ = compare("control", src, p)
            ok = v == "DIFFERS"
            print(f"\npositive control (planted byte change): {v} -> "
                  f"{'comparator DISCRIMINATES' if ok else 'UNTESTED — do not trust IDENTICAL'}")
            if not ok:
                return 1
    n_diff = sum(1 for r in rows if r[0] in ("DIFFERS", "MISSING"))
    print(f"\nVERDICT: {len(rows)} artifacts compared, {n_diff} substantive difference(s)")
    print("  => dirty artifacts were SOUND; the code_dirty caveat is bookkeeping"
          if n_diff == 0 else
          "  => the uncommitted fixes CHANGED output — see DIFFERS rows above")
    return 0


if __name__ == "__main__":
    sys.exit(main())
