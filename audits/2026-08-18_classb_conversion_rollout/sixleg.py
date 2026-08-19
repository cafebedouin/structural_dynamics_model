#!/usr/bin/env python3
"""sixleg.py — the six-leg clean-vs-edited pair harness.

Runs run_pipeline.classify_corpus over the five live legs plus the kernel_v1 breadth
archive, into per-leg outputs that never touch the canonical pipeline_output.json.

GATES, because a pair that did not actually run reads byte-identical and is a FALSE PASS
(swipl_load_path_and_probe_gotchas.md §5):
  * per leg, the output file's mtime must ADVANCE past a marker taken before the run;
  * per leg, the corpus is md5-FINGERPRINTED before and after — an operator topic run
    landing stories mid-session has been witnessed twice, and it silently invalidates a
    pair by changing the substrate between halves;
  * fingerprints from the clean half are compared against the edited half by the differ,
    not just recorded.

timeout=1800 / soft_timeout=900: the classify_corpus default of 300 s is sized on the live
leg (~35 s) and is NOT enough for the 960-1106 file legs — the first attempt at this pair
refused on testsets_flash after three full-length attempts. soft_timeout keeps a genuine hang
caught early and retried rather than sitting for the full ceiling.

expected_model is None for every leg deliberately: a leg's model is not its directory name
(OQ-78), and this is a same-leg PAIR, so the discriminator is the diff, not the fingerprint.
The other classify_corpus refusals (zero-glob, load-completeness, raw freshness,
seen==classified) all still apply.

Usage:  sixleg.py clean | edited          then:  sixleg.py diff
"""
from __future__ import annotations

import hashlib
import json
import subprocess
import sys
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent
REPO = HERE.parent.parent
sys.path.insert(0, str(REPO / "python"))

LEGS = [
    ("testsets", "testsets"),
    ("testsets_haiku", "testsets_haiku"),
    ("testsets_flash", "testsets_flash"),
    ("testsets_kimi", "testsets_kimi"),
    ("testsets_sonnet", "testsets_sonnet"),
    ("kernel_v1", "archives/datasets/kernel_v1"),
]
OUTPUTS = REPO / "outputs"


def fingerprint(corpus_rel: str) -> tuple[str, int]:
    d = REPO / "prolog" / corpus_rel
    files = sorted(d.glob("*.pl"))
    h = hashlib.md5()
    for f in files:
        h.update(f.name.encode())
        h.update(hashlib.md5(f.read_bytes()).digest())
    return h.hexdigest(), len(files)


def run_phase(phase: str) -> int:
    from run_pipeline import classify_corpus  # noqa: E402

    record: dict[str, dict] = {}
    for name, rel in LEGS:
        out_name = f"oq303b_{name}_{phase}.json"
        out_path = OUTPUTS / out_name
        fp_before, n_before = fingerprint(rel)
        marker = time.time()
        out_path.unlink(missing_ok=True)
        t0 = time.time()
        try:
            manifest = classify_corpus(rel, out_name, None,
                                       timeout=1800, soft_timeout=900)
        except Exception as e:  # refusals are loud, and they end the phase
            print(f"  {name}: REFUSED — {type(e).__name__}: {e}", flush=True)
            return 1
        elapsed = time.time() - t0
        if not out_path.exists():
            print(f"  {name}: RED — {out_name} not written", flush=True)
            return 1
        mtime = out_path.stat().st_mtime
        if mtime <= marker:
            print(f"  {name}: RED — output mtime did not advance "
                  f"({mtime} <= {marker}); the diff would compare a stale file "
                  f"against itself", flush=True)
            return 1
        fp_after, n_after = fingerprint(rel)
        if (fp_before, n_before) != (fp_after, n_after):
            print(f"  {name}: RED — corpus CHANGED during the run "
                  f"({n_before}->{n_after} files); the pair is invalid", flush=True)
            return 1
        record[name] = {"corpus_md5": fp_after, "n_files": n_after,
                        "n_constraints": manifest.get("n_constraints"),
                        "code_commit": manifest.get("code_commit"),
                        "code_dirty": manifest.get("code_dirty"),
                        "output": out_name, "seconds": round(elapsed, 1)}
        print(f"  {name}: ok  n={manifest.get('n_constraints')}  "
              f"corpus_md5={fp_after[:12]}  {elapsed:.1f}s", flush=True)
    (HERE / f"sixleg_{phase}_manifest.json").write_text(json.dumps(record, indent=1))
    print(f"sixleg {phase}: all {len(LEGS)} legs ok", flush=True)
    return 0


def strip_manifest(doc: dict) -> dict:
    """per_constraint only. The manifest re-stamps pipeline_run_at every run, so a
    whole-file cross-run diff ALWAYS differs even when behaviour is preserved."""
    return doc.get("per_constraint", doc)


def diff() -> int:
    clean_m = json.loads((HERE / "sixleg_clean_manifest.json").read_text())
    edit_m = json.loads((HERE / "sixleg_edited_manifest.json").read_text())
    problems, lines = [], []
    for name, _rel in LEGS:
        c, e = clean_m.get(name), edit_m.get(name)
        if not c or not e:
            problems.append(f"{name}: missing a half ({bool(c)}/{bool(e)})")
            continue
        if c["corpus_md5"] != e["corpus_md5"]:
            problems.append(
                f"{name}: corpus md5 differs BETWEEN halves "
                f"({c['corpus_md5'][:12]} vs {e['corpus_md5'][:12]}) — the pair compares "
                f"two different substrates and witnesses nothing")
            continue
        if c["code_commit"] == e["code_commit"] and c["code_dirty"] == e["code_dirty"]:
            problems.append(
                f"{name}: both halves report the same code state "
                f"({c['code_commit'][:8]}, dirty={c['code_dirty']}) — the edited half may "
                f"not have picked up the edit")
        a = strip_manifest(json.loads((OUTPUTS / c["output"]).read_text()))
        b = strip_manifest(json.loads((OUTPUTS / e["output"]).read_text()))
        keys = sorted(set(a) | set(b))
        changed = [k for k in keys if a.get(k) != b.get(k)]
        lines.append(f"{name:16} n={len(keys):5}  changed={len(changed)}")
        if changed:
            lines.append(f"    first 10: {changed[:10]}")
            for k in changed[:3]:
                av, bv = a.get(k), b.get(k)
                if isinstance(av, dict) and isinstance(bv, dict):
                    sub = sorted(set(av) | set(bv))
                    d = [f"{s}: {av.get(s)!r} -> {bv.get(s)!r}" for s in sub
                         if av.get(s) != bv.get(s)]
                    lines.append(f"      {k}: " + "; ".join(d[:6]))
    print("\n".join(lines))
    (HERE / "sixleg_diff.txt").write_text("\n".join(lines) + "\n")
    if problems:
        for p in problems:
            print(f"  {p}")
        print("sixleg diff: RED")
        return 1
    total_changed = sum(int(l.split("changed=")[1]) for l in lines if "changed=" in l)
    print(f"sixleg diff: {total_changed} changed constraint(s) across "
          f"{len(LEGS)} legs")
    return 0


if __name__ == "__main__":
    cmd = sys.argv[1] if len(sys.argv) > 1 else ""
    if cmd in ("clean", "edited"):
        sys.exit(run_phase(cmd))
    if cmd == "diff":
        sys.exit(diff())
    sys.exit("usage: sixleg.py clean|edited|diff")
