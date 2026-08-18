#!/usr/bin/env python3
"""leg_sweep.py — one half of the clean-vs-edited witness pairs (Phase 4).

Usage: python3 audits/2026-08-17_bound_dispatch_hardening/leg_sweep.py clean|edited

For each of the six legs (five live + kernel_v1 archive): md5-fingerprint the leg dir
BEFORE and AFTER the run (operator topic runs can land stories mid-session — a drifted
fingerprint voids the pair), time classify_corpus, write outputs/bdh_<half>_<leg>.json.
Serialized by construction (shared pipeline_output.raw.json). expected_model per the
leg's story_provenance (OQ-78: model is not the directory name; the fingerprint refusal
inside classify_corpus is the check).
"""
import hashlib
import json
import subprocess
import sys
import time
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(REPO / "python"))
import run_pipeline as rp  # noqa: E402

HALF = sys.argv[1]
assert HALF in ("clean", "edited"), "arg must be clean|edited"

LEGS = [
    ("testsets",        "testsets",                     None),  # mixed-model live leg
    ("testsets_haiku",  "testsets_haiku",               "claude-haiku-4-5"),
    ("testsets_flash",  "testsets_flash",               "gemini-2.5-flash"),
    ("testsets_kimi",   "testsets_kimi",                "kimi-k2.6"),
    ("testsets_sonnet", "testsets_sonnet",              "claude-sonnet-5"),
    ("kernel_v1",       "archives/datasets/kernel_v1",  None),  # mixed archive
]


def dir_fingerprint(rel):
    d = REPO / "prolog" / rel
    h = hashlib.md5()
    files = sorted(d.glob("*.pl"))
    for p in files:
        h.update(p.name.encode())
        h.update(hashlib.md5(p.read_bytes()).digest())
    return f"{len(files)}files:{h.hexdigest()}"


results = {}
for name, path, model in LEGS:
    fp_before = dir_fingerprint(path)
    out = f"bdh_{HALF}_{name}.json"
    t0 = time.monotonic()
    rp.classify_corpus(path, out, model)
    dt = time.monotonic() - t0
    fp_after = dir_fingerprint(path)
    n = json.load(open(REPO / "outputs" / out))["manifest"]["n_constraints"]
    row = dict(leg=name, seconds=round(dt, 2), n=n,
               fp_before=fp_before, fp_after=fp_after,
               fp_stable=(fp_before == fp_after))
    results[name] = row
    print(f"[{name:16}] {dt:8.2f}s n={n:5} fp_stable={row['fp_stable']}", flush=True)

outfile = REPO / f"audits/2026-08-17_bound_dispatch_hardening/leg_sweep_{HALF}.json"
git_head = subprocess.run(["git", "rev-parse", "HEAD"], cwd=REPO,
                          capture_output=True, text=True).stdout.strip()
git_dirty = bool(subprocess.run(["git", "status", "--porcelain", "prolog/drl_core.pl",
                                 "prolog/signature_detection.pl"], cwd=REPO,
                                capture_output=True, text=True).stdout.strip())
json.dump(dict(half=HALF, head=git_head, engine_files_dirty=git_dirty, legs=results),
          open(outfile, "w"), indent=2)
print(f"WROTE {outfile.name} head={git_head[:8]} engine_dirty={git_dirty}")
