#!/usr/bin/env python3
"""OQ-60 C-LATENT witness driver: classify_corpus on testsets + flash.

Usage: python3 clatent_witness.py <phase>   # phase in {base, edit}
Runs serialized (shared raw artifact). Prints corpus md5 fingerprints so the
base/edit pair can assert no corpus drift between runs.
"""
import sys, os, subprocess, hashlib, glob

REPO = "/home/scott/bin/structural_dynamics_model"
sys.path.insert(0, os.path.join(REPO, "python"))
os.chdir(REPO)

def corpus_md5(leg):
    files = sorted(glob.glob(os.path.join(REPO, "prolog", leg, "*.pl")))
    h = hashlib.md5()
    for f in files:
        h.update(open(f, "rb").read())
    return len(files), h.hexdigest()

phase = sys.argv[1]
assert phase in ("base", "edit")

for leg in ("testsets", "testsets_flash"):
    n, d = corpus_md5(leg)
    print(f"[corpus-fingerprint] {leg} n={n} md5={d}", flush=True)

from run_pipeline import classify_corpus

m1 = classify_corpus("testsets", f"oq60_clatent_{phase}_testsets.json", None)
print(f"[done] testsets manifest: n={m1.get('n_constraints')} commit={m1.get('code_commit_short')} dirty={m1.get('code_dirty')}", flush=True)

m2 = classify_corpus("testsets_flash", f"oq60_clatent_{phase}_flash.json", "gemini-2.5-flash")
print(f"[done] flash manifest: n={m2.get('n_constraints')} commit={m2.get('code_commit_short')} dirty={m2.get('code_dirty')}", flush=True)

for leg in ("testsets", "testsets_flash"):
    n, d = corpus_md5(leg)
    print(f"[corpus-fingerprint-post] {leg} n={n} md5={d}", flush=True)
