#!/usr/bin/env python3
"""precision_probe.py — measure the census's FALSE-POSITIVE direction.

Motivation (operator, 2026-08-18): 10 of 11 directories added since 2026-08-10 landed
in the census numerator — 91% against a 42% base rate. Two mechanisms with opposite
consequences: the population genuinely changed (defect-hunting month), or the prose
quotes the lexicon because the project spent the month writing ABOUT silent failure.

The census (Appendix B 5.4) is a keyword proxy. Its RECALL failures are measured
(5.4 defects 2 and 3). Its PRECISION has never been measured. This probe measures one
specific precision failure mode: a HYGIENE PHRASE. Project prose routinely says
"recorded rather than silently dropped" / "declared, not silently applied" — sentences
describing the AUTHOR'S OWN reporting discipline, not a defect the audit found. Those
lines satisfy the census keyword and carry no defect.

A directory is scored HYGIENE-ONLY when EVERY one of its matching lines is a hygiene
form. That is a conservative false-positive estimate: a directory with one genuine hit
and nine hygiene hits scores genuine.

This is a proxy over a proxy and is labelled as such. It bounds; it does not correct.
"""
import re, subprocess, sys, pathlib

CENSUS = r'for its whole life\|never fired\|never ran\|read.*0 for\|was never\|silently'

# Hygiene forms: the author describing their own recording discipline.
HYGIENE = re.compile(
    # A CONTRASTIVE marker within two words of "silent(ly)". This is the whole
    # discriminator: hygiene prose says "X rather than silently Y" / "declared, not
    # silently applied" / "listed, never dropped silently". A defect report says
    # "the value was silently dropped" with no contrastive marker.
    r'(rather\s+than|instead\s+of|and\s+not|,\s*not|not|never)'
    r'(\s+\w+){0,2}\s+silent(ly)?\b',
    re.I)

# The FIRST version of this regex also listed bare verb forms
# (silently deleted|dropped|rewritten|...). Its own DECLINE control caught it: the
# corpus line "the value was silently dropped" is a genuine defect report and the
# regex fired on it. The verb list is removed; the contrastive marker is the only
# discriminator. Recorded rather than silently fixed - and note the recursion, since
# that sentence is itself a hygiene form this probe now scores.

def hits(d: pathlib.Path):
    out = subprocess.run(
        ["bash","-c", f"/usr/bin/grep -rn '{CENSUS}' --include='*.md' {d}/"],
        capture_output=True, text=True)
    return [l for l in out.stdout.splitlines() if l.strip()]

def main():
    frame = pathlib.Path(sys.argv[1])
    dirs = [l.strip() for l in frame.read_text().splitlines() if l.strip()]
    hygiene_only, genuine, empty = [], [], []
    for d in dirs:
        h = hits(pathlib.Path("audits")/d)
        if not h:
            empty.append(d); continue
        if all(HYGIENE.search(l) for l in h):
            hygiene_only.append((d, len(h)))
        else:
            genuine.append((d, len(h)))

    print(f"numerator members scanned : {len(dirs)}")
    print(f"  with no hits (BUG if >0) : {len(empty)}  {empty}")
    print(f"  HYGIENE-ONLY             : {len(hygiene_only)}")
    print(f"  at least one non-hygiene : {len(genuine)}")
    print()
    print("HYGIENE-ONLY directories (candidate census false positives):")
    for d, n in sorted(hygiene_only):
        print(f"  {d}  ({n} hit{'s' if n!=1 else ''})")

    # --- POSITIVE CONTROL on the hygiene regex, two-sided ---------------------
    print()
    print("CONTROL — the hygiene regex must FIRE on known hygiene lines and DECLINE on")
    print("known defect lines. Both drawn from the corpus, not authored for this test.")
    fire = [
        "Cells below n=50 were excluded and are listed, never dropped silently: scaffold 24",
        "Recorded, not silently skipped.",
        "marked on close, not silently rewritten",
        "Recorded rather than silently deleted, for two reasons. It is instance seven",
        "## 9. AMENDMENT 1 - scorer repaired mid-run (declared, not silently applied)",
    ]
    decline = [
        "the value was silently dropped.** A truncated-but-plausible field value that parses",
        "an artifact stratum that grows silently inside a denominator gets WORSE while reading",
        "It stayed undetected across 151 commits touching one or",
        "The pass that the power floor governs never ran.",
        "the formalization axis silently degenerates.",
        "can never fire and its `textual_combined` sum is silently mountain-only",
        "prevalence surface silently reading 0",
        "produced by a query that **silently failed to dispatch** the lock clauses",
    ]
    bad = 0
    for l in fire:
        ok = bool(HYGIENE.search(l))
        print(f"  FIRE    {'ok  ' if ok else 'FAIL'} {l[:72]}")
        bad += 0 if ok else 1
    for l in decline:
        ok = not HYGIENE.search(l)
        print(f"  DECLINE {'ok  ' if ok else 'FAIL'} {l[:72]}")
        bad += 0 if ok else 1
    print()
    if bad:
        print(f"CONTROL FAILED ({bad} of {len(fire)+len(decline)}) — the hygiene proxy does not")
        print("discriminate and its numbers may not be cited.")
        return 1
    print(f"CONTROL PASSED — {len(fire)} fire, {len(decline)} decline, two-sided.")
    return 0

sys.exit(main())
