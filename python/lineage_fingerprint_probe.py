#!/usr/bin/env python3
"""Per-constraint six-dim fingerprint dump + multiset reproduction diff (OQ-71 Step 5).

Serialization is READ OFF the salvaged original artifact
(audits/2026-06-04_oq71_depth_lineage/v5_sixdim.txt), not assumed: one line per constraint,

    six(shift(P,M,I,A),Props,Voids,actors(B,V),drift(E,S,T),zone(EZ,SZ))

emitted via format/2 ~w of the terms returned by logical_fingerprint's
fingerprint_shift/2 .. fingerprint_zone/2 (term shapes witnessed at
logical_fingerprint.pl:113,297,321,353; props/voids are list-valued).

The 5-dim STRUCTURAL class (OQ-71 H1/H2) = the line with the shift(...) field
dropped; shift is its own 1-dim space. Both derive from the same dump.

Usage:
  python3 python/lineage_fingerprint_probe.py --corpus archives/prolog_v5 \
      --out audits/2026-06-04_oq71_depth_lineage/v5_repro.txt \
      --out-tagged audits/2026-06-04_oq71_depth_lineage/v5_repro_tagged.tsv \
      --diff audits/2026-06-04_oq71_depth_lineage/v5_sixdim.txt

  --corpus      path relative to prolog/ (swipl cwd), overlaid onto
                config:param(corpus_path,...) BEFORE [stack] (perturb.py pattern)
  --diff FILE   multiset-compare the bare output against FILE (order-insensitive);
                exact match or per-line excess/deficit report
"""
import argparse
import subprocess
import sys
import tempfile
from collections import Counter
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
PROLOG_DIR = REPO_ROOT / "prolog"

# Non-corpus engine demos asserted by [stack] itself (constraint_instances.pl), so
# known_constraint/1 finds them under ANY corpus_path overlay. The v5 reproduction
# run (2026-06-04) witnessed exactly this: 3,381 dumped vs 3,380 corpus, the +1
# localized to catholic_church_1200 — the same known +1 CLAUDE.md documents for
# pipeline per_constraint. Excluded by name, with this provenance.
EXCLUDE_NON_CORPUS = {"catholic_church_1200"}

OVERLAY = """\
:- asserta(config:param(corpus_path, '{corpus}')).
:- [stack].
:- use_module(logical_fingerprint).
:- corpus_loader:load_all_testsets,
   findall(C0, logical_fingerprint:known_constraint(C0), Cs0),
   sort(Cs0, Cs),
   forall(member(C, Cs),
     ( catch(
         ( logical_fingerprint:fingerprint_shift(C, S),
           logical_fingerprint:fingerprint_properties(C, P),
           logical_fingerprint:fingerprint_voids(C, V),
           logical_fingerprint:fingerprint_actors(C, A),
           logical_fingerprint:fingerprint_drift(C, D),
           logical_fingerprint:fingerprint_zone(C, Z),
           format('DATA\\t~w\\tsix(~w,~w,~w,~w,~w,~w)~n', [C, S, P, V, A, D, Z])
         ),
         Error,
         format(user_error, 'ERROR on ~w: ~w~n', [C, Error])
       )
     )),
   halt.
:- halt(1).
"""


def run_dump(corpus: str):
    with tempfile.NamedTemporaryFile("w", suffix=".pl", dir=PROLOG_DIR,
                                     delete=False) as f:
        f.write(OVERLAY.format(corpus=corpus))
        ovl = Path(f.name)
    try:
        proc = subprocess.run(
            ["swipl", "-q", str(ovl.name)],
            cwd=PROLOG_DIR, capture_output=True, text=True, timeout=3600)
    finally:
        ovl.unlink()
    tagged = []
    for line in proc.stdout.splitlines():
        if line.startswith("DATA\t"):
            _, cid, six = line.split("\t", 2)
            if cid in EXCLUDE_NON_CORPUS:
                continue
            tagged.append((cid, six))
    errors = [l for l in proc.stderr.splitlines() if l.startswith("ERROR")]
    return tagged, errors, proc


def multiset_diff(ours, theirs_path):
    theirs = Counter(l.strip() for l in Path(theirs_path).read_text().splitlines()
                     if l.strip())
    mine = Counter(ours)
    missing = theirs - mine   # in original, not reproduced
    extra = mine - theirs     # reproduced, not in original
    return missing, extra, sum(theirs.values()), sum(mine.values())


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--corpus", default="testsets")
    ap.add_argument("--out", required=True)
    ap.add_argument("--out-tagged", default=None)
    ap.add_argument("--diff", default=None)
    args = ap.parse_args()

    tagged, errors, proc = run_dump(args.corpus)
    print(f"corpus={args.corpus}  constraints dumped: {len(tagged)}  "
          f"errors: {len(errors)}")
    for e in errors[:5]:
        print(f"  {e}")
    if not tagged:
        print("--- swipl stderr tail ---")
        print("\n".join(proc.stderr.splitlines()[-10:]))
        sys.exit(1)

    bare = [six for _, six in tagged]
    Path(args.out).write_text("\n".join(bare) + "\n", encoding="utf-8")
    print(f"wrote {args.out}")
    if args.out_tagged:
        Path(args.out_tagged).write_text(
            "\n".join(f"{c}\t{s}" for c, s in tagged) + "\n", encoding="utf-8")
        print(f"wrote {args.out_tagged}")

    if args.diff:
        missing, extra, n_orig, n_mine = multiset_diff(bare, args.diff)
        print(f"\nREPRODUCTION DIFF vs {args.diff}")
        print(f"  original lines: {n_orig}   reproduced lines: {n_mine}")
        print(f"  missing (in original, not reproduced): {sum(missing.values())}")
        print(f"  extra   (reproduced, not in original): {sum(extra.values())}")
        for label, c in (("MISSING", missing), ("EXTRA", extra)):
            for line, n in list(c.items())[:3]:
                print(f"  {label} x{n}: {line[:140]}")
        if not missing and not extra:
            print("  VERDICT: EXACT MULTISET MATCH — binning faithful")
        else:
            print("  VERDICT: MISMATCH — HALT depth-arm readout until localized "
                  "(OQ-71 gate; 'probably term-ordering' is not acceptance)")
            sys.exit(2)


if __name__ == "__main__":
    main()
