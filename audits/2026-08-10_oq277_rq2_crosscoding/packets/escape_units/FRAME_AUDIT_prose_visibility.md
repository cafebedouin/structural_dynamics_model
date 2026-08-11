# FRAME AUDIT — what the census proxy could never see

**Executed:** 2026-08-11, by the escape extractor, on operator instruction ("audit it, and audit it
before the row is read").
**Scope:** the 101 non-census directories in the frozen frame — the population the escape check
samples from. Not a claim about any directory's contents beyond file visibility.
**Frame pin:** `frame/non_census_dirs.txt`, md5 `ecc91562c0888aeb246d90fa6dd56da2`, matching
`frame_manifest.txt` (repo commit `8d7e5aba`, 174 dirs, 73 / 101 split). The listing I read is
byte-identical to the frozen one; the census would be meaningless otherwise.
**Evidence:** `frame_audit_prose_census.py`, `frame_audit_nonmd_recall.py` (both in this directory,
both self-controlling — they exit non-zero if their controls fail).

## The proxy, quoted from the frame builder

```sh
grep -rl 'for its whole life\|never fired\|never ran\|read.*0 for\|was never\|silently' \
  --include='*.md' audits/
```

Two filters do the work, and each has its own blind spot: **`--include='*.md'`** decides which files
are ever opened, and the six patterns decide what counts once opened. They fail independently, so I
measured them separately.

## Result 1 — 4 of 101 directories are UNSEEABLE (zero `.md` anywhere, recursive)

| directory | file types present |
|---|---|
| `2025-02-23_gap_tests` | `.pl` |
| `2026-06-04_oq71_depth_lineage` | `.json` `.py` `.tsv` `.txt` |
| `2026-06-12_gate_partial_fix` | `.txt` |
| `2026-07-24_oq153_update_authority_step2` | `.pl` |

These can be neither a hit nor a miss: the proxy never opens a file in them. Per the operator's
2026-08-11 ruling they are a **third category — outside the frame**, not in-frame NO-UNITs. The
effective population is **97, not 101**.

Two-sided control, both directions, all PASS: three directories known to carry `.md` classify
seeable (including one whose only `.md` files are inside `outputs*/` subdirectories — `grep -r`
recurses, so it is genuinely seeable); the two directories I read myself and know carry no `.md`
classify unseeable. A census that could only fire one way would witness nothing.

## Result 2 — 12 of 101 carry the proxy's OWN keywords in files it never opens

Same six patterns, same grep, one filter changed from `--include='*.md'` to `--exclude='*.md'`:

```
2026-06-04_oq65_bait_census                1 file   .jsonl
2026-06-05_generation_pipeline_deleak      1 file   .txt
2026-06-11_oq93_grid_migration             1 file   .py
2026-06-13_oq122_retype_discriminator      1 file   .pl
2026-06-13_oq50_power_scaling_residue      1 file   .pl
2026-06-21_oq35_field_counterfactual       1 file   .pl
2026-07-01_oq197_acceptance_controls       1 file   .pl
2026-07-23_oq232_falsifier_redesign        1 file   .txt
2026-07-27_cross_author_epsilon_probe      1 file   .json
2026-08-03_oq258_referent_discriminator   28 files  .json .pl
2026-08-06_oq259_item2_tframework          4 files  .json .log
2026-08-09_oq151_dual_gauge                2 files  .pl .txt
```

**What this is:** 12 directories the proxy's own patterns would have flagged had its file filter
been wider. **What this is NOT:** 12 incidents. A keyword in a code comment is exactly the false
positive the proxy has everywhere else; this measures the filter, not the finding. No judgement of
any kind entered this count.

Two-sided control, both PASS: my grep reproduces the proxy exactly on the other stratum (73/73
incident-bearing directories hit), and finds **zero** `.md` hits across the 101 — which is what the
partition asserts, and is the check that would have caught me running a different grep than the
frame did.

## Result 3 — the two blind spots are independent, witnessed on one directory

The escape unit extracted from `2026-06-10_signature_liveness_crosscorpus` reports its incident in a
`.out` file, in prose that uses **none** of the six patterns ("a clean read identical to a read that
never looked" — near-miss on `never ran`/`never fired`, matched by neither). That directory is
seeable (it has an `.md`), is not among the 12, and still carries a reported incident the proxy
missed. So widening the file filter would not have caught it, and widening the keyword list would
not have caught the 12. Three defect classes, not one:

1. **unseeable directory** — mechanical, 4/101, needs no judgement
2. **file-filter blind spot** — mechanical, 12/101, needs no judgement
3. **keyword blind spot proper** — prose in an opened `.md` using none of the six patterns. This is
   what most of the six escape candidates are, and it is the only class that needs the threshold
   probe, because it is the only one where "is this an incident" is a judgement rather than a grep.

## What this does to the numbers

**The escape sample drew 2 of the 4 unseeable directories.** Hypergeometric P(≥2 of 4 in a draw of
8 from 101) = **0.031**; expected count was 0.32. The seed was fixed before extraction and I did not
choose the frame, so this is an unlucky draw rather than selection — but it means my slice is
enriched about six-fold for unseeable directories and cannot be extrapolated to the other 14.

**Consequence for my row, stated because it cuts against me, not for me:** with the 4 excluded, my
two NO-UNITs are *both* out-of-frame. In-frame, the escape result is **6 candidates from 6
directories**, not 6 from 8. Removing the unseeable directories does not soften the finding — it
removes the only two directories that were making it look softer.

**Consequence for the null licence.** The stated bound, ≤12.7% at 95%, is 1 − 0.05^(1/22) and is
computed on n = 22. Every unseeable directory in the full escape sample must come out of that n:

| effective n | bound | change |
|---|---|---|
| 22 (as stated) | 12.7% | — |
| 21 | 13.3% | +0.6 |
| 20 | 13.9% | +1.2 |
| 18 | 15.3% | +2.6 |
| 17 | 16.2% | +3.4 |

Expected unseeable in a draw of 22 is 0.87, so in expectation the loosening is under a point — but
expectation is not the count, and my own slice already ran six-fold over expectation. **The count
must be done over all 22 drawn directories, not extrapolated from my 8.** That is the same
`frame_audit_prose_census.py` run against the full escape sample, and I cannot run it: I have not
seen the other 14 and should not.

Two riders. First, the bound is moot while six candidates are proposed — it prices the null, and
the null is not currently the live hypothesis. Second, the direction is one-way: a weaker frame
makes the bound **looser** than stated, which makes a null **less** reassuring, never more.

## The class this belongs to

Third arrival in the same statistic's frame, after the empty untracked directory and the positional
parse: a count that is correct arithmetic over a population that quietly excludes cases it cannot
represent. The tell is identical each time — the defect appears when you count what the rule
*produces* rather than read what the rule *says*. Both results here were found by running the
frame's own command with one argument changed, which is the cheapest form that check ever takes.

**Fired:** live — the frame's effective population changed (101 → 97), the escape row's in-frame
denominator changed (8 → 6), and 12 directories were identified as reachable by the proxy's own
patterns under a wider filter.
