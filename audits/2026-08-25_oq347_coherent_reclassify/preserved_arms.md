# Preserved §9 arms — OQ-345 backfill diff (preserved 2026-08-25, before the coherent reclassify)

The coherent reclassify (this audit) overwrites `outputs/pipeline_output.<leg>.json` on adoption.
`backfill_diff.py` pinned its AFTER arm to exactly those paths, so all four §9 arm files are
preserved here BEFORE anything writes. Blobs: `outputs/_arms_oq345_2026-08-25/*.gz` (gitignored —
this file is the durable committed half). Roundtrip md5 verified identical at preservation time.

## The §9 pin — the leg→commit mapping, explicit

**§9 (OQ-342 step 3's writeup) is pinned to these artifacts, NOT to whatever the coherent
reclassify produces:**

| §9 arm | file | code_commit_short | code_dirty | run_at | md5 |
|---|---|---|---|---|---|
| haiku AFTER | `pipeline_output.haiku.json` | **`0f432fb`** | True | 2026-08-22T17:57:21Z | `01a97bc3d20d1acf49bf8d1f776260c9` |
| flash AFTER | `pipeline_output.flash.json` | **`2ce8e18`** | False | 2026-08-22T20:18:53Z | `cc6a005eae7c68cc0bb3a7f7a86ce902` |
| haiku BEFORE | `pipeline_output.haiku.prebackfill.json` | `7597aa7` | False | 2026-08-22T14:42:04Z | `aed219cd10a40b044bdbbdc98751c347` |
| flash BEFORE | `pipeline_output.flash.prebackfill.json` | `f0ef08a` | True | 2026-08-21T22:59:12Z | `79d2a55ef73e0341523642d9f117f158` |

All four: `n_constraints` 960, `n_stories` 960.

The committed diffs `audits/2026-08-22_oq345_stakeholder_backfill/backfill_diff_{haiku,flash}_2026-08-22.txt`
were computed on the AFTER arms above. Any future §9 run must pass these preserved copies as the
AFTER path (`backfill_diff.py --after`, parameterized in this session) — reading the live
`outputs/pipeline_output.<leg>.json` post-adoption would silently compare against the coherent
reclassify instead (the S19 defect, second file).

## Declared fresh-clone gap

The `.gz` blobs live under gitignored `outputs/`. On a fresh clone they are gone; the documented
rebuild path is: check out `522def40^` (the pre-backfill corpus state) and re-run the leg
classifies for the BEFORE arms; the AFTER arms rebuild from the post-backfill corpus at
`0f432fb` / `2ce8e18` respectively. This is a declared gap, not an accident: 67 MB of JSON does
not belong in git, and the md5s above make any rebuild checkable.
