# audit_log — OQ-120 Phase 0 **v2**

## OPEN stamp (v2)

- **OPEN HEAD:** `f88c8c3cf4040e5994c8c1af11e40dcb6c484cf0`
- **OPEN date:** 2026-08-23
- **v2 PREREGISTRATION.md md5:** `7d0a85d93ae1b9e540ac54d2d4cc4ba7`
- **v1 PREREGISTRATION.md md5:** `b181e1a2a9cd42b86d190be09f61d400` — the genuinely-prior one,
  preserved unedited at `audits/2026-08-21_oq120_epsilon_boundary/`.

**v2's prereg is NOT frozen-before-results and says so on its own face.** v1 completed first and
produced G1b; v2's specification repairs criteria that v1 proved unsatisfiable, vacuous, non-total
or underspecified. Recording the md5 here preserves *this* document's integrity going forward — it
does not, and is not claimed to, confer the ordering property v1's prereg actually has.

**Substrate identical to v1** — 18 live legs at the same file counts, `testsets_nemotron_think`
and `testsets_glm` still empty, HEAD unmoved at `f88c8c3c`. v2's sweep is therefore a
re-derivation under final code plus the new per-stratum quantities. **A transition-level difference
from v1 would be a determinism bug, not a new result** — this doubles as a determinism check on a
fork that was patched mid-flight during v1.

## Gate baseline at v2 OPEN

Carried forward and re-observed: pristine `f88c8c3c` is **GREEN** (28 rows). The v1 directory now
holds a `WRITEUP.md` with its `**Fired:**` line, so v1 no longer reds the gate; **v2's own
directory now does**, for the same transient reason, until v2's WRITEUP lands. Re-observed at CLOSE.

## Assumed substrate

S1–S13 verified first-hand during v1 and unchanged at v2 OPEN (same HEAD, same substrate):
`audits/2026-08-21_oq120_epsilon_boundary/substrate_check.md`. The one row that was FALSE as
written (S8) is re-stated above.

---

## Results (nothing above this line was written after a v2 sweep ran)


## CLOSE stamp

- **CLOSE HEAD:** `6dd69469a74f8e21167ec70ec09d631e017dc283`
- **OPEN HEAD:** `f88c8c3c` — **HEAD MOVED during the window, as the concurrency contract
  predicted.** Intervening commits, both by the concurrent corpus-generation instance:
  - `bdb84ee00` — `testsets_nemotron_think` first pass, 732/1005 stories (thinking-ON
    regime-contrast leg).
  - `bcf2905dc` — its regime-pair audit (off vs on, 728 seeds).
  - plus this audit's own four: `964cb49a8` (evidence), `2bfb41417` (trackers),
    `ee1031515` (RUNS.md), `6dd69469a` (CLAUDE.md line fix).

- **Blast radius on THIS audit's read-set: ZERO — observed, not predicted.**
  `git diff --stat f88c8c3c bcf2905dc` over the engine/config/manifest files the probes read
  (`drl_core.pl`, `config.pl`, `constraint_indexing.pl`, `maxent_classifier.pl`,
  `signature_detection.pl`, `corpus_loader.pl`, `sweeps/epsilon_stability.py`,
  `outputs/pipeline_output.json`) is **empty**, and over all 18 swept legs + `kernel_v1` is
  **empty**. The 1,468 changed files are entirely the new `testsets_nemotron_think` leg and its
  audit directory.
- **`testsets_nemotron_think` was 0 files at BOTH enumerations** (v1 and v2) and was therefore
  correctly excluded from both sweeps by the S5 enumerate-at-execution rule. It now holds 732. A
  future run will pick it up; this one did not measure it and does not claim to.
- **Per-leg count-pair guard: no leg moved during its own sweep**, on either run — the mechanism
  that would have caught an in-flight leg reported clean 19/19 twice.

## Gate at CLOSE — OBSERVED

Re-observed after `WRITEUP.md` landed in both directories. `audit writeup` and `apparatus`,
which were RED at OPEN for the stated transient reason, are now **GREEN**:

- `audit writeup`: `OK (206 dirs, 38 enforced, 0 problems)`
- `apparatus`: `GREEN — catch-rate 30L/2l/0n of 32 bits; ledger 0 open / 29 closed; channel 33/33`

One RED appeared mid-close and was this audit's: `audit cites ERRORS: 1` —
`untracked-frozen-evidence`, v2's `audit_log.md:31` citing
`audits/2026-08-21_oq120_epsilon_boundary/substrate_check.md` while nothing was yet committed. A
frozen-evidence citation that is not tracked would vanish on a fresh clone, so the checker was
right and the fix was to commit, not to re-word. Final gate state recorded below.

**FINAL: `GATE: GREEN`, exit 0, all 28 rows** (transcript: `gate_close.txt`).

## Storage form (2026-08-23) — appended, no finding changed

The bulk artifacts were re-packaged after the first commit pushed two files over GitHub's 100 MB
hard limit (105 MB and 91 MB), blocking a concurrent instance's push. **No number, finding or
conclusion changed** — only how the same bytes are stored:

- `gate_readout.json` keeps its **cited path** and now holds the summary + per-stratum table
  (~3 KB). Its 132,246 per-transition rows moved to `gate_readout_rows.jsonl.gz` beside it, one
  JSON object per line. Both are DERIVED: `analyze.py` regenerates them from `raw/`.
- `raw/tm_*.json` (the primary datum, ~97 MB) is now `raw/tm_sweep_json.tar.gz` — 19 files,
  **verified byte-identical on round-trip extraction**. `sweep_log.txt`, `strata.json` and the
  per-leg `out_*.txt`/`err_*.txt` remain uncompressed.
- The superseded defective run is `raw_PREFIX_double_emission/superseded_run.tar.gz`; its
  `README.md` stays uncompressed so the negative-control provenance is readable without extracting.

Directory totals went 370 MB → 12 MB (v1) and 190 MB → 8.9 MB (v2). The oversized blobs were purged
from the four unpushed commits that carried them.
