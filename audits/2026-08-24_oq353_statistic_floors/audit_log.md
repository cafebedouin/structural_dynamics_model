# OQ-353 Phase 1 — execution log

**PRE-REGISTRATION md5: `f960db2c4f6fd3be7491b33c063b814f`** (`PREREGISTRATION.md`, logged here before any counted row,
per the OQ-301 precedent).

Freeze-time state: HEAD `e01951de21d96a4ac5d777bd64a9adea8704b2a5`, `git status --short` empty,
`python/run_pipeline.py` sha256 `c23d3208a995adcadab913056af9cef1b27d5cdecd16ae78` (full value
in PREREGISTRATION.md §0), 2026-08-24T16:35:36Z.

## Order of execution (the order is the point — each check's ledger line opened BEFORE it ran)

| # | step | outcome |
|---|---|---|
| 0 | plan-review run row landed (`2026-08-23-2`) | `grep -cF` = 1, `tail -2` shows no merged line |
| 1 | **C1** line opened, then v6 loaded at HEAD | Fired: **live** — loads clean; seal vacuous |
| 2 | C1 three-arm positive control | fires / declines / silent-on-v6-shape |
| 3 | **C2** line opened, then density + feasibility | Fired: **live** — feasibility changes the winner |
| 4 | **C3** line opened, then strip built and loaded | Fired: **live** — reconciles exactly; F5 corrected |
| 5 | **C4** line opened, then arm (c) built and loaded | Fired: **no** — passes both sides |
| 6 | commit `ee8934a6a` — preconditions | — |
| 7 | Verification 0 baseline captured (pre-refactor) | md5 `9f8bb4d4…` / `61d72ae1…` |
| 8 | instrument refactored, selftest written | 30 checks GREEN |
| 9 | Verification 0 re-run, roster pinned to the same 19 legs | **both TSVs byte-identical** |
| 10 | gate row `oq353 floors` wired | — |
| 11 | full gate run | 2 reds, both diagnosed two-sided and fixed |
| 12 | commit `e01951de2` — instrument | — |
| 13 | **PREREGISTRATION.md frozen and md5'd** | `c4d9f7ce5d196c5f6588df78541e247c` |

## Counted rows

**None.** Phase 1 runs no arms. Every B cell in `outputs/leg_diagnostic_verdicts.tsv` reads
`NOT_MEASURED`; the five `giant_comp` rows read `PENDING OQ-356`. The exposure counts (12/56
vintage, 15/56 mixture) are **registry declarations, not measurements**, and they carry their
coverage: 44/56 and 41/56 are `UNTRACED`, so they are a floor on exposure, never a census.
