# Audit log — 2026-08-25_gauge_fixed_prediction

## HEAD stamp pair (audits/README.md, operator ruling 2026-08-17)

| Event | HEAD | When |
|---|---|---|
| OPEN  | `c0ed4b3b2fc86af8b48753679a17f2dec33d36f1` | before any probe ran, at substrate verification |
| CLOSE | `c0ed4b3b2fc86af8b48753679a17f2dec33d36f1` | after the last probe run |

**IDENTICAL.** No intervening commit; no blast-radius diff to compute. Four other `claude`
processes were live during the run, but none landed a commit, and `git status --porcelain`
showed only this audit's own paths dirty. This is DETECTION, not prevention (no lock file
exists) — the clean result is a checked fact, distinct from never having looked.

## No PREREGISTRATION.md — and why

Deliberate, per the plan's own subtraction pass: the read-off was stated in the plan text before
the run rather than in a separate frozen artifact, so there is no md5 to record. The three
firing-domain outcomes and which one is a stop were written down before the first `swipl`
invocation and are quoted verbatim in WRITEUP.md §1.

## Run order (each line written after the run above it completed)

1. Substrate verification (read-only) — 23 plan lines checked, 3 pin drifts found.
2. `audits/INVESTIGATIONS.md` line opened — BEFORE the first probe run.
3. `gauge_fixed_prediction_probe.pl` over the live leg → `rows.tsv` (1140 rows).
   First run emitted `~t` column-fill instead of tabs; probe fixed and re-run. Both runs
   produced identical stderr counters, so the defect was formatting only.
4. `analysis.txt`, `analysis_detail.txt` — derived from `rows.tsv`, never from a loop counter.
5. `fixture_run.pl` over the scratch fixture corpus → `fixture_rows.tsv` (16 rows).
6. Suppression-fallback exposure probe (live leg, then `testsets_kimi`).

## Corpus stamp

Live leg at run time: **258 stories / 27 axiom_contradiction / 285 members**, counted live from
`corpus_loader`'s own census line, not recalled. Cross-check: `outputs/pipeline_output.json`
manifest `n_stories: 258`, `n_constraints: 285`, `pipeline_run_at 2026-08-25T02:36:02Z`.
Second leg used for the exposure check only: `testsets_kimi`, 1005 stories.
