# audit_log — OQ-352 per-leg REPORT driver

Chronological execution log. Every counted row is preceded by its stamp.

---

## Step 0 — the OQ-301 gate, EXECUTED not remembered

OQ-352 is `blocked_on OQ-301`. The plan carried a line saying both conditions
pass; that line is exactly the wrong place to hold this fact (the entry read
`partial` earlier in the planning session), so it was re-executed:

```
$ git rev-parse --short HEAD
0c1191239
$ git status --short
                                    (clean)
$ .venv/bin/python python/issues_status.py | grep -E '^OQ-301\b'
OQ-301	resolved
$ git status --short audits/2026-08-17_giant_comp_segv_hang ISSUES.md
                                    (clean — the close is landed, not in-tree)
```

PASS. `resolved` is not in `omega_resolver.ACTIVE` = {open, investigating,
partial}, and the close is committed rather than uncommitted. HEAD equals the
commit the plan's ledger quotation is pinned to, so the quotation did not need
re-extraction — verified by re-reading the OQ-352 fact paragraph at HEAD and
confirming the twelve enumerated tool names verbatim.

"Not reproducible here" != "cannot happen", so the per-stage retry ledger stays
in the design regardless.

## Step 1 — preconditions RE-WITNESSED, not back-filled from prose

The plan asserted P1/P2/P3/P3b/P3c were run during planning; `INVESTIGATIONS.md`
had no line for any of them. Rather than register the plan's prose (a memory
citation), all five were re-derived first-hand this session and registered
retroactively with what was actually observed. Commit `e3424ca7a`.

Raw witnesses, this session:

```
$ ls prolog/testsets_sonnet2/*.pl | wc -l          1003
$ ls prolog/testsets_sonnet3/*.pl | wc -l          1003
$ ls prolog/archives/datasets/original_v6/*.pl | wc -l   3380
$ comm -12 <(cd prolog/testsets_sonnet2 && ls *.pl|sort) \
           <(cd prolog/testsets_sonnet3 && ls *.pl|sort) | wc -l    1002
```

P3b prompt-hash scan, FULL LEG (the plan recorded this had been a 2-file spot
check before):

```
testsets_sonnet2   1003/1003 carry story_provenance   1003 x e03e2210ef39e1af4d109acadf9515e5d2d8b7d7
testsets_sonnet3   1003/1003 carry story_provenance   1003 x e03e2210ef39e1af4d109acadf9515e5d2d8b7d7
original_v6           0/3380 carry story_provenance   0 distinct hashes
```

POSITIVE CONTROL ON THE EXTRACTOR — without it the uniformity claim would be
untested-instrument. The extractor takes odd-position 40-hex tokens as the
prompt hash. If it were position-blind it would report one value; it reports
2006 = 2 x 1003 tokens per leg, and the EVEN positions are a DIFFERENT constant
(`685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28`, the schema hash). It discriminates.

## Step 2 — Commit 1, stage parameterization (`89194672c`)

Corpus FROZEN for the whole witness pair: `prolog/testsets` corpus_hash
`d5325e6856aa`, 285 files, re-checked before and after each run and unchanged.
No `c-orchestrator` or `run_pipeline` process running at start (`pgrep`, clean).

One background writer identified and ruled non-interfering: an hourly
rate-limited probe appending `leg at 732; probe HTTP 429` to
`outputs/no_scope_runs_nemotron_think/auto_resume.log`. It touches only
`testsets_nemotron_think`, which is not a leg this audit uses.

| run | code | exit | wall | pipeline_output.json mtime |
|---|---|---|---|---|
| 1 baseline | pre-change | 0 | 93.4 s | 1787528869 |
| 2 after | post-change | 0 | 461.4 s | 1787529488 (ADVANCED) |
| 3 control | post-change (identical to 2) | 0 | 76.6 s | — |

Exit 0 AND mtime advanced, so the 2026-06-24 false-pass shape (a gate abort
diffing a stale file against itself and reading byte-identical) does not apply.

18 of 331 top-level `outputs/` artifacts differ across the edit. Because 18 != 0
needs an account and only hashes had been snapshotted, run 3 established the
run-to-run NOISE FLOOR at identical code: 19 artifacts differ between two runs of
the same code, and the 18 are a strict SUBSET of that 19 (extra:
`epsilon_stability_results.json`). No base->after change lies outside the noise
the pipeline produces against itself.

Run 2's 461 s is NOT attributable to the change: the slowdown is entirely in
UNTOUCHED Python steps (`reading_linter` 0.2 -> 95.2 s, `omega` 6.9 -> 120.4 s,
`meta_report` 6.9 -> 120.0 s, `manifest_inject` 0.4 -> 42.2 s) while all eleven
Prolog stages stayed flat (1.5-9.7 s), and run 3 at the same code returned to
76.6 s. Machine contention.

DECISIVE INERTNESS WITNESS, because the artifact diff above is an inference and
this is not: a harness stubs `run_prolog`/`subprocess.run` and captures the argv
and goal string each stage emits.
  ARM A  — all 11 stages under default args write only directly under `outputs/`.
  ARM A2 — every default goal literal is present VERBATIM in
           `git show HEAD:python/run_pipeline.py`, checked mechanically against
           the old source rather than against memory.
  ARM B  — under an overlay every goal DIFFERS and every write moves to
           `out_dir` (except giant_comp's documented transit `raw.json`).
  RESULT: PASS — inert under defaults, live under overlay.

## Step 3 — Commits 2 and 3 (`38ae3b062`, `db1d60787`)

Transit set verified by reading all eleven stage `.pl` files rather than the
docs. Three real `open(...write)` sites; four further `../outputs/` strings
(`giant_component_analysis.{log,md}`, `covering_analysis.md`,
`maxent_diag_stderr.txt`) inspected individually and found to be `%` comment
usage-examples. The transit set is exactly three, checked, not recalled.

TWO DEFECTS FOUND BY THE CONTROLS, both recorded rather than quietly adjusted:

1. `stages=[]` fell through a falsy test (`if stages`) and ran ALL ELEVEN stages
   instead of zero — absence wearing the shape of a default. Found by the
   selftest's isolation post-condition, which saw eleven swipl children logged by
   a call that had asked for none. The control earned its keep on its first
   execution. Fixed to `stages is not None`.

2. `outputs/prolog_children.log` is appended by `run_prolog`, a REUSED helper, so
   a real leg run writes under `outputs/` outside `legs/<leg>/`. ENUMERATED in
   the write invariant rather than excluded from the check: suppressing it would
   delete the OQ-301 forensic channel exactly where this driver needs it most
   (~20 `run_giant_component_analysis` executions at n~1000). Surfaced for the
   operator.

The isolation post-condition uses `(size, mtime_ns)`, not sha256, MEASURED not
assumed: `outputs/` is 13,150 files / 3.8 GB, hashing costs ~242 s per manifest
(~8 min per selftest, outside the row's charter) while `stat` costs 0.07 s. Not a
weakening — the question is "did the driver write where it promised not to", and
a write moves mtime even when content is byte-identical, which a hash cannot see.

```
$ .venv/bin/python python/report_legs.py --selftest
  ... 43/43 ...
  [OK  ] ISOLATION: outputs/ + validation_suite.pl byte-identical  no exclusions
  report_legs selftest: 43/43 controls passed          real 0m2.652s
$ ./scripts/gate.sh
  ✓ report legs      report_legs selftest: 43/43 controls passed
  GATE: GREEN
```

## Step 3b — a third defect, found by trying to use the driver

`MISSING_CLASSIFY_OUTPUT` resolved `pipeline_output_<leg>.json`, while every
per-leg classify output on disk is `pipeline_output.<short>.json`. The refusal
would have fired on EVERY leg forever while looking like a working gate. Fixed to
mirror `python/audits/leg_diagnostic_table.py:57-59` exactly — the instrument
OQ-353's `Files:` line says to "extend, do not fork", so the driver must look for
the file that instrument reads. Pinned by four selftest controls plus a literal
check against the consumer's source and a two-sided check against real on-disk
files, so the mapping cannot silently drift.

## Step 4 — PREREGISTRATION frozen

`PREREGISTRATION.md` md5: **`fdaed841b0e33f0212513874b255518e`** — stamped at commit, before the first
counted row.
